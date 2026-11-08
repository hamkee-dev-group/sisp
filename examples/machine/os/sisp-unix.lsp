; ============================================================
; SISP/UNIX V3 — a real preemptive multitasking OS
;
;   DISK -> fs -> kernel -> init -> shell -> user programs
;
; Everything below the shell line runs as V2.1 machine code on
; the CPU from ../hardware/cpu.lsp.  What is real here:
;
;   * Context switch: the full 16-register file is saved to and
;     restored from the process table on every switch.  Each
;     process owns a 512-word physical memory window; the MMU
;     base/limit registers follow the dispatched process.
;   * Preemption: the timer IRQ vectors to a *machine-code* ISR
;     which counts quanta and, when the slice expires, issues a
;     reschedule hypercall (sys 15).  The kernel unspins the
;     exact ISR frame from the kernel stack (3 saved regs +
;     PC/FLG) when parking the interrupted process.
;   * fork: copies the parent's whole 512-word window and
;     register image; child sees fork return 0, parent sees the
;     child pid.  exec replaces a process image by name.  wait
;     blocks a parent until a child exits and delivers the exit
;     code.  Zombies are reaped by wait, or automatically when
;     the parent is init or the hosted shell.
;   * Syscalls trap out of the CPU (VM exit): exit, yield,
;     fork, exec, wait, write, read (blocking kbd lines, fed by
;     a machine-code keyboard ISR through a 16-entry ring),
;     fcreate/fwrite/fread/flist/fdelete on the sector disk,
;     kill.
;   * Filesystem: superblock + inode table + block bitmaps + a
;     flat root directory, persisted sector-by-sector through
;     the disk controller ports.  Unmount + remount proves the
;     data lives on the platter, not in kernel variables.
;   * The shell is hosted (not machine code) but goes through
;     the same kernel entry points as machine-code programs.
;
; Memory map (8192 words):
;   0-31    interrupt vectors        [0]=reset [2]=timer [4]=kbd
;   40-43   kernel cells: TICKS QUANTUM KBD-H KBD-T
;   48-63   keyboard ring (16 entries)
;   64-159  kernel machine code (timer ISR @64, kbd ISR @96)
;   448-511 kernel stack (grows down from 511)
;   512+    15 process windows of 512 words
;
; Syscall ABI: sys n, args in R1 R2 R3, result in R0.
; ============================================================

(load "examples/machine/hardware/cpu.lsp")
(setq MEM 8192)
(define nl (code-char 10))

(print "")
(print "=========================================================")
(print "  SISP/UNIX V3 — Booting")
(print "=========================================================")

; ---- kernel layout constants ---------------------------------
(define KSTACK 511)          ; kernel stack top (grows down)
(define TICKS-CELL 40) (define QUANTUM-CELL 41)
(define KBDH-CELL 42) (define KBDT-CELL 43) (define KBD-RING 48)
(define TIMER-ISR 64) (define KBD-ISR 96)
(define WIN-SIZE 512) (define WIN-BASE 512) (define NSLOTS 15)
(define (slot-base s) (+ WIN-BASE (* s WIN-SIZE)))

(define (iota n) (labels (((go i acc) (cond ((= i n) (reverse acc)) (t (go (+ i 1) (cons i acc)))))) (go 0 nil)))
(define (str-join parts sep)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (cat (car parts) sep) (str-join (cdr parts) sep)))))
(define (rep s n) (cond ((= n 0) "") (t (cat s (rep s (- n 1))))))
(define (nl-join xs) (str-join (map (lambda (s) (cat s nl)) xs) ""))
; split a string at every occurrence of character ch
(define (split-on s ch)
  (labels (((go cs cur acc)
      (cond ((null cs)
             (cond ((= (length cur) 0) (reverse acc))
                   (t (reverse (cons cur acc)))))
            ((equal (car cs) ch)
             (go (cdr cs) "" (cons cur acc)))
            (t (go (cdr cs) (cat cur (car cs)) acc)))))
    (go (str-to-list s) "" nil)))
(define (split-lines s)
  (filter (lambda (l) (> (length l) 0)) (split-on s (code-char 10))))

; ============================================================
; PROCESS TABLE
; proc = (pid ppid state slot regs waitfor exitcode cmd)
;   state:  ready | blocked | zombie | hosted
;   regs:   list of 16 words — the full saved register file
;   waitfor: nil | kbd | (wait pid) with pid 0 = any child
; ============================================================
(define *cpu* nil) (define *procs* nil) (define *curpid* 0) (define *nextpid* 1)

(define (p-id p) (nth 0 p)) (define (p-ppid p) (nth 1 p))
(define (p-state p) (nth 2 p)) (define (p-slot p) (nth 3 p))
(define (p-regs p) (nth 4 p)) (define (p-waitfor p) (nth 5 p))
(define (p-exitcode p) (nth 6 p)) (define (p-cmd p) (nth 7 p))
(define (p-set p i v) (set-car (drop p i) v))
(define (p-reg p r) (nth r (p-regs p)))
(define (p-set-reg p r v) (set-car (drop (p-regs p) r) v))
(define (state-str s)
  (cond ((eq s 'ready) "ready") ((eq s 'blocked) "blocked")
        ((eq s 'zombie) "zombie") ((eq s 'hosted) "hosted")
        (t "?")))

(define (proc-find pid) (find (lambda (p) (= (p-id p) pid)) *procs*))

; ---- slot allocator ------------------------------------------
(define *slots* nil)   ; list of NSLOTS booleans, t = in use
(define (slots-init) (setq *slots* (map (lambda (x) nil) (iota NSLOTS))))
(define (slot-alloc)
  (labels (((go s rest)
      (cond ((null rest) nil)
            ((car rest) (go (+ s 1) (cdr rest)))
            (t (set-car rest t) s))))
    (go 0 *slots*)))
(define (slot-free s)
  (cond ((numberp s) (set-car (drop *slots* s) nil))))

; ---- spawn / image loading ------------------------------------
(define (regs-fresh slot entry)
  (let ((rs (map (lambda (x) 0) (iota 16))))
    (set-car (drop rs PCI) (+ (slot-base slot) entry))
    (set-car (drop rs FLG) 4)          ; user mode, interrupts on
    (set-car (drop rs SPI) KSTACK)
    (set-car (drop rs USP) (- WIN-SIZE 1))
    (set-car (drop rs IMSK) 6)         ; timer + keyboard unmasked
    rs))

(define (proc-add pid ppid state slot regs cmd)
  (let ((p (list pid ppid state slot regs nil 0 cmd)))
    (setq *procs* (append *procs* (list p)))
    p))

(define (k-spawn-image name ppid)
  (let ((img (assoc name *images* (quote equal))) (slot (slot-alloc)))
    (cond ((or (null img) (null slot)) nil)
          (t (load-words *cpu* (slot-base slot) (cadr img))
             (let ((pid *nextpid*))
               (setq *nextpid* (+ *nextpid* 1))
               (p-id (proc-add pid ppid 'ready slot
                               (regs-fresh slot 0) name)))))))

; ---- context switch -------------------------------------------
; Save the whole register file into the process image.  A plain
; park just reads the registers out; a preempt park (sys 15 from
; the timer ISR) first unspins the 5-word ISR frame left on the
; kernel stack: R2 R1 R0 FLG PC (top to bottom).
(define (park-regs p)
  (p-set p 4 (map (lambda (r) (rr *cpu* r)) (iota 16))))

; NB: sisp folds identifiers to lowercase, so a local named `flg`
; would capture the global FLG register index — keep them distinct.
(define (park-preempt p)
  (let ((r2 (k-pop *cpu*)) (r1 (k-pop *cpu*)) (r0 (k-pop *cpu*))
        (flags (k-pop *cpu*)) (pc (k-pop *cpu*)))
    (park-regs p)
    (p-set-reg p 0 r0) (p-set-reg p 1 r1) (p-set-reg p 2 r2)
    (p-set-reg p FLG flags) (p-set-reg p PCI pc)
    (rw *cpu* SPI KSTACK)))

; ---- scheduler -------------------------------------------------
(define (k-pick)
  (labels (((scan n)
      (cond ((= n 0) nil)
            ((null *procs*) nil)
            ((eq (p-state (car *procs*)) 'ready)
             (let ((p (car *procs*)))
               (setq *procs* (append (cdr *procs*) (list p)))
               p))
            (t (setq *procs* (append (cdr *procs*) (list (car *procs*))))
               (scan (- n 1))))))
    (scan (length *procs*))))

(define (k-done-p)
  (null (find (lambda (p) (and (not (= (p-id p) 1))
                               (or (eq (p-state p) 'ready)
                                   (eq (p-state p) 'blocked))))
              *procs*)))

(define (k-pickable-p)
  (find (lambda (p) (eq (p-state p) 'ready)) *procs*))

; One scheduler step: dispatch the next ready process, park it,
; then service whatever stopped it (halt / syscall / fault /
; quantum hypercall).
(define (k-step)
  (let ((p (k-pick)))
    (cond ((null p) nil)
          (t (setq *curpid* (p-id p))
             (map (lambda (r) (rw *cpu* r (p-reg p r))) (iota 16))
             (setq mmu-base (slot-base (p-slot p)))
             (setq mmu-limit WIN-SIZE)
             (run1 *cpu* 100000 0)
             (let ((trap (rr *cpu* FLT)))
               (cond ((= trap 271) (park-preempt p))
                     (t (park-regs p)))
               (p-set-reg p FLT 0)
               (rw *cpu* FLT 0)
               (cond ((halt-p *cpu*) (k-exit p 0))
                     ((>= trap 256) (k-syscall p (- trap 256)))
                     ((> trap 0) (k-fault p trap))
                     (t nil)))))))

; hook called once per scheduling iteration (used by cron, netd)
(define *k-tick-hook* nil)
(define (k-run fuel)
  (cond ((= fuel 0) (print "[K] slice budget exhausted (runaway?)"))
        ((k-done-p) (print "[K] all user processes finished"))
        (t (k-step)
           (k-reap-scan)
           (k-unblock-scan)
           (cond (*k-tick-hook* (funcall *k-tick-hook*)))
           (cond ((k-done-p) (print "[K] all user processes finished"))
                 ((null (k-pickable-p)) (print "[K] deadlock: everything blocked"))
                 (t (k-run (- fuel 1)))))))

; ---- exit / reap / fault ---------------------------------------
(define (k-exit p code)
  (p-set p 2 'zombie)
  (p-set p 6 code)
  (print (str-join (list "[K] pid " (num-to-str (p-id p))
                         " (" (p-cmd p) ") exit " (num-to-str code)) ""))
  (slot-free (p-slot p))
  ; orphan children -> init
  (map (lambda (c) (cond ((= (p-ppid c) (p-id p)) (p-set c 1 1))))
       *procs*)
  (k-try-deliver p))

(define (k-try-deliver z)
  ; if z's parent waits for it (or any), deliver and reap; init
  ; and the hosted shell reap their zombies automatically
  (let ((parent (proc-find (p-ppid z))))
    (cond ((and (not (null parent))
                (or (= (p-id parent) 1)
                    (eq (p-state parent) 'hosted)))
           (k-reap z))
          ((and (not (null parent))
                (eq (p-state parent) 'blocked)
                (consp (p-waitfor parent))
                (eq (car (p-waitfor parent)) 'wait)
                (or (= (cadr (p-waitfor parent)) 0)
                    (= (cadr (p-waitfor parent)) (p-id z))))
           (p-set parent 2 'ready)
           (p-set parent 5 nil)
           (p-set-reg parent 0 (p-id z))
           (p-set-reg parent 1 (p-exitcode z))
           (k-reap z))
          (t nil))))

(define (k-reap z)
  (setq *procs* (remove (lambda (p) (= (p-id p) (p-id z))) *procs*)))

(define (k-reap-scan)
  (map (lambda (z) (cond ((eq (p-state z) 'zombie) (k-try-deliver z))))
       *procs*))

(define (k-fault p f)
  (print (str-join (list "[K] pid " (num-to-str (p-id p))
                         " fault " (num-to-str f) " (killed)") ""))
  (k-exit p (+ 128 f)))

; ---- keyboard ring (fed by the kbd ISR) -------------------------
(define (kbd-line-len)
  ; chars available up to and including the first newline, else 0
  (let ((h (rd *cpu* KBDH-CELL)) (tl (rd *cpu* KBDT-CELL)))
    (labels (((go i n)
        (cond ((= i tl) 0)
              ((= (rd *cpu* (+ KBD-RING i)) 10) (+ n 1))
              (t (go (mod (+ i 1) 16) (+ n 1))))))
      (go h 0))))

(define (kbd-take-line buf-phys maxlen)
  ; move min(line-len, maxlen) chars from the ring to buf-phys
  (let ((n (cond ((< (kbd-line-len) maxlen) (kbd-line-len)) (t maxlen))))
    (labels (((go i)
        (cond ((= i n) n)
              (t (let ((h (rd *cpu* KBDH-CELL)))
                   (wr *cpu* (+ buf-phys i) (rd *cpu* (+ KBD-RING h)))
                   (wr *cpu* KBDH-CELL (mod (+ h 1) 16))
                   (go (+ i 1)))))))
      (go 0))))

(define (k-unblock-scan)
  (map (lambda (p)
         (cond ((and (eq (p-state p) 'blocked) (eq (p-waitfor p) 'kbd)
                     (> (kbd-line-len) 0))
                (p-set p 2 'ready)
                (p-set p 5 nil)
                (p-set-reg p 0 (kbd-take-line (u-addr p (p-reg p 2))
                                               (p-reg p 3))))))
       *procs*))

; ---- user memory access (through the process window) -----------
(define (u-addr p va) (+ (slot-base (p-slot p)) va))
(define (u-range-ok-p va len) (and (>= va 0) (<= (+ va len) WIN-SIZE)))
(define (u-read-n p va n)
  (labels (((go i acc) (cond ((= i n) (reverse acc))
                             (t (go (+ i 1) (cons (rd *cpu* (u-addr p (+ va i))) acc))))))
    (go 0 nil)))
(define (u-write-n p va bytes)
  (labels (((go i bs) (cond ((null bs) i)
                            (t (wr *cpu* (u-addr p (+ va i)) (car bs))
                               (go (+ i 1) (cdr bs))))))
    (go 0 bytes)))
(define (u-read-str p va)
  (labels (((go i acc)
      (let ((w (rd *cpu* (u-addr p (+ va i)))))
        (cond ((or (= w 0) (> i 64)) (list-to-str (reverse acc)))
              (t (go (+ i 1) (cons (code-char w) acc)))))))
    (go 0 nil)))

; ---- syscalls ---------------------------------------------------
(define (k-syscall p n)
  (cond ((= n 0) (k-exit p (p-reg p 1)))
        ((= n 1) nil)                                   ; yield
        ((= n 2) (k-sys-fork p))
        ((= n 3) (p-set-reg p 0 (k-exec p (u-read-str p (p-reg p 1)))))
        ((= n 4) (k-sys-wait p (p-reg p 1)))
        ((= n 5) (k-sys-write p))
        ((= n 6) (k-sys-read p))
        ((= n 7) (p-set-reg p 0 (kf-create (u-read-str p (p-reg p 1)))))
        ((= n 8) (p-set-reg p 0
                   (kf-write-bytes (u-read-str p (p-reg p 1))
                                   (u-read-n p (p-reg p 2) (p-reg p 3)))))
        ((= n 9) (k-sys-fread p))
        ((= n 10) (k-sys-flist p))
        ((= n 11) (p-set-reg p 0 (k-kill (p-reg p 1))))
        ((= n 12) (p-set-reg p 0 (kf-delete (u-read-str p (p-reg p 1)))))
        ((= n 15) nil)                                  ; reschedule (ISR)
        (t (print (str-join (list "[K] bad syscall " (num-to-str n)) ""))
           (k-exit p 139))))

(define (k-sys-fork p)
  (let ((slot (slot-alloc)))
    (cond ((null slot) (p-set-reg p 0 (- WORD 1)))
          (t (let ((pb (slot-base (p-slot p))) (cb (slot-base slot)))
               (labels (((go i) (cond ((= i WIN-SIZE) 0)
                                      (t (wr *cpu* (+ cb i) (rd *cpu* (+ pb i)))
                                         (go (+ i 1))))))
                 (go 0))
               (let ((child-regs (append (p-regs p) nil)))
                 (set-car (drop child-regs 0) 0)
                 ; the child resumes in its own window: relocate the
                 ; physical PC from the parent's window to the child's
                 (set-car (drop child-regs PCI)
                          (+ cb (- (nth PCI (p-regs p)) pb)))
                 (let ((pid *nextpid*))
                   (setq *nextpid* (+ *nextpid* 1))
                   (proc-add pid (p-id p) 'ready slot child-regs (p-cmd p))
                   (p-set-reg p 0 pid))))))))

(define (k-exec p name)
  (let ((img (assoc name *images* (quote equal))))
    (cond ((null img)
           (print (str-join (list "[K] exec: " name ": not found") "")) 1)
          (t (labels (((go i) (cond ((= i WIN-SIZE) 0)
                                    (t (wr *cpu* (u-addr p i) 0) (go (+ i 1))))))
               (go 0))
             (load-words *cpu* (slot-base (p-slot p)) (cadr img))
             (p-set p 4 (regs-fresh (p-slot p) 0))
             (p-set p 7 name)
             0))))

(define (k-sys-wait p pid)
  (let ((z (find (lambda (c) (and (eq (p-state c) 'zombie)
                                  (= (p-ppid c) (p-id p))
                                  (or (= pid 0) (= (p-id c) pid))))
                 *procs*)))
    (cond (z (p-set-reg p 0 (p-id z))
             (p-set-reg p 1 (p-exitcode z))
             (k-reap z))
          ((find (lambda (c) (and (not (eq (p-state c) 'zombie))
                                  (= (p-ppid c) (p-id p))
                                  (or (= pid 0) (= (p-id c) pid))))
                 *procs*)
           (p-set p 2 'blocked)
           (p-set p 5 (list 'wait pid)))
          (t (p-set-reg p 0 (- WORD 1))))))

(define (k-sys-write p)
  (let ((fd (p-reg p 1)) (buf (p-reg p 2)) (len (p-reg p 3)))
    (cond ((not (= fd 1)) (p-set-reg p 0 (- WORD 1)))
          ((not (u-range-ok-p buf len)) (k-fault p 3))
          (t (labels (((go i) (cond ((= i len) 0)
                                    (t (con-emit (rd *cpu* (u-addr p (+ buf i))))
                                       (go (+ i 1))))))
               (go 0))
             (p-set-reg p 0 len)))))

(define (k-sys-read p)
  (let ((fd (p-reg p 1)) (buf (p-reg p 2)) (maxlen (p-reg p 3)))
    (cond ((not (= fd 0)) (p-set-reg p 0 (- WORD 1)))
          ((not (u-range-ok-p buf maxlen)) (k-fault p 3))
          ((> (kbd-line-len) 0)
           (p-set-reg p 0 (kbd-take-line (u-addr p buf) maxlen)))
          (t (p-set p 2 'blocked)
             (p-set p 5 'kbd)))))

(define (k-sys-fread p)
  (let ((bytes (fs-read (u-read-str p (p-reg p 1)) FS-ROOT))
        (buf (p-reg p 2)) (maxlen (p-reg p 3)))
    (cond ((null bytes) (p-set-reg p 0 (- WORD 1)))
          ((not (u-range-ok-p buf maxlen)) (k-fault p 3))
          (t (let ((n (cond ((< (length bytes) maxlen) (length bytes)) (t maxlen))))
               (u-write-n p buf (take bytes n))
               (p-set-reg p 0 n))))))

(define (k-sys-flist p)
  (let ((bytes (map char-code (str-to-list (cat (kf-list "/") " "))))
        (buf (p-reg p 2)) (maxlen (p-reg p 3)))
    (let ((n (cond ((< (length bytes) maxlen) (length bytes)) (t maxlen))))
      (u-write-n p buf (take bytes n))
      (p-set-reg p 0 n))))

(define (k-kill pid)
  (let ((p (proc-find pid)))
    (cond ((or (null p) (= pid 1) (eq (p-state p) 'hosted)) (- WORD 1))
          (t (k-exit p 137) 0))))

; ============================================================
; FILESYSTEM V2 — hierarchical, on the sector disk
;
; sector 0: superblock  [0-3] "SFS2"  [4]=32 inodes  [5]=56 data
;                        [6-9] inode bitmap  [10-17] data bitmap
; sector 1-2: inode table, 32 inodes x 16 bytes
;   inode: [0]used [1]type(0=file 1=dir) [2-3]size [4-9] 6 blocks
;            (block numbers stored +1; 0 = empty)
; sector 8-63: data blocks (block i = sector 8+i)
;
; A directory's data is a sequence of 16-byte entries:
;   [0]used [1-14]name (NUL padded) [15]inode
; The root directory is inode 0; "." and ".." are real entries.
; Paths are resolved component by component from root (absolute)
; or a cwd inode (relative).
; ============================================================
(define FS-NINODE 32) (define FS-NDATA 56) (define FS-MAXFILE 1536)
(define FS-ROOT 0)

(define (drv-read n)
  (io-wr 3842 n) (io-wr 3843 1)
  (labels (((go i acc) (cond ((= i 256) (reverse acc))
                             (t (go (+ i 1) (cons (io-rd 3844) acc))))))
    (go 0 nil)))
(define (drv-write n bytes)
  (io-wr 3842 n) (setq dsk-buf nil)
  (labels (((go bs) (cond ((null bs) (io-wr 3843 2))
                          (t (io-wr 3844 (car bs)) (go (cdr bs))))))
    (go bytes)))
(define (splice-256 sec off newbytes)
  (let ((old (drv-read sec)))
    (drv-write sec (append (append (take old off) newbytes)
                           (drop old (+ off (length newbytes)))))))

; bitmap helpers (bitmap = list of bytes)
(define (bm-get bm i)
  (mod (truncate (/ (nth (truncate (/ i 8)) bm) (^ 2 (mod i 8)))) 2))
(define (bm-set bm i v)
  (let ((byte-idx (truncate (/ i 8))) (bit (mod i 8)))
    (labels (((go j rest acc)
        (cond ((null rest) (reverse acc))
              ((= j byte-idx)
               (go (+ j 1) (cdr rest)
                   (cons (cond ((= v 1) (+ (car rest) (^ 2 bit)))
                               (t (- (car rest) (^ 2 bit))))
                         acc)))
              (t (go (+ j 1) (cdr rest) (cons (car rest) acc))))))
      (go 0 bm nil))))
(define (bm-alloc bm n)
  (labels (((go i) (cond ((= i n) nil)
                         ((= (bm-get bm i) 0) i)
                         (t (go (+ i 1))))))
    (go 0)))

(define *fs-ibm* nil) (define *fs-dbm* nil)

(define (fs-format)
  (drv-write 0 (append (map char-code (str-to-list "SFS2"))
                       (list FS-NINODE FS-NDATA)
                       (list 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0)))
  (drv-write 1 nil) (drv-write 2 nil)
  (setq *fs-ibm* (list 1 0 0 0))      ; inode 0 = root directory
  (setq *fs-dbm* (list 0 0 0 0 0 0 0 0))
  (fs-sync)
  (inode-write FS-ROOT (append (list 1 1 0 0)
                               (list 0 0 0 0 0 0)
                               (list 0 0 0 0 0 0)))
  (fs-write-ino FS-ROOT (append (dir-entry "." FS-ROOT)
                                (dir-entry ".." FS-ROOT))))

(define (fs-mount)
  (let ((sb (drv-read 0)))
    (cond ((equal (take sb 4) (map char-code (str-to-list "SFS2")))
           (setq *fs-ibm* (take (drop sb 6) 4))
           (setq *fs-dbm* (take (drop sb 10) 8))
           t)
          (t nil))))

(define (fs-sync)
  (let ((sb (drv-read 0)))
    (drv-write 0 (append (append (take sb 6) *fs-ibm*) *fs-dbm*))))

(define (fs-alloc-inode)
  (let ((i (bm-alloc *fs-ibm* FS-NINODE)))
    (cond ((null i) nil)
          (t (setq *fs-ibm* (bm-set *fs-ibm* i 1)) (fs-sync) i))))
(define (fs-free-inode i)
  (setq *fs-ibm* (bm-set *fs-ibm* i 0)) (fs-sync))
(define (fs-alloc-block)
  (let ((i (bm-alloc *fs-dbm* FS-NDATA)))
    (cond ((null i) nil)
          (t (setq *fs-dbm* (bm-set *fs-dbm* i 1)) (fs-sync) i))))
(define (fs-free-block i)
  (setq *fs-dbm* (bm-set *fs-dbm* i 0)) (fs-sync))

; inode table access
(define (inode-read i)
  (take (drop (drv-read (+ 1 (truncate (/ i 16)))) (* 16 (mod i 16))) 16))
(define (inode-write i bytes16)
  (splice-256 (+ 1 (truncate (/ i 16))) (* 16 (mod i 16)) bytes16))

; ---- inode-level file I/O ---------------------------------------
; read the blocks of an inode and take the first size bytes
; (block numbers in the inode are stored +1; 0 marks empty)
(define (fs-read-blocks inode size)
  (labels (((go b acc)
      (cond ((or (= b 6) (>= (length acc) size)) acc)
            (t (go (+ b 1)
                   (append acc (drv-read (+ 7 (nth (+ 4 b) inode)))))))))
    (go 0 nil)))
(define (fs-read-ino ino)
  (let ((inode (inode-read ino)))
    (let ((size (+ (nth 2 inode) (* 256 (nth 3 inode)))))
      (take (fs-read-blocks inode size) size))))
(define (fs-free-blocks inode)
  (labels (((go b) (cond ((= b 6) 0)
                        (t (cond ((> (nth (+ 4 b) inode) 0)
                                  (fs-free-block (- (nth (+ 4 b) inode) 1))))
                           (go (+ b 1))))))
    (go 0)))
(define (fs-write-ino ino bytes)
  (cond ((> (length bytes) FS-MAXFILE) nil)
        (t (let ((inode (inode-read ino)))
             (fs-free-blocks inode)
             (let ((nb (cond ((= (length bytes) 0) 0)
                             (t (+ 1 (truncate (/ (- (length bytes) 1) 256)))))))
               (let ((blklist (labels (((go b bs acc)
                                  (cond ((= b nb) acc)
                                        (t (let ((blk (fs-alloc-block)))
                                             (cond ((null blk) acc)
                                                   (t (drv-write (+ 8 blk) (take bs 256))
                                                      (go (+ b 1) (drop bs 256)
                                                          (append acc (list (+ blk 1)))))))))))
                                (go 0 bytes nil))))
                 (inode-write ino
                   (append (list 1 (nth 1 inode) (mod (length bytes) 256)
                                 (truncate (/ (length bytes) 256)))
                           (append blklist
                                   (map (lambda (x) 0)
                                        (iota (- 6 (length blklist)))))
                           (list 0 0 0 0 0 0)))
                 (length bytes)))))))
(define (fs-free-ino ino)
  (fs-free-blocks (inode-read ino))
  (fs-free-inode ino))
(define (fs-make-ino type)
  (let ((ino (fs-alloc-inode)))
    (cond ((null ino) nil)
          (t (inode-write ino (append (list 1 type 0 0)
                                      (list 0 0 0 0 0 0)
                                      (list 0 0 0 0 0 0)))
             ino))))

; ---- directory entries ------------------------------------------
(define (dir-entry name ino)
  (let ((chars (map char-code (str-to-list name))))
    (append (list 1)
            (append chars (map (lambda (x) 0) (iota (- 14 (length chars)))))
            (list ino))))
(define (dir-entry-name e)
  (list-to-str (map code-char
                    (filter (lambda (c) (not (= c 0))) (take (cdr e) 14)))))
; live entries of a directory inode as an alist (name . ino)
(define (dir-entries ino)
  (labels (((go es acc)
      (cond ((null es) (reverse acc))
            (t (let ((e (take es 16)))
                 (cond ((= (car e) 1)
                        (go (drop es 16)
                            (cons (cons (dir-entry-name e) (nth 15 e)) acc)))
                       (t (go (drop es 16) acc))))))))
    (go (fs-read-ino ino) nil)))
(define (dir-add ino name child)
  (let ((data (fs-read-ino ino)))
    (labels (((find-free es off)
        (cond ((null es) off)
              ((= (car (take es 16)) 0) off)
              (t (find-free (drop es 16) (+ off 16))))))
      (let ((off (find-free data 0)))
        (fs-write-ino ino
          (append (take data off)
                  (append (dir-entry name child) (drop data (+ off 16)))))))))
(define (dir-remove ino name)
  (let ((data (fs-read-ino ino)))
    (labels (((go es off)
        (cond ((null es) nil)
              ((and (= (car (take es 16)) 1)
                    (equal (dir-entry-name (take es 16)) name))
               (fs-write-ino ino
                 (append (take data off)
                         (append (map (lambda (x) 0) (iota 16))
                                 (drop data (+ off 16)))))
               t)
              (t (go (drop es 16) (+ off 16))))))
      (go data 0))))

; ---- path handling -----------------------------------------------
; split a path into (absolute-p component ...)
(define (split-path p)
  (labels (((go chars cur comps)
      (cond ((null chars)
             (cond ((= (length cur) 0) comps)
                   (t (append comps (list cur)))))
            ((equal (car chars) "/")
             (cond ((= (length cur) 0) (go (cdr chars) "" comps))
                   (t (go (cdr chars) "" (append comps (list cur))))))
            (t (go (cdr chars) (cat cur (car chars)) comps)))))
    (let ((comps (go (str-to-list p) "" nil)))
      (cond ((and (> (length p) 0) (equal (substr 0 1 p) "/"))
             (cons t comps))
            (t (cons nil comps))))))
; walk components from a start inode; -> inode or nil
(define (fs-walk ino comps)
  (cond ((null comps) ino)
        (t (let ((inode (inode-read ino)))
             (cond ((not (= (nth 1 inode) 1)) nil)
                   (t (let ((e (assoc (car comps) (dir-entries ino) (quote equal))))
                        (cond ((null e) nil)
                              (t (fs-walk (cdr e) (cdr comps)))))))))))
(define (fs-resolve-ino path cwd-ino)
  (let ((sp (split-path path)))
    (fs-walk (cond ((car sp) FS-ROOT) (t cwd-ino)) (cdr sp))))
; -> (parent-ino basename) or nil
(define (fs-resolve-parent path cwd-ino)
  (let ((sp (split-path path)))
    (let ((comps (cdr sp)))
      (cond ((null comps) nil)
            (t (let ((p-ino (fs-walk (cond ((car sp) FS-ROOT) (t cwd-ino))
                                     (take comps (- (length comps) 1)))))
                 (cond ((null p-ino) nil)
                       (t (list p-ino (car (drop comps (- (length comps) 1))))))))))))

; ---- path-level file operations (cwd is an inode) ----------------
(define (fs-create path cwd-ino)
  (let ((r (fs-resolve-parent path cwd-ino)))
    (cond ((null r) 1)
          ((> (length (cadr r)) 14) 1)
          ((assoc (cadr r) (dir-entries (car r)) (quote equal)) 1)
          (t (let ((ino (fs-make-ino 0)))
               (cond ((null ino) 1)
                     (t (dir-add (car r) (cadr r) ino) 0)))))))
(define (fs-mkdir path cwd-ino)
  (let ((r (fs-resolve-parent path cwd-ino)))
    (cond ((null r) 1)
          ((> (length (cadr r)) 14) 1)
          ((assoc (cadr r) (dir-entries (car r)) (quote equal)) 1)
          (t (let ((ino (fs-make-ino 1)))
               (cond ((null ino) 1)
                     (t (fs-write-ino ino (append (dir-entry "." ino)
                                                  (dir-entry ".." (car r))))
                        (dir-add (car r) (cadr r) ino)
                        0)))))))
(define (fs-read path cwd-ino)
  (let ((ino (fs-resolve-ino path cwd-ino)))
    (cond ((null ino) nil)
          ((not (= (nth 1 (inode-read ino)) 0)) nil)
          (t (fs-read-ino ino)))))
(define (fs-write path cwd-ino bytes)
  (let ((ino (fs-resolve-ino path cwd-ino)))
    (cond ((null ino) nil)
          ((not (= (nth 1 (inode-read ino)) 0)) nil)
          (t (fs-write-ino ino bytes)))))
(define (fs-delete path cwd-ino)
  (let ((ino (fs-resolve-ino path cwd-ino))
        (r (fs-resolve-parent path cwd-ino)))
    (cond ((or (null ino) (null r)) 1)
          ((not (= (nth 1 (inode-read ino)) 0)) 1)
          (t (fs-free-ino ino)
             (dir-remove (car r) (cadr r))
             0))))
; directories only; fails unless empty (just "." and "..")
(define (fs-rmdir path cwd-ino)
  (let ((ino (fs-resolve-ino path cwd-ino))
        (r (fs-resolve-parent path cwd-ino)))
    (cond ((or (null ino) (null r)) 1)
          ((not (= (nth 1 (inode-read ino)) 1)) 1)
          ((> (length (dir-entries ino)) 2) 1)
          (t (fs-free-ino ino)
             (dir-remove (car r) (cadr r))
             0))))
(define (fs-readdir path cwd-ino)
  (let ((ino (fs-resolve-ino path cwd-ino)))
    (cond ((null ino) nil)
          ((not (= (nth 1 (inode-read ino)) 1)) nil)
          (t (map car (dir-entries ino))))))
(define (fs-stat path cwd-ino)
  (let ((ino (fs-resolve-ino path cwd-ino)))
    (cond ((null ino) nil) (t (inode-read ino)))))
(define (fs-is-dir path cwd-ino)
  (let ((ino (fs-resolve-ino path cwd-ino)))
    (cond ((null ino) nil)
          (t (= (nth 1 (inode-read ino)) 1)))))

; ---- kernel-facing fs helpers (used by syscalls and shell) -----
(define (kf-create name) (fs-create name FS-ROOT))
(define (kf-delete name) (fs-delete name FS-ROOT))
(define (kf-list path)
  (let ((names (fs-readdir path FS-ROOT)))
    (cond ((null names) "")
          (t (str-join (filter (lambda (n) (and (not (equal n "."))
                                                (not (equal n ".."))))
                               names) " ")))))
(define (kf-mkdir name) (fs-mkdir name FS-ROOT))
(define (kf-write-bytes name bytes)
  (let ((r (fs-write name FS-ROOT bytes)))
    (cond ((null r) (- WORD 1)) (t r))))
(define (kf-write name str)
  (kf-write-bytes name (map char-code (str-to-list str))))
(define (kf-read name)
  (let ((bytes (fs-read name FS-ROOT)))
    (cond ((null bytes) nil)
          (t (list-to-str (map code-char bytes))))))

; ---- disk image backup / restore (host file) -------------------
; The platter (dsk-store) is serialized as nested lists of numbers
; and written to a host file; restore reads it back and remounts.
(define (nums-str nums) (str-join (map num-to-str nums) " "))
(define (sector-str e)
  (cat (cat "(" (num-to-str (car e)))
       (cat " (" (cat (nums-str (cdr e)) "))"))))
(define (disk-backup path)
  (write-file path
    (cat (cat "(" (str-join (map sector-str dsk-store) " ")) ")")))
(define (disk-restore path)
  (let ((s (read-file path)))
    (cond ((null s) nil)
          (t (setq dsk-store
                   (map (lambda (e) (cons (car e) (cadr e)))
                        (car (read-from-string s))))
             (fs-mount)))))

; ============================================================
; INTERRUPT SERVICE ROUTINES (machine code, installed at boot)
; ============================================================
; Timer ISR: count ticks; when the quantum expires, zero the
; counter and hypercall a reschedule (sys 15).  Frame contract
; with the kernel: push R0-R2 first, so the preempt park pops
; exactly R2 R1 R0 FLG PC (5 words).
(define isr-timer-src
  '((push 0) (push 1) (push 2)
    (loadi 0 TICKS) (load 1 0)          ; R1 = TICKS
    (loadi 0 1) (add 1 0)               ; R1++
    (loadi 0 QUANT) (load 2 0)          ; R2 = QUANTUM
    (cmp 1 2) (jl keep)
    (loadi 0 TICKS) (loadi 1 0) (store 0 1)   ; TICKS = 0
    (sys 15)                            ; -> kernel reschedules
    (label keep)
    (loadi 0 TICKS) (store 0 1)         ; TICKS = R1
    (pop 2) (pop 1) (pop 0)
    (iret)))

; Keyboard ISR: drain the UART FIFO into the ring (several chars
; may be pending for one IRQ).  loadw clobbers R7, so it is saved.
(define isr-kbd-src
  '((push 0) (push 1) (push 2) (push 7)
    (label loop)
    (loadw 0 3841)                      ; kbd status port
    (inb 1 0)
    (jz 1 done)                         ; FIFO empty -> done
    (loadw 0 3840)
    (inb 1 0)                           ; R1 = char
    (loadi 0 KBDT) (load 2 0)           ; R2 = tail
    (loadi 0 KBRING) (add 0 2)          ; ring base + tail
    (store 0 1)
    (loadi 0 1) (add 2 0)
    (loadi 0 15) (and 2 0)              ; & 15
    (loadi 0 KBDT) (store 0 2)
    (jmp loop)
    (label done)
    (pop 7) (pop 2) (pop 1) (pop 0)
    (iret)))

; The ISR sources use symbolic cell names; substitute numbers.
(define (subst-consts src)
  (map (lambda (stmt)
         (map (lambda (x)
                (cond ((eq x 'TICKS) TICKS-CELL)
                      ((eq x 'QUANT) QUANTUM-CELL)
                      ((eq x 'KBDT) KBDT-CELL)
                      ((eq x 'KBRING) KBD-RING)
                      (t x)))
              stmt))
       src))

; ============================================================
; USER PROGRAMS (V2.1 assembly, org 0, run in their own window)
; ============================================================
(define *images*
  (list
    (list "init"
      (assemble '((label l) (sys 1) (jmp l))))

    (list "hello"
      (assemble '((loadi 1 1) (loadi 2 msg) (loadi 3 20) (sys 5)
                  (loadi 1 0) (sys 0)
                  (label msg) (dbs "hello from userland") (word 10))))

    ; count: prints 12345 via the write syscall
    (list "count"
      (assemble '((loadi 4 200)              ; buf
                  (loadi 6 1)                ; i
                  (loadi 5 6)                ; limit+1
                  (label lp)
                  (loadi 1 48) (add 1 6)     ; '0'+i
                  (store 4 1)
                  (loadi 1 1) (mov 2 4) (loadi 3 1) (sys 5)
                  (loadi 1 1) (add 6 1)
                  (cmp 6 5) (jne lp)
                  (loadi 1 10) (store 4 1)
                  (loadi 1 1) (mov 2 4) (loadi 3 1) (sys 5)
                  (loadi 1 0) (sys 0))))

    ; squares: prints 1 4 9 16 25 via the number device
    (list "squares"
      (assemble '((loadi 6 1)
                  (loadw 4 3841)             ; number port
                  (label lp)
                  (mov 1 6) (mul 1 6)
                  (outb 4 1)
                  (loadi 1 1) (add 6 1)
                  (loadi 2 6) (cmp 6 2) (jne lp)
                  (loadi 1 0) (sys 0))))

    ; regs: registers must survive 50 forced reschedules
    (list "regs"
      (assemble '((loadi 3 11) (loadi 4 22) (loadi 5 33)
                  (loadi 6 44) (loadi 7 55)
                  (loadi 2 50)
                  (label lp)
                  (sys 1)                    ; yield
                  (loadi 0 1) (sub 2 0)
                  (jnz 2 lp)
                  (mov 0 3) (add 0 4) (add 0 5) (add 0 6) (add 0 7)
                  (loadi 1 165) (cmp 0 1) (je good)
                  (loadi 1 1) (loadi 2 bad) (loadi 3 4) (sys 5)
                  (loadi 1 1) (sys 0)
                  (label good)
                  (loadi 1 1) (loadi 2 ok) (loadi 3 3) (sys 5)
                  (loadi 1 0) (sys 0)
                  (label ok) (dbs "ok") (word 10)
                  (label bad) (dbs "bad") (word 10))))

    ; forkdemo: fork/wait/exit-code end to end
    (list "forkdemo"
      (assemble '((sys 2)
                  (jz 0 child)
                  (mov 1 0) (sys 4)          ; parent waits for child
                  (loadi 1 1) (loadi 2 pmsg) (loadi 3 2) (sys 5)
                  (loadi 1 0) (sys 0)
                  (label child)
                  (loadi 1 1) (loadi 2 cmsg) (loadi 3 2) (sys 5)
                  (loadi 1 7) (sys 0)        ; exit(7)
                  (label pmsg) (dbs "P") (word 10)
                  (label cmsg) (dbs "C") (word 10))))

    ; fstest: create, write, read back, print — all via syscalls
    (list "fstest"
      (assemble '((loadi 1 name) (sys 7)
                  (loadi 1 name) (loadi 2 data) (loadi 3 5) (sys 8)
                  (loadi 1 name) (loadi 2 200) (loadi 3 16) (sys 9)
                  (mov 3 0) (loadi 1 1) (loadi 2 200) (sys 5)
                  (loadi 1 0) (sys 0)
                  (label name) (db "log")
                  (label data) (dbs "SFS!") (word 10))))

    ; echo: read one kbd line, write it back
    (list "echo"
      (assemble '((loadi 1 0) (loadi 2 200) (loadi 3 16) (sys 6)
                  (mov 3 0) (loadi 1 1) (loadi 2 200) (sys 5)
                  (loadi 1 0) (sys 0))))

    ; spin: never exits (for ps/kill)
    (list "spin"
      (assemble '((label l) (sys 1) (jmp l))))))

; ============================================================
; SHELL (hosted) — commands go through the same kernel services
; ============================================================
(define (tokenize input) (tok (str-to-list input) "" nil))
(define (tok chars cur tokens)
  (cond ((null chars) (cond ((= (length cur) 0) (reverse tokens))
                            (t (reverse (cons cur tokens)))))
        ((equal (car chars) " ")
         (cond ((= (length cur) 0) (tok (cdr chars) "" tokens))
               (t (tok (cdr chars) "" (cons cur tokens)))))
        (t (tok (cdr chars) (cat cur (car chars)) tokens))))
(define (parse-pipes tokens)
  (let ((pos (index-of "|" tokens)))
    (cond ((eq pos nil) (list tokens))
          (t (cons (take tokens pos) (parse-pipes (drop tokens (+ pos 1))))))))
(define (str-contains s pat)
  (cond ((> (length pat) (length s)) nil)
        ((equal (substr 0 (length pat) s) pat) t)
        (t (str-contains (substr 1 (- (length s) 1) s) pat))))
(define (parse-redir tokens)
  (let ((pos (index-of ">" tokens)))
    (cond ((eq pos nil) (list tokens nil))
          (t (list (take tokens pos) (car (drop tokens (+ pos 1))))))))

(define (sh-ps)
  (str-join
    (map (lambda (p)
           (str-join (list "pid " (num-to-str (p-id p))
                           " ppid " (num-to-str (p-ppid p))
                           " " (state-str (p-state p))
                           " " (p-cmd p)) ""))
         *procs*)
    " "))

; ---- shell working directory ------------------------------------
(define *sh-cwd* "/")
; collapse "." and ".." in an absolute path
(define (norm-path p)
  (let ((comps (cdr (split-path p))))
    (let ((r (foldl (lambda (acc c)
                      (cond ((equal c ".") acc)
                            ((equal c "..")
                             (cond ((null acc) nil)
                                   (t (take acc (- (length acc) 1)))))
                            (t (append acc (list c)))))
                    nil comps)))
      (cond ((null r) "/")
            (t (cat "/" (str-join r "/")))))))
(define (sh-abs path)
  (cond ((and (> (length path) 0) (equal (substr 0 1 path) "/"))
         (norm-path path))
        (t (norm-path (cat (cat *sh-cwd* "/") path)))))
(define (sh-cd path)
  (let ((abs (sh-abs path)))
    (cond ((fs-is-dir abs FS-ROOT) (setq *sh-cwd* abs) "")
          (t (str-join (list "cd: " path ": no such directory") "")))))

(define (sh-builtin tokens)
  (let ((cmd (car tokens)) (args (cdr tokens)))
    (cond ((equal cmd "help") "ls cd pwd mkdir rmdir cat echo rm ps kill run jobs exit | > &")
          ((equal cmd "ls") (kf-list (cond ((null args) *sh-cwd*) (t (sh-abs (car args))))))
          ((equal cmd "cd") (cond ((null args) (sh-cd "/")) (t (sh-cd (car args)))))
          ((equal cmd "pwd") *sh-cwd*)
          ((equal cmd "mkdir")
           (cond ((null args) "mkdir: missing dir")
                 ((= (fs-mkdir (sh-abs (car args)) FS-ROOT) 0) "")
                 (t (str-join (list "mkdir: " (car args) ": cannot create") ""))))
          ((equal cmd "rmdir")
           (cond ((null args) "rmdir: missing dir")
                 ((= (fs-rmdir (sh-abs (car args)) FS-ROOT) 0) "")
                 (t (str-join (list "rmdir: " (car args) ": failed") ""))))
          ((equal cmd "cat")
           (cond ((null args) "cat: missing file")
                 (t (let ((s (kf-read (sh-abs (car args)))))
                      (cond ((null s) (str-join (list "cat: " (car args) ": no such file") ""))
                            (t s))))))
          ((equal cmd "echo") (str-join args " "))
           ((equal cmd "rm")
            (cond ((null args) "rm: missing file")
                  ((= (kf-delete (sh-abs (car args))) 0) (str-join (list "removed " (car args)) ""))
                  (t (str-join (list "rm: " (car args) ": no such file") ""))))
          ((equal cmd "ps") (sh-ps))
          ((equal cmd "kill")
           (cond ((null args) "kill: missing pid")
                 ((= (k-kill (str-to-num (car args))) 0)
                  (str-join (list "killed " (car args)) ""))
                 (t (str-join (list "kill: " (car args) ": no such process") ""))))
          ((equal cmd "run")
           (cond ((null args) "run: missing program")
                 (t (let ((pid (k-spawn-image (car args) 2)))
                      (cond ((null pid) (str-join (list "run: " (car args) ": not found") ""))
                            (t (k-run 2000)
                               (str-join (list "[" (num-to-str pid) "] "
                                               (car args)) "")))))))
          ((equal cmd "jobs") (sh-ps))
          ((equal cmd "exit") "exit")
          (t nil))))

(define (sh-exec tokens stdin)
  (let ((cmd (car tokens)) (args (cdr tokens)))
    (cond ((equal cmd "grep")
           (cond ((null args) "grep: missing pattern")
                 (t (str-join (filter (lambda (w) (str-contains w (car args)))
                                      (tokenize stdin)) " "))))
          ((equal cmd "wc") (num-to-str (length (tokenize stdin))))
          (t (let ((builtin (sh-builtin tokens)))
             (cond ((null builtin) (str-join (list "sh: " cmd ": not found") ""))
                   (t builtin)))))))

(define (sh-pipeline segments stdin)
  (cond ((null segments) stdin)
        (t (sh-pipeline (cdr segments) (sh-exec (car segments) stdin)))))

(define (sh-run input)
  (let ((tokens (tokenize input)))
    (cond ((null tokens) "")
          (t (let ((last (car (reverse tokens))))
               (cond ((equal last "&")
                      (sh-builtin (take tokens (- (length tokens) 1))))
                     (t (let ((rd (parse-redir tokens)))
                          (cond ((cadr rd)
                                 (let ((out (sh-pipeline (parse-pipes (car rd)) ""))
                                       (dest (sh-abs (cadr rd))))
                                   (cond ((null (fs-resolve-ino dest FS-ROOT))
                                          (kf-create dest)))
                                   (kf-write dest out)
                                   (str-join (list "wrote " (cadr rd)) "")))
                                (t (sh-pipeline (parse-pipes tokens) "")))))))))))

; ============================================================
; HOST SISP — metacircular evaluator (runs on the host)
; ============================================================
(define *s-global* (list 'genv))
(define (s-global-define name val)
  (set-cdr *s-global* (cons (cons name val) (cdr *s-global*))) name)
(define (s-env-extend env names vals)
  (cond ((null names) env)
        (t (s-env-extend (cons (cons (car names) (car vals)) env) (cdr names) (cdr vals)))))
(define (s-env-lookup name env)
  (let ((e (assoc name env)))
    (cond ((not (null e)) (cdr e))
          (t (let ((g (assoc name (cdr *s-global*))))
               (cond ((not (null g)) (cdr g)) (t name)))))))
(define (s-eval expr env)
  (cond ((numberp expr) expr)
        ((equal expr t) t) ((equal expr nil) nil)
        ((symbolp expr) (s-env-lookup expr env))
        ((eq (car expr) 's-quote) (cadr expr))
        ((eq (car expr) 's-if) (cond ((s-eval (cadr expr) env) (s-eval (caddr expr) env)) (t (s-eval (cadddr expr) env))))
        ((eq (car expr) 's-lambda) (list 'closure (cadr expr) (cddr expr) env))
        ((eq (car expr) 's-define) (s-global-define (cadr expr) (s-eval (caddr expr) env)))
        ((eq (car expr) 's-let) (let ((bindings (cadr expr)) (body (cddr expr)))
                                 (s-eval (car body) (s-env-extend env (map car bindings) (map (lambda (b) (s-eval (cadr b) env)) bindings)))))
        (t (s-apply (s-eval (car expr) env) (map (lambda (e) (s-eval e env)) (cdr expr)) env))))
(define (s-apply fn args env)
  (cond ((and (consp fn) (eq (car fn) 'closure))
         (s-eval (car (caddr fn)) (s-env-extend (cadddr fn) (cadr fn) args)))
        ((eq fn 's-car) (car (car args)))
        ((eq fn 's-cdr) (cdr (car args)))
        ((eq fn 's-cons) (cons (car args) (cadr args)))
        ((eq fn 's-+) (foldl + 0 args))
        ((eq fn 's--) (cond ((null (cdr args)) (- 0 (car args))) (t (- (car args) (cadr args)))))
        ((eq fn 's-*) (foldl * 1 args))
        ((eq fn 's-=) (= (car args) (cadr args)))
        ((eq fn 's-<) (< (car args) (cadr args)))
        ((eq fn 's-eq) (eq (car args) (cadr args)))
        ((eq fn 's-null) (null (car args)))
        ((eq fn 's-consp) (consp (car args)))
        ((eq fn 's-list) args)
        ((eq fn 's-print) (print (car args)) (car args))
        (t (print (list 's-apply-unknown-fn fn)) nil)))

; ============================================================
; BOOT
; ============================================================
(define (os-boot)
  (setq *cpu* (mk-cpu))
  (setq *procs* nil) (setq *nextpid* 1) (setq *curpid* 0)
  (slots-init)
  ; interrupt vectors
  (wr *cpu* 2 TIMER-ISR) (wr *cpu* 4 KBD-ISR)
  ; kernel machine code
  (load-words *cpu* TIMER-ISR (assemble-at (subst-consts isr-timer-src) TIMER-ISR))
  (load-words *cpu* KBD-ISR (assemble-at (subst-consts isr-kbd-src) KBD-ISR))
  ; kernel cells
  (wr *cpu* TICKS-CELL 0) (wr *cpu* QUANTUM-CELL 4)
  (wr *cpu* KBDH-CELL 0) (wr *cpu* KBDT-CELL 0)
  (setq timr-intv 64)                  ; one tick every 64 cycles
  ; disk: mount existing filesystem or format a fresh one
  (cond ((fs-mount) (print "[K] found SFS2 filesystem on disk"))
        (t (fs-format) (print "[K] formatted new SFS2 filesystem")))
  ; pid 1 = init, pid 2 = shell (hosted)
  (k-spawn-image "init" 0)
  (proc-add 2 1 'hosted nil nil "sh")
  (setq *nextpid* 3)
  (print "[K] boot: pid 1 = init, pid 2 = sh (hosted)")
  (print "[K] timer: 64-cycle ticks, quantum 4 ticks (~256 cycles/slice)"))

