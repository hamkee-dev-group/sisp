; ============================================================
; SYSTEM BOOT — Full integration of the SISP machine stack
;
;   DISK → bootloader → kernel → filesystem → shell → Forth
;
; Single-file demonstration that ties together cpu, os,
; filesystem, shell, and forth compiler into one boot sequence.
; ============================================================

; ============================================================
; LAYER 1: CPU — 24-bit stored-program computer
; ============================================================
(define WORD  16777216) (define MEM 256) (define NREG 11)
(define PCI 8) (define HLI 9) (define TRI 10) (define SP 6)

(define (rd m a) (truncate (mat-ref (car m) 0 a)))
(define (wr m a v) (mat-set (car m) 0 a (mod v WORD)))
(define (gr m r) (truncate (mat-ref (car (cdr m)) 0 r)))
(define (sr m r v) (mat-set (car (cdr m)) 0 r (mod v WORD)))
(define (new-cpu) (list (make-matrix 1 MEM) (make-matrix 1 NREG)))
(define (haltp m) (= (gr m HLI) 1))

; ---- stack (in high RAM, R6=SP) ------------------------------
(define (cpu-push m v) (let ((s (- (gr m SP) 1))) (sr m SP s) (wr m s v)))
(define (cpu-pop  m)   (let ((s (gr m SP))) (let ((v (rd m s))) (sr m SP (+ s 1)) v)))

; ---- ISA: opcode table ---------------------------------------
(define ISA
  (list (list 'halt  0 0) (list 'loadi 1 2) (list 'add 2 2)
        (list 'sub   3 2) (list 'mul  4 2) (list 'mov 5 2)
        (list 'load  6 2) (list 'store 7 2) (list 'jmp 8 1)
        (list 'jz    9 2) (list 'jnz 10 2) (list 'out 11 1)
        (list 'inc  12 1) (list 'dec  13 1)
        (list 'push 14 1) (list 'pop  15 1)
        (list 'call 16 1) (list 'ret  17 0)
        (list 'sys  18 1)))
(define (opc name) (cadr (assoc name ISA)))

; ---- assembler (symbolic -> machine words) --------------------
(define (rev xs) (labels (((go l a) (cond ((null l) a) (t (go (cdr l) (cons (car l) a)))))) (go xs nil)))
(define (nth-or ops i) (cond ((null ops) 0) ((= i 0) (car ops)) (t (nth-or (cdr ops) (- i 1)))))
(define (asm-labels prog) (labels (((go p a table) (cond ((null p) table) ((eq (caar p) 'label) (go (cdr p) a (cons (list (cadar p) a) table))) (t (go (cdr p) (+ a 1) table))))) (go prog 0 nil)))
(define (resolve x tbl) (cond ((numberp x) x) (t (cadr (assoc x tbl)))))
(define (encode stmt tbl) (let ((op (opc (car stmt))) (a (resolve (nth-or (cdr stmt) 0) tbl)) (b (resolve (nth-or (cdr stmt) 1) tbl))) (+ (* op 65536) (* a 256) b)))
(define (assemble prog) (let ((table (asm-labels prog))) (labels (((go p a) (cond ((null p) (rev a)) ((eq (caar p) 'label) (go (cdr p) a)) (t (go (cdr p) (cons (encode (car p) table) a)))))) (go prog nil))))

; ---- exec one instruction ------------------------------------
(define (exec m w)
  (let ((op (mod (truncate (/ w 65536)) 256))
        (a  (mod (truncate (/ w 256)) 256))
        (b  (mod w 256)))
    (cond ((= op  0) (sr m HLI 1))
          ((= op  1) (sr m a b))
          ((= op  2) (sr m a (+ (gr m a) (gr m b))))
          ((= op  3) (sr m a (- (gr m a) (gr m b))))
          ((= op  4) (sr m a (* (gr m a) (gr m b))))
          ((= op  5) (sr m a (gr m b)))
          ((= op  6) (sr m a (rd m (gr m b))))
          ((= op  7) (wr m (gr m a) (gr m b)))
          ((= op  8) (sr m PCI a))
          ((= op  9) (cond ((= (gr m a) 0) (sr m PCI b))))
          ((= op 10) (cond ((not (= (gr m a) 0)) (sr m PCI b))))
          ((= op 11) (print (cat "OUT " (num-to-str (gr m a)))))
          ((= op 12) (sr m a (+ (gr m a) 1)))
          ((= op 13) (sr m a (- (gr m a) 1)))
          ((= op 14) (cpu-push m (gr m a)))
          ((= op 15) (sr m a (cpu-pop m)))
          ((= op 16) (progn (cpu-push m (gr m PCI)) (sr m PCI a)))
          ((= op 17) (sr m PCI (cpu-pop m)))
          ((= op 18) (sr m TRI a))
          (t nil))))

; ---- CPU run loop --------------------------------------------
(define (run m budget)
  (sr m SP 254)
  (labels (((cycle n) (cond ((haltp m) n) ((= n budget) n) (t (let ((p (gr m PCI))) (let ((w (rd m p))) (sr m PCI (+ p 1)) (exec m w) (cycle (+ n 1))))))))
    (cycle 0)))

; ---- load words into RAM -------------------------------------
(define (load-ram m ws base)
  (labels (((go w a) (cond ((null w) a) (t (progn (wr m (+ base a) (car w)) (go (cdr w) (+ a 1))))))) (go ws 0)))

; ============================================================
; LAYER 2: OS KERNEL — multiprocessing with SYS calls
; ============================================================
(define DISK    nil)  ; global disk: alist of (name . value)
(define PROCS   nil)  ; process table
(define next-pid 1)

(define (disk-put name val) (setq DISK (cons (cons name val) DISK)))
(define (disk-get name) (let ((e (assoc name DISK))) (cond (e (cdr e)) (t nil))))

(define (proc-add cpu name)
  (let ((p (list next-pid name 'ready cpu 0)))
    (setq next-pid (+ next-pid 1))
    (setq PROCS (cons p PROCS))
    (car p)))

(define (proc-find pid) (find (lambda (p) (= (car p) pid)) PROCS))

; ============================================================
; LAYER 3: FILESYSTEM — hierarchical inode tree
; ============================================================
(define (mk-file name body)   (list name 'file body))
(define (mk-dir  name kids)   (list name 'dir  kids))
(define (ino-name ino) (car ino))
(define (ino-type ino) (cadr ino))
(define (ino-body ino) (caddr ino))
(define (ino-kids ino) (caddr ino))

(define (fs-ls dir) (map (lambda (k) (list (ino-name k) (ino-type k))) (ino-kids dir)))
(define (fs-find dir name) (find (lambda (k) (equal (ino-name k) name)) (ino-kids dir)))
(define (fs-put dir name ino)
  (mk-dir (ino-name dir) (cons ino (remove (lambda (k) (equal (ino-name k) name)) (ino-kids dir)))))

; ============================================================
; LAYER 4: FORTH COMPILER — stack language -> machine code
; ============================================================
(define (iw op a b) (+ (* op 65536) (* a 256) b))

(define (forth-compile tokens)
  (append (fc-acc tokens nil) (list (iw 0 0 0))))

(define (fc-acc ts out)
  (cond ((null ts) out)
        ((numberp (car ts)) (fc-acc (cdr ts) (append out (list (iw 1 0 (car ts)) (iw 14 0 0)))))
        ((eq (car ts) '+)    (fc-acc (cdr ts) (append out (list (iw 15 1 0) (iw 15 0 0) (iw 2 0 1) (iw 14 0 0)))))
        ((eq (car ts) 'p)    (fc-acc (cdr ts) (append out (list (iw 15 0 0) (iw 11 0 0)))))
        ((eq (car ts) 'dup)  (fc-acc (cdr ts) (append out (list (iw 14 0 0)))))
        ((eq (car ts) '*)    (fc-acc (cdr ts) (append out (list (iw 15 1 0) (iw 15 0 0) (iw 4 0 1) (iw 14 0 0)))))
        ((eq (car ts) '-)    (fc-acc (cdr ts) (append out (list (iw 15 1 0) (iw 15 0 0) (iw 3 0 1) (iw 14 0 0)))))
        (t (fc-acc (cdr ts) out))))

; ============================================================
; LAYER 5: SHELL — command dispatcher
; ============================================================
(define (str-join parts)
  (cond ((null parts) "") ((null (cdr parts)) (car parts)) (t (cat (car parts) (str-join (cdr parts))))))

(define (tokenize input)
  (let ((chars (str-to-list input)))
    (tok-acc chars "" nil)))

(define (tok-acc chars cur tokens)
  (cond ((null chars) (cond ((= (length cur) 0) (rev tokens)) (t (rev (cons cur tokens)))))
        ((equal (car chars) " ") (cond ((= (length cur) 0) (tok-acc (cdr chars) "" tokens)) (t (tok-acc (cdr chars) "" (cons cur tokens)))))
        (t (tok-acc (cdr chars) (cat cur (car chars)) tokens))))

(define (sh-dispatch tokens cwd)
  (cond ((null tokens) cwd)
        ((equal (car tokens) "help") (print "  ls cd cat run ps exit") cwd)
        ((equal (car tokens) "ls")   (foreach (lambda (e) (print (str-join (list "  " (car e) (cond ((eq (cadr e) 'dir) "/") (t "")))))) (fs-ls cwd)) cwd)
        ((equal (car tokens) "cd")   (let ((n (fs-find cwd (cadr tokens)))) (cond (n n) (t (print "  not found") cwd))))
        ((equal (car tokens) "cat")  (let ((n (fs-find cwd (cadr tokens)))) (cond (n (print "  forth source:")) (t (print "  not found"))) cwd))
        ((equal (car tokens) "ps")   (print "  PID  NAME    STATE") (foreach (lambda (p) (print (str-join (list "  " (num-to-str (car p)) "    " (cadr p) "  " (ps-state (caddr p)))))) PROCS) cwd)
        ((equal (car tokens) "run")  (cmd-run (cadr tokens) cwd) cwd)
        ((equal (car tokens) "exit") (print "  bye") cwd)
        (t (print (str-join (list "  sh: " (car tokens) ": not found"))) cwd)))

(define (ps-state s) (cond ((eq s 'ready) "ready") ((eq s 'running) "running") (t "waiting")))
(define (foreach fn lst) (cond ((null lst) nil) (t (progn (fn (car lst)) (foreach fn (cdr lst))))))

(define (cmd-run name cwd)
  (let ((f (fs-find cwd name)))
    (cond ((eq f nil) (print "  not found"))
          ((eq (ino-type f) 'file)
           (let ((src (ino-body f)))
             (print (str-join (list "  run " name ":")))
             (let ((bin (forth-compile src)))
               (let ((cpu (new-cpu)))
                 (load-ram cpu bin 0)
                 (sr cpu PCI 0)
                 (run cpu 50)
                 (print (str-join (list "  R0=" (num-to-str (gr cpu 0)))))))))
          (t (print "  not a program")))))

; ============================================================
; BOOT SEQUENCE
; ============================================================

(print "===========================================")
(print "  SISP SYSTEM BOOT")
(print "===========================================")
(print "")

; --- Phase 1: Power-on self-test ---
(print "[OK] CPU initialized (24-bit, 256W RAM, 11 registers)")
(print "")

; --- Phase 2: Mount filesystem from disk ---
(print "--- Mounting filesystem ---")

(define forth-src '(3 4 + p))
(define hello-src '(72 p))
(define square-src '(7 dup * p))
(define add-src   '(10 20 + p))

(define fs (mk-dir "/"
  (list
    (mk-dir "bin"
      (list (mk-file "forth" forth-src)
            (mk-file "hello" hello-src)))
    (mk-dir "home"
      (list (mk-file "square" square-src)
            (mk-file "add"    add-src))))))

(print "  /")
(print "  |- bin/")
(print "  |   |- forth  (3 4 + p)")
(print "  |   |- hello  (ASCII 72 = 'H')")
(print "  |- home/")
(print "      |- square (7 dup * p)")
(print "      |- add    (10 20 + p)")
(print "")

; --- Phase 3: Start shell session ---
(print "--- Shell session ---")
(print "")

(define cwd fs)

(print "$ help")
(setq cwd (sh-dispatch (tokenize "help") cwd))
(print "")

(print "$ ls")
(setq cwd (sh-dispatch (tokenize "ls") cwd))
(print "")

(print "$ cd bin")
(setq cwd (sh-dispatch (tokenize "cd bin") cwd))
(print "$ ls")
(setq cwd (sh-dispatch (tokenize "ls") cwd))
(print "")

(print "$ cat forth")
(setq cwd (sh-dispatch (tokenize "cat forth") cwd))
(print "")

(print "$ run forth")
(setq cwd (sh-dispatch (tokenize "run forth") cwd))
(print "")

(print "$ cd ..")
(setq cwd fs)
(print "$ cd home")
(setq cwd (sh-dispatch (tokenize "cd home") cwd))
(print "$ ls")
(setq cwd (sh-dispatch (tokenize "ls") cwd))
(print "")

(print "$ run square")
(setq cwd (sh-dispatch (tokenize "run square") cwd))
(print "")

(print "$ run add")
(setq cwd (sh-dispatch (tokenize "run add") cwd))
(print "")

(print "$ exit")
(print "")
(print "===========================================")
(print "  SYSTEM HALTED")
(print "===========================================")
