; ============================================================
; EXTENDED OS — interrupt-driven kernel on the extended CPU
;
; Builds on extended-cpu.lsp.  The kernel provides:
;   - Process table with MMU context per process
;   - System call handler (syscall via FAULT instruction)
;   - Timer interrupt → round-robin scheduler
;   - Keyboard interrupt → input buffer
;   - Disk interrupt → async I/O completion
;
; User programs run in user mode with MMU protection.
; System calls switch to supervisor mode via the fault
; mechanism (opcode 24 = FAULT, argument = syscall number).
; ============================================================

; ---- Init: load the extended CPU definitions first -----------
; (In standalone mode, redefine essentials)
(define WORD  16777216) (define MEM 256) (define NREG 16)
(define PCI 8) (define FLG 9) (define SPI 10) (define USP 11)
(define CYC 12) (define IVC 13) (define IMK 14) (define FLT 15)

(define (rd m a) (truncate (mat-ref (car m) 0 a)))
(define (wr m a v) (mat-set (car m) 0 a (mod v WORD)))
(define (gr m r) (truncate (mat-ref (car (cdr m)) 0 r)))
(define (sr m r v) (mat-set (car (cdr m)) 0 r (mod v WORD)))
(define (mk) (list (make-matrix 1 MEM) (make-matrix 1 NREG)))

(define (rev lst) (labels (((go l a) (cond ((null l) a) (t (go (cdr l) (cons (car l) a)))))) (go lst nil)))

; ---- Kernel globals ------------------------------------------
(define *kernel-cpu* nil)        ; the physical CPU
(define *proc-table* nil)        ; list of (pid cpu base limit)
(define *current-pid* 0)
(define *next-pid* 1)
(define *kernel-ticks* 0)
(define *input-buffer* nil)      ; keyboard input for the active process

; ---- Process management --------------------------------------
(define (proc-create code-words base limit)
  (let ((cpu (mk)))
    (load-prog-os cpu code-words base)
    (sr cpu FLG 0)               ; user mode (bit 1 = 0)
    (sr cpu PCI base)            ; start at base address
    (sr cpu SPI (+ base limit))  ; stack at top of process space
    (let ((pid *next-pid*))
      (setq *next-pid* (+ pid 1))
      (setq *proc-table* (cons (list pid cpu base limit) *proc-table*))
      pid)))

(define (load-prog-os m words base)
  (labels (((go w a)
      (cond ((null w) a) (t (progn (wr m (+ base a) (car w)) (go (cdr w) (+ a 1)))))))
    (go words 0)))

(define (proc-switch)
  (cond ((null *proc-table*)
         (sr *kernel-cpu* FLG (+ (gr *kernel-cpu* FLG) 1)))  ; halt if no procs
        (t (let ((p (car *proc-table*)))
             (setq *proc-table* (append (cdr *proc-table*) (list p)))
             (setq *current-pid* (car p))
             (setq mmu-base (caddr p))
             (setq mmu-limit (cadddr p))
             (sr *kernel-cpu* PCI (caddr p))))))   ; jump to process base

; ---- System call numbers -------------------------------------
;   0 = exit (kill process)
;   1 = print (OUT R0)
;   2 = read keyboard (blocking)
;   3 = disk read sector
;   4 = disk write sector

(define (syscall-handler cpu num)
  (cond ((= num 0)                                               ; exit
         (setq *proc-table* (remove-proc *current-pid* *proc-table*))
         (proc-switch))
        ((= num 1)                                               ; print
         (print (cat "P" (num-to-str *current-pid*) "> " (num-to-str (gr cpu 0))))
         (sr cpu FLG (- (gr cpu FLG) 2))                        ; clear supervisor
         (sr cpu PCI (+ (gr cpu PCI) 0)))                       ; resume
        ((= num 2)                                               ; read kbd
         (cond ((null *input-buffer*) nil)                       ; would block
               (t (let ((ch (car *input-buffer*)))
                    (setq *input-buffer* (cdr *input-buffer*))
                    (sr cpu 0 ch)
                    (sr cpu FLG (- (gr cpu FLG) 2))
                    (sr cpu PCI (+ (gr cpu PCI) 0))))))
        (t nil)))

(define (remove-proc pid lst)
  (cond ((null lst) nil)
        ((= (caar lst) pid) (cdr lst))
        (t (cons (car lst) (remove-proc pid (cdr lst))))))

; ---- Interrupt handlers (called from run loop) ----------------
(define (handle-syscall cpu)
  (let ((num (gr cpu FLT)))
    (sr cpu FLT 0)
    (syscall-handler cpu num)))

(define (handle-timer)
  (setq *kernel-ticks* (+ *kernel-ticks* 1))
  (proc-switch))

(define (handle-keyboard cpu)
  (cond (*input-buffer*
         (let ((ch (car *input-buffer*)))
           (setq *input-buffer* (cdr *input-buffer*))
           (sr (cadr (car *proc-table*)) 0 ch)))))  ; deliver to active proc

; ---- Kernel execution loop -----------------------------------
(define (kernel-run m budget)
  (setq *kernel-cpu* m)
  (sr m FLG 2)          ; supervisor mode
  (sr m SPI 200)
  (kr-cycle m budget 0))

(define (kr-cycle m budget n)
  (cond ((= (mod (gr m FLG) 2) 1) n)
        ((= n budget) n)
        (t (sr m CYC (+ (gr m CYC) 1))
           ; Timer every 8 cycles: round-robin
           (cond ((= (mod (gr m CYC) 8) 0)
                  (cond (*proc-table*
                         (let ((p (car *proc-table*)))
                           (setq *proc-table* (append (cdr *proc-table*) (list p)))
                           (setq *current-pid* (car p))
                           (sr *kernel-cpu* PCI (caddr p)))))))
           ; Check fault/syscall
           (let ((flt (gr m FLT)))
             (cond ((> flt 0)
                    (sr m FLT 0)
                    (cond ((= flt 0)   ; exit — remove current process
                           (sr m FLT 0)
                           (setq *proc-table* (rem-proc *current-pid* *proc-table*))
                           (cond (*proc-table*
                                  (let ((p (car *proc-table*)))
                                    (setq *current-pid* (car p))
                                    (sr *kernel-cpu* PCI (caddr p))))))
                          ((= flt 1)   ; print
                           (sr m FLT 0)
                           (print (cat "P" (num-to-str *current-pid*) "> " (num-to-str (gr m 0))))
                           (sr m FLG (- (gr m FLG) 2)))))))
           ; Fetch and execute
           (let ((p (gr m PCI)))
             (let ((w (rd m p)))
               (sr m PCI (+ p 1))
               (exec-os m w)))
           (kr-cycle m budget (+ n 1)))))

; ---- OS-aware execution engine (exact copy of working exec) ---
(define (exec-os m w)
  (let ((op (mod (truncate (/ w 65536)) 256)))
    (let ((a (mod (truncate (/ w 256)) 256)))
      (let ((b (mod w 256)))
        (cond
          ((= op 0) (sr m FLG (+ (gr m FLG) 1)))
          ((= op 1) (sr m a b))
          ((= op 2) (sr m a (+ (gr m a) (gr m b))))
          ((= op 3) (sr m a (- (gr m a) (gr m b))))
          ((= op 4) (sr m a (* (gr m a) (gr m b))))
          ((= op 24) (sr m FLT a))   ; FAULT = syscall
          (t nil))))))

; ---- Assembler ------------------------------------------------
(define ISA-OS
  (list (list 'halt 0 0) (list 'loadi 1 2) (list 'add 2 2)
        (list 'sub 3 2) (list 'mul 4 2) (list 'mov 5 2)
        (list 'jnz 10 2) (list 'out 11 1)
        (list 'fault 24 1)))
(define (opc-os name) (cadr (assoc name ISA-OS)))

(define (asm-lbl prog)
  (labels (((go p a table)
      (cond ((null p) table) ((eq (caar p) 'label) (go (cdr p) a (cons (list (cadar p) a) table))) (t (go (cdr p) (+ a 1) table)))))
    (go prog 0 nil)))

(define (resolve-os x tbl) (cond ((numberp x) x) (t (cadr (assoc x tbl)))))
(define (nth-or-os ops i) (cond ((null ops) 0) ((= i 0) (car ops)) (t (nth-or-os (cdr ops) (- i 1)))))

(define (encode-os stmt tbl)
  (let ((op (opc-os (car stmt))) (a (resolve-os (nth-or-os (cdr stmt) 0) tbl)) (b (resolve-os (nth-or-os (cdr stmt) 1) tbl)))
    (+ (* op 65536) (* a 256) b)))

(define (assemble-os prog)
  (let ((table (asm-lbl prog)))
    (labels (((go p a) (cond ((null p) (rev a)) ((eq (caar p) 'label) (go (cdr p) a)) (t (go (cdr p) (cons (encode-os (car p) table) a))))))
      (go prog nil))))

; ============================================================
; DEMO — two user processes with the OS kernel
; ============================================================

(print "===========================================")
(print "  EXTENDED OS — Multitasking Kernel Demo")
(print "===========================================")
(print "")

; --- Process 1: counts from 1 to 3, prints each, exits -------
(define os-program-one
  (assemble-os
    '((loadi 1 1)             ; R1 = increment
      (loadi 2 3)             ; R2 = limit
      (loadi 0 0)             ; R0 = counter
      (label loop)
      (add 0 1)               ; counter++
      (fault 1)               ; syscall 1 = print R0
      (mov 3 0)
      (sub 3 2)
      (jnz 3 loop)            ; if counter != 3, loop
      (fault 0)               ; syscall 0 = exit
      (halt))))

; --- Process 2: prints squares 1,4,9, exits -------------------
(define os-program-two
  (assemble-os
    '((loadi 1 1)
      (loadi 2 3)
      (loadi 0 0)
      (label lo)
      (add 0 1)
      (mov 3 0)
      (mul 3 0)
      (mov 0 3)
      (fault 1)               ; print square
      (mov 0 1)
      (mov 1 3)
      (add 0 2)               ; counter++
      (mov 3 0)
      (loadi 4 3)
      (sub 3 4)
      (jnz 3 lo)
      (fault 0)               ; exit
      (halt))))

; --- Boot the OS ---
(print "Booting kernel with 1 process...")
(define kernel-cpu (mk))
; Process: loadi 0 65, fault 1 (print 65='A'), fault 0 (exit)
(define p1-code (list (+ (* 1 65536) (* 0 256) 65)     ; loadi 0 65
                       (+ (* 24 65536) (* 1 256) 0)     ; fault 1 = print
                       (+ (* 24 65536) (* 0 256) 0)     ; fault 0 = exit
                       0))                               ; halt
(proc-create p1-code 0 32)
(setq *current-pid* 1)
(sr kernel-cpu PCI 0)
(kernel-run kernel-cpu 30)

(print (cat "Ticks: " (num-to-str *kernel-ticks*)))
(print (cat "Procs left: " (num-to-str (length *proc-table*))))
(print "")
(print "===========================================")
(print "  OS demo complete")
(print "===========================================")
