; ============================================================
; CPU DEBUGGER / MONITOR — INSPECT AND CONTROL THE SISP MACHINE
;
; Provides single-step execution, register inspection, memory
; dump and breakpoints for a small SISP machine: a 14-opcode
; simplification of cpu.lsp with its own opcode numbering.
; ============================================================

; ---- machine state (same word layout as cpu.lsp) ----------
(define WORD  16777216)
(define MEM   256)
(define NREG  10)
(define PCI   8)
(define HLI   9)

(define (hi w) (mod (truncate (/ w 65536)) 256))
(define (mid w) (mod (truncate (/ w 256)) 256))
(define (lo w) (mod w 256))

(define (make-cpu) (list (make-matrix 1 MEM) (make-matrix 1 NREG)))
(define (ram  m) (car m))
(define (regs m) (car (cdr m)))
(define (rd  m a)   (truncate (mat-ref (ram m) 0 a)))
(define (wr  m a v) (mat-set (ram m) 0 a (mod v WORD)))
(define (rreg m r)  (truncate (mat-ref (regs m) 0 r)))
(define (wreg m r v) (mat-set (regs m) 0 r (mod v WORD)))
(define (pc   m)   (rreg m PCI))
(define (set-pc m v) (wreg m PCI v))
(define (halted-p m) (= (rreg m HLI) 1))
(define (do-halt m)  (wreg m HLI 1))

(define ISA
  (list (list 'halt  0 0) (list 'loadi 1 2) (list 'add 2 2)
        (list 'sub   3 2) (list 'mul  4 2) (list 'mov 5 2)
        (list 'load  6 2) (list 'store 7 2) (list 'jmp 8 1)
        (list 'jz    9 2) (list 'jnz 10 2) (list 'out 11 1)
        (list 'inc  12 1) (list 'dec  13 1)))

(define (isa-name op)
  (labels (((scan tbl)
      (cond ((null tbl) "???")
            ((= (cadr (car tbl)) op) (car (car tbl)))
            (t (scan (cdr tbl))))))
    (let ((s (scan ISA)))
      (cond ((eq s 'halt) "halt") ((eq s 'loadi) "loadi")
            ((eq s 'add) "add") ((eq s 'sub) "sub")
            ((eq s 'mul) "mul") ((eq s 'mov) "mov")
            ((eq s 'load) "load") ((eq s 'store) "store")
            ((eq s 'jmp) "jmp") ((eq s 'jz) "jz")
            ((eq s 'jnz) "jnz") ((eq s 'out) "out")
            ((eq s 'inc) "inc") ((eq s 'dec) "dec")
            (t "???")))))

; ---- debugger state -----------------------------------------
(define bp-list nil)   ; list of breakpoint addresses
(define step-mode nil) ; t when single-stepping

(define (debug-exec m w)
  (let ((op (hi w)) (a (mid w)) (b (lo w)))
    (print (str-join (list "  [" (num-to-str (pc m)) "] "
                           (isa-name (truncate op)) " "
                           (num-to-str a) " " (num-to-str b))))
    (cond ((= op 0) (do-halt m))
          ((= op 1) (wreg m a b))
          ((= op 2) (wreg m a (+ (rreg m a) (rreg m b))))
          ((= op 3) (wreg m a (- (rreg m a) (rreg m b))))
          ((= op 4) (wreg m a (* (rreg m a) (rreg m b))))
          ((= op 5) (wreg m a (rreg m b)))
          ((= op 6) (wreg m a (rd m (rreg m b))))
          ((= op 7) (wr m (rreg m a) (rreg m b)))
          ((= op 8) (set-pc m a))
          ((= op 9) (cond ((= (rreg m a) 0) (set-pc m b))))
          ((= op 10) (cond ((not (= (rreg m a) 0)) (set-pc m b))))
          ((= op 11) (print (str-join (list "  OUT> " (num-to-str (rreg m a))))))
          ((= op 12) (wreg m a (+ (rreg m a) 1)))
          ((= op 13) (wreg m a (- (rreg m a) 1)))
          (t nil))))

; ---- debugger commands -------------------------------------
(define (db-dump-regs m)
  (print "  R0-R9:")
  (db-print-regs m 0))

(define (db-print-regs m r)
  (cond ((>= r NREG) nil)
        (t (progn (print (str-join (list "    R" (num-to-str r) " = "
                                    (num-to-str (rreg m r)))))
                  (db-print-regs m (+ r 1))))))

(define (db-dump-mem m start count)
  (db-dump-range m start count))

(define (db-dump-range m addr left)
  (cond ((= left 0) nil)
        (t (progn (print (str-join (list "    [" (num-to-str addr) "] = "
                                    (num-to-str (rd m addr)))))
                  (db-dump-range m (+ addr 1) (- left 1))))))

(define (db-add-bp addr)
  (setq bp-list (cons addr bp-list))
  (print (str-join (list "  breakpoint set at " (num-to-str addr)))))

(define (db-at-bp m)
  (in (pc m) bp-list))

; ---- single-step run ----------------------------------------
(define (db-step m)
  (cond ((halted-p m) (print "  ** halted **"))
        (t (let ((p (pc m)))
             (let ((w (rd m p)))
               (set-pc m (+ p 1))
               (debug-exec m w))))))

; Run until halt, a breakpoint, or max-steps instructions.
(define (db-continue m max-steps)
  (labels (((run n)
      (cond ((halted-p m) (print "  ** halted **") n)
            ((= n max-steps) (print "  ** timeout **") n)
            ((db-at-bp m) (print (str-join (list "  ** breakpoint at "
                                       (num-to-str (pc m)) " **"))) n)
            (t (let ((p (pc m)))
                 (let ((w (rd m p)))
                   (set-pc m (+ p 1))
                   (exec-silent m w)
                   (run (+ n 1))))))))
    (run 0)))

(define (exec-silent m w)
  (let ((op (hi w)) (a (mid w)) (b (lo w)))
    (cond ((= op 0) (do-halt m))
          ((= op 1) (wreg m a b))
          ((= op 2) (wreg m a (+ (rreg m a) (rreg m b))))
          ((= op 3) (wreg m a (- (rreg m a) (rreg m b))))
          ((= op 4) (wreg m a (* (rreg m a) (rreg m b))))
          ((= op 5) (wreg m a (rreg m b)))
          ((= op 6) (wreg m a (rd m (rreg m b))))
          ((= op 7) (wr m (rreg m a) (rreg m b)))
          ((= op 8) (set-pc m a))
          ((= op 9) (cond ((= (rreg m a) 0) (set-pc m b))))
          ((= op 10) (cond ((not (= (rreg m a) 0)) (set-pc m b))))
          ((= op 11) (rreg m a))
          ((= op 12) (wreg m a (+ (rreg m a) 1)))
          ((= op 13) (wreg m a (- (rreg m a) 1)))
          (t nil))))

(define (str-join parts)
  (cond ((null parts) "")
        ((null (cdr parts)) (car parts))
        (t (cat (car parts) (str-join (cdr parts))))))

; ---- demonstration ------------------------------------------
(define demo-cpu (make-cpu))
; No program is loaded, so memory is all zeros and the first step
; executes word 0, which decodes as HALT.
(db-step demo-cpu)
(db-dump-regs demo-cpu)
(db-dump-mem demo-cpu 0 4)
(db-add-bp 3)
