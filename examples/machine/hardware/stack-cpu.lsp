; ============================================================
; STACK CPU — AN EXTENDED VM FOR THE SISP MACHINE
;
; Extends cpu.lsp with a hardware stack (in RAM at addresses
; 240-255), call/ret instructions, and push/pop operations.
; Stack pointer is register R6, frame pointer is R7.
; ============================================================
; ---- word geometry (shared with cpu.lsp) -------------------
(define WORD  16777216)
(define MEM   256)
(define NREG  10)
(define PCI   8)
(define HLI   9)
(define SPI   6)
(define FPI   7)
(define STACK_BASE 255)
(define STACK_MIN  240)

(define (hi w) (mod (truncate (/ w 65536)) 256))
(define (mid w) (mod (truncate (/ w 256)) 256))
(define (lo w) (mod w 256))

; ---- machine state -----------------------------------------
(define (make-cpu) (list (make-matrix 1 MEM) (make-matrix 1 NREG)))
(define (ram  m) (car m))
(define (regs m) (car (cdr m)))
(define (rd  m a)   (truncate (mat-ref (ram m) 0 a)))
(define (wr  m a v) (mat-set (ram m) 0 a (mod v WORD)))
(define (rreg m r)  (truncate (mat-ref (regs m) 0 r)))
(define (wreg m r v) (mat-set (regs m) 0 r (mod v WORD)))
(define (pc   m)   (rreg m PCI))
(define (set-pc  m v) (wreg m PCI v))
(define (halted-p m) (= (rreg m HLI) 1))
(define (do-halt m)   (wreg m HLI 1))

; ---- stack operations --------------------------------------
(define (init-stack m)
  (wreg m SPI STACK_BASE)
  (wreg m FPI STACK_BASE))

(define (stk-push m val)
  (let ((sp (- (rreg m SPI) 1)))
    (wreg m SPI sp)
    (wr m sp val)))

(define (stk-pop m)
  (let ((sp (rreg m SPI)))
    (let ((v (rd m sp)))
      (wreg m SPI (+ sp 1))
      v)))

; ---- ISA with stack extensions ------------------------------
(define ISA
  (list (list 'halt  0 0) (list 'loadi 1 2) (list 'add 2 2)
        (list 'sub   3 2) (list 'mul  4 2) (list 'mov 5 2)
        (list 'load  6 2) (list 'store 7 2) (list 'jmp 8 1)
        (list 'jz    9 2) (list 'jnz 10 2) (list 'out 11 1)
        (list 'inc  12 1) (list 'dec  13 1)
        (list 'push 14 1) (list 'pop  15 1)
        (list 'call 16 1) (list 'ret  17 0)))
(define (opcode name) (car (cdr (assoc name ISA))))

; ---- assembler ----------------------------------------------
(define (rev lst)
  (labels (((go l a) (cond ((null l) a) (t (go (cdr l) (cons (car l) a)))))) (go lst nil)))
(define (nth-or ops i)
  (cond ((null ops) 0) ((= i 0) (car ops)) (t (nth-or (cdr ops) (- i 1)))))
(define (labels-pass1 prog)
  (labels (((go p addr tbl)
      (cond ((null p) tbl)
            ((eq (car (car p)) 'label)
             (go (cdr p) addr (cons (list (cadr (car p)) addr) tbl)))
            (t (go (cdr p) (+ addr 1) tbl)))))
    (go prog 0 nil)))
(define (resolve x tbl) (cond ((numberp x) x) (t (cadr (assoc x tbl)))))
(define (encode stmt tbl)
  (let ((op (opcode (car stmt)))
        (a  (resolve (nth-or (cdr stmt) 0) tbl))
        (b  (resolve (nth-or (cdr stmt) 1) tbl)))
    (+ (* op 65536) (* a 256) b)))
(define (assemble prog)
  (let ((tbl (labels-pass1 prog)))
    (labels (((go p acc)
        (cond ((null p) (rev acc))
              ((eq (car (car p)) 'label) (go (cdr p) acc))
              (t (go (cdr p) (cons (encode (car p) tbl) acc))))))
      (go prog nil))))
(define (load-image m words)
  (labels (((go w a)
      (cond ((null w) a) (t (progn (wr m a (car w)) (go (cdr w) (+ a 1)))))))
    (go words 0)))

; ---- execution engine --------------------------------------
(define (exec m w)
  (let ((op (hi w)) (a (mid w)) (b (lo w)))
    (cond           ((= op  0) (do-halt m))
          ((= op  1) (wreg m a b))
          ((= op  2) (wreg m a (+ (rreg m a) (rreg m b))))
          ((= op  3) (wreg m a (- (rreg m a) (rreg m b))))
          ((= op  4) (wreg m a (* (rreg m a) (rreg m b))))
          ((= op  5) (wreg m a (rreg m b)))
          ((= op  6) (wreg m a (rd m (rreg m b))))
          ((= op  7) (wr m (rreg m a) (rreg m b)))
          ((= op  8) (set-pc m a))
          ((= op  9) (cond ((= (rreg m a) 0) (set-pc m b))))
          ((= op 10) (cond ((not (= (rreg m a) 0)) (set-pc m b))))
          ((= op 11) (print (num-to-str (rreg m a))))
          ((= op 12) (wreg m a (+ (rreg m a) 1)))
          ((= op 13) (wreg m a (- (rreg m a) 1)))
          ((= op 14) (stk-push m (rreg m a)))
          ((= op 15) (wreg m a (stk-pop m)))
          ((= op 16) (progn (stk-push m (pc m)) (set-pc m a)))
          ((= op 17) (set-pc m (stk-pop m)))
          (t nil))))

(define (run m budget)
  (init-stack m)
  (labels (((cycle n)
      (cond ((halted-p m) n) ((= n budget) n)
            (t (let ((p (pc m)))
                 (let ((w (rd m p)))
                   (set-pc m (+ p 1))
                   (exec m w)
                   (cycle (+ n 1))))))))
    (cycle 0)))

; ---- demonstration: recursive factorial --------------------
(define fact-src
  (list '(loadi 0 5)          ; n = 5
        '(call f)              ; factorial(n)
        '(out 0)               ; print result
        '(halt)
        '(label f)             ; factorial entry
        '(loadi 1 1)
        '(jnz 0 r)             ; if n!=0 goto recurse
        '(loadi 0 1)           ; base: return 1
        '(ret)
        '(label r)             ; recurse
        '(push 0)              ; save n
        '(dec 0)               ; n -= 1
        '(call f)              ; recurse
        '(pop 1)               ; restore n
        '(mul 0 1)             ; multiply
        '(ret)))

(define cpu (make-cpu))
(load-image cpu (assemble fact-src))
(run cpu 1000)
(rreg cpu 0)   ; => 120
