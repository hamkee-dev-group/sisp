; ============================================================
; SECD MACHINE — Landin's abstract machine for lambda calculus
;
; Four stacks: Stack, Environment, Control, Dump
; Executes lambda calculus expressions via CEK-style reduction.
; ============================================================

; Machine state: (S E C D)
(define (secd-S m) (car m))
(define (secd-E m) (cadr m))
(define (secd-C m) (caddr m))
(define (secd-D m) (cadddr m))

(define (mk-secd) (list nil nil nil nil))

; Environment: alist of (var value) frames
(define (env-lookup var env)
  (let ((slot (assoc var env)))
    (cond ((null slot) var)        ; free var = itself
          (t (cadr slot)))))

; ---- SECD step function --------------------------------------
(define (secd-step state)
  (let ((s (secd-S state))
        (e (secd-E state))
        (c (secd-C state))
        (d (secd-D state)))
    (cond ((null c) state)  ; halt
          ; LDC: push constant onto S
          ((eq (caar c) 'LDC)
           (list (cons (cadar c) s) e (cdr c) d))
          ; LD: push variable value onto S
          ((eq (caar c) 'LD)
           (list (cons (env-lookup (cadar c) e) s) e (cdr c) d))
          ; ADD / SUB / MUL: pop two values, combine, push
          ((eq (caar c) 'ADD)
           (list (cons (+ (cadr s) (car s)) (cddr s)) e (cdr c) d))
          ((eq (caar c) 'SUB)
           (list (cons (- (cadr s) (car s)) (cddr s)) e (cdr c) d))
          ((eq (caar c) 'MUL)
           (list (cons (* (cadr s) (car s)) (cddr s)) e (cdr c) d))
          ; LDF: build a closure capturing the CURRENT environment.
          ; A closure is (CLOSURE param body env).
          ((eq (caar c) 'LDF)
           (list (cons (list 'CLOSURE (cadar c) (caddar c) e) s) e (cdr c) d))
          ; AP: apply — argument on top of the stack, closure beneath
          ((eq (caar c) 'AP)
           (let ((arg (car s)) (clo (cadr s)))
             (cond ((eq (car clo) 'CLOSURE)
                    (list nil
                          (cons (list (cadr clo) arg) (cadddr clo))
                          (caddr clo)
                          (list (cddr s) e (cdr c) d)))
                   (t (print "AP: not a closure") state))))
          ; RTN: return from function
          ((eq (caar c) 'RTN)
           (let ((val (car s))
                 (old-s (car d))
                 (old-e (cadr d))
                 (old-c (caddr d))
                 (old-d (cadddr d)))
             (list (cons val old-s) old-e old-c old-d)))
          ; SEL: pop the test, run a branch, and park the rest of C on
          ; the dump for the matching JOIN to pick back up.
          ((eq (caar c) 'SEL)
           (let ((then-branch (cadar c)) (else-branch (caddar c)))
             (list (cdr s) e
                   (cond ((= (car s) 0) else-branch) (t then-branch))
                   (list (cdr c) d))))
          ; JOIN: resume the code SEL parked on the dump
          ((eq (caar c) 'JOIN)
           (list s e (car d) (cadr d)))
          ; DUM: dummy — create empty env frame
          ((eq (caar c) 'DUM)
           (list s (cons nil e) (cdr c) d))
          ; RAP: recursive apply.  (RAP f) applies the closure under the
          ; argument and also binds it, under the name f, in the very
          ; environment it runs in — so the body can call itself.  The
          ; knot is tied with set-car after the frame exists.
          ((eq (caar c) 'RAP)
           (let ((arg (car s)) (clo (cadr s)))
             (let ((frame (list (cadar c) nil)))
               (let ((shared (cons frame (cadddr clo))))
                 (set-car (cdr frame)
                          (list 'CLOSURE (cadr clo) (caddr clo) shared))
                 (list nil
                       (cons (list (cadr clo) arg) shared)
                       (caddr clo)
                       (list (cddr s) e (cdr c) d))))))
          ; caar c is the opcode symbol itself, so print the instruction.
          (t (print (list 'unknown-secd-instruction (car c))) state))))

(define (secd-run state steps)
  (cond ((or (null (secd-C state)) (= steps 0)) state)
        (t (secd-run (secd-step state) (- steps 1)))))

; ---- Compile lambda expression -> SECD code ------------------
; (fn x => x + 1) 2  compiles to
;   LDF x ((LD x) (LDC 1) (ADD) (RTN)), LDC 2, AP
; Closure representation: (CLOSURE param body env)

(define (str-join parts sep)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (cat (car parts) sep) (str-join (cdr parts) sep)))))

; ============================================================
; DEMO
; ============================================================
(print "=============================================")
(print "  SECD MACHINE — Lambda Calculus Abstract Machine")
(print "=============================================")
(print "")

; Program: compute (lambda x => x * 2 + 3) 5
; AP wants the argument on top of the stack and the closure beneath it,
; so the closure is loaded first.
(define closure-body (list
  (list 'LD 'x)
  (list 'LDC 2)
  (list 'MUL)
  (list 'LDC 3)
  (list 'ADD)
  (list 'RTN)))

(define program (list
  (list 'LDF 'x closure-body)
  (list 'LDC 5)
  (list 'AP)))

(print "Program: (lambda x => x*2+3) 5")
(define state (list nil nil program nil))
(define result (secd-run state 20))
(define stack (secd-S result))
(print (str-join (list "Result on stack: " (num-to-str (car stack)) " (expected 13)") ""))
(print "")

; Program: ((lambda x => lambda y => x + y) 10) 20
; The inner lambda must be built with LDF *inside* the outer body, so
; that it captures x.  A pre-built constant closure would not see it.
(define inner-body (list (list 'LD 'x) (list 'LD 'y) (list 'ADD) (list 'RTN)))
(define outer-body (list (list 'LDF 'y inner-body) (list 'RTN)))
(define curry-prog (list (list 'LDF 'x outer-body)
                    (list 'LDC 10) (list 'AP)
                    (list 'LDC 20) (list 'AP)))

(define state2 (list nil nil curry-prog nil))
(define result2 (secd-run state2 30))
(define stack2 (secd-S result2))
(print "Program: ((lambda x => lambda y => x+y) 10) 20")
(print (str-join (list "Result: " (num-to-str (car stack2)) " (expected 30)") ""))
(print "")

; Program: recursive factorial, exercising SEL/JOIN and RAP.
;   fact = lambda n => if n = 0 then 1 else n * fact(n-1)
(define fact-body (list
  (list 'LD 'n)
  (list 'SEL
        (list (list 'LD 'fact) (list 'LD 'n) (list 'LDC 1) (list 'SUB)
              (list 'AP) (list 'LD 'n) (list 'MUL) (list 'JOIN))   ; n /= 0
        (list (list 'LDC 1) (list 'JOIN)))                          ; n = 0
  (list 'RTN)))

(define prog3 (list (list 'LDF 'n fact-body)
                    (list 'LDC 5)
                    (list 'RAP 'fact)))
(define result3 (secd-run (list nil nil prog3 nil) 400))
(define stack3 (secd-S result3))
(print "Program: fact 5 (recursive, via SEL/JOIN + RAP)")
(print (str-join (list "Result: " (num-to-str (car stack3)) " (expected 120)") ""))
(print "--- Checks ---")

(check "SECD: (lambda x => x*2+3) 5 = 13" (= (car stack) 13))
(check "SECD: application leaves exactly one value on S"
       (= (length stack) 1))
(check "SECD: control is exhausted when the machine halts"
       (null (secd-C result)))
(check "SECD: curried closure captures its outer binding, 10+20 = 30"
       (= (car stack2) 30))
(check "SECD: recursion via RAP, fact 5 = 120" (= (car stack3) 120))

; Individual instructions, checked one step at a time.
(check "LDC pushes its constant"
       (equal (secd-S (secd-step (list nil nil (list (list 'LDC 7)) nil))) (list 7)))
(check "LD resolves through the environment"
       (equal (secd-S (secd-step (list nil (list (list 'z 9)) (list (list 'LD 'z)) nil)))
              (list 9)))
(check "SUB subtracts the top of stack from the one below"
       (equal (secd-S (secd-step (list (list 3 10) nil (list (list 'SUB)) nil)))
              (list 7)))
(check "SEL takes the else branch on zero and parks the rest of C"
       (let ((st (secd-step (list (list 0) nil
                                  (list (list 'SEL (list (list 'LDC 1))
                                                    (list (list 'LDC 2)))
                                        (list 'RTN))
                                  nil))))
         (and (equal (secd-C st) (list (list 'LDC 2)))
              (equal (car (secd-D st)) (list (list 'RTN))))))
(check "JOIN restores the code SEL parked"
       (equal (secd-C (secd-step (list nil nil (list (list 'JOIN))
                                       (list (list (list 'RTN)) nil))))
              (list (list 'RTN))))
(check "a halted machine is a fixed point of secd-step"
       (equal (secd-step (list (list 1) nil nil nil)) (list (list 1) nil nil nil)))
(check "an unknown opcode leaves the state untouched"
       (let ((st (list (list 1) nil (list (list 'NOPE)) nil)))
         (equal (secd-step st) st)))
(print "")

(print "=============================================")
(print "  SECD Machine — higher-order functions work")
(print "=============================================")
