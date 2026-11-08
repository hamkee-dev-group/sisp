; Futamura projections: specialization of interpreters.
; (mix int prog) = specialize interpreter to program => compiled program.
; 1st projection: specialize interpreter to program.
; 2nd projection: specialize specializer to interpreter = compiler.
; 3rd projection: specialize specializer to specializer = compiler generator.
; Verify that (run (mix int prog) input) = (run int prog input).

(define (fut-int-apply op args)
  (cond ((eq op '+) (+ (car args) (cadr args)))
        ((eq op '-) (- (car args) (cadr args)))
        ((eq op '*) (* (car args) (cadr args)))
        ((eq op '/) (truncate (/ (car args) (cadr args))))
        ((eq op 'if) (cond ((car args) (cadr args)) (t (caddr args))))
        ((eq op '=) (cond ((= (car args) (cadr args)) t) (t nil)))
        ((eq op '<) (cond ((< (car args) (cadr args)) t) (t nil)))
        ((eq op '>) (cond ((> (car args) (cadr args)) t) (t nil)))
        (t 0)))

(define (fut-int-eval expr env)
  (cond ((numberp expr) expr)
        ((symbolp expr)
         (let ((entry (find (lambda (b) (eq (car b) expr)) env)))
           (cond (entry (cdr entry)) (t expr))))
        ((eq (car expr) 'lambda)
         (list 'closure (cadr expr) (caddr expr) env))
        ((and (listp expr) (eq (car expr) 'apply))
         (let ((fun (fut-int-eval (cadr expr) env))
               (args (map (lambda (a) (fut-int-eval a env)) (cddr expr))))
           (cond ((and (listp fun) (eq (car fun) 'closure))
                  (fut-int-eval (caddr fun)
                    (fut-extend (caadr fun) (car args) (cadddr fun))))
                 ((symbolp fun)
                  (fut-int-apply fun args))
                 (t expr))))
        (t (let ((vals (map (lambda (x) (fut-int-eval x env)) expr)))
             (cond ((symbolp (car expr))
                    (fut-int-apply (car expr) (cdr vals)))
                   (t vals))))))

(define (fut-extend var val env)
  (cons (cons var val) env))

(define (fut-interpret prog input)
  (fut-int-eval (list 'apply prog input) nil))

(define (fut-program f)
  (list 'lambda (list 'x) (list f 'x)))

(define (fut-mix int prog)
  (let ((prog-body (caddr prog)))
    (fut-specialize int prog-body)))

(define (fut-specialize int prog)
  (cond ((numberp prog)
         (list 'const prog))
        ((symbolp prog)
         (list 'var prog))
        ((eq (car prog) 'lambda)
         (list 'lam (cadr prog) (fut-specialize int (caddr prog))))
        ((eq (car prog) 'apply)
         (let ((f (fut-specialize int (cadr prog)))
               (a (fut-specialize int (caddr prog))))
           (list 'app f a)))
        (t (list 'op (car prog) (map (lambda (x) (fut-specialize int x)) (cdr prog))))))

(define (fut-exec spec input)
  (fut-exec-spec spec (list (cons 'x input))))

(define (fut-exec-spec spec env)
  (cond ((and (listp spec) (eq (car spec) 'const)) (cadr spec))
        ((and (listp spec) (eq (car spec) 'var))
         (let ((entry (find (lambda (b) (eq (car b) (cadr spec))) env)))
           (cond (entry (cdr entry)) (t 0))))
        ((and (listp spec) (eq (car spec) 'op))
         (let ((args (map (lambda (s) (fut-exec-spec s env)) (caddr spec))))
           (fut-int-apply (cadr spec) args)))
        ((and (listp spec) (eq (car spec) 'app))
         (let ((fun (fut-exec-spec (cadr spec) env))
               (arg (fut-exec-spec (caddr spec) env)))
           (cond (fun arg) (t 0))))
        (t 0)))

; Test interpreter on simple expressions
(define fut-identity '(lambda (x) x))
(define fut-add1 '(lambda (x) (+ x 1)))
(define fut-square '(lambda (x) (* x x)))

; Direct interpretation
(check "fut-9" (equal (fut-interpret fut-identity 42) 42))
(check "fut-10" (equal (fut-interpret fut-add1 10) 11))
(check "fut-11" (equal (fut-interpret fut-square 5) 25))

; 1st Futamura projection: (mix int prog) produces specialized code
(define fut-mix-id (fut-mix nil fut-identity))
(define fut-mix-add1 (fut-mix nil fut-add1))

; Execute specialized code
(check "fut-12" (equal (fut-exec fut-mix-id 42) 42))
(check "fut-13" (equal (fut-exec fut-mix-add1 10) 11))

; Verify equivalence: (run (mix int prog) input) = (run int prog input)
(define (fut-verify-prog prog input)
  (let ((direct (fut-interpret prog input))
        (compiled (fut-mix nil prog)))
    (let ((specialized-result (fut-exec compiled input)))
      (equal direct specialized-result))))

; Verify on multiple programs and inputs
(check "fut-1" (equal (fut-verify-prog fut-identity 42) t))
(check "fut-2" (equal (fut-verify-prog fut-identity 100) t))
(check "fut-3" (equal (fut-verify-prog fut-add1 0) t))
(check "fut-4" (equal (fut-verify-prog fut-add1 99) t))
(check "fut-5" (equal (fut-verify-prog fut-square 7) t))
(check "fut-6" (equal (fut-verify-prog fut-square 0) t))

; Test more complex program: if x < 5 then x+1 else x-1
(define fut-cond '(lambda (x) (if (< x 5) (+ x 1) (- x 1))))
(check "fut-14" (equal (fut-interpret fut-cond 3) 4))
(check "fut-15" (equal (fut-interpret fut-cond 7) 6))
(check "fut-7" (equal (fut-verify-prog fut-cond 3) t))
(check "fut-8" (equal (fut-verify-prog fut-cond 10) t))

; Identity function on various inputs
(check "fut-16" (equal (fut-interpret fut-identity 0) 0))
(check "fut-17" (equal (fut-interpret fut-identity 999) 999))
