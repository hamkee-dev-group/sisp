; Supercompilation: Turchin's method. Drive information forward through evaluation.
; (supercompile '(app (fix f (lam x (if (zero x) 1 (* x (f (- x 1))))) 3))
; -> 6 (fully evaluated)

(define (sc-peval expr env)
  (cond ((numberp expr) expr)
        ((symbolp expr)
         (let ((e (find (lambda (b) (eq (car b) expr)) env)))
           (cond (e (cdr e)) (t expr))))
        ((eq (car expr) 'if)
         (let ((cv (sc-peval (cadr expr) env)))
           (cond ((eq cv 'true) (sc-peval (caddr expr) env))
                 ((eq cv 'nil) (sc-peval (cadddr expr) env))
                 (t (list 'if cv (sc-peval (caddr expr) env)
                          (sc-peval (cadddr expr) env))))))
        ((eq (car expr) '+)
         (let ((a (sc-peval (cadr expr) env)) (b (sc-peval (caddr expr) env)))
           (cond ((and (numberp a) (numberp b)) (+ a b)) (t (list '+ a b)))))
        ((eq (car expr) '-)
         (let ((a (sc-peval (cadr expr) env)) (b (sc-peval (caddr expr) env)))
           (cond ((and (numberp a) (numberp b)) (- a b)) (t (list '- a b)))))
        ((eq (car expr) '*)
         (let ((a (sc-peval (cadr expr) env)) (b (sc-peval (caddr expr) env)))
           (cond ((and (numberp a) (numberp b)) (* a b)) (t (list '* a b)))))
        ((eq (car expr) '=)
         (let ((a (sc-peval (cadr expr) env)) (b (sc-peval (caddr expr) env)))
           (cond ((and (numberp a) (numberp b)) (cond ((= a b) 'true) (t 'nil)))
                 (t (list '= a b)))))
        ((eq (car expr) 'zero)
         (let ((v (sc-peval (cadr expr) env)))
           (cond ((and (numberp v) (= v 0)) 'true) ((numberp v) 'nil) (t (list 'zero v)))))
        ; do not evaluate under lambda: a recursive reference inside a
        ; body would unfold forever (standard supercompilation needs
        ; generalization there, which is out of scope for this demo)
        ((eq (car expr) 'lambda) (list 'lambda (cadr expr) (caddr expr)))
        ((eq (car expr) 'fix) (list 'fix (cadr expr)))
        ((eq (car expr) 'app)
         (let ((fun (sc-peval (cadr expr) env)) (arg (sc-peval (caddr expr) env)))
           (cond ((and (listp fun) (eq (car fun) 'lambda))
                  (sc-peval (caddr fun) (cons (cons (caadr fun) arg) env)))
                 ((and (listp fun) (eq (car fun) 'fix)
                       (listp (cadr fun)) (eq (car (cadr fun)) 'lambda))
                  (let* ((lam (cadr fun))
                         (param (caadr lam))
                         (body (caddr lam))
                         ; unfold once: f is bound to the fixpoint itself
                         (inner (sc-peval body (cons (cons param fun) env))))
                    (cond ((and (listp inner) (eq (car inner) 'lambda)
                                (numberp arg))
                           (let ((expanded (sc-peval (caddr inner)
                                                     (cons (cons (caadr inner) arg)
                                                           (cons (cons param fun)
                                                                 env)))))
                             (cond ((numberp expanded) expanded)
                                   (t (sc-drive expanded (list 'app fun arg) 20)))))
                          (t (list 'app fun arg)))))
                 (t (list 'app fun arg)))))
        (t (map (lambda (x) (sc-peval x env)) expr))))

(define (sc-drive expr prev fuel)
  (cond ((<= fuel 0) prev)
        ((numberp expr) expr)
        ((equal expr prev) expr)
        (t (sc-drive (sc-peval expr nil) expr (- fuel 1)))))

(define (sc-supercompile expr)
  (sc-drive expr nil 50))

; Factorial: fix f. lam n. if (zero n) 1 (* n (app f (- n 1)))
(define sc-fac '(fix (lambda (f) (lambda (n) (if (zero n) 1 (* n (app f (- n 1))))))))

; (app fact 3) -> 6
(define sc3 (sc-supercompile (list 'app sc-fac 3)))
(check "sc-1" (equal sc3 6))

; (app fact 4) -> 24
(define sc4 (sc-supercompile (list 'app sc-fac 4)))
(check "sc-2" (equal sc4 24))

; (app fact 0) -> 1
(define sc0 (sc-supercompile (list 'app sc-fac 0)))
(check "sc-3" (equal sc0 1))

; (app fact 1) -> 1
(define sc1 (sc-supercompile (list 'app sc-fac 1)))
(check "sc-4" (equal sc1 1))

; Constant folding
(check "sc-5" (equal (sc-peval '(+ 1 2) nil) 3))
(check "sc-6" (equal (sc-peval '(* 4 5) nil) 20))

; Conditional with known condition
(check "sc-7" (equal (sc-peval '(if true 10 20) nil) 10))
(check "sc-8" (equal (sc-peval '(if nil 10 20) nil) 20))
