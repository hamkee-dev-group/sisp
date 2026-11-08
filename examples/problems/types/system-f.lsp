; System F: Polymorphic lambda calculus. Type checker.
; Uses 'biglam' for type abstraction (Lambda-case-fold-collision in SISP).

(define (fv-p ty) (and (symbolp ty) (not (in ty '(-> forall int bool)))))
(define (ff-p ty) (and (listp ty) (eq (car ty) '->)))
(define (fa-p ty) (and (listp ty) (eq (car ty) 'forall)))

(define (f-lk nm env)
  (let ((e (find (lambda (b) (eq (car b) nm)) env))) (cond (e (cdr e)) (t nil))))

(define (f-ex nm ty env) (cons (cons nm ty) env))

(define (f-sb var repl ty)
  (cond ((equal ty var) repl)
        ((ff-p ty) (list '-> (f-sb var repl (cadr ty)) (f-sb var repl (caddr ty))))
        ((fa-p ty) (cond ((eq (cadr ty) var) ty) (t (list 'forall (cadr ty) (f-sb var repl (caddr ty))))))
        (t ty)))

(define (f-eq a b)
  (cond ((eq a nil) (eq b nil))
        ((atom a) (cond ((atom b) (eq a b)) (t nil)))
        ((atom b) nil)
        ((eq (car a) '->) (cond ((eq (car b) '->) (cond ((f-eq (cadr a) (cadr b)) (f-eq (caddr a) (caddr b))) (t nil))) (t nil)))
        ((eq (car a) 'forall) (cond ((eq (car b) 'forall) (cond ((eq (cadr a) (cadr b)) (f-eq (caddr a) (caddr b))) (t nil))) (t nil)))
        (t nil)))

(define (f-inf expr env)
  (cond ((eq expr nil) 'err)
        ((numberp expr) 'int)
        ((symbolp expr)
         (cond ((eq expr 'true) 'bool) ((eq expr 'nil) 'bool)
               (t (let ((s (f-lk expr env))) (cond (s s) (t 'err))))))
        ((cond ((listp expr) nil) (t t)) 'err)
        ((eq (car expr) 'lambda)
         (let ((arg (caadr expr)) (aty (cadadr expr)) (body (caddr expr)))
           (list '-> aty (f-inf body (f-ex arg aty env)))))
        ((eq (car expr) 'biglam)
         (let ((var (cadr expr)) (body (caddr expr)))
           (list 'forall var (f-inf body (f-ex var var env)))))
        ((eq (car expr) 'tyapp)
         (let ((term (cadr expr)) (targ (caddr expr)))
           (let ((tty (f-inf term env)))
             (cond ((fa-p tty) (f-sb (cadr tty) targ (caddr tty))) (t 'err)))))
        (t (let ((fty (f-inf (car expr) env)))
             (cond ((eq fty 'err) 'err)
                   ((ff-p fty)
                    (let ((aty (f-inf (cadr expr) env)))
                      (cond ((eq aty 'err) 'err)
                            ((f-eq aty (cadr fty)) (caddr fty))
                            (t 'err))))
                   (t 'err))))))

(define (typecheck expr env)
  (let ((ty (f-inf expr env))) (cond ((eq ty 'err) nil) (t ty))))

; Identity: biglam a (lambda (x a) x) : forall a (-> a a)
(define tc1 (typecheck '(biglam a (lambda (x a) x)) '()))
(equal tc1 '(forall a (-> a a))) t

; Type app to int
(define tc2 (typecheck '(tyapp (biglam a (lambda (x a) x)) int) '()))
(equal tc2 '(-> int int)) t

; Constant polymorphic function
(define tc3 (typecheck '(biglam a (biglam b (lambda (x a) (lambda (y b) x)))) '()))
(equal (fa-p tc3) t) t

; Simple application
(define tc4 (typecheck '((lambda (x int) x) 5) '()))
(equal tc4 'int) t

; Type mismatch -> nil
(define tc5 (typecheck '((lambda (x int) x) true) '()))
(equal tc5 nil) t

; Self-app to bool via type application
(define tc6 (typecheck '(tyapp (biglam a (lambda (x a) x)) bool) '()))
(equal tc6 '(-> bool bool)) t
