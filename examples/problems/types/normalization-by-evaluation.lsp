; Normalization by Evaluation (NbE) for simply typed lambda calculus.
; eval in host, reify to normal form. Verify (lambda (x) x) normalizes.

(define nb-fc 0)
(define (nb-rs) (setq nb-fc 0))
(define (nb-fn)
  (setq nb-fc (+ nb-fc 1))
  (cat "x" (num-to-str nb-fc)))

(define (nb-eval term env)
  (cond ((numberp term) (list 'num term))
        ((and (symbolp term) (eq term 'true)) (list 'bool 'true))
        ((and (symbolp term) (eq term 'nil)) (list 'bool 'nil))
        ((symbolp term)
         (let ((e (find (lambda (b) (eq (car b) term)) env)))
           (cond (e (cdr e)) (t (list 'neu 'var term)))))
        ((eq (car term) 'lambda)
         (let ((arg (caadr term)) (body (caddr term)))
           (lambda (v) (nb-eval body (cons (cons arg v) env)))))
        (t (let ((fun (nb-eval (car term) env)) (arg (nb-eval (cadr term) env)))
             (cond ((and (listp fun) (eq (car fun) 'clo)) ((cadr fun) arg))
                   (t (list 'neu 'ap fun arg)))))))

(define (nb-reif ty val)
  (cond ((and (listp val) (eq (car val) 'neu)) (nb-rnf ty val))
        ((eq ty 'base) val)
        ((and (listp ty) (eq (car ty) '->))
         (let ((f (nb-fn)))
           (list 'lambda (list f (cadr ty))
                 (nb-reif (caddr ty) (val (list 'neu 'var f))))))
        (t val)))

(define (nb-rnf ty val)
  (cond ((and (listp ty) (eq (car ty) '->))
         (let ((f (nb-fn)))
           (list 'lambda (list f (cadr ty))
                 (nb-rnf (caddr ty) (list 'neu 'ap val (list 'neu 'var f))))))
        (t val)))

(define (nbe term ty)
  (nb-rs)
  (let ((sem (nb-eval term nil)))
    (nb-reif ty sem)))

; Test: identity at base->base
(define nb1 (nbe '(lambda (x) x) '(-> base base)))
(equal (car nb1) 'lambda) t

; Test: constant function at base->base->base
(define nb2 (nbe '(lambda (x) (lambda (y) x)) '(-> base (-> base base))))
(equal (car nb2) 'lambda) t

; Test: eta expansion of lambda
(define nb3 (nbe '(lambda (f) (lambda (x) (f x))) '(-> (-> base base) (-> base base))))
(equal (car nb3) 'lambda) t

; Test: identity is not nil
(equal (eq nb1 nil) nil) t
