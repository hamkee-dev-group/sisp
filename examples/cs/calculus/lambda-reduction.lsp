; Lambda calculus reduction strategies.
; Terms: (var name), (lam param body), (app f x).
; alpha-conversion: rename bound variables to avoid capture.
; beta-reduction: (app (lam p b) a) -> subst(b, p, a).
; eta-reduction: (lam x (app f x)) -> f when x not free in f.
; Normal order: leftmost outermost. Applicative order: leftmost innermost.
; Church-Rosser property demonstration.

(define (lc-var-p term) (symbolp term))
(define (lc-lam-p term) (and (consp term) (eq (car term) 'lam) (= (length term) 3)))
(define (lc-app-p term) (and (consp term) (eq (car term) 'app) (= (length term) 3)))

(define (lc-free-vars term)
  (cond ((lc-var-p term) (list term))
        ((lc-lam-p term) (remove (lambda (v) (eq v (cadr term))) (lc-free-vars (caddr term))))
        ((lc-app-p term) (dedup (append (lc-free-vars (cadr term)) (lc-free-vars (caddr term)))))
        (t nil)))

(define (lc-fresh-name used)
  (let ((candidates '(x y z u v w a b c p q r s)))
    (let ((found (find (lambda (c) (not (contains c used))) candidates)))
      (cond (found found) (t 'x)))))

(define (lc-subst term var val)
  (cond ((lc-var-p term)
         (cond ((eq term var) val) (t term)))
        ((lc-lam-p term)
         (cond ((eq (cadr term) var) term)
               ((contains (cadr term) (lc-free-vars val))
                (let ((fresh (lc-fresh-name (append (lc-free-vars val) (lc-free-vars (caddr term))))))
                  (list 'lam fresh
                    (lc-subst (lc-subst (caddr term) (cadr term) fresh) var val))))
               (t (list 'lam (cadr term) (lc-subst (caddr term) var val)))))
        ((lc-app-p term)
         (list 'app (lc-subst (cadr term) var val) (lc-subst (caddr term) var val)))
        (t term)))

(define (lc-beta-redex-p term)
  (and (lc-app-p term) (lc-lam-p (cadr term))))

(define (lc-beta-reduce term)
  (cond ((lc-beta-redex-p term)
         (lc-subst (caddr (cadr term)) (cadr (cadr term)) (caddr term)))
        (t term)))

; Normal order: leftmost outermost redex
(define (lc-normal-order term)
  (cond ((lc-var-p term) term)
        ((lc-beta-redex-p term) (lc-beta-reduce term))
        ((lc-app-p term)
         (let ((f-reduced (lc-normal-order (cadr term))))
           (cond ((not (equal f-reduced (cadr term)))
                  (list 'app f-reduced (caddr term)))
                 (t (list 'app (cadr term) (lc-normal-order (caddr term)))))))
        ((lc-lam-p term)
         (list 'lam (cadr term) (lc-normal-order (caddr term))))
        (t term)))

; Applicative order: leftmost innermost redex
(define (lc-app-order term)
  (cond ((lc-var-p term) term)
        ((lc-beta-redex-p term)
         (let ((f (lc-app-order (cadr term)))
               (a (lc-app-order (caddr term))))
           (cond ((and (equal f (cadr term)) (equal a (caddr term)))
                  (lc-beta-reduce term))
                 (t (lc-app-order (list 'app f a))))))
        ((lc-app-p term)
         (let ((f (lc-app-order (cadr term)))
               (a (lc-app-order (caddr term))))
           (list 'app f a)))
        ((lc-lam-p term)
         (list 'lam (cadr term) (lc-app-order (caddr term))))
        (t term)))

(define (lc-reduce-normal term budget)
  (cond ((= budget 0) term)
        (t (let ((next (lc-normal-order term)))
             (cond ((equal next term) term)
                   (t (lc-reduce-normal next (- budget 1))))))))

(define (lc-reduce-app term budget)
  (cond ((= budget 0) term)
        (t (let ((next (lc-app-order term)))
             (cond ((equal next term) term)
                   (t (lc-reduce-app next (- budget 1))))))))

; Church-Rosser demonstration
(define (lc-steps-normal term budget acc)
  (cond ((or (= budget 0) (equal term (car (or acc (list nil))))) (reverse acc))
        (t (let ((next (lc-normal-order term)))
             (lc-steps-normal next (- budget 1) (cons next acc))))))

(print "=== Lambda Calculus Reduction ===")

; Omega combinator: (lam x (app x x)) (lam x (app x x))
(define omega (list 'app (list 'lam 'x (list 'app 'x 'x)) (list 'lam 'x (list 'app 'x 'x))))
(print "Omega = (lam x (app x x))(lam x (app x x))")
(print "Normal order Omega (2 steps):") (print (lc-reduce-normal omega 2))
(print "Diverges (continues)")

; SKKx -> x: (app (app (app s k) k) x)
; S = (lam x (lam y (lam z (app (app x z) (app y z)))))
; K = (lam x (lam y x))
; But let's use a simpler test
; (lam f (lam x (app f x))) is essentially eta
(define id (list 'lam 'x 'x))
(define const (list 'lam 'x (list 'lam 'y 'x)))
; (const id) omega -> id
(define term1 (list 'app (list 'app const id) (list 'lam 'z (list 'app 'z 'z))))
(print "Normal order (K I Omega):") (print (lc-reduce-normal term1 10))
(print "Expected: I = (lam x x)")

; (lam x (lam y x))(lam z z)(lam w (app w w)) -> (lam z z) in normal order
(define K (list 'lam 'x (list 'lam 'y 'x)))
(define I (list 'lam 'z 'z))
(define W (list 'lam 'w (list 'app 'w 'w)))
(define test-term (list 'app (list 'app K I) W))
(print "Term: K I W = (lam x lam y x)(lam z z)(lam w (app w w))")
(print "Normal order (5 steps):") (print (lc-reduce-normal test-term 5))
(print "Expected: (lam z z)")

(print "=== Lambda Reduction Done ===")
