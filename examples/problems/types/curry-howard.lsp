; Curry-Howard correspondence: proof terms for intuitionistic propositional logic.
; conjunction = pair, implication = function. (prove '(-> A (-> B A)))
; returns (lambda (x A) (lambda (y B) x))

(define ch-count 0)
(define (ch-fr)
  (setq ch-count (+ ch-count 1))
  (cat "h" (num-to-str ch-count)))

(define (ch-prove goal ctx)
  (setq ch-count 0)
  (ch-search goal ctx 12))

; and-elimination: project from a conjunction in the context
(define (ch-elim-and goal ctx)
  (cond ((null ctx) nil)
        (t (let ((ty (cadr (car ctx))))
             (cond ((and (listp ty) (eq (car ty) 'and))
                    (cond ((eq (cadr ty) goal) (list 'fst (car (car ctx))))
                          ((eq (caddr ty) goal) (list 'snd (car (car ctx))))
                          (t (ch-elim-and goal (cdr ctx)))))
                   (t (ch-elim-and goal (cdr ctx))))))))

(define (ch-search goal ctx depth)
  (cond ((<= depth 0) nil)
        ((symbolp goal)
         (let ((e (find (lambda (b) (eq (cadr b) goal)) ctx)))
           (cond (e (car e))
                 (t (ch-elim-and goal ctx)))))
        ((and (listp goal) (eq (car goal) '->))
         (let ((ante (cadr goal)) (conse (caddr goal)))
           (let ((v (ch-fr)))
             (let ((body (ch-search conse (cons (list v ante) ctx) (- depth 1))))
               (cond (body (list 'lambda (list v ante) body)) (t nil))))))
        ((and (listp goal) (eq (car goal) 'and))
         (let ((l (ch-search (cadr goal) ctx (- depth 2)))
               (r (ch-search (caddr goal) ctx (- depth 2))))
           (cond ((and l r) (list 'pair l r)) (t nil))))
        (t nil)))

(define (ch-norm term)
  (cond ((atom term) term)
        ((eq (car term) 'pair)
         (list 'pair (ch-norm (cadr term)) (ch-norm (caddr term))))
        ((eq (car term) 'lambda)
         (list 'lambda (cadr term) (ch-norm (caddr term))))
        ((eq (car term) 'apply)
         (let ((fun (ch-norm (cadr term))) (arg (ch-norm (caddr term))))
           (cond ((and (listp fun) (eq (car fun) 'lambda))
                  (ch-norm (ch-subs (caadr fun) arg (caddr fun))))
                 (t (list 'apply fun arg)))))
        (t (map ch-norm term))))

(define (ch-subs var val term)
  (cond ((symbolp term) (cond ((eq term var) val) (t term)))
        ((atom term) term)
        ((eq (car term) 'lambda)
         (cond ((eq (caadr term) var) term)
               (t (list 'lambda (cadr term) (ch-subs var val (caddr term))))))
        (t (map (lambda (x) (ch-subs var val x)) term))))

(define (prove formula) (ch-prove formula nil))

(define p1 (prove '(-> A A)))
(check "identity proof is a lambda" (eq (car p1) 'lambda))
(check "identity binds one variable" (equal (caadr p1) "h1"))

(define p2 (prove '(-> A (-> B A))))
(check "K combinator is a lambda" (eq (car p2) 'lambda))
(check "K combinator nests" (eq (car (caddr p2)) 'lambda))
(check "K combinator body returns the first variable"
       (equal (caddr (caddr p2)) "h1"))

(define p3 (prove '(-> (and A B) A)))
(check "and-elim is a lambda" (eq (car p3) 'lambda))

; no proof exists for an atomic goal from an empty context
(check "and A B unprovable from empty context"
       (eq (prove '(and A B)) nil))

; Normalize: (apply (lambda (x A) x) Z) -> Z
(define n1 (ch-norm '(apply (lambda (h1 A) h1) Z)))
(check "beta reduction: identity applied is the argument"
       (equal n1 'Z))

; Double normalization
(define n2 (ch-norm '(apply (lambda (h1 A) (apply (lambda (h2 B) h1) W)) V)))
(check "nested beta reduction (inner lambda discards its argument)"
       (equal n2 'V))
