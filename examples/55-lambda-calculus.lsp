; ============================================================
; 55 — Lambda Calculus Evaluator
; Beta reduction on symbolic lambda terms
; A language writing an interpreter for itself — meta-circular
; ============================================================

(print "=== Lambda Calculus Evaluator ===")

; Lambda terms as S-expressions:
;   variable: a symbol (x, y, z)
;   abstraction: (lam x body)
;   application: (app func arg)

; --- Free variable check ---
(define (free-in var term)
  (cond ((symbolp term) (eq var term))
        ((eq (car term) 'lam)
         (if (eq var (car (cdr term))) nil
             (free-in var (car (cdr (cdr term))))))
        ((eq (car term) 'app)
         (or (free-in var (car (cdr term)))
             (free-in var (car (cdr (cdr term))))))
        (t nil)))

(print "--- Free variables ---")
(print "x free in x?")
(print (free-in 'x 'x))
(print "x free in (lam x x)?")
(print (free-in 'x '(lam x x)))
(print "x free in (lam y x)?")
(print (free-in 'x '(lam y x)))

; --- Substitution: term[var := val] ---
(define (lc-subst term var val)
  (cond ((symbolp term)
         (if (eq term var) val term))
        ((eq (car term) 'lam)
         (if (eq (car (cdr term)) var)
             term  ; bound variable, don't substitute
             (list 'lam (car (cdr term))
                   (lc-subst (car (cdr (cdr term))) var val))))
        ((eq (car term) 'app)
         (list 'app
               (lc-subst (car (cdr term)) var val)
               (lc-subst (car (cdr (cdr term))) var val)))
        (t term)))

(print "--- Substitution ---")
(print "(app x y)[x:=z] =")
(print (lc-subst '(app x y) 'x 'z))
(print "(lam x x)[x:=z] =")
(print (lc-subst '(lam x x) 'x 'z))
(print "(lam y x)[x:=z] =")
(print (lc-subst '(lam y x) 'x 'z))

; --- Beta reduction: (app (lam x body) arg) → body[x:=arg] ---
(define (beta-reduce term)
  (cond ((symbolp term) term)
        ((eq (car term) 'lam)
         (list 'lam (car (cdr term))
               (beta-reduce (car (cdr (cdr term))))))
        ((eq (car term) 'app)
         (let ((func (car (cdr term)))
               (arg (car (cdr (cdr term)))))
           (if (and (consp func) (eq (car func) 'lam))
               ; Beta redex!
               (lc-subst (car (cdr (cdr func)))
                         (car (cdr func))
                         arg)
               (list 'app (beta-reduce func)
                          (beta-reduce arg)))))
        (t term)))

(print "--- Beta Reduction ---")
; (λx.x) y → y (identity)
(print "((lam x x) y) →")
(print (beta-reduce '(app (lam x x) y)))

; (λx.λy.x) a b → a (K combinator first step)
(print "((lam x (lam y x)) a) →")
(print (beta-reduce '(app (lam x (lam y x)) a)))

; Apply result again
(define step1 (beta-reduce '(app (lam x (lam y x)) a)))
(print "(result b) →")
(print (beta-reduce (list 'app step1 'b)))

; --- Church booleans in lambda calc ---
; TRUE = λx.λy.x
; FALSE = λx.λy.y
(print "--- Church Booleans ---")
(print "TRUE = (lam x (lam y x))")
(print "FALSE = (lam x (lam y y))")
(print "TRUE a b →")
(define t1 (beta-reduce '(app (lam x (lam y x)) a)))
(print (beta-reduce (list 'app t1 'b)))
(print "FALSE a b →")
(define f1 (beta-reduce '(app (lam x (lam y y)) a)))
(print (beta-reduce (list 'app f1 'b)))

(print "=== Lambda Calculus Done ===")
