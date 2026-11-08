; Pi-calculus — Milner's calculus of mobile processes.

(define (pi-in x y P) (list 'in x y P))
(define (pi-out x y P) (list 'out x y P))
(define (pi-par P Q) (list 'par P Q))
(define (pi-rep P) (list 'rep P))
(define (pi-nu x P) (list 'nu x P))

(define pi-fresh-counter 0)
(define (pi-gen-fresh)
  (setq pi-fresh-counter (+ pi-fresh-counter 1))
  pi-fresh-counter)

; Substitution: P{y := z}
(define (pi-subst P y z)
  (cond ((eq P nil) nil)
        ((symbolp P) (cond ((equal P y) z) (t P)))
        ((equal (car P) 'in)
         (let ((x (cadr P)) (v (caddr P)) (Q (cadddr P)))
           (cond ((equal v y) (pi-in x v Q))
                 (t (pi-in x v (pi-subst Q y z))))))
        ((equal (car P) 'out)
         (let ((x (cadr P)) (v (caddr P)) (Q (cadddr P)))
           (pi-out (cond ((equal x y) z) (t x))
                   (cond ((equal v y) z) (t v))
                   (pi-subst Q y z))))
        ((equal (car P) 'par)
         (pi-par (pi-subst (cadr P) y z) (pi-subst (caddr P) y z)))
        ((equal (car P) 'rep)
         (pi-rep (pi-subst (cadr P) y z)))
        ((equal (car P) 'nu)
         (let ((x (cadr P)) (Q (caddr P)))
           (cond ((equal x y) (pi-nu x Q))
                 (t (pi-nu x (pi-subst Q y z))))))
        (t P)))

; Single reduction step
(define (pi-reduce1 P)
  (cond ((eq P nil) nil)
        ((equal (car P) 'par)
         (let ((L (cadr P)) (R (caddr P)))
           (cond ((and (consp L) (equal (car L) 'out)
                       (consp R) (equal (car R) 'in)
                       (equal (cadr L) (cadr R)))
                  (pi-par (cadddr L) (pi-subst (cadddr R) (caddr R) (caddr L))))
                 ((and (consp L) (equal (car L) 'in)
                       (consp R) (equal (car R) 'out)
                       (equal (cadr L) (cadr R)))
                  (pi-par (pi-subst (cadddr L) (caddr L) (caddr R)) (cadddr R)))
                 (t (let ((lr (pi-reduce1 L)))
                      (cond (lr (pi-par lr R))
                            (t (let ((rr (pi-reduce1 R)))
                                 (cond (rr (pi-par L rr))
                                       (t nil))))))))))
        ((equal (car P) 'nu)
         (let ((x (cadr P)) (Q (caddr P)))
           (let ((rq (pi-reduce1 Q)))
             (cond (rq (pi-nu x rq)) (t nil)))))
        ((equal (car P) 'rep)
         (pi-par (cadr P) P))
        (t nil)))

(define (pi-reduce P n)
  (cond ((= n 0) P)
        (t (let ((next (pi-reduce1 P)))
             (cond (next (pi-reduce next (- n 1)))
                   (t P))))))

; Structural congruence (partial: syntactic equality and P|Q ≡ Q|P only)
(define (pi-congruent P Q)
  (cond ((equal P Q) t)
        ((and (consp P) (consp Q)
              (equal (car P) 'par) (equal (car Q) 'par))
         (or (and (pi-congruent (cadr P) (caddr Q))
                  (pi-congruent (caddr P) (cadr Q)))
             nil))
        (t nil)))

; ========== demonstrations ==========

; Basic comm: xbar<z>.0 | x(y).ybar<x>.0 -> 0 | zbar<x>.0
(print "=== Basic comm ===")
(define comm (pi-par (pi-out 'x 'z nil) (pi-in 'x 'y (pi-out 'y 'x nil))))
(print "xbar<z>.0 | x(y).ybar<x>.0 ->" (pi-reduce comm 3))
(print "Expected: (par nil (out z x nil))" (equal (pi-reduce comm 3)
  (pi-par nil (pi-out 'z 'x nil))))

; Mobile handoff: (nu talk)(talkbar<phone>.0 | talk(s).sbar<done>.0)
(print "=== Mobile handoff ===")
(define handoff (pi-nu 'talk
  (pi-par (pi-out 'talk 'phone nil)
          (pi-in 'talk 's (pi-out 's 'done nil)))))
(define R2 (pi-reduce handoff 5))
(print "->" R2)

; Replication
(print "=== Replication ===")
(define serv (pi-rep (pi-in 'x 'y (pi-out 'y 'x nil))))
(define client (pi-out 'x 'z nil))
(define sys (pi-par serv client))
(print "!x(y).ybar<x>.0 | xbar<z>.0 ->" (pi-reduce sys 8))

; Structural congruence
(print "=== Congruence ===")
(print "P|0 ??? P:" (pi-congruent (pi-par (pi-out 'a 'b nil) nil) (pi-out 'a 'b nil)))
(print "P|Q ??? Q|P:" (pi-congruent
  (pi-par (pi-in 'x 'y nil) (pi-out 'a 'b nil))
  (pi-par (pi-out 'a 'b nil) (pi-in 'x 'y nil))))
(print "nu x.0 no reduction:" (eq (pi-reduce1 (pi-nu 'x nil)) nil))

(print "=== Pi done ===")
t
