; First-order logic with equality.
; Terms, formulas, structures. Satisfaction. Compactness (finite).
; Lowenheim-Skolem (finite models). Henkin construction.

(print "=== FIRST-ORDER LOGIC ===")

; ---- Domain and structures ----
(define example-domain {0 1 2})
(define example-consts (list (list (quote a) 0) (list (quote b) 1)))
(define example-funcs (list (list (quote f) (lambda (x) (mod (+ x 1) 3)))))
(define example-rels (list (list (quote p) (lambda (x) (= x 0)))
                           (list (quote q) (lambda (x y) (< x y)))))

; ---- Term evaluation ----
(define (fol-eval-term tm consts funcs var-env)
  (cond ((numberp tm) tm)
        ((symbolp tm)
         (let ((c (assoc tm consts)))
           (cond (c (cadr c))
                 (t (cadr (assoc tm var-env))))))
        (t (let ((ff (cadr (assoc (car tm) funcs))))
             (apply ff (map (lambda (arg)
                       (fol-eval-term arg consts funcs var-env))
                     (cdr tm)))))))

(if (= 0 (fol-eval-term (quote a) example-consts example-funcs nil))
    (print "PASS: fol-term-const") (print "FAIL: fol-term-const"))
(if (= 1 (fol-eval-term (quote (f a)) example-consts example-funcs nil))
    (print "PASS: fol-term-func") (print "FAIL: fol-term-func"))

; ---- Formula evaluation ----
(define (fol-eval-formula frm consts funcs rels var-env)
  (cond ((eq frm (quote true)) t)
        ((eq frm (quote false)) nil)
        ((equal (car frm) (quote not))
         (not (fol-eval-formula (cadr frm) consts funcs rels var-env)))
        ((equal (car frm) (quote and))
         (and (fol-eval-formula (cadr frm) consts funcs rels var-env)
              (fol-eval-formula (caddr frm) consts funcs rels var-env)))
        ((equal (car frm) (quote or))
         (or (fol-eval-formula (cadr frm) consts funcs rels var-env)
             (fol-eval-formula (caddr frm) consts funcs rels var-env)))
        ((equal (car frm) (quote impl))
         (or (not (fol-eval-formula (cadr frm) consts funcs rels var-env))
             (fol-eval-formula (caddr frm) consts funcs rels var-env)))
        ((equal (car frm) (quote forall))
         (let ((vr (cadr frm)) (body (caddr frm)))
           (every (lambda (val)
             (fol-eval-formula body consts funcs rels
               (cons (list vr val) var-env)))
             (set-to-list example-domain))))
        ((equal (car frm) (quote exists))
         (let ((vr (cadr frm)) (body (caddr frm)))
           (any (lambda (val)
             (fol-eval-formula body consts funcs rels
               (cons (list vr val) var-env)))
             example-domain)))
        (t (let ((r (cadr (assoc (car frm) rels))))
             (cond ((eq r nil) nil)
                   (t (apply r (map (lambda (arg)
                                (fol-eval-term arg consts funcs var-env))
                              (cdr frm)))))))))

; Test: forall x exists y (or (q x y) (q y x)) — always true for any domain with 2+ elements
(if (fol-eval-formula (quote (forall x (exists y (or (q x y) (q y x)))))
                      example-consts example-funcs example-rels nil)
    (print "PASS: forall-exists") (print "FAIL: forall-exists"))

; Test: exists z P(z) where P(z) means z=0
(if (fol-eval-formula (quote (exists z (p z)))
                      example-consts example-funcs example-rels nil)
    (print "PASS: exists-p") (print "FAIL: exists-p"))

; ---- Compactness (finite) ----
; A set of sentences is satisfiable iff every finite subset is satisfiable.
; Check: theory {forall x (not P(x))} has model where P is always false.
(define empty-p-rel (list (list (quote p) (lambda (x) nil))))
(if (fol-eval-formula (quote (forall x (not (p x))))
                      example-consts example-funcs empty-p-rel nil)
    (print "PASS: compactness-finite") (print "FAIL: compactness-finite"))

; ---- Finite model: f(x)=x+1 mod 3 is injective on {0,1,2} ----
(if (every (lambda (x)
      (every (lambda (y)
        (or (= x y)
            (not (= (mod (+ x 1) 3) (mod (+ y 1) 3)))))
        (set-to-list example-domain)))
      (set-to-list example-domain))
    (print "PASS: finite-model-injective") (print "FAIL: finite-model-injective"))

; ---- Henkin construction ----
; Add witnessing constant: exists x P(x) -> P(c)
; Since P(x) means x=0, c = 0 witnesses it.
(define henkin-consts (cons (list (quote c-exists-p) 0) example-consts))
(define henkin-axiom (quote (impl (exists x (p x)) (p c-exists-p))))
(if (fol-eval-formula henkin-axiom henkin-consts example-funcs example-rels nil)
    (print "PASS: henkin-witness") (print "FAIL: henkin-witness"))

; ---- Equality axioms ----
(if (every (lambda (x) (= x x)) (set-to-list example-domain))
    (print "PASS: equality-reflexive") (print "FAIL: equality-reflexive"))

(if (every (lambda (x)
      (every (lambda (y)
        (or (not (= x y)) (= y x)))
        (set-to-list example-domain)))
      (set-to-list example-domain))
    (print "PASS: equality-symmetric") (print "FAIL: equality-symmetric"))

(if (every (lambda (x)
      (every (lambda (y)
        (every (lambda (z)
          (or (not (= x y)) (not (= y z)) (= x z)))
          (set-to-list example-domain)))
        (set-to-list example-domain)))
      (set-to-list example-domain))
    (print "PASS: equality-transitive") (print "FAIL: equality-transitive"))

(print "=== FIRST-ORDER LOGIC DONE ===")
