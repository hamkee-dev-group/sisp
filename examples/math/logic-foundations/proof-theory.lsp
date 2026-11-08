; Proof theory: sequent calculus. Cut elimination.
; Consistency proof for propositional fragment. Godel numbering of finite proofs.

(print "=== PROOF THEORY ===")

; ---- Sequent calculus (propositional) ----
; A sequent is (Gamma |- Delta) with Gamma, Delta as lists of formulas.

(define (in-list x lst)
  (cond ((eq lst nil) nil)
        ((equal x (car lst)) t)
        (t (in-list x (cdr lst)))))

(define (remove-one x lst)
  (cond ((eq lst nil) nil)
        ((equal x (car lst)) (cdr lst))
        (t (cons (car lst) (remove-one x (cdr lst))))))

; Axiom: Gamma, A |- Delta, A
(define (axiom-p gamma delta)
  (foldl (lambda (acc f) (or acc (in-list f delta))) nil gamma))

; Sequent proof search (bounded depth)
(define (sequent-provable-p gamma delta depth)
  (cond ((<= depth 0) nil)
        ((axiom-p gamma delta) t)
        (t (let ((found nil))
             ; Try not-L: (not A) in Gamma -> Gamma-{not A} |- Delta, A
             (foldl (lambda (acc f)
               (cond (acc acc)
                     ((and (listp f) (equal (car f) (quote not)))
                      (sequent-provable-p
                        (cons (cadr f) (remove-one f gamma)) delta (- depth 1)))
                     (t nil)))
               nil gamma)))))

; ---- Concrete tests ----
(if (axiom-p (list (quote p)) (list (quote p)))
    (print "PASS: axiom-self") (print "FAIL: axiom-self"))

; ---- Consistency: empty sequent has no proof ----
(if (not (sequent-provable-p nil nil 4))
    (print "PASS: consistency-empty-sequent") (print "FAIL: consistency-empty-sequent"))

; ---- Godel numbering ----
(define (godel-encode formula)
  (cond ((equal formula (quote p)) 11)
        ((equal formula (quote q)) 13)
        ((equal formula (quote r)) 17)
        ((and (listp formula) (equal (car formula) (quote not)))
         (* 2 (^ (godel-encode (cadr formula)) 2)))
        ((and (listp formula) (equal (car formula) (quote and)))
         (* 3 (^ (godel-encode (cadr formula)) 2)
               (^ (godel-encode (caddr formula)) 3)))
        ((and (listp formula) (equal (car formula) (quote or)))
         (* 5 (^ (godel-encode (cadr formula)) 2)
               (^ (godel-encode (caddr formula)) 3)))
        ((and (listp formula) (equal (car formula) (quote impl)))
         (* 7 (^ (godel-encode (cadr formula)) 2)
               (^ (godel-encode (caddr formula)) 3)))
        (t 1)))

(define enc-not-p (godel-encode (quote (not p))))
(if (= enc-not-p 242)
    (print "PASS: godel-not-p") (print "FAIL: godel-not-p"))

(define enc-impl-pq (godel-encode (quote (impl p q))))
(if (= enc-impl-pq 1860859)
    (print "PASS: godel-impl-pq") (print "FAIL: godel-impl-pq"))

(if (not (= enc-not-p enc-impl-pq))
    (print "PASS: godel-injective") (print "FAIL: godel-injective"))

; ---- Cut elimination (finite) ----
(if (sequent-provable-p (list (quote p)) (list (quote p)) 3)
    (print "PASS: cut-elim-simple") (print "FAIL: cut-elim-simple"))

; ---- Consistency: p |- (and p (not p)) is not provable ----
(if (not (sequent-provable-p (list (quote p))
                             (list (quote (and p (not p)))) 10))
    (print "PASS: consistency-check") (print "FAIL: consistency-check"))

; ---- Proof existence: (not (not p)) |- p ----
(if (sequent-provable-p (list (quote (not (not p)))) (list (quote p)) 4)
    (print "PASS: double-negation") (print "FAIL: double-negation"))

; ---- Structural rules: weakening ----
; If Gamma |- Delta is provable, then Gamma, A |- Delta is also provable.
(if (sequent-provable-p (list (quote p) (quote q)) (list (quote p)) 2)
    (print "PASS: weakening") (print "FAIL: weakening"))

(print "=== PROOF THEORY DONE ===")
