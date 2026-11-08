; ============================================================
; 59 — The Bourbaki Structure Tower (Universal Algebra)
; A structure is (carrier, operations); its axioms are predicates
; decided by enumeration. One generic checker classifies concrete
; carriers up the tower: magma -> semigroup -> monoid -> group ->
; abelian group, then rings and fields with two operations.
; ============================================================

(print "=== Bourbaki Structures ===")


; ---------- generic axiom checkers over a finite carrier ----------

(define (closed-p op s)
  (every (lambda (a)
    (every (lambda (b) (in (op a b) s)) s)) s))

(define (associative-p op s)
  (every (lambda (a)
    (every (lambda (b)
      (every (lambda (c)
        (equal (op (op a b) c) (op a (op b c)))) s)) s)) s))

(define (commutative-p op s)
  (every (lambda (a)
    (every (lambda (b) (equal (op a b) (op b a))) s)) s))

(define (identity-of op s)
  (find (lambda (e)
    (every (lambda (a)
      (and (equal (op e a) a) (equal (op a e) a))) s)) s))

(define (inverses-p op s e)
  (every (lambda (a)
    (any (lambda (b)
      (and (equal (op a b) e) (equal (op b a) e))) s)) s))

; Classification: the strongest structure the axioms support.
(define (classify op s)
  (cond ((not (closed-p op s)) (quote not-closed))
        ((not (associative-p op s)) (quote magma))
        ((eq (identity-of op s) nil) (quote semigroup))
        ((not (inverses-p op s (identity-of op s))) (quote monoid))
        ((not (commutative-p op s)) (quote group))
        (t (quote abelian-group))))

; ---------- instances climbing the tower ----------

(define z4 {0 1 2 3})
(define (add4 a b) (mod (+ a b) 4))
(define (sub4 a b) (mod (- a b) 4))
(define (mul4 a b) (mod (* a b) 4))

(check "z4-addition-is-abelian-group" (eq (classify add4 z4) (quote abelian-group)))
; subtraction is closed but not associative: (a-b)-c /= a-(b-c)
(check "z4-subtraction-is-only-magma" (eq (classify sub4 z4) (quote magma)))
; multiplication has identity 1 but 0 and 2 have no inverses
(check "z4-multiplication-is-monoid" (eq (classify mul4 z4) (quote monoid)))
; the units of Z/5 under multiplication form a group
(define z5-units {1 2 3 4})
(define (mul5 a b) (mod (* a b) 5))
(check "z5-units-form-abelian-group" (eq (classify mul5 z5-units) (quote abelian-group)))
; a non-commutative group: bijections of {0 1 2} under composition,
; encoded as lists (f0 f1 f2); this is S3.
(define s3 {(0 1 2) (1 2 0) (2 0 1) (0 2 1) (2 1 0) (1 0 2)})
(define (perm-comp f g) (map (lambda (x) (nth x f)) g))
(check "s3-is-a-group" (eq (classify perm-comp s3) (quote group)))
(check "s3-is-not-abelian" (equal (commutative-p perm-comp s3) nil))

; ---------- two operations: rings and fields ----------

(define (distributive-p add mul s)
  (every (lambda (a)
    (every (lambda (b)
      (every (lambda (c)
        (and (equal (mul a (add b c)) (add (mul a b) (mul a c)))
             (equal (mul (add b c) a) (add (mul b a) (mul c a))))) s)) s)) s))

(define (ring-p add mul s)
  (and (eq (classify add s) (quote abelian-group))
       (closed-p mul s)
       (associative-p mul s)
       (not (eq (identity-of mul s) nil))
       (distributive-p add mul s)))

; A field: a commutative ring whose nonzero elements form a
; multiplicative group.
(define (field-p add mul s)
  (let ((zero (identity-of add s)))
    (let ((nonzero (filter (lambda (x) (not (equal x zero))) s)))
      (and (ring-p add mul s)
           (commutative-p mul s)
           (closed-p mul nonzero)
           (inverses-p mul nonzero (identity-of mul nonzero))))))

(define z6 {0 1 2 3 4 5})
(define (add6 a b) (mod (+ a b) 6))
(define (mul6 a b) (mod (* a b) 6))
(define z5 {0 1 2 3 4})
(define (add5 a b) (mod (+ a b) 5))

(check "z6-is-a-ring" (ring-p add6 mul6 z6))
; 2*3 = 0 in Z/6: zero divisors prevent a field
(check "z6-is-not-a-field" (equal (field-p add6 mul6 z6) nil))
(check "z6-has-zero-divisors"
  (any (lambda (a)
    (any (lambda (b)
      (and (not (equal a 0)) (not (equal b 0)) (equal (mul6 a b) 0))) z6)) z6))
(check "z5-is-a-field" (field-p add5 mul5 z5))

; ---------- morphisms: structure-preserving maps ----------

(define (homomorphism-p f op-a op-b dom)
  (every (lambda (a)
    (every (lambda (b)
      (equal (f (op-a a b)) (op-b (f a) (f b)))) dom)) dom))

(define (mod2 x) (mod x 2))
(define (add2 a b) (mod (+ a b) 2))
(check "reduction-mod-2-is-a-homomorphism" (homomorphism-p mod2 add4 add2 z4))
(check "successor-is-not-a-homomorphism"
  (equal (homomorphism-p (lambda (x) (mod (+ x 1) 4)) add4 add4 z4) nil))

; The kernel of a homomorphism is a subgroup.
(define kernel (filter (lambda (x) (equal (mod2 x) 0)) z4))
(check "kernel-is-a-subgroup" (eq (classify add4 kernel) (quote abelian-group)))
(check "kernel-is-even-residues" (equal kernel {0 2}))

(print "=== Bourbaki Structures Done ===")
