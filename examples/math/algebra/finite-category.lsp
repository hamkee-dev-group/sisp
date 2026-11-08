; ============================================================
; 58 — Finite Category Theory
; Thin categories from posets, universal properties by search,
; functors as monotone maps, an adjunction (Galois connection),
; and a monoid as a one-object category.
; category laws are decidable by enumeration, so every
; theorem here is machine-checked, not asserted.
; ============================================================

(load "lib/collections/relations.lsp")

(print "=== Finite Category Theory ===")


; ---------- 1. A thin category: the divisor poset of 12 ----------
; Objects: divisors of 12. Hom(a,b) has one arrow iff a | b.
; Composition is transitivity; identities are reflexivity.

(define (divides-p a b) (= 0 (mod b a)))
(define objs (enumerate {tau : (divides-p tau 12)} {1 2 3 4 5 6 7 8 9 10 11 12}))
(define leq (pairs-such-that (lambda (a b) (divides-p a b)) objs objs))

(print (list (quote objects) objs))
(check "identities-exist"
  (every (lambda (x) (in (list x x) leq)) objs))
(check "composition-closes"
  (equal (transitive-closure leq) leq))
(check "antisymmetry"
  (every (lambda (p)
    (or (equal (car p) (cadr p))
        (notin (list (cadr p) (car p)) leq))) leq))

; ---------- 2. Universal properties by exhaustive search ----------
; A product of a and b in a thin category is an object p with
; p->a, p->b, and every c with c->a, c->b factors through p.

(define (arrow a b) (in (list a b) leq))
(define (product-p p a b)
  (and (arrow p a) (arrow p b)
    (every (lambda (c)
      (or (not (and (arrow c a) (arrow c b)))
          (arrow c p))) objs)))
(define (poset-product a b)
  (find (lambda (p) (product-p p a b)) objs))

(define (coproduct-p q a b)
  (and (arrow a q) (arrow b q)
    (every (lambda (c)
      (or (not (and (arrow a c) (arrow b c)))
          (arrow q c))) objs)))
(define (poset-coproduct a b)
  (find (lambda (q) (coproduct-p q a b)) objs))

; In the divisibility poset, product = gcd and coproduct = lcm.
(check "product-4-6-is-gcd" (= (poset-product 4 6) (gcd 4 6)))
(check "coproduct-4-6-is-lcm" (= (poset-coproduct 4 6) (lcm 4 6)))
(check "products-are-gcd-everywhere"
  (every (lambda (p)
    (= (poset-product (car p) (cadr p)) (gcd (car p) (cadr p))))
    (cartesian-product objs objs)))
(check "terminal-object-is-12"
  (every (lambda (x) (arrow x 12)) objs))
(check "initial-object-is-1"
  (every (lambda (x) (arrow 1 x)) objs))

; ---------- 3. Functors between thin categories ----------
; A functor between posets is exactly a monotone map.

(define objs6 (enumerate {tau : (divides-p tau 6)} {1 2 3 4 5 6}))
(define leq6 (pairs-such-that (lambda (a b) (divides-p a b)) objs6 objs6))

(define (functor-p f dom-rel cod-rel)
  (every (lambda (p)
    (in (list (f (car p)) (f (cadr p))) cod-rel)) dom-rel))

(check "gcd-with-6-is-a-functor"
  (functor-p (lambda (x) (gcd x 6)) leq leq6))
(check "successor-is-not-a-functor"
  (equal (functor-p (lambda (x) (+ x 1)) leq leq) nil))

; ---------- 4. An adjunction: inclusion -| gcd ----------
; i : div(6) -> div(12) inclusion, g : div(12) -> div(6), g(b) = gcd(b,6).
; Adjunction law: i(a) | b  <=>  a | g(b)  for all a in div6, b in div12.
; A Galois connection is precisely an adjunction of thin categories.

(define (adjoint-pair-p pairs)
  (every (lambda (p)
    (let ((a (car p)) (b (cadr p)))
      (equal (divides-p a b) (divides-p a (gcd b 6)))))
    pairs))
(check "inclusion-left-adjoint-to-gcd"
  (adjoint-pair-p (cartesian-product objs6 objs)))

; Left adjoints preserve colimits: i(lcm) = lcm of images, checked in div(12).
(check "left-adjoint-preserves-joins"
  (every (lambda (p)
    (= (lcm (car p) (cadr p)) (poset-coproduct (car p) (cadr p))))
    (cartesian-product objs6 objs6)))

; ---------- 5. A monoid is a one-object category ----------
; (Z/3, +) : three morphisms on a single object; composition table
; checked for associativity, identity, and functoriality of negation.

(define z3 {0 1 2})
(define (mcomp f g) (mod (+ f g) 3))

(check "monoid-associative"
  (every (lambda (f)
    (every (lambda (g)
      (every (lambda (h)
        (= (mcomp (mcomp f g) h) (mcomp f (mcomp g h)))) z3)) z3)) z3))
(check "monoid-identity"
  (every (lambda (f) (and (= (mcomp 0 f) f) (= (mcomp f 0) f))) z3))

; A functor from a one-object category to itself is a monoid
; homomorphism. Negation preserves composition and the identity.
(define (neg x) (mod (- 3 x) 3))
(check "negation-is-an-endofunctor"
  (and (= (neg 0) 0)
    (every (lambda (f)
      (every (lambda (g)
        (= (neg (mcomp f g)) (mcomp (neg f) (neg g)))) z3)) z3)))

(print "=== Finite Category Theory Done ===")
