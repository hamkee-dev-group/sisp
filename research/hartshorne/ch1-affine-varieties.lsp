; ============================================================
; Hartshorne Ch.1 S1-2: Affine Varieties
; Affine varieties over algebraically closed fields. Zariski
; topology. Coordinate ring. Morphisms <-> algebra maps.
; Verified on finite F_q-points and small examples.
; ============================================================

(print "=== Hartshorne S1-2: Affine Varieties ===")

(define (fp-add p a b) (mod (+ a b) p))
(define (fp-mul p a b) (mod (* a b) p))
(define (fp-sub p a b) (mod (- a b) p))

(define (affine-line p) (list-to-set (seq 0 (- p 1))))
(define (affine-plane p) (cartesian-product (affine-line p) (affine-line p)))

(define (poly-eval p coeffs x)
  (labels (((go-eval cs acc pow)
    (cond ((eq cs nil) (mod acc p))
          (t (go-eval (cdr cs) (mod (+ acc (* (car cs) pow)) p) (mod (* pow x) p))))))
    (go-eval coeffs 0 1)))

(define (affine-zero-set p coeffs)
  (let ((fp-set (affine-line p)))
    (filter (lambda (x) (= (poly-eval p coeffs x) 0)) fp-set)))

; V is irreducible when it is nonempty and is not the union of two PROPER
; closed subsets. The quantifiers must range over subsets of V, i.e. over
; the powerset -- ranging over the elements of V instead is a type error.
(define (irreducible-p V)
  (and (not (equal V {}))
       (not (any (lambda (v1)
               (any (lambda (v2)
                 (and (not (equal v1 V))
                      (not (equal v2 V))
                      (equal V (union v1 v2)))) (powerset V))) (powerset V)))))

(define (regular-map-p domain codomain f)
  (every (lambda (x) (in (f x) codomain)) domain))

(print "--- F_5: Affine Line ---")
(define p5 5)
(define A1-5 (affine-line p5))
(check "A1-F5-has-5-elements" (= (length A1-5) 5))

(define v-x2minus1 (affine-zero-set p5 (list (mod -1 5) 0 1)))
(check "V-x2minus1-in-F5-is-1-and-4" (equal v-x2minus1 {1 4}))

; V(x^2+1) in F_5: roots are 2 and 3 since 2^2+1=5=0, 3^2+1=10=0
(define v-x2plus1 (affine-zero-set p5 (list 1 0 1)))
(check "V-x2plus1-in-F5-is-2-and-3" (equal v-x2plus1 {2 3}))

(define v-012 (affine-zero-set p5 (list 0 2 2 1)))
(check "V-x-xminus1-xminus2-is-0-1-2" (equal v-012 {0 1 2}))

; For 1D affine varieties: irreducible = single point (or empty excluded)
(check "single-point-is-irreducible" (irreducible-p {1}))
(check "empty-not-irreducible" (not (irreducible-p {})))
; A two-point set in the Zariski topology on A^1 splits as {1} u {4}.
(check "two-points-are-reducible" (not (irreducible-p v-x2minus1)))
(check "every-singleton-of-A1-is-irreducible"
  (every (lambda (x) (irreducible-p (list-to-set (list x)))) A1-5))

(print "--- F_3: Affine Line ---")
(define p3 3)
(define A1-3 (affine-line p3))
(check "A1-F3-has-3-elements" (= (length A1-3) 3))

(define v-zero-f3 (affine-zero-set p3 (list 0 1)))
(check "V-x-in-F3-is-0" (equal v-zero-f3 {0}))

(define v-x2minusx (affine-zero-set p3 (list 0 2 1)))
(check "V-x2minusx-in-F3-is-0-and-1" (equal v-x2minusx {0 1}))

(print "--- F_2: Affine Plane ---")
(define p2 2)
(define A2 (affine-plane p2))
(check "A2-F2-has-4-elements" (= (length A2) 4))

; V(y - x^2): points where y = x^2 in F_2
(define v-y-eq-x2
  (filter (lambda (pt)
    (= (fp-sub p2 (nth 0 pt) (fp-mul p2 (nth 1 pt) (nth 1 pt))) 0))
    A2))
(check "V-y-x2-in-F2-has-2-points" (= (length v-y-eq-x2) 2))

; V(xy): points where x*y = 0 in F_2
(define v-xy-zero
  (filter (lambda (pt)
    (= (fp-mul p2 (nth 0 pt) (nth 1 pt)) 0))
    A2))
(check "V-xy-in-F2-has-3-points" (= (length v-xy-zero) 3))

(print "--- Morphisms of Affine Varieties ---")
(define A2-3 (affine-plane p3))

(define (poly-map-f3 x) (list x (mod (* x x) 3)))
(check "regular-map-A1-to-A2"
  (every (lambda (x) (in (poly-map-f3 x) A2-3)) A1-3))

(print "--- Coordinate Ring ---")
; For a finite variety V of n points over F_q, A(V) = F_q[x]/I(V) is
; isomorphic to F_q^n: every function V -> F_q is a restriction of a
; polynomial. Verified by enumerating the restrictions of a + b x to
; V = {0,1} and counting the distinct functions obtained.
(define v01 (affine-zero-set p5 (list 0 (mod -1 5) 1)))
(check "V-x2-minus-x-in-F5-is-0-and-1" (equal v01 {0 1}))

(define (restrict-to-v01 a b)
  (map (lambda (x) (poly-eval p5 (list a b) x)) (set-to-list v01)))

(define all-restrictions
  (dedup (foldr append nil
                (map (lambda (a) (map (lambda (b) (restrict-to-v01 a b)) (seq 0 4)))
                     (seq 0 4)))))

(check "coordinate-ring-of-2-points-is-Fq-squared"
  (= (length all-restrictions) (^ p5 (length v01))))
; Distinct points of V are separated by A(V): some regular function takes
; different values on them, so the projection A(V) -> F_q^n is onto.
(check "coordinate-ring-separates-points"
  (any (lambda (fv) (not (= (car fv) (cadr fv)))) all-restrictions))

(print "=== Hartshorne Affine Varieties Done ===")
