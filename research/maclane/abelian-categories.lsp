; ============================================================
; Mac Lane — Abelian Categories
; Zero object, biproducts, kernels, cokernels.
; Exact sequences. Snake lemma.
; Verify R-Mod (Z/6Z-modules) is abelian.
; ============================================================

(print "=== Mac Lane: Abelian Categories ===")

; ---------- Z/6Z-modules ----------
; The category of Z/6Z-modules is abelian.
; It's equivalent to the product category Mod(Z/2Z) × Mod(Z/3Z).
; All submodules, quotient modules, kernels, cokernels exist.

(define z6 {0 1 2 3 4 5})
(define (add6 a b) (mod (+ a b) 6))
(define (mul6 a b) (mod (* a b) 6))
(define (neg6 a) (mod (- 0 a) 6))

; ---------- Zero object ----------
; The zero module {0} is both initial and terminal in Mod(Z/6Z).
(define zero-mod {0})
(check "zero-mod-is-terminal" (= (length zero-mod) 1))

; ---------- Biproduct = direct sum ----------
; M ⊕ N = M × N = M ⊔ N (product = coproduct in additive categories).
(define M2 (list-to-set (list 0 2 4)))
(define M3 (list-to-set (list 0 3)))
(define M2oplusM3 (cartesian-product M2 M3))

(check "biproduct-size-6" (= (length M2oplusM3) 6))

; In abelian categories M (+) N is both product and coproduct, with
; injections i1, i2 and projections p1, p2 satisfying
;   p1.i1 = id, p2.i2 = id, p2.i1 = 0, p1.i2 = 0, i1.p1 + i2.p2 = id.
; All five equations are computed below on the actual elements.
(define m2l (set-to-list M2))
(define m3l (set-to-list M3))
(define bipl (set-to-list M2oplusM3))

(define (inj1 m) (list m 0))
(define (inj2 n) (list 0 n))
(define (prj1 z) (nth 0 z))
(define (prj2 z) (nth 1 z))
(define (bip-add z w)
  (list (add6 (nth 0 z) (nth 0 w)) (add6 (nth 1 z) (nth 1 w))))

(check "biproduct-p1-i1-is-identity" (every (lambda (m) (= (prj1 (inj1 m)) m)) m2l))
(check "biproduct-p2-i2-is-identity" (every (lambda (n) (= (prj2 (inj2 n)) n)) m3l))
(check "biproduct-p2-i1-is-zero" (every (lambda (m) (= (prj2 (inj1 m)) 0)) m2l))
(check "biproduct-p1-i2-is-zero" (every (lambda (n) (= (prj1 (inj2 n)) 0)) m3l))
(check "biproduct-exists-in-mod6"
  (every (lambda (z) (equal (bip-add (inj1 (prj1 z)) (inj2 (prj2 z))) z)) bipl))
; The projections are module maps and the summands really are submodules,
; so the biproduct is closed under addition.
(check "biproduct-is-closed-under-addition"
  (every (lambda (z) (every (lambda (w) (in (bip-add z w) M2oplusM3)) bipl)) bipl))
; Negative control: swapping the two injections breaks the identity above.
(check "biproduct-identity-fails-for-swapped-injections"
  (not (every (lambda (z) (equal (bip-add (inj1 (prj2 z)) (inj2 (prj1 z))) z))
              (filter (lambda (z) (not (equal z (list 0 0)))) bipl))))

; ---------- Kernels ----------
; ker(f: M→N) = {m∈M: f(m)=0_N}.
; For the Z/6Z-linear map f: Z/6Z → Z/6Z given by f(x)=2x:
; ker(f) = {0,3} = 3Z/6Z.

(define (f-kernel x) (mul6 2 x))
(define ker-f (filter (lambda (x) (= (f-kernel x) 0)) z6))
(check "ker-of-mul2-is-3Z6"
  (equal ker-f M3))

; For the map g(x) = 3x: ker(g) = {0,2,4} = 2Z/6Z.
(define (g-kernel x) (mul6 3 x))
(define ker-g (filter (lambda (x) (= (g-kernel x) 0)) z6))
(check "ker-of-mul3-is-2Z6"
  (equal ker-g M2))

; ---------- Cokernels ----------
; coker(f: M→N) = N / im(f).
; For f(x) = 2x: im(f) = {0,2,4} = 2Z/6Z. coker = Z/6Z/2Z/6Z ≅ Z/2Z.

(define im-f (list-to-set (map f-kernel (set-to-list z6))))
(check "im-of-mul2-is-2Z6" (equal im-f M2))

; For g(x) = 3x: im(g) = {0,3} = 3Z/6Z. coker = Z/6Z/3Z/6Z ≅ Z/3Z.
(define im-g (list-to-set (map g-kernel (set-to-list z6))))
(check "im-of-mul3-is-3Z6" (equal im-g M3))

; ---------- Exact sequences ----------
(print "--- Exact sequences ---")

; 0 → 3Z/6Z → Z/6Z → 2Z/6Z → 0
; kernel of (x→2x) on Z/6Z = {0,3} = M3 = im(incl: M3→Z6)
(check "ses-exact-at-middle"
  (let ((ker-g2 (filter (lambda (x) (= (mod (* x 2) 6) 0)) z6)))
    (equal ker-g2 M3)))

(check "ses-exact-at-right"
  (let ((im-f2 (list-to-set (map (lambda (x) (mod (* x 2) 6)) (set-to-list z6)))))
    (equal im-f2 M2)))

; 0 → 2Z/6Z → Z/6Z → 3Z/6Z → 0
; f: 2Z/6Z ↪ Z/6Z (inclusion), g: Z/6Z → 3Z/6Z (x→3x)
; ker(g) = {0,2,4} = 2Z/6Z = im(incl). ✓

; The second sequence uses g(x) = 3x, whose kernel is 2Z/6Z = M2 --
; NOT ker(f). Exactness at the middle is im(incl: M2 -> Z6) = ker(g).
(check "ses2-exact-at-middle"
  (equal ker-g M2))

; The two SESs are dual; 2Z/6Z ≅ Z/3Z and 3Z/6Z ≅ Z/2Z as Z/6Z-modules.

; ---------- Snake Lemma ----------
(print "--- Snake Lemma ---")

; Diagram of Z/6Z-modules with exact rows:
;       0 -> M2 --incl--> Z6 --x3--> M3 -> 0
;            | x2         | x2       | x2
;            v            v          v
;       0 -> M2 --incl--> Z6 --x3--> M3 -> 0
;
; A = A' = M2 = {0,2,4}, B = B' = Z6, C = C' = M3 = {0,3}.
; Nothing below is transcribed from the discussion above: every kernel,
; image, cokernel and connecting map is computed from the maps themselves.

(define z6l (set-to-list z6))
(define (row-i a) a)                       ; inclusion M2 -> Z6
(define (row-p b) (mul6 3 b))              ; Z6 -> M3
(define (vert x) (mul6 2 x))               ; the three vertical maps

(define (img-of dom fn) (dedup (sort < (map fn dom))))
(define (ker-of dom fn zro) (dedup (sort < (filter (lambda (x) (= (fn x) zro)) dom))))

; The rows really are short exact sequences.
(check "snake-top-row-is-exact"
  (and (= (length (img-of m2l (lambda (a) (row-i a)))) (length m2l))
       (equal (img-of m2l (lambda (a) (row-i a))) (ker-of z6l (lambda (b) (row-p b)) 0))
       (equal (img-of z6l (lambda (b) (row-p b))) (dedup (sort < m3l)))))

; Cokernels: label each coset by its least element, so every object of the
; snake sequence is a plain list of integers and can be compared directly.
(define (canon-rep sub b) (car (sort < (map (lambda (s) (add6 b s)) sub))))
(define (quot xs sub) (dedup (sort < (map (lambda (b) (canon-rep sub b)) xs))))

(define ker-vert-a (ker-of m2l (lambda (x) (vert x)) 0))
(define ker-vert-b (ker-of z6l (lambda (x) (vert x)) 0))
(define ker-vert-c (ker-of m3l (lambda (x) (vert x)) 0))
(define im-vert-a (img-of m2l (lambda (x) (vert x))))
(define im-vert-b (img-of z6l (lambda (x) (vert x))))
(define im-vert-c (img-of m3l (lambda (x) (vert x))))
(define coker-a (quot m2l im-vert-a))
(define coker-b (quot z6l im-vert-b))
(define coker-c (quot m3l im-vert-c))

(check "snake-kernels-and-cokernels"
  (and (equal ker-vert-a (list 0)) (equal ker-vert-b (list 0 3))
       (equal ker-vert-c (list 0 3))
       (= (length coker-a) 1) (= (length coker-b) 2) (= (length coker-c) 2)))

; The connecting map delta: ker(h) -> coker(f). Lift c to some b with
; p(b) = c, push down to g(b), pull back along i, take the class.
(define (lifts-of c) (filter (lambda (b) (= (row-p b) c)) z6l))
(define (delta-via c b) (canon-rep im-vert-a (vert b)))
(define (delta c) (delta-via c (car (lifts-of c))))

; delta must not depend on the lift chosen -- that is the whole content of
; the snake construction being well defined.
(check "snake-delta-is-independent-of-the-lift"
  (every (lambda (c)
           (= 1 (length (dedup (map (lambda (b) (delta-via c b)) (lifts-of c))))))
         ker-vert-c))
(check "snake-every-element-of-ker-h-has-a-lift"
  (every (lambda (c) (not (eq nil (lifts-of c)))) ker-vert-c))

; The six maps of the snake sequence.
(define (s1-s2 a) (row-i a))
(define (s2-s3 b) (row-p b))
(define (s3-s4 c) (delta c))
(define (s4-s5 a) (canon-rep im-vert-b a))
(define (s5-s6 b) (canon-rep im-vert-c (row-p b)))

; Exactness at each of the six positions: image in = kernel out.
(check "snake-exact-at-ker-f" (equal (ker-of ker-vert-a (lambda (a) (s1-s2 a)) 0) (list 0)))
(check "snake-exact-at-ker-g"
  (equal (img-of ker-vert-a (lambda (a) (s1-s2 a)))
         (ker-of ker-vert-b (lambda (b) (s2-s3 b)) 0)))
(check "snake-exact-at-ker-h"
  (equal (img-of ker-vert-b (lambda (b) (s2-s3 b)))
         (ker-of ker-vert-c (lambda (c) (s3-s4 c)) (canon-rep im-vert-a 0))))
(check "snake-exact-at-coker-f"
  (equal (img-of ker-vert-c (lambda (c) (s3-s4 c)))
         (ker-of coker-a (lambda (a) (s4-s5 a)) (canon-rep im-vert-b 0))))
(check "snake-exact-at-coker-g"
  (equal (img-of coker-a (lambda (a) (s4-s5 a)))
         (ker-of coker-b (lambda (b) (s5-s6 b)) (canon-rep im-vert-c 0))))
(check "snake-exact-at-coker-h"
  (equal (img-of coker-b (lambda (b) (s5-s6 b))) coker-c))

; Euler characteristic: for an exact sequence of finite modules the
; alternating product of the orders is 1.
(check "snake-lemma-holds-for-mod6"
  (= (* (length ker-vert-a) (length ker-vert-c) (length coker-b))
     (* (length ker-vert-b) (length coker-a) (length coker-c))))

; ---------- R-Mod is abelian ----------
(print "--- R-Mod is abelian ---")

; Properties of abelian categories, computed for every Z/6Z-linear
; endomorphism of Z/6Z (multiplication by k, k = 0..5).
(define scalars (list 0 1 2 3 4 5))
(define (mul-map k) (lambda (x) (mul6 k x)))

(check "zero-object" (= (length zero-mod) 1))
(check "kernels-exist"
  (and (equal ker-f M3) (equal ker-g M2)))

; Every morphism factors as epi followed by mono, and the first
; isomorphism theorem |ker f| * |im f| = |source| holds for each.
(check "first-isomorphism-theorem-for-every-scalar-map"
  (every (lambda (k)
           (= 6 (* (length (ker-of z6l (mul-map k) 0))
                   (length (img-of z6l (mul-map k))))))
         scalars))
; Mono iff kernel trivial; epi iff image is everything; in Z/6Z-Mod on a
; finite module the two coincide, and exactly the units give isomorphisms.
(check "monos-are-exactly-the-epis-here"
  (every (lambda (k)
           (eq (= 1 (length (ker-of z6l (mul-map k) 0)))
               (= 6 (length (img-of z6l (mul-map k))))))
         scalars))
(check "isomorphisms-are-exactly-the-units-mod-6"
  (equal (filter (lambda (k) (= 1 (length (ker-of z6l (mul-map k) 0)))) scalars)
         (filter (lambda (k) (= 1 (gcd k 6))) scalars)))
; Every mono is a kernel: the inclusion of im(f) into Z/6Z is exactly the
; kernel of the projection Z/6Z -> Z/6Z / im(f).
(check "abelian-R-Mod-Z6"
  (every (lambda (k)
           (let ((im-k (img-of z6l (mul-map k))))
             (equal im-k
                    (ker-of z6l (lambda (x) (canon-rep im-k x)) (canon-rep im-k 0)))))
         scalars))
; Negative control: a non-module map (x |-> x+1) is not additive.
(check "additivity-test-rejects-non-linear-map"
  (not (every (lambda (x)
                (every (lambda (y) (= (mod (+ (add6 x y) 1) 6)
                                      (add6 (mod (+ x 1) 6) (mod (+ y 1) 6))))
                       z6l))
              z6l)))

(print "=== Abelian Categories Done ===")
