; ============================================================
; galois-theory/field-extensions.lsp — Field Extensions
; Algebraic/transcendental, finite extensions, primitive element
; theorem, splitting fields.
; ============================================================

(print "=== Field Extensions ===")


; ---------- F_5 field ops ----------
(define (f+ a b) (mod (+ a b) 5))
(define (f* a b) (mod (* a b) 5))
(define (f- a b) (mod (- a b) 5))
(define q5 5)
(define f5 {0 1 2 3 4})

(define (field-p add mul ring zero one)
  (let ((nonzero (filter (lambda (x) (not (equal x zero))) ring)))
    (and (every (lambda (a) (every (lambda (b) (equal (add a b) (add b a))) ring)) ring)
         (every (lambda (a) (every (lambda (b) (every (lambda (c)
           (equal (add (add a b) c) (add a (add b c)))) ring)) ring)) ring)
         (every (lambda (a) (equal (add a zero) a)) ring)
         (every (lambda (a) (in (mod (- 0 a) q5) ring)) ring)
         (every (lambda (a) (every (lambda (b) (every (lambda (c)
           (equal (mul (mul a b) c) (mul a (mul b c)))) ring)) ring)) ring)
         (every (lambda (a) (every (lambda (b) (equal (mul a b) (mul b a))) ring)) ring)
         (every (lambda (a) (equal (mul a one) a)) ring)
         (every (lambda (a)
           (or (equal a zero) (any (lambda (b) (equal (mul a b) one)) nonzero))) ring)
         (every (lambda (a) (every (lambda (b) (every (lambda (c)
           (equal (mul a (add b c)) (add (mul a b) (mul a c)))) ring)) ring)) ring))))

(check "f5-is-a-field" (field-p f+ f* f5 0 1))


; ---------- Construct F_25 = F_5[x]/(x^2 + 2) ----------
; x^2 + 2 is irreducible over F_5: roots test:
; p(0)=2, p(1)=3, p(2)=6=1, p(3)=11=1, p(4)=18=3 — none zero.
(define q25 25)
(define (check-irred-poly n)
  (let ((output t))
    (labels (((go k)
      (cond ((= k 5) t)
            ((= (mod (+ (* k k) n) 5) 0) nil)
            (t (go (+ k 1))))))
      (go 0))))

(check "x^2+2-is-irred-over-f5" (check-irred-poly 2))
(check "x^2+3-is-irred-over-f5" (check-irred-poly 3))
(check "x^2+1-not-irred-over-f5" (equal (check-irred-poly 1) nil))

; Represent F_25 elements as (a0 a1) for a0 + a1·α, where α² = 3 (= -2 mod 5).
(define f25 (cartesian-product f5 f5))
(check "f25-has-25-elements" (= (length f25) 25))

(define f25-zero (list 0 0))
(define f25-one (list 1 0))
(define alpha (list 0 1))

(define (f25-add u v)
  (list (mod (+ (nth 0 u) (nth 0 v)) 5)
        (mod (+ (nth 1 u) (nth 1 v)) 5)))

(define (f25-mul u v)
  (let ((a0 (nth 0 u)) (a1 (nth 1 u))
        (b0 (nth 0 v)) (b1 (nth 1 v)))
    (list (mod (+ (* a0 b0) (* 3 (* a1 b1))) 5)
          (mod (+ (* a0 b1) (* a1 b0)) 5))))

(check "alpha^2=3" (equal (f25-mul alpha alpha) (list 3 0)))

; F_25 is a field: every nonzero element has an inverse
(define f25-nonzero (filter (lambda (x) (not (equal x f25-zero))) f25))
(check "f25-nonzero-size-24" (= (length f25-nonzero) 24))

(define (f25-has-inv a)
  (any (lambda (b) (equal (f25-mul a b) f25-one)) f25-nonzero))

(check "f25-every-nonzero-invertible"
  (every (lambda (a) (f25-has-inv a)) f25-nonzero))


; ---------- Algebraic vs Transcendental ----------
; α = √3 is algebraic of degree 2 over F_5 because α² = 3 ∈ F_5.
; The minimal polynomial of α is x² - 3 = x² + 2 over F_5.

; Evaluate polynomial in F_5 coefficients on an F_25 element:
(define (poly-eval-f25 poly x)
  (cond ((eq (cdr poly) nil) (list (car poly) 0))
        (t (f25-add (list (car poly) 0)
                     (f25-mul x (poly-eval-f25 (cdr poly) x))))))

(define minpoly (list 2 0 1)) ; x^2 + 2
(check "alpha-satisfies-minimal-polynomial"
  (equal (poly-eval-f25 minpoly alpha) f25-zero))

; alpha is ALGEBRAIC over F_5 of degree exactly 2:
;  - it is a root of the nonzero polynomial x^2 + 2  (degree <= 2), and
;  - it is not a root of any nonzero polynomial of degree <= 1, i.e. it does
;    not lie in F_5 and no monic linear x + c kills it  (degree >= 2).
(check "alpha-is-algebraic-over-f5"
  (and (equal (poly-eval-f25 minpoly alpha) f25-zero)
       (every (lambda (c) (not (equal alpha (list c 0)))) f5)
       (every (lambda (c)
         (not (equal (poly-eval-f25 (list c 1) alpha) f25-zero))) f5)))

; negative control: an element that IS in F_5 has degree 1 -- some monic linear
; polynomial does kill it -- so the test above distinguishes the two cases.
(check "degree-1-elements-are-killed-by-a-linear-poly"
  (every (lambda (c)
    (any (lambda (d)
      (equal (poly-eval-f25 (list d 1) (list c 0)) f25-zero)) f5)) f5))


; ---------- Finite Extensions: degree ----------
; [F_25 : F_5] = 2
; The degree is the dimension of F_25 as an F_5-vector space.
; Basis: {1, α}
(define (basis-span coeffs)
  (f25-add (list (nth 0 coeffs) 0)
           (list 0 (nth 1 coeffs))))

(define f5x2 (cartesian-product f5 f5))
(check "basis-1-alpha-spans-f25"
  (equal (list-to-set (map (lambda (c) (basis-span c)) (set-to-list f5x2))) f25))

(check "extension-degree-is-2" (= (length f5x2) 25))


; ---------- Primitive element theorem ----------
; F_25 = F_5(α) where α = √3. α is a primitive element.
; The multiplicative group F_25^× is cyclic. A generator is a primitive element.
; Find a generator of F_25^×.
(define (f25-pow a n)
  (cond ((= n 0) f25-one)
        (t (f25-mul a (f25-pow a (- n 1))))))

(define (f25-order a)
  (labels (((go k)
    (cond ((> k 24) nil)
          ((equal (f25-pow a k) f25-one) k)
          (t (go (+ k 1))))))
    (go 1)))

(define gen (find (lambda (a) (= (f25-order a) 24)) f25-nonzero))
(check "f25-has-generator" (not (eq gen nil)))
(check "f25-generator-order-24" (= (f25-order gen) 24))

; A PRIMITIVE ELEMENT of F_25/F_5 is one with F_5(a) = F_25.  Since
; [F_25:F_5] = 2, F_5(a) = F_5[a] = { c0 + c1*a : c0,c1 in F_5 }, so build that
; subfield explicitly and compare it with F_25.
(define (f5-embed c) (list c 0))
(define (f5-adjoin a)
  (list-to-set
    (foldr append nil
      (map (lambda (c0)
             (map (lambda (c1) (f25-add (f5-embed c0) (f25-mul (f5-embed c1) a)))
                  (seq 0 4)))
           (seq 0 4)))))

; F_5 itself, as a subset of F_25
(define f5-in-f25 (list-to-set (map (lambda (c) (f5-embed c)) (seq 0 4))))

(check "generator-is-primitive-element"
  (and (equal (f5-adjoin gen) f25)
       (equal (f5-adjoin alpha) f25)))

; ...and the elements that are NOT primitive are exactly those of F_5, which
; generate only the 5-element prime field.  So the subfield lattice of F_25 is
; {F_5, F_25} and nothing else.
(check "only-two-subfields-of-f25"
  (and (every (lambda (a) (equal (f5-adjoin a) f5-in-f25)) f5-in-f25)
       (equal (list-to-set (map (lambda (a) (length (f5-adjoin a))) (set-to-list f25)))
              (list-to-set (list 5 25)))))

; Every element of F_25 can be expressed as a polynomial in the generator.
(check "generator-powers-cover-nonzero"
  (= (length (list-to-set (map (lambda (k) (f25-pow gen k)) (seq 0 23)))) 24))


; ---------- Splitting field ----------
; The polynomial x^2 + 2 over F_5 splits in F_25 as (x-α)(x+α).
; α = (0,1), -α = (0,4). Check: α + (-α) = 0 (coefficient of x), α·(-α) = -α² = -3 = 2.
(define (f25-sub u v)
  (list (mod (- (nth 0 u) (nth 0 v)) 5)
        (mod (- (nth 1 u) (nth 1 v)) 5)))

(define neg-alpha (list 0 4))

; x^2 + 2 splits as (x-α)(x+α): α and -α are its two distinct roots in F_25.
(check "x^2+2-splits-in-f25"
  (and (equal (poly-eval-f25 minpoly alpha) f25-zero)
       (equal (poly-eval-f25 minpoly neg-alpha) f25-zero)
       (not (equal alpha neg-alpha))))

(check "alpha-plus-negalpha-is-zero" (equal (f25-add alpha neg-alpha) f25-zero))
(check "alpha-times-negalpha-is-2" (equal (f25-mul alpha neg-alpha) (list 2 0)))

; The SPLITTING FIELD of x^2+2 over F_5 is the smallest extension in which the
; polynomial has a full set of roots.  Three things must hold, all computed:
;  - x^2+2 has NO root in F_5, so F_5 is not big enough;
;  - it has exactly deg = 2 distinct roots in F_25, so it splits there;
;  - F_5 adjoined with those roots is already all of F_25, so F_25 is minimal.
(define f25-roots-of-minpoly
  (filter (lambda (x) (equal (poly-eval-f25 minpoly x) f25-zero)) f25))

(check "f25-is-splitting-field-of-x^2+2"
  (and (= (length (filter (lambda (c)
                (equal (poly-eval-f25 minpoly (f5-embed c)) f25-zero)) f5)) 0)
       (= (length f25-roots-of-minpoly) 2)
       (equal f25-roots-of-minpoly (list-to-set (list alpha neg-alpha)))
       (equal (f5-adjoin alpha) f25)
       (equal (f5-adjoin neg-alpha) f25)))


; ---------- Construct F_125 = F_5[x]/(x^3 + x + 1) ----------
; x^3 + x + 1 is irreducible over F_5 (no roots in F_5):
(define (check-cubic-irred)
  (every (lambda (a) (not (= (mod (+ (* a a a) a 1) 5) 0))) f5))

(check "x^3+x+1-is-irred-over-f5" (check-cubic-irred))

; NOTE: cartesian-product NESTS -- (cartesian-product a (cartesian-product b c))
; yields (x (y z)), not a flat triple -- so flatten before doing arithmetic.
(define f125 (list-to-set
               (map (lambda (p) (flatten p))
                    (set-to-list (cartesian-product f5 (cartesian-product f5 f5))))))
(check "f125-has-125-elements" (= (length f125) 125))

(define f125-zero (list 0 0 0))
(define f125-one  (list 1 0 0))

(define (f125-add u v)
  (list (mod (+ (nth 0 u) (nth 0 v)) 5)
        (mod (+ (nth 1 u) (nth 1 v)) 5)
        (mod (+ (nth 2 u) (nth 2 v)) 5)))

; F_125 = F_5[x]/(x^3 + x + 1), so x^3 = -x-1 = 4x+4 and x^4 = 4x^2+4x.
(define (f125-mul u v)
  (let ((a0 (nth 0 u)) (a1 (nth 1 u)) (a2 (nth 2 u))
        (b0 (nth 0 v)) (b1 (nth 1 v)) (b2 (nth 2 v)))
    (let ((c0 (* a0 b0))
          (c1 (+ (* a0 b1) (* a1 b0)))
          (c2 (+ (* a0 b2) (* a1 b1) (* a2 b0)))
          (c3 (+ (* a1 b2) (* a2 b1)))
          (c4 (* a2 b2)))
      (list (mod (+ c0 (* 4 c3)) 5)
            (mod (+ c1 (* 4 c3) (* 4 c4)) 5)
            (mod (+ c2 (* 4 c4)) 5)))))

; the defining relation really holds in this model
(define beta125 (list 0 1 0))
(check "beta-cubed-is-4beta+4-in-f125"
  (equal (f125-mul beta125 (f125-mul beta125 beta125)) (list 4 4 0)))

(define f125-nonzero (filter (lambda (x) (not (equal x f125-zero))) f125))
(define f125-is-a-field
  (every (lambda (a)
    (any (lambda (b) (equal (f125-mul a b) f125-one)) f125-nonzero)) f125-nonzero))

(check "f125-every-nonzero-invertible" f125-is-a-field)


; ---------- Tower of extensions ----------
; F_5 ⊂ F_25 ⊂ F_125? No! 25 = 5^2 and 125 = 5^3. F_25 ⊆ F_5^n iff 2|n.
; So F_25 is NOT a subfield of F_125. But F_5 is in both, and it is ALL they
; share: a common subfield would have to appear in both subfield lattices.
;
; Compute both lattices by brute force -- for every element a, form F_5(a) and
; record its size -- then intersect the two sets of sizes.  Nothing here quotes
; the "gcd of the degrees" theorem; the answer 5 falls out of the computation.

; F_5(a) inside F_125 is { c0 + c1*a + c2*a^2 }, since [F_125:F_5] = 3.
(define (f5-embed-125 c) (list c 0 0))
(define (f5-adjoin-125 a)
  (let ((a2 (f125-mul a a)))
    (list-to-set
      (foldr append nil
        (map (lambda (c0)
          (foldr append nil
            (map (lambda (c1)
                   (map (lambda (c2)
                          (f125-add (f5-embed-125 c0)
                                    (f125-add (f125-mul (f5-embed-125 c1) a)
                                              (f125-mul (f5-embed-125 c2) a2))))
                        (seq 0 4)))
                 (seq 0 4))))
          (seq 0 4))))))

(define subfield-sizes-25
  (list-to-set (map (lambda (a) (length (f5-adjoin a))) (set-to-list f25))))
(define subfield-sizes-125
  (list-to-set (map (lambda (a) (length (f5-adjoin-125 a))) (set-to-list f125))))

(define common-sizes
  (filter (lambda (s) (in s subfield-sizes-125)) subfield-sizes-25))

(check "f5-is-the-intersection-of-f25-and-f125-as-fields"
  (and f125-is-a-field
       (equal subfield-sizes-25 (list-to-set (list 5 25)))
       (equal subfield-sizes-125 (list-to-set (list 5 125)))
       (equal common-sizes (list-to-set (list 5)))
       ; the one shared size is the prime field, and F_25 is NOT inside F_125
       (notin 25 subfield-sizes-125)))


; ---------- Polynomial evaluation and roots ----------
(define (poly-eval-f5 coeffs x)
  (cond ((eq coeffs nil) 0)
        (t (mod (+ (car coeffs) (* x (poly-eval-f5 (cdr coeffs) x))) 5))))

; Check that x^2 + 2 has no root in F_5:
(check "x^2+2-has-no-root-in-f5"
  (every (lambda (a) (not (= (poly-eval-f5 (list 2 0 1) a) 0))) f5))

; But x^2 - 1 = x^2 + 4 has roots 1 and 4:
(check "x^2-1-has-roots-in-f5"
  (and (= (poly-eval-f5 (list 4 0 1) 1) 0)
       (= (poly-eval-f5 (list 4 0 1) 4) 0)))


; ---------- Separation: degree of inseparable extensions ----------
; Over F_5 (char 5), x^5 - a may be inseparable.
; x^5 - 2: derivative is 5x^4 = 0, so repeated roots possible.
; In F_5, x^5 = x for all x (Fermat's little theorem). So x^5 - 2 = x - 2.
; Thus x^5 - 2 has unique root x=2. The extension is purely inseparable
; only in characteristic p.

(check "x^5-x-is-zero-on-all-f5"
  (every (lambda (a) (= (mod (- (* a a a a a) a) 5) 0)) f5))


(print "=== Field Extensions Done ===")
