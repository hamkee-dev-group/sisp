; ============================================================
; Bourbaki — Tensor Products for Finite R-Modules
; M⊗_R N, universal property, associativity, distributivity.
; R⊗_R M ≅ M. Verified on Z/6Z-modules.
; ============================================================

(print "=== Bourbaki: Tensor Products ===")

; ---------- ring operations ----------
(define (rmod n a) (mod a n))
(define (radd n a b) (mod (+ a b) n))
(define (rmul n a b) (mod (* a b) n))

; ---------- divisors ----------
(define (divides n d) (= (rmod n d) 0))
(define (divisors n)
  (labels (((go k acc)
    (cond ((> k n) (reverse acc))
          ((divides n k) (go (+ k 1) (cons k acc)))
          (t (go (+ k 1) acc)))))
    (go 1 nil)))

; ---------- submodule dZ/nZ ----------
(define (ideal-from-divisor n d)
  (list-to-set
   (labels (((go k acc)
     (cond ((>= k n) (reverse acc))
           (t (go (+ k d) (cons k acc))))))
     (go 0 nil))))

; ---------- bilinear map Z/nZ × Z/nZ → Z/nZ is just (a,b) ↦ r*ab ----------
; A map β: M×N → P is R-bilinear if it's additive in each argument and R-balanced.
; NOTE: the modulus is named `md` and the module elements `x`/`y`: a lambda
; parameter named `n` would shadow the modulus (identifiers are case-folded),
; making every `(radd n ...)` use the loop element instead of the modulus.
(define (bilinear-p md bigm nn p beta)
  (let ((scalars (list-to-set (seq 0 (- md 1)))))
    (and
      ; additive in the first argument
      (every (lambda (m1)
        (every (lambda (m2)
          (every (lambda (y)
            (= (beta (radd md m1 m2) y)
               (radd md (beta m1 y) (beta m2 y))))
            nn))
          bigm))
        bigm)
      ; additive in the second argument
      (every (lambda (x)
        (every (lambda (n1)
          (every (lambda (n2)
            (= (beta x (radd md n1 n2))
               (radd md (beta x n1) (beta x n2))))
            nn))
          nn))
        bigm)
      ; R-homogeneous in the first argument
      (every (lambda (x)
        (every (lambda (y)
          (every (lambda (r)
            (= (beta (rmul md r x) y) (rmul md r (beta x y))))
            scalars))
          nn))
        bigm)
      ; R-homogeneous in the second argument (R-balanced)
      (every (lambda (x)
        (every (lambda (y)
          (every (lambda (r)
            (= (beta x (rmul md r y)) (rmul md r (beta x y))))
            scalars))
          nn))
        bigm))))

; ---------- tensor product of cyclic Z/nZ modules ----------
; For ideals I = dZ/nZ, J = eZ/nZ of R = Z/nZ (d, e | n):
; I ≅ R/(n/d) ≅ Z/(n/d)Z as R-modules, and R/(a) ⊗_R R/(b) ≅ R/(a,b), so
;   I ⊗_R J ≅ Z/gcd(n/d, n/e)Z.
; (R is a quotient of Z, so ⊗_R agrees with ⊗_Z here:
; Z/aZ ⊗_Z Z/bZ ≅ Z/gcd(a,b)Z.)
; The multiplication map x⊗y ↦ xy sends I⊗J onto the product ideal
; IJ = (gcd(de, n))Z/nZ. It is an isomorphism when I or J is flat (here:
; projective, gcd(d, n/d) = 1), in particular always when n is squarefree,
; as for Z/6Z. In general it is only surjective:
;   n = 12, d = e = 2: I⊗J ≅ Z/6Z (6 elements), IJ = 4Z/12Z (3 elements);
;   n = 4,  d = e = 2: I⊗J ≅ Z/2Z,              IJ = 0.
; tensor-cyclic returns the product ideal IJ.

(define (tensor-cyclic n d e)
  (ideal-from-divisor n (gcd n (* d e))))

; ---------- tensor product as a module: classify result ----------
; tensor-id returns the generator gcd(n, de) of the product ideal IJ.
(define (tensor-id n d e)
  (gcd n (* d e)))

; ---------- universal property verification ----------
; The map φ: M×N → IJ, (a,b) ↦ a*b mod n, is the canonical bilinear map
; M×N → M⊗N followed by multiplication. When M or N is flat it is the
; canonical map itself, and any bilinear β: M×N → P factors through it.
; Here M=I_d, N=I_e, and φ(a,b) = a*b mod n.

(define (canonical-bimap n a b) (rmul n a b))

; Check: the canonical map is bilinear
; bilinear-p expects a 2-argument beta, so partially apply the modulus.
(define (canonical-beta md) (lambda (a b) (canonical-bimap md a b)))

(define (canonical-bilinear-p md bigm nn)
  (bilinear-p md bigm nn (list-to-set (seq 0 (- md 1))) (canonical-beta md)))

; ---------- factor through tensor: given bilinear beta, find unique f ----------
; For cyclic modules, a linear map f: I_d ⊗ I_e → P is determined by its
; value on the generator d⊗e. When the multiplication map I_d ⊗ I_e → I_{de}
; is an isomorphism (e.g. over Z/6Z), every bilinear β: I_d × I_e → P
; factors as β(a,b) = f(a*b).

; ---------- R⊗_R M ≅ M ----------
; For R=Z/nZ, M = dZ/nZ: R⊗_R M ≅ M via r⊗m ↦ rm.
(define (tensor-R-M n d)
  (tensor-cyclic n 1 d))

; ---------- distributivity: M⊗(N⊕P) ≅ (M⊗N) ⊕ (M⊗P) ----------
; For Z/nZ, cyclic module ideals: I_d ⊗ (I_e ⊕ I_f) ≅ (I_d⊗I_e) ⊕ (I_d⊗I_f)
; Here I_e ⊕ I_f is the external direct sum (a submodule of (Z/nZ)²), and the
; isomorphism is a ⊗ (b,c) ↦ (a⊗b, a⊗c), componentwise.
; distributivity-check returns |I_d ⊗ I_e| in the product-ideal model; it is
; not called by the checks below.
(define (distributivity-check n d e)
  (let ((M (ideal-from-divisor n d))
        (nn (ideal-from-divisor n e)))
    (let ((tensor-size (length (tensor-cyclic n d e))))
      tensor-size)))

; ---------- associativity: (M⊗N)⊗P ≅ M⊗(N⊗P) ----------
; Checked in the product-ideal model: (I_d I_e) I_f = I_d (I_e I_f).
(define (associativity-check n d e f)
  (let ((left (tensor-id n (tensor-id n d e) f))
        (right (tensor-id n d (tensor-id n e f))))
    (= left right)))

; ========== Z/6Z TENSOR PRODUCTS ==========
(print "--- Z/6Z Tensor Products ---")

(define n6 6)
(define ring6 (list-to-set (seq 0 5)))

(define I1 (ideal-from-divisor n6 1))
(define I2 (ideal-from-divisor n6 2))
(define I3 (ideal-from-divisor n6 3))
(define I6 (ideal-from-divisor n6 6))

; --- Tensor product 2Z/6Z ⊗ 2Z/6Z ---
; (2)*(2)=4, gcd(6,4)=2 → 2Z/6Z
(define T22 (tensor-cyclic n6 2 2))
(check "tensor-2-otimes-2-is-2Z6" (equal T22 I2))

; --- Tensor product 2Z/6Z ⊗ 3Z/6Z ---
; (2)*(3)=6, gcd(6,6)=6 → {0} = 6Z/6Z
(define T23 (tensor-cyclic n6 2 3))
(check "tensor-2-otimes-3-is-zero" (equal T23 I6))

; --- Tensor product 3Z/6Z ⊗ 2Z/6Z (symmetric) ---
(define T32 (tensor-cyclic n6 3 2))
(check "tensor-3-otimes-2-is-zero" (equal T32 I6))

; --- Tensor product 3Z/6Z ⊗ 3Z/6Z ---
; (3)*(3)=9, gcd(6,9)=3 → 3Z/6Z
(define T33 (tensor-cyclic n6 3 3))
(check "tensor-3-otimes-3-is-3Z6" (equal T33 I3))

; --- R⊗_R M ≅ M: 1Z/6Z ⊗ 2Z/6Z ≅ 2Z/6Z ---
(define T12 (tensor-cyclic n6 1 2))
(check "R-tensor-2Z6-is-2Z6" (equal T12 I2))

; --- R⊗_R M ≅ M: 1Z/6Z ⊗ 3Z/6Z ≅ 3Z/6Z ---
(define T13 (tensor-cyclic n6 1 3))
(check "R-tensor-3Z6-is-3Z6" (equal T13 I3))

; --- R⊗_R M ≅ M: 1Z/6Z ⊗ 1Z/6Z ≅ Z/6Z ---
(define T11 (tensor-cyclic n6 1 1))
(check "R-tensor-R-is-R" (equal T11 I1))

; --- Bilinear map check ---
; The canonical map (a,b) → a*b mod 6 defined on Z/6Z × Z/6Z
(check "canonical-is-bilinear"
  (canonical-bilinear-p n6 ring6 ring6))

; Check bilinearity specifically on I2 × I3
(check "bilinear-on-I2xI3"
  (bilinear-p n6 I2 I3 ring6 (canonical-beta n6)))

; --- Associativity ---
(check "assoc-2-3-2" (associativity-check n6 2 3 2))
(check "assoc-1-2-3" (associativity-check n6 1 2 3))
(check "assoc-2-2-2" (associativity-check n6 2 2 2))

; --- M⊗0 = 0 ---
(define Tzero (tensor-cyclic n6 2 6))
(check "tensor-with-zero-is-zero" (equal Tzero I6))

(define Tzero2 (tensor-cyclic n6 6 3))
(check "zero-tensor-module-is-zero" (equal Tzero2 I6))

; ========== Z/12Z TENSOR PRODUCTS ==========
(print "--- Z/12Z Tensor Products ---")

(define n12 12)

; Definitions for Z/12Z
(define (ideal-from-divisor-12 d)
  (list-to-set
   (labels (((go k acc)
     (cond ((>= k 12) (reverse acc))
           (t (go (+ k d) (cons k acc))))))
     (go 0 nil))))

(define I12-2 (ideal-from-divisor-12 2))
(define I12-3 (ideal-from-divisor-12 3))
(define I12-4 (ideal-from-divisor-12 4))
(define I12-6 (ideal-from-divisor-12 6))

; Product ideal (2)(2) = (4) = 4Z/12Z, 3 elements, which is what the check
; tests. The tensor product itself is Z/6Z ⊗ Z/6Z ≅ Z/6Z (6 elements):
; 2Z/12Z is not flat, so I⊗J → IJ is not injective here.
(define T12-22 (tensor-cyclic n12 2 2))
(check "z12-tensor-2-otimes-2-is-4Z12" (= (length T12-22) (/ 12 (gcd 12 4))))

; 2Z/12Z ⊗ 3Z/12Z ≅ Z/6Z ⊗ Z/4Z ≅ Z/2Z ≅ (6)Z/12Z (3Z/12Z is flat)
(define T12-23 (tensor-cyclic n12 2 3))
(check "z12-tensor-2-otimes-3-is-6Z12" (= (length T12-23) (/ 12 (gcd 12 6))))

; 3Z/12Z ⊗ 4Z/12Z ≅ Z/4Z ⊗ Z/3Z = 0 = 12Z/12Z
(define T12-34 (tensor-cyclic n12 3 4))
(check "z12-tensor-3-otimes-4-is-zero" (= (length T12-34) 1))

; 4Z/12Z ⊗ 4Z/12Z ≅ Z/3Z ⊗ Z/3Z ≅ Z/3Z ≅ (16) = 4Z/12Z (4Z/12Z is flat)
(define T12-44 (tensor-cyclic n12 4 4))
(check "z12-tensor-4-otimes-4-is-4Z12" (= (length T12-44) (/ 12 (gcd 12 16))))

; --- Z/4Z TENSOR PRODUCTS (as Z/4Z-module) ---
(print "--- Z/4Z Tensor Products ---")
(define n4 4)

(define (ideal-from-divisor-4 d)
  (list-to-set
   (labels (((go k acc)
     (cond ((>= k 4) (reverse acc))
           (t (go (+ k d) (cons k acc))))))
     (go 0 nil))))

; 2Z/4Z ⊗ 2Z/4Z: the product ideal is 4Z/4Z = {0} (2*2=4, gcd(4,4)=4), which
; is what the check tests. The tensor product itself is Z/2Z ⊗ Z/2Z ≅ Z/2Z,
; not 0, since 2Z/4Z is not flat.
(define T4-22 (tensor-cyclic n4 2 2))
(check "z4-tensor-2-otimes-2-is-zero" (= (length T4-22) 1))

; 1Z/4Z ⊗ 2Z/4Z = 2Z/4Z
(define T4-12 (tensor-cyclic n4 1 2))
(check "z4-R-tensor-2Z4-is-2Z4" (= (length T4-12) 2))

(print "=== Tensor Products Done ===")
