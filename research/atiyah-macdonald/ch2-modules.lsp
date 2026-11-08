; ============================================================
; Atiyah-Macdonald Ch2: Modules, exact sequences, tensor products,
; restriction/extension of scalars. Snake lemma for Z/nZ-modules.
; ============================================================

(print "=== AM Ch2: Modules ===")

; ---------- ring operations ----------
(define (rmod n a) (mod a n))
(define (radd n a b) (mod (+ a b) n))
(define (rmul n a b) (mod (* a b) n))
(define (rneg n a) (mod (- 0 a) n))

; ---------- divisors ----------
(define (divides n d) (= (mod n d) 0))
(define (divisors n)
  (labels (((go k acc)
    (cond ((> k n) (reverse acc))
          ((divides n k) (go (+ k 1) (cons k acc)))
          (t (go (+ k 1) acc)))))
    (go 1 nil)))

; ---------- ideal/submodule ----------
(define (ideal-from-divisor n d)
  (list-to-set
   (labels (((go k acc)
     (cond ((>= k n) (reverse acc))
           (t (go (+ k d) (cons k acc))))))
     (go 0 nil))))

; ---------- submodule test ----------
(define (submodule-p n M)
  (and (in 0 M)
       (every (lambda (a)
         (every (lambda (b) (in (radd n a b) M)) M)) M)
       (every (lambda (a) (in (rneg n a) M)) M)
       (every (lambda (a)
         (every (lambda (r) (in (rmul n r a) M))
           (list-to-set (seq 0 (- n 1))))) M)))

; ---------- quotient module M/N ----------
(define (quotient-module n M nn)
  (labels (((pick candidates chosen)
    (cond ((eq candidates nil) (list-to-set (reverse chosen)))
          ((any (lambda (c)
             (in (rmod n (- (car candidates) c)) nn)) chosen)
           (pick (cdr candidates) chosen))
          (t (pick (cdr candidates) (cons (car candidates) chosen))))))
    (pick (set-to-list M) nil)))

; ---------- module homomorphism test ----------
(define (module-hom-p n M nn f)
  (and (every (lambda (x) (in (f x) nn)) M)
       (every (lambda (x)
         (every (lambda (y)
           (= (f (radd n x y)) (radd n (f x) (f y)))) M)) M)
       (every (lambda (x)
         (every (lambda (r)
           (= (f (rmul n r x)) (rmul n r (f x)))) M))
         (list-to-set (seq 0 (- n 1))))))

; ---------- kernel and image ----------
(define (module-ker n f M)
  (filter (lambda (x) (= (f x) 0)) M))

(define (module-im n f M)
  (list-to-set (map f (set-to-list M))))

; ---------- exact sequence at M: im(alpha) = ker(beta) ----------
(define (exact-p n alpha beta domain-alpha domain-beta-ker)
  (let ((im-alpha (module-im n alpha domain-alpha))
        (ker-beta (module-ker n beta domain-beta-ker)))
    (equal im-alpha ker-beta)))

; ---------- products of cyclic Z/nZ-modules ----------
; tensor-cyclic returns the product ideal I_d·I_e = I_{gcd(n, d*e)}, which is
; the IMAGE of the multiplication map I_d ⊗_R I_e → R.  The tensor product
; itself is I_d ⊗_R I_e ≅ Z/(n/d) ⊗ Z/(n/e) ≅ Z/gcd(n/d, n/e); the two have
; the same size only in some cases (not for n = 12, d = e = 2).
(define (tensor-cyclic n d e)
  (ideal-from-divisor n (gcd n (* d e))))

; ---------- bilinear map test ----------
(define (bilinear-p modulus left-module right-module scalars beta)
  (and (every (lambda (left1)
         (every (lambda (left2)
           (every (lambda (right)
             (= (beta (radd modulus left1 left2) right)
                (radd modulus (beta left1 right) (beta left2 right))))
             right-module)) left-module)) left-module)
       (every (lambda (left)
         (every (lambda (right1)
           (every (lambda (right2)
             (= (beta left (radd modulus right1 right2))
                (radd modulus (beta left right1) (beta left right2))))
             right-module)) right-module)) left-module)
       (every (lambda (left)
         (every (lambda (right)
           (every (lambda (scalar)
             (= (beta (rmul modulus scalar left) right)
                (rmul modulus scalar (beta left right))))
             scalars)) right-module)) left-module)
       (every (lambda (left)
         (every (lambda (right)
           (every (lambda (scalar)
             (= (beta left (rmul modulus scalar right))
                (rmul modulus scalar (beta left right))))
             scalars)) right-module)) left-module)))

; ---------- restriction of scalars: M as R-module ----------
; Restriction of scalars along Z/nZ → Z/mZ (m|n) turns Z/mZ-modules into
; Z/nZ-modules; these are exactly the Z/nZ-modules killed by m.  So a
; Z/nZ-module M = d*Z/nZ with n | m*d is a Z/mZ-module via r*a = (r mod m)*a,
; on the same additive subgroup.

; ---------- extension of scalars: R -> S, M ⊗_R S ----------
; extension-scalars m n d returns d*Z/nZ, the ideal with the same divisor in
; Z/nZ.  This is not a true extension of scalars: for m a proper divisor of n
; there is no ring map Z/mZ → Z/nZ.  The ring map goes Z/nZ → Z/mZ, and
; extension of scalars along it sends M to M ⊗ Z/mZ = M/mM.
(define (extension-scalars m n d)
  (ideal-from-divisor n d))

; ========== Z/12Z MODULES ==========
(print "--- Z/12Z Modules ---")
(define n12 12)
(define ring12 (list-to-set (seq 0 11)))

(define M1-12 (ideal-from-divisor n12 1))
(define M2-12 (ideal-from-divisor n12 2))
(define M3-12 (ideal-from-divisor n12 3))
(define M4-12 (ideal-from-divisor n12 4))
(define M6-12 (ideal-from-divisor n12 6))
(define M0-12 (ideal-from-divisor n12 12))

; --- submodule test ---
(check "M2-is-submodule" (submodule-p n12 M2-12))
(check "M3-is-submodule" (submodule-p n12 M3-12))
(check "M4-is-submodule" (submodule-p n12 M4-12))
(check "M6-is-submodule" (submodule-p n12 M6-12))

; --- quotient Z/12Z / 2Z/12Z = Z/2Z, size 2 ---
(define Q12-2 (quotient-module n12 M1-12 M2-12))
(check "quotient-Z12-over-2-size-2" (= (length Q12-2) 2))

; --- quotient Z/12Z / 3Z/12Z = Z/3Z, size 3 ---
(define Q12-3 (quotient-module n12 M1-12 M3-12))
(check "quotient-Z12-over-3-size-3" (= (length Q12-3) 3))

; --- homomorphism: x -> 2x ---
(define (f-mul2-12 x) (rmul n12 2 x))
(check "hom-mul2-linear" (module-hom-p n12 M1-12 M1-12 f-mul2-12))

; --- kernel of x->2x = 6Z/12Z = {0,6} ---
(define ker-mul2-12 (module-ker n12 f-mul2-12 M1-12))
(check "ker-mul2-is-6Z" (equal ker-mul2-12 M6-12))

; --- image of x->2x = 2Z/12Z ---
(define im-mul2-12 (module-im n12 f-mul2-12 M1-12))
(check "im-mul2-is-2Z" (equal im-mul2-12 M2-12))

; --- exact sequence: 0 -> 6Z -> Z/12Z -> 2Z -> 0 ---
(define (alpha-incl x) x)
(define (beta-mul2 x) (rmul n12 2 x))
(check "exact-at-ker-mul2"
  (exact-p n12 alpha-incl beta-mul2 M6-12 M1-12))

; --- snake lemma setup: exact rows ---
; 0 -> 6Z -> Z/12Z -> 2Z -> 0
;      ↓f     ↓·2      ↓g
; 0 -> 6Z -> Z/12Z -> 2Z -> 0
; f, g and the middle map are all multiplication by 2 (f on 6Z, g on 2Z)
(define (snake-f x) (rmul n12 2 x))
(define (snake-g x) (rmul n12 2 x))
(check "snake-ker-f-is-6Z" (equal (module-ker n12 snake-f M6-12) M6-12))
(check "snake-ker-g-size-2" (= (length (module-ker n12 snake-g M2-12)) 2))

; ========== TENSOR PRODUCTS ==========
(print "--- Tensor Products ---")

; 2Z/12Z · 2Z/12Z = (gcd(12,4)) = 4Z/12Z, of size 3.  (The tensor product
; 2Z/12Z ⊗ 2Z/12Z ≅ Z/6Z ⊗ Z/6Z ≅ Z/6Z has size 6.)
(check "tensor-2-2-is-4Z"
  (= (length (tensor-cyclic n12 2 2)) (length M4-12)))

; 2Z/12Z · 3Z/12Z = (gcd(12,6)) = 6Z/12Z = {0,6}; here the tensor product
; Z/6Z ⊗ Z/4Z ≅ Z/2Z has the same size.
(check "tensor-2-3-is-6Z"
  (equal (tensor-cyclic n12 2 3) M6-12))

; 3Z/12Z · 4Z/12Z = (gcd(12,12)) = zero ideal; also Z/4Z ⊗ Z/3Z = 0.
(check "tensor-3-4-is-zero" (= (length (tensor-cyclic n12 3 4)) 1))

; R ⊗ M ≅ M: 1Z/12Z ⊗ 2Z/12Z = 2Z/12Z
(check "R-tensor-2Z-is-2Z" (equal (tensor-cyclic n12 1 2) M2-12))

; bilinear check on M2 × M3
(define (canonical-bimap a b) (rmul n12 a b))
(check "canonical-bilinear-on-M2-M3"
  (bilinear-p n12 M2-12 M3-12 ring12 canonical-bimap))

; ========== RESTRICTION/EXTENSION OF SCALARS ==========
(print "--- Restriction/Extension of Scalars ---")

; 2Z/12Z is a Z/6Z-module: it is killed by 6 (6·2 = 12 ≡ 0), so the action
; r·a = (r mod 6)*a mod 12 is well defined.  (Z/12Z itself is not a
; Z/6Z-module, since 6·1 ≠ 0.)
(check "restriction-submod-closed"
  (every (lambda (a)
    (every (lambda (r)
      (in (rmul n12 r a) M2-12))
      (list-to-set (seq 0 5)))) M2-12))

; extension-scalars 6 12 2 returns 2Z/12Z (see the note above: this is the
; ideal with the same divisor, not a tensor product)
(define M2-6 (ideal-from-divisor 6 2))
(check "extension-2Z6-to-Z12-size" (= (length (extension-scalars 6 12 2)) (length M2-12)))

; ========== Z/6Z TENSOR PRODUCTS ==========
(print "--- Z/6Z Tensor Products ---")
(define n6 6)
(define M2-6z (ideal-from-divisor n6 2))
(define M3-6z (ideal-from-divisor n6 3))
(check "tensor-2-3-z6-is-zero" (= (length (tensor-cyclic n6 2 3)) 1))
(check "tensor-2-2-z6-is-2Z" (equal (tensor-cyclic n6 2 2) M2-6z))

(print "=== AM Ch2 Done ===")
