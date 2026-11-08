; ============================================================
; EGA -- Riemann-Roch for Curves over Finite Fields
; l(D) - l(K-D) = deg(D) - g + 1
; For elliptic curves (g=1) and P^1 (g=0). Compute l(D).
; ============================================================

(print "=== EGA: Riemann-Roch ===")

; ---------- pow ----------
(define (pow b e)
  (cond ((= e 0) 1)
        (t (* b (pow b (- e 1))))))

; ============================================================
; 1. RIEMANN-ROCH THEOREM FOR CURVES
; ============================================================
; For a smooth projective curve C over a field k of genus g:
; For any divisor D on C:
;   l(D) - l(K - D) = deg(D) - g + 1
; where:
;   l(D) = dim_k H^0(C, O_C(D)) = dimension of global
;          sections of the line bundle associated to D.
;   K = canonical divisor (of degree 2g-2).
;   g = genus = dim_k H^1(C, O_C).

(print "--- 1. Riemann-Roch formula ---")
(print "l(D) - l(K-D) = deg(D) - g + 1")
(print "l(D): dim H^0(C, O(D)), l(K-D): dim H^0(C, O(K-D))")

; Special cases:
; D = 0: l(0) = 1 (constant functions), l(K) = g (definition of genus)
;       1 - g = 0 - g + 1 => 1-g = 1-g ✓
; D = K: l(K) = g, l(0) = 1.
;       g - 1 = (2g-2) - g + 1 => g - 1 = g - 1 ✓

; The two sides are computed independently: the left side from the
; dimension data (l(0)=1, l(K)=g), the right side from deg(D)-g+1 with
; deg(0)=0 and deg(K)=2g-2. Verified for every genus g in 0..6.

(define (l-of-zero-divisor genus) 1)      ; l(0) = 1: only the constants
(define (l-of-canonical genus) genus)     ; l(K) = g: definition of genus
(define (deg-of-canonical genus) (- (* 2 genus) 2))
(define (rr-rhs deg-of-d genus) (+ (- deg-of-d genus) 1))

(check "RR-for-D=0"
  (every (lambda (genus)
           (= (- (l-of-zero-divisor genus) (l-of-canonical genus))
              (rr-rhs 0 genus)))
         (seq 0 6)))

(check "RR-for-D=K"
  (every (lambda (genus)
           (= (- (l-of-canonical genus) (l-of-zero-divisor genus))
              (rr-rhs (deg-of-canonical genus) genus)))
         (seq 0 6)))

; ============================================================
; 2. RIEMANN-ROCH FOR P^1 (g=0)
; ============================================================
; For P^1: g=0, K = -2·∞ (degree -2).
; For a divisor D = n·∞ (point at infinity):
;   l(D) = n + 1 if n ≥ 0 (polynomials of degree ≤ n),
;   l(D) = 0 if n < 0.
;   l(K - D) = l((-2-n)·∞) = max(0, -n-1).
;   RR: l(n·∞) - l((-2-n)·∞) = n - 0 + 1 = n+1.
;   Check: n≥0 => l(n·∞) = n+1, l((-2-n)·∞) = 0. n+1 - 0 = n+1 ✓
;          n=-1 => l(-∞) = 0, l(-1·∞) = 0 => 0-0=0. But RHS: -1+1=0 ✓
;          n=-2 => l(-2∞)=0, l(0·∞)=1 => 0-1=-1. RHS: -2+1=-1 ✓
;          n=-3 => l(-3∞)=0, l(1·∞)=2 => 0-2=-2. RHS: -3+1=-2 ✓

(print "--- 2. Riemann-Roch for P^1 (g=0) ---")

(define (l-D-P1 n)
  (cond ((>= n 0) (+ n 1))
        (t 0)))

(define (deg-P1 n) n)

(check "RR-P1-n=0"  (= (- (l-D-P1 0)  (l-D-P1 (- 0 0 2))) (+ (deg-P1 0) 0 1)))
(check "RR-P1-n=1"  (= (- (l-D-P1 1)  (l-D-P1 (- 0 1 2))) (+ (deg-P1 1) 0 1)))
(check "RR-P1-n=2"  (= (- (l-D-P1 2)  (l-D-P1 (- 0 2 2))) (+ (deg-P1 2) 0 1)))
(check "RR-P1-n=-1" (= (- (l-D-P1 -1) (l-D-P1 (- 0 -1 2))) (+ (deg-P1 -1) 0 1)))
(check "RR-P1-n=-2" (= (- (l-D-P1 -2) (l-D-P1 (- 0 -2 2))) (+ (deg-P1 -2) 0 1)))
(check "RR-P1-n=-3" (= (- (l-D-P1 -3) (l-D-P1 (- 0 -3 2))) (+ (deg-P1 -3) 0 1)))

; ============================================================
; 3. RIEMANN-ROCH FOR ELLIPTIC CURVE (g=1)
; ============================================================
; For elliptic curve E: g=1, deg(K) = 0, and K ~ 0: the invariant differential
; dx/(2y) has neither zeros nor poles, so we may take K = 0.
; So l(K-D) = l(-D).
;
; l(D) - l(-D) = deg(D) for all D. (Riemann-Roch simplified)
;
; Special cases:
; - deg(D) > 0: l(D) = deg(D) and l(-D) = 0.
; - deg(D) = 0: D principal iff D = [0] in Cl^0 => l(D)=1.
;   If D not principal: l(D)=0 and l(-D)=0.
;   RR: 0 - 0 = 0 ✓
; - deg(D) = 0 and D principal: l(D) = l(-D) = 1. RR: 1 - 1 = 0 ✓
; - deg(D) < 0: l(D)=0 and l(-D) = -deg(D), so l(D) - l(-D) = deg(D).
;   For D = -P: deg = -1, l(D)=0, l(K-D)=l(0-(-P))=l(P)=1.
;   RR: 0 - 1 = -1 - 1 + 1 = -1 ✓

(print "--- 3. Riemann-Roch for elliptic curve (g=1) ---")

(define (l-D-E deg-D principal-p-D)
  (cond ((= deg-D 0) (cond (principal-p-D 1) (t 0)))
        ((> deg-D 0) deg-D)
        (t 0)))

(define (l-KminusD-E deg-D principal-p-D)
  (let ((kminusd-deg (- 0 deg-D)))
    (cond ((= kminusd-deg 0) (cond (principal-p-D 1) (t 0)))
          ((> kminusd-deg 0) kminusd-deg)
          (t 0))))

; Verify for divisors on E:
; D = 0 (deg=0, principal): l=1, l(K-D)=l(0)=1. RR: 1-1=0 ✓
(check "RR-E-D=0"
  (= (- (l-D-E 0 t) (l-KminusD-E 0 t)) (+ (- 0 1) 1)))

; D = P (a point, deg=1): l(P)=1, l(K-P)=l(-P)=0. RR: 1-0=1 ✓
(check "RR-E-D=point"
  (= (- (l-D-E 1 nil) (l-KminusD-E 1 nil)) (+ (- 1 1) 1)))

; D = 2P (deg=2): l(2P)=2, l(-2P)=0. RR: 2-0=2 ✓
(check "RR-E-D=2P"
  (= (- (l-D-E 2 nil) (l-KminusD-E 2 nil)) (+ (- 2 1) 1)))

; D = -P (deg=-1): l(-P)=0, l(P)=1. RR: 0-1=-1 ✓
(check "RR-E-D=minusP"
  (= (- (l-D-E -1 nil) (l-KminusD-E -1 nil)) (+ (- -1 1) 1)))

; ============================================================
; 4. COMPUTE l(D) FOR EXPLICIT DIVISORS
; ============================================================
; --- P^1 over F_5 ---
; Points of P^1(F_5): {[0:1], [1:0], [1:1], [1:2], [1:3], [1:4]} = 6 points.
; Let D = 2·[0:1] + 1·[1:0]. deg(D) = 3.
; l(D) = dim H^0(P^1, O(3)) = 4. (Polynomials of degree ≤ 3 in the
; homogeneous coordinate representation.)

(print "--- 4. Compute l(D) for explicit divisors on P^1 ---")

(define n-pts-P1-F5 6)

(define (l-Euler-P1 deg-D)
  (+ deg-D 1))

(check "l-2*P0+1*Pinf-deg3-l=4" (= (l-Euler-P1 3) 4))
(check "l-1*P0-deg1-l=2" (= (l-Euler-P1 1) 2))

; --- Elliptic curve over F_5 ---
; E: y^2 = x^3 + x, g=1, #E(F_5) = 4
; Let D = 0 (the zero divisor: degree 0, principal).
; l(0) = 1 (only constant functions).  The divisor O = 1·(point at infinity)
; has degree 1, and l(O) = 1 as well.

; D = P = (0,0) (a 2-torsion point).
; l(P) = 1 (by RR: deg=1 > 0 => l = deg = 1 since g=1 and D non-special).
; D = 2P: l(2P) = 2.
; D = 3P: l(3P) = 3.

; Count E(F_5) for y^2 = x^3 + x by brute force over the 25 affine pairs,
; plus the point at infinity.  #E(F_5) = 4, within the Hasse bound
; |4 - 6| ≤ 2√5; g = 1 because E is a smooth plane cubic.
(define (e-rhs-f5 x) (mod (+ (* x x x) x) 5))

(define e-f5-affine-count
  (foldl (lambda (acc x)
           (+ acc (count (lambda (y) (= (mod (* y y) 5) (e-rhs-f5 x)))
                         (seq 0 4))))
         0 (seq 0 4)))

(define e-f5-order (+ e-f5-affine-count 1))

(check "E-F5-point-count-is-4" (= e-f5-order 4))

; l(D) for D = m·P on E, computed by the section-dimension function above.
; D = 0 is degree 0 and principal; m·P for m >= 1 has degree m.
(check "E-F5-l-O-is-1"  (= (l-D-E 0 t) 1))
(check "E-F5-l-P-is-1"  (= (l-D-E 1 nil) 1))
(check "E-F5-l-2P-is-2" (= (l-D-E 2 nil) 2))
(check "E-F5-l-3P-is-3" (= (l-D-E 3 nil) 3))

; ============================================================
; 5. EULER CHARACTERISTIC
; ============================================================
; χ(D) = l(D) - l(K-D) = deg(D) - g + 1.
; For P^1 (g=0): χ(D) = deg(D) + 1.
; For elliptic curve (g=1): χ(D) = deg(D).

(print "--- 5. Euler characteristic ---")

(define (euler-char deg-D genus-g)
  (+ (- deg-D genus-g) 1))

(check "euler-P1-D0" (= (euler-char 0 0) 1))
(check "euler-P1-D1" (= (euler-char 1 0) 2))
(check "euler-E-D0"  (= (euler-char 0 1) 0))
(check "euler-E-D1"  (= (euler-char 1 1) 1))

; ============================================================
; 6. SPECIAL DIVISORS
; ============================================================
; A divisor D is special if l(K-D) > 0 (usually asked of effective D, so that
; both l(D) and l(K-D) are > 0).
; For P^1 (g=0): K has degree -2, so K-D has deg -2-deg(D).
;   l(K-D) > 0 iff deg(K-D) ≥ 0 iff deg(D) ≤ -2. But l(D)=0 for deg(D)<0.
;   So no special divisors on P^1 (only trivial ones with l=0).
;
; For elliptic curve (g=1): K=0, so K-D = -D.
;   D special iff l(D) > 0 and l(-D) > 0. This happens only when deg(D)=0
;   and D is principal. Then l(D)=1, l(-D)=1 (both constants).

(print "--- 6. Special divisors ---")
(print "P^1: no special divisors (g=0, K has negative degree)")
(print "Elliptic: D special iff D~0 (principal degree-0 divisor)")

; For the 2-torsion point P = (0,0) on E/F_5:
; P + P = O in the group law means that the divisor 2(P) - 2(O) is principal.
; Indeed it is div(x): x vanishes to order 2 at (0,0) and has a double pole
; at O.  So 2P ~ 2O, not 2P ~ 0 (a principal divisor has degree 0, while
; deg(2P) = 2).  The group identity O corresponds to the class of the zero
; divisor in Pic⁰(E) under P ↦ [(P) - (O)].
; Hence l(2P) = l(2O) = 2 (L(2O) is spanned by 1 and x), and
; l(K - 2P) = l(-2P) = 0 since deg(-2P) < 0.
; RR: 2 - 0 = 2 = deg(2P) - g + 1 = 2 - 1 + 1. ✓
;
; So for any divisor D on an elliptic curve:
;   l(D) = deg(D) when deg(D) ≥ 1 (every such D is non-special),
;   l(D) = 0 when deg(D) < 0,
;   l(D) ∈ {0,1} when deg(D) = 0, with l(D) = 1 iff D is principal.

; Concretely: for every effective divisor D of degree m >= 1 on E,
;   l(D) = m           (left side, from the section-dimension function)
;   l(D) - l(K-D) = χ(D) = deg(D) - g + 1 = m   (right side, Euler char)
; The two sides are computed by different functions.
(check "elliptic-RR-consistent"
  (every (lambda (m)
           (and (= (l-D-E m nil) m)
                (= (- (l-D-E m nil) (l-KminusD-E m nil))
                   (euler-char m 1))))
         (seq 1 6)))

(print "=== Riemann-Roch Done ===")
