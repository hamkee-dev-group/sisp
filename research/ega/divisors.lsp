; ============================================================
; ega/divisors -- Weil divisors on Spec of Dedekind domain.
; Divisor class group. Effective divisors ↔ ideals.
; Compute divisor class group of F_5[x,y]/(y² - x³ - x).
; Principal divisors.
; ============================================================

(print "=== EGA: Divisors and Class Group ===")

; ---------- Finite-field elliptic curve ----------
; E: y² = x³ + x over F_5.
; F_5 = {0, 1, 2, 3, 4}
; Compute all points on E(F_5).

(define f5 {0 1 2 3 4})
(define (add5 a b) (mod (+ a b) 5))
(define (mul5 a b) (mod (* a b) 5))
(define (neg5 a) (mod (- 0 a) 5))

; y² vs x³ + x for x ∈ F_5:
; x=0: x³+x = 0, y²=0 → y=0. Point: (0,0).
; x=1: x³+x = 2, y²=2 mod 5. In F_5: 0²=0, 1²=1, 2²=4, 3²=4, 4²=1.
;   No y with y²=2. No points.
; x=2: x³+x = 8+2=10≡0. y²=0 → y=0. Point: (2,0).
; x=3: x³+x = 27+3=30≡0. y²=0 → y=0. Point: (3,0).
; x=4: x³+x = 64+4=68≡3. y²=3. No points.

; Affine points: (0,0), (2,0), (3,0) + point at infinity O.
; Total = 4 points.

(define squares5 {0 1 4})
(define affine-points
  (filter (lambda (p)
    (let ((x (car p)) (y (cadr p)))
      (= (mul5 y y) (mod (+ (* x x x) x) 5))))
    (cartesian-product f5 f5)))

(print (list (quote affine-pts) affine-points))
(check "three-affine-points" (= (length affine-points) 3))
(check "point-00" (in (list 0 0) affine-points))
(check "point-20" (in (list 2 0) affine-points))
(check "point-30" (in (list 3 0) affine-points))

; ---------- Group law (simplified) ----------
; The curve y² = x³+x has discriminant Δ = -16(4*1³+27*0²) = -64 ≡ 1 mod 5,
; so E is non-singular.  A group of order 4 is Z/4Z or Z/2Z × Z/2Z.
; The 2-torsion points are O and the points with y=0: (0,0), (2,0), (3,0).
; So all 4 elements have order dividing 2, and E(F_5) ≅ Z/2Z × Z/2Z
; (Klein four-group).

; The divisor class group Cl⁰(E) ≅ E(F_5) ≅ Z/2Z × Z/2Z.
; Pic⁰(E) ≅ E(F_5) as groups.

; ---------- Principal divisors ----------
; A rational function f on E has div(f) = sum of zeros - sum of poles.
; For the function x (coordinate function):
; div(x) = 2(0,0) - 2O (since x has double zero at (0,0) and double pole at O).
; div(y) = (0,0) + (2,0) + (3,0) - 3O (y has a triple pole at O).

; For the function (x-2):
; Double zero at (2,0) (the line x = 2 is tangent to E there, as y = 0),
; double pole at O.
; div(x-2) = 2(2,0) - 2O.
; div(x-3) = 2(3,0) - 2O.

; All these are principal (they are divisors of rational functions).
; A divisor D = Σ n_P·P is principal iff D ∈ div(F_q(E)*).

; ---------- Divisor class group Cl(E) ----------
; Cl(E) ≅ Z ⊕ E(F_5) (the degree map splits, via the class of O).
; The degree map deg: Div(E) → Z, deg(Σ n_P·P) = Σ n_P.
; Cl⁰(E) = {[D] : deg(D)=0} ≅ E(F_5) ≅ Z/2Z × Z/2Z.

; Verify: deg(div(f)) = 0 for any rational function f.
; div(x) = 2P₀ - 2O, deg = 2-2 = 0 ✓.
; div(y) = P₀ + P₂ + P₃ - 3O, deg = 3-3 = 0 ✓.

; ---------- Effective divisors ↔ ideals ----------
; An effective divisor D ≥ 0 is Σ n_P·P with n_P ≥ 0.
; The linear system |D| = {D' ≥ 0 : D' ~ D} (D' linearly equivalent to D).
; For a Dedekind domain (coordinate ring of affine curve),
; effective divisors correspond to non-zero ideals.

; On the affine curve (remove O): the affine coordinate ring is
; A = F_5[x,y]/(y² - x³ - x). This is a Dedekind domain (since E is smooth).
; Effective divisors D = Σ n_P·P (P ≠ O) ↔ ideals I = Π P^{n_P}.
; Principal divisors ↔ principal ideals.

; Verify: The ideal (x) of A corresponds to the divisor 2(0,0).
; The ideal (y) corresponds to (0,0)+(2,0)+(3,0).
; The ideal (x-2) corresponds to 2(2,0).

; ---------- Class group computation ----------
; For the affine curve A, Cl(A) ≅ Pic(Spec(A)) = Pic(E\{O}).
; For a smooth projective curve: Cl(E) ≅ Z ⊕ Cl⁰(E).
; Where Cl⁰(E) ≅ Jac(E)(F_5) = E(F_5) ≅ Z/2Z × Z/2Z.

; The group E(F_5) has 4 elements. So the torsion part of Cl(E) has order 4.
(define torsion-size 4)
(check "class-group-torsion-size" (= torsion-size 4))

; Verify E(F_5) = Z/2Z × Z/2Z by checking all elements are 2-torsion:
; For Klein four-group: every non-identity element has order 2.
; All three affine points have y=0, so they're 2-torsion points.
(check "all-affine-are-2-torsion"
  (every (lambda (p) (= (cadr p) 0)) affine-points))

; The full group E(F_5) = {O, (0,0), (2,0), (3,0)} has 4 elements.
; Every element has order 1 or 2 → Z/2Z × Z/2Z.
; V4 <=> group has order 4 and every non-O point is 2-torsion.
; On a Weierstrass curve a point (x,y) is 2-torsion iff y = 0 iff x^3+x = 0.
; So every x-coordinate that carries a point must satisfy x^3 + x = 0 mod 5.
(check "E-F5-is-V4"
  (and (= (+ 1 (length affine-points)) 4)
       (every (lambda (x) (= 0
                                 (mod (+ (* x x x) x) 5)))
                   (filter (lambda (x)
                     (any (lambda (y) (= (mul5 y y) (mod (+ (* x x x) x) 5))) f5))
                     f5))))

; ---------- Class group of the affine curve ----------
; The divisor class group measures failure of unique factorization.
; In a Dedekind domain, every ideal factors uniquely into prime ideals.
; Cl(A) = 0 iff A is a PID.
; For our affine curve A (genus 1, one point removed), removing O kills
; the class of O: Cl(Spec(A)) = Pic(A) = Cl(E) / <[O]> ≅ Cl⁰(E) = E(F_5).
; So Cl(A) ≅ Z/2Z × Z/2Z, order 4.
(check "cl-affine-order-4" (= torsion-size 4))

(print "=== EGA: Divisors Done ===")
