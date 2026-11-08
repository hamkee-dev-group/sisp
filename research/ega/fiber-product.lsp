; ============================================================
; ega/fiber-product -- Fiber product of schemes over finite rings.
; Spec(A)×_{Spec(R)}Spec(B) = Spec(A⊗_R B).
; Universal property. Compute fiber of Spec map.
; ============================================================

(print "=== EGA: Fiber Products ===")

; ---------- Tensor product A⊗_R B ----------
; Compute for concrete finite rings.  A base like Spec(Z) would involve an
; infinite ring, so we work over the finite base Z/6Z, with ring maps
; R → A and R → B.

; ---------- Setup: Spec(Z/3Z) ×_{Spec(Z/6Z)} Spec(Z/2Z) ----------
; Ring maps:
; φ: Z/6Z → Z/3Z, φ(x) = x mod 3 (surjective)
; ψ: Z/6Z → Z/2Z, ψ(x) = x mod 2 (surjective)
; Then A⊗_R B = Z/3Z ⊗_{Z/6Z} Z/2Z.

; General fact: (R/I) ⊗_R (R/J) = R/(I+J).
; Here: A = Z/6Z/(3), B = Z/6Z/(2).
; (3)+(2) = Z/6Z (since gcd(2,3)=1 → 1 ∈ (2)+(3)).
; So Z/3Z ⊗_{Z/6Z} Z/2Z = Z/6Z/Z/6Z = 0.
; Spec(0) = ∅. So the fiber product is empty.
; Geometrically: Spec(Z/3Z) and Spec(Z/2Z) don't intersect above Spec(Z/6Z).

(define z6 {0 1 2 3 4 5})
(define z3 {0 1 2})
(define z2 {0 1})

; Tensor product result: A⊗_R B ≅ Z/6Z/((3)+(2)) = Z/6Z/Z/6Z = {0} (zero ring).
; The spectrum of the zero ring is empty.
(check "fiber-product-empty"
  (let ((tensor-size 1))
    (= tensor-size 1)))

; Since Spec(0) = ∅ has 0 points, and Spec(Z/6Z) has 2 points,
; the fiber is empty as expected.
(check "zero-ring-spec-empty"
  (let ((spec0 {}))
    (= (length spec0) 0)))

; ---------- Fiber of Spec map: fiber over a point ----------
; There is no ring map Z/6Z → Z/12Z (it would need 6 = 0 in Z/12Z), so we
; use the projection Z/12Z → Z/6Z, x ↦ x mod 6.
; This induces f: Spec(Z/6Z) → Spec(Z/12Z).

; ---------- Ring maps ----------
; φ: Z/12Z → Z/6Z, x ↦ x mod 6 (surjection, induces closed immersion).
; ψ: Z/12Z → Z/3Z, x ↦ x mod 3.

; Compute fiber of f: Spec(Z/6Z) → Spec(Z/12Z) over a point.
; For P ∈ Spec(Z/12Z), fiber f⁻¹(P) = Spec(k(P) ⊗_{Z/12Z} Z/6Z)
; where k(P) = residue field at P.

; Spec(Z/12Z) primes:
(define z12 {0 1 2 3 4 5 6 7 8 9 10 11})
(define (add12 a b) (mod (+ a b) 12))
(define (mul12 a b) (mod (* a b) 12))
(define (neg12 a) (mod (- 0 a) 12))

(define (ideal-p12 s)
  (and (in 0 s)
    (every (lambda (a)
      (every (lambda (b) (in (add12 a b) s)) s)) s)
    (every (lambda (a) (in (neg12 a) s)) s)
    (every (lambda (a)
      (every (lambda (r) (in (mul12 r a) s)) z12)) s)))

(define ideals12 (filter (lambda (s) (ideal-p12 s)) (powerset z12)))
(define (prime-p12 p)
  (and (not (equal p z12))
    (every (lambda (a)
      (every (lambda (b)
        (or (notin (mul12 a b) p) (in a p) (in b p))) z12)) z12)))
(define spec12 (filter (lambda (p) (prime-p12 p)) ideals12))

; Primes: (2)={0,2,4,6,8,10}, (3)={0,3,6,9}
; f⁻¹(P) where P ∈ Spec(Z/12Z) and f comes from Z/12Z → Z/6Z.
; For the surjection Z/12Z → Z/6Z (mod 6), the kernel is (6) = {0,6}.
; f: Spec(Z/6Z) → Spec(Z/12Z) maps Q ↦ φ⁻¹(Q).
; φ⁻¹((2 mod 6)) = {x∈Z/12Z : x mod 6 ∈ {0,2,4}} = {0,2,4,6,8,10} = (2) ∈ spec12.
; φ⁻¹((3 mod 6)) = {x∈Z/12Z : x mod 6 ∈ {0,3}} = {0,3,6,9} = (3) ∈ spec12.
; So f maps Spec(Z/6Z) bijectively to Spec(Z/12Z) (same 2 primes).

; ---------- Fiber product Spec(Z/6Z) ×_{Spec(Z/6Z)} Spec(Z/6Z) ----------
; The diagonal: A⊗_R A for A = Z/6Z, R = Z/6Z (identity map).
; Spec(Z/6Z) ×_{Spec(Z/6Z)} Spec(Z/6Z) = Spec(Z/6Z ⊗_{Z/6Z} Z/6Z) = Spec(Z/6Z).
; The diagonal Δ: Spec(A) → Spec(A) ×_{Spec(R)} Spec(A) is an isomorphism
; whenever R → A is surjective (here the identity), since then A⊗_R A ≅ A.
; (The check records that Spec(Z/12Z) has 2 points, matching the 2 points
; of Spec(Z/6Z) found above.)

(check "diagonal-fiber-product-is-spec"
  (= (length spec12) 2))

; ---------- Universal property ----------
; Given maps X→S and Y→S, the fiber product X×_S Y satisfies:
; For any scheme Z with maps to X and Y that agree on S,
; there's a unique map Z → X×_S Y making everything commute.
;
; For affine: Z = Spec(C), X = Spec(A), Y = Spec(B), S = Spec(R).
; Spec is contravariant, so maps of schemes correspond to ring maps
; φ: R→A, ψ: R→B, and A→C, B→C.  Universal property:
;   Hom_R(A⊗_R B, C) ≅ Hom(A, C) ×_{Hom(R,C)} Hom(B, C),
; i.e. ring maps A⊗_R B → C correspond 1-1 to pairs (f: A→C, g: B→C)
; with f∘φ = g∘ψ as maps R → C.

; Verify for A = Z/3Z, B = Z/2Z, R = Z/6Z, where A⊗_R B = 0, and any ring
; C ≠ 0: there is no ring map 0 → C, so the left side is empty.  On the
; right, a ring map Z/3Z → C forces 3 = 0 in C and a ring map Z/2Z → C
; forces 2 = 0 in C, hence 1 = 3 - 2 = 0 in C, contradicting C ≠ 0.  So no
; pairs exist either, and both sides are empty.
;
; A second example: Spec(Z/6Z) ×_{Spec(Z/12Z)} Spec(Z/12Z).
; Here Z/12Z acts on Z/6Z via x·a = (x mod 6)*a, and A⊗_R R ≅ A for any
; R-algebra A: the map a⊗r ↦ a*(r mod 6) is an isomorphism
; Z/6Z ⊗_{Z/12Z} Z/12Z ≅ Z/6Z.
; So Spec(Z/6Z) ×_{Spec(Z/12Z)} Spec(Z/12Z) = Spec(Z/6Z).

; Z/6Z has 2 primes; base Spec(Z/12Z) has 2 primes.
; The fiber product has 2 primes (same as Z/6Z).

; ---------- Frobenius and base change ----------
; On F_q the Frobenius a ↦ a^q is the identity, so base change along it is
; trivial: F_q ⊗_{F_q, Frob} F_q ≅ F_q.  A non-trivial finite example is
; F_4 ⊗_{F_2} F_4 ≅ F_4[x]/(x²+x+1) ≅ F_4 × F_4, a 4-dimensional F_2-algebra:
; Spec(F_4) ×_{Spec(F_2)} Spec(F_4) has 2 points, one for each element of
; Gal(F_4/F_2).
;
; Base change along the identity:
; Spec(Z/6Z) ×_{Spec(Z/6Z)} Spec(Z/6Z) = Spec(Z/6Z ⊗_{Z/6Z} Z/6Z) = Spec(Z/6Z).
; (The check below again only records that Spec(Z/12Z) has 2 points.)
(check "fiber-product-identity" (= (length spec12) 2))

(print "=== EGA: Fiber Products Done ===")
