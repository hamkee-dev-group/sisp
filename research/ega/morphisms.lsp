; ============================================================
; ega/morphisms -- Morphisms of schemes
; Ring homomorphisms φ:R→S induce maps f:Spec(S)→Spec(R).
; Continuity, sheaf maps. Affine morphisms. Closed immersion.
; Verified: Spec(Z/3Z)→Spec(Z/6Z) and surjection Z/12Z→Z/6Z.
; ============================================================

(print "=== EGA: Morphisms of Schemes ===")

; ---------- Spec(Z/6Z) computation ----------
(define z6 {0 1 2 3 4 5})
(define (add6 a b) (mod (+ a b) 6))
(define (mul6 a b) (mod (* a b) 6))
(define (neg6 a) (mod (- 0 a) 6))

(define (ideal-p6 s)
  (and (in 0 s)
    (every (lambda (a)
      (every (lambda (b) (in (add6 a b) s)) s)) s)
    (every (lambda (a) (in (neg6 a) s)) s)
    (every (lambda (a)
      (every (lambda (r) (in (mul6 r a) s)) z6)) s)))

(define ideals6
  (filter (lambda (s) (ideal-p6 s)) (powerset z6)))

(define (prime-p6 p)
  (and (not (equal p z6))
    (every (lambda (a)
      (every (lambda (b)
        (or (notin (mul6 a b) p) (in a p) (in b p))) z6)) z6)))

(define spec6 (filter (lambda (p) (prime-p6 p)) ideals6))
(print (list (quote spec6) spec6))

; ---------- Spec(Z/3Z) ----------
(define z3 {0 1 2})
(define (add3 a b) (mod (+ a b) 3))
(define (mul3 a b) (mod (* a b) 3))
(define (neg3 a) (mod (- 0 a) 3))

(define (ideal-p3 s)
  (and (in 0 s)
    (every (lambda (a)
      (every (lambda (b) (in (add3 a b) s)) s)) s)
    (every (lambda (a) (in (neg3 a) s)) s)
    (every (lambda (a)
      (every (lambda (r) (in (mul3 r a) s)) z3)) s)))

(define ideals3
  (filter (lambda (s) (ideal-p3 s)) (powerset z3)))

(define (prime-p3 p)
  (and (not (equal p z3))
    (every (lambda (a)
      (every (lambda (b)
        (or (notin (mul3 a b) p) (in a p) (in b p))) z3)) z3)))

(define spec3 (filter (lambda (p) (prime-p3 p)) ideals3))
(print (list (quote spec3) spec3))

(check "spec3-has-one-point" (= (length spec3) 1))
(check "zero-ideal-is-prime-in-z3" (in {0} spec3))

; ---------- morphism from projection Z/6 → Z/3 ----------
; φ: Z/6 → Z/3, φ(x) = x mod 3
; Induces f: Spec(Z/3) → Spec(Z/6), f(Q) = φ⁻¹(Q)
; For Q = {0} (only prime in Z/3), f({0}) = φ⁻¹({0}) = {0,3} = (3) ∈ Spec(Z/6)
; So f maps the single point of Spec(Z/3) to the point (3) in Spec(Z/6).

(define (pullback-ideal phi inv img-elements domain-elements prime-in-cod)
  (filter
    (lambda (x)
      (in (phi x) prime-in-cod))
    domain-elements))

(define (mod3 x) (mod x 3))

(define f-of-zero
  (pullback-ideal mod3 (lambda (y) y) {0 1 2} z6 {0}))

(print (list (quote f-sends-zero-to) f-of-zero))

(check "inclusion-maps-spec3-to-prime3"
  (in f-of-zero spec6))

(check "inclusion-maps-to-0-3"
  (equal f-of-zero {0 3}))

; ---------- continuity: preimage of closed is closed ----------
; Closed sets in Spec(Z/6) are V(I) for ideals I of Z/6.
; Preimage under f of a closed set V(I) is V(φ(I)).
; Verify: for the closed set V((2)) = {(2)} in spec6, f⁻¹(V((2))) = V(φ((2))).
; φ((2)) = {0,2,4} mod 3 = {0,2,1} = Z/3, so
; V(φ((2))) = {Q ∈ Spec(Z/3) : φ((2)) ⊆ Q} = ∅ (since φ((2))=Z/3 ⊈ (0)).
; So f⁻¹({(2)}) = ∅.
; Closed set V((3)) = {(3)}: φ((3)) = {0,3} mod 3 = {0} 
; f⁻¹({(3)}) = V(φ((3))) = V({0}) = {Q : {0} ⊆ Q} = Spec(Z/3) (all).
; Closed set V((2)+(3)=R) = ∅: its preimage is ∅.

(define (phi-of-ideal i)
  (filter (lambda (y)
    (any (lambda (x) (= (mod x 3) y)) i))
    z3))

(check "phi-of-2-is-all-z3" (equal (phi-of-ideal {0 2 4}) z3))
(check "phi-of-3-is-zero" (equal (phi-of-ideal {0 3}) {0}))

; Preimage of closed V(I) under f is the set of primes in spec3
; containing φ(I).
(define (fpreimage Vclosed-I)
  (filter (lambda (q)
    (let ((phiI (phi-of-ideal (find (lambda (i) (equal (V6 i) Vclosed-I)) ideals6))))
      (subset phiI q))) spec3))

(define (V6 i) (filter (lambda (p) (subset i p)) spec6))
(define (V3 i) (filter (lambda (q) (subset i q)) spec3))

; Verify continuity: for each closed in spec6, preimage is closed in spec3.
; Closed sets in spec6: ∅ (=V(R)), {(2)} (=V(2)), {(3)} (=V(3)), total (=V(0)).
; Corresponding closed in spec3: ∅ (=V(Z/3)), ∅, all, all.
; All are closed sets in Spec(Z/3).

(check "continuity-closed-V0"
  (in (fpreimage (V6 {0})) (map (lambda (i) (V3 i)) ideals3)))

(check "continuity-closed-V2"
  (in (fpreimage (V6 {0 2 4})) (map (lambda (i) (V3 i)) ideals3)))

(check "continuity-closed-V3"
  (in (fpreimage (V6 {0 3})) (map (lambda (i) (V3 i)) ideals3)))

; ---------- closed immersion from surjective ring map ----------
; φ: Z/12 → Z/6, φ(x) = x mod 6 (surjective)
; Induces closed immersion f: Spec(Z/6) → Spec(Z/12).
; For a surjection R → R/I, the map on spectra identifies Spec(R/I)
; with the closed subset V(I) of Spec(R).
; Here: Spec(Z/6) ≅ V((6)) ⊂ Spec(Z/12).

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

(define ideals12
  (filter (lambda (s) (ideal-p12 s)) (powerset z12)))

(define (prime-p12 p)
  (and (not (equal p z12))
    (every (lambda (a)
      (every (lambda (b)
        (or (notin (mul12 a b) p) (in a p) (in b p))) z12)) z12)))

(define spec12 (filter (lambda (p) (prime-p12 p)) ideals12))

; The surjection Z/12 → Z/6 maps prime (2)∈spec6 to (2)∈spec12
; and prime (3)∈spec6 to (3)∈spec12.
; Both are in V((6)) = {P ∈ spec12 : (6) ⊆ P}.
; V((6)) = {P : {0,6} ⊆ P}, and both primes contain 6:
; (2) = {0,2,4,6,8,10} and (3) = {0,3,6,9}.
; So V((6)) = {(2), (3)} = spec12 itself.

; Indeed Spec(Z/6) and Spec(Z/12) have the same underlying space:
; both are two points {(2), (3)}.
; The difference is in the structure sheaf.
; But for closed immersion: image of Spec(Z/6) is V(ker(φ)) = V(6) = whole spec12,
; which is a closed subset. So the map is a homeomorphism onto its image.

(check "closed-immersion-homeo"
  (= (length spec6) (length spec12)))

(check "V6-in-spec12-is-full"
  (equal
    (filter (lambda (p) (subset {0 6} p)) spec12)
    spec12))

; The map f# on sheaves: O_{Spec(Z/12)} → f_* O_{Spec(Z/6)}
; is surjective (since φ is surjective), confirming closed immersion.
(check "surjection-is-epi-mod-crt"
  (= (length (map (lambda (x) (mod x 6)) z12)) 6))

(print "=== EGA: Morphisms Done ===")
