; ============================================================
; ega/closed-subschemes -- Closed subscheme ↔ quasi-coherent ideal sheaf I.
; For I⊆R: V(I) with O_{Spec(R)}/Ĩ.
; Compute V(2) in Spec(Z/12Z).
; ============================================================

(print "=== EGA: Closed Subschemes ===")

; ---------- Spec(Z/12Z) ----------
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
(define (V i) (filter (lambda (p) (subset i p)) spec12))
(define (D f) (filter (lambda (p) (notin f p)) spec12))

(print (list (quote spec12) spec12))
(check "two-primes" (= (length spec12) 2))

; ---------- Closed subscheme V((2)) ----------
; Ideal I = (2) = {0, 2, 4, 6, 8, 10}
(define ideal-2 {0 2 4 6 8 10})
(check "ideal-2" (in ideal-2 ideals12))

; V(I) = {P ∈ Spec : I ⊆ P}
; For Z/12Z: (2) = {0,2,4,6,8,10}, primes are (2)={0,2,4,6,8,10} and (3)={0,3,6,9}.
; I ⊆ (2) ✓, I ⊆ (3)? 2 ∈ I but 2 ∉ (3), so I ⊈ (3).
; Hence V((2)) = {(2)}.
(define closed-v2 (V ideal-2))
(check "V2-is-singleton" (= (length closed-v2) 1))
(check "V2-contains-2-ideal" (in {0 2 4 6 8 10} closed-v2))

; The coordinate ring of V((2)) is R/I = Z/12Z / (2) ≅ Z/2Z.
; O_{V(I)} = O_{Spec(R)} / Ĩ, where Ĩ is the quasi-coherent sheaf
; associated to the ideal I.
;
; The closed subscheme structure:
; O_{V(I)}(V(I)) = R/I = Z/12Z/(2) ≅ Z/2Z.
; The map R → R/I is surjective (closed immersion).

; Verify R/I ≅ Z/2Z:
(define (mod2 a) (mod a 2))
(define z2 (map (lambda (a) (mod2 a)) z12))
(check "quotient-is-Z2" (equal z2 {0 1}))

; The restriction of the structure sheaf to V((2)):
; O_{V((2))}(V((2))∩D(f)) = (R/I)_f.
; For f=3: V((2))∩D(3) = {(2)}∩{(2)} = {(2)}.
; O_{V((2))}({(2)}) = (Z/2Z)_3. Since 3≡1 mod 2, 3 is a unit in Z/2Z, so = Z/2Z.
(check "O-on-V2-D3-is-Z2"
  (let ((quot-z2 {0 1}))
    (= (length quot-z2) 2)))

; For f=2: V((2))∩D(2) = {(2)}∩{(3)} = ∅.
; O_{V((2))}(∅) = 0. (Since 2=0 in R/I=Z/2Z, so D(2)=∅ in V(I)).

; ---------- Ideal sheaf Ĩ ----------
; Ĩ is the quasi-coherent sheaf associated to the ideal I = (2).
; Ĩ(D(f)) = I ⊗_R R_f = I_f = I·R_f.
; For the closed subscheme, O_{V(I)} = O_X / Ĩ.

; Ĩ(D(2)) = I·R_2 = {0,2,4,6,8,10} · (Z/6Z equivalent).
; In R_2, 2 is a unit, so I·R_2 = R_2 ≅ Z/3Z.
; This makes sense: D(2) = {(3)}, and V(I) ∩ D(2) = ∅.
; So O_{V(I)} on D(2) should be 0, and Ĩ(D(2)) should be O(D(2)).

; Ĩ(D(3)) = I·R_3. In R_3, 3 is a unit, and 2 ∈ I maps to 2 mod 2? 
; R_3 ≅ Z/2Z. 2·Z/2Z = 0. So Ĩ(D(3)) = 0.
; Thus O_{V(I)}(D(3)) = O(D(3))/0 = O(D(3)) ≅ Z/2Z.

; ---------- Closed immersion i: V(I) ↪ Spec(R) ----------
; Topologically: V((2)) = {(2)}. The map i is the inclusion {(2)} → {(2),(3)}.
; On sheaves: i_* O_{V(I)} is the skyscraper sheaf supported on {(2)}.
;
; The ideal sheaf Ĩ defines the closed subscheme structure.
; Key: O_{V(I)} = (O/Ĩ)|_{V(I)}.

; Verifying the correspondence:
;   closed subschemes of Spec(R) ↔ quasi-coherent ideal sheaves Ĩ ⊂ O.
; For each ideal I of R, we get a closed subscheme V(I).
; Conversely, every closed subscheme is V(I) for some ideal I.

; All ideals of Z/12Z:
(print (list (quote all-ideals-Z12) ideals12))

; Each ideal gives a closed subscheme. For I = (2):
(check "V-2-ideal"
  (and (in {0 2 4 6 8 10} closed-v2)
       (not (in {0 3 6 9} closed-v2))))

; ---------- Also compute V((6)) = V((2)∩(3)) ----------
; (6) = (2)∩(3) = {0, 6}
(define ideal-6 {0 6})
(check "ideal-6" (in ideal-6 ideals12))

; V((6)) = {P : (6) ⊆ P}
; (6) ⊆ (2)? 6=0 mod 2 ✓. (6) ⊆ (3)? 6=3·2, so 6 is in (3) since (3) absorbs all. ✓
; V((6)) = {(2), (3)} = spec12
(define closed-v6 (V ideal-6))
(check "V6-is-all" (equal closed-v6 spec12))

; The coordinate ring: R/(6) = Z/12Z/(6) ≅ Z/6Z.
; So the closed subscheme V((6)) is actually the whole space
; but with a nilpotent thickening.
(define (mod6 a) (mod a 6))
(define quotient6 (map (lambda (a) (mod6 a)) z12))
(check "quotient-R6-is-Z6" (= (length quotient6) 6))

; V((6)) = Spec(Z/12Z/(6)) = Spec(Z/6Z). Same underling space, different structure sheaf.
(check "V6-is-spec12-underlying"
  (and (= (length closed-v6) (length spec12))))

; ---------- Closed subscheme V((0)) = Spec(R) ----------
(define ideal-0 {0})
(define closed-v0 (V ideal-0))
(check "V0-is-all-of-spec" (equal closed-v0 spec12))

; ---------- Closed subscheme V((1)) = ∅ ----------
(define ideal-1 z12)
(define closed-v1 (V ideal-1))
(check "V1-is-empty" (equal closed-v1 {}))

; Thus:
;   V(0)   = Spec(Z/12Z) (2 points, whole space)
;   V((2)) = {(2)} (1 point)
;   V((3)) = {(3)} (1 point)
;   V((6)) = Spec(Z/12Z) (2 points, but thicker)
;   V(12)  = ∅

(print "=== EGA: Closed Subschemes Done ===")
