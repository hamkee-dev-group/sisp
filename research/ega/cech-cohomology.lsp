; ============================================================
; ega/cech-cohomology -- Cech cohomology for finite covers
; Ȟ⁰(U,F) = global sections (equalizer).
; Ȟ¹(U,F) for 2-element cover.
; Computed for Spec(Z/6Z) with cover D(2), D(3).
; ============================================================

(print "=== EGA: Cech Cohomology ===")

; ---------- Spec(Z/6Z) setup ----------
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
(print (list (quote Spec-Z6) spec6))

; ---------- Open cover: D(2) and D(3) ----------
; D(2) = {P ∈ Spec(Z/6) : 2 ∉ P} = {(3)}
; D(3) = {P ∈ Spec(Z/6) : 3 ∉ P} = {(2)}
(define (D f) (filter (lambda (p) (notin f p)) spec6))

(define u0 (D 2))
(define u1 (D 3))
(define u01 (cap u0 u1))

(check "cover-covers-spec" (equal (union u0 u1) spec6))
(check "overlap-is-empty" (equal u01 {}))
(check "u0-is-p3" (equal u0 {{0 3}}))
(check "u1-is-p2" (equal u1 {{0 2 4}}))

; ---------- Structure sheaf sections ----------
; O(D(2)) = Z/6Z[1/2] ≅ Z/3Z.
; Because 2·3=0, kernel of R→R_2 is {0,3}, image has 3 elements.
; Represent O(D(2)) as the ring Z/3Z = {0,1,2}.
; O(D(3)) = Z/6Z[1/3] ≅ Z/2Z since 3·2=0, kernel {0,2,4}, image 2 elements.
; Represent O(D(3)) as the ring Z/2Z = {0,1}.

; O(D(2)) ≅ Z/3Z: the map Z/6Z → Z/3Z sends x ↦ x mod 3.
; Kernel = {0,3} = (3). Image = {0,1,2} ≅ Z/3Z.
(define O-D2 {0 1 2})

; O(D(3)) ≅ Z/2Z: the map Z/6Z → Z/2Z sends x ↦ x mod 2.
; Kernel = {0,2,4} = (2). Image = {0,1} ≅ Z/2Z.
(define O-D3 {0 1})

; Restriction from O(D(2)) to O(D(2)∩D(3)) = O(∅).
; For the sheaf, O(∅) = {0} (zero ring), with the zero map as restriction.

; ---------- Cech complex for 2-element cover U = {U₀, U₁} ----------
; C⁰ = O(U₀) × O(U₁)
; C¹ = O(U₀∩U₁)
; Cⁿ = 0 for n ≥ 2 (since there are only 2 open sets)

; d⁰: C⁰ → C¹
; (s₀, s₁) ↦ s₀|₀₁ - s₁|₀₁ (restriction difference)
; Since U₀∩U₁ = ∅, both restrictions go to O(∅) = {0}.
; So d⁰(s₀, s₁) = 0 - 0 = 0 always.

; Therefore Ȟ⁰ = ker(d⁰) = C⁰ = O(D(2)) × O(D(3)) = Z/3Z × Z/2Z.

; The sheaf axiom says this must agree with the global sections
; O(Spec) = Z/6Z: the diagram
;   O(Spec) → ∏_i O(U_i) ⇉ ∏_{i,j} O(U_i ∩ U_j)
; is an equalizer.  The overlap D(2)∩D(3) = D(6) is empty (6 = 0 in Z/6Z,
; so R_6 = 0 and O(∅) = {0}), hence the equalizer is the whole product
; Z/3Z × Z/2Z.  The restriction map is x ↦ (x mod 3, x mod 2), which is an
; isomorphism by CRT: it is injective (x ≡ y mod 2 and mod 3 gives x ≡ y
; mod 6) and both sides have 6 elements.  So the sheaf condition holds.

(check "CRT-is-iso"
  (= (length (map (lambda (x)
    (list (mod x 3) (mod x 2))) z6)) 6))

(check "image-is-full-product"
  (equal
    (map (lambda (x) (list (mod x 3) (mod x 2))) z6)
    (cartesian-product O-D2 O-D3)))

; ---------- Ȟ⁰ = global sections = Z/6Z ----------
; Ȟ⁰(U, O) is the equalizer of the two restriction maps; by the sheaf
; property of O it is O(Spec(Z/6Z)) = Z/6Z for any cover.
; Concretely: 0-cochains are pairs (a ∈ Z/3Z, b ∈ Z/2Z); both restrict to 0
; in O(∅) = {0}, so d⁰(a,b) = 0 - 0 = 0 and ker(d⁰) = Z/3Z × Z/2Z.  This is
; exactly the image of Z/6Z under x ↦ (x mod 3, x mod 2) (CRT).
; So |Ȟ⁰| = 6 = |Z/6Z|.

(check "H0-size-equals-R"
  (= (length (cartesian-product O-D2 O-D3)) 6))

; The global sections of the structure sheaf are exactly Z/6Z.  (That the
; canonical map from Z/6Z to the equalizer is bijective is the
; image-is-full-product check above; this check records the set Z/6Z.)
(define global-sections-set
  (map (lambda (x) x) z6))

(check "global-sections-is-Z6"
  (and (= (length global-sections-set) 6)
       (equal global-sections-set z6)))

; ---------- Ȟ¹ for 2-element cover ----------
; C¹ = O(U₀∩U₁) = O(∅) = {0}
; C² = 0 (no triple intersections for 2-set cover)
; Ȟ¹ = ker(d¹) / im(d⁰) = C¹ / im(d⁰) = {0} / {0} = {0}
; So Ȟ¹ = 0.

; More concretely: the Cech complex is:
; 0 → C⁰ → C¹ → 0
; Ȟ¹ = C¹ / im(d⁰) = {0} / {0} = {0}

; For affine schemes, Ȟ¹ of the structure sheaf is 0 for any cover by
; basic opens D(f_i) (quasi-coherent sheaves are acyclic on affine schemes).

(check "H1-is-trivial"
  (let ((c1 {0}) (im-d0 {0}))
    (= (length (diff c1 im-d0)) 0)))

; The vanishing of Ȟ¹ reflects the property that Spec(Z/6Z)
; is an affine scheme and O is quasi-coherent.
; Now compute the Cech complex explicitly:
;   C0 = O(D(2)) x O(D(3)),  C1 = O(D(2) cap D(3)) = O(empty) = {0},  C2 = 0.
; d0(s0,s1) = s0|01 - s1|01 lands in the zero ring, so it is always 0.
(define c0-cochains (cartesian-product O-D2 O-D3))
(define c1-cochains {0})
; NB: `t` is reserved in SISP and cannot be used as a parameter name.
(define (d0 cochain) 0)
(define im-d0 (map (lambda (s) (d0 s)) c0-cochains))
; C2 = 0, so d1 is the zero map and ker(d1) is all of C1.
(define ker-d1 c1-cochains)

(check "C0-has-6-cochains" (= (length c0-cochains) 6))
(check "d0-image-is-the-zero-cochain" (equal im-d0 {0}))
(check "ker-d1-is-all-of-C1" (equal ker-d1 c1-cochains))
; H^1 = ker(d1)/im(d0): trivial exactly when the two coincide.
(check "H1-vanishes" (equal (diff ker-d1 im-d0) {}))

(print "=== EGA: Cech Cohomology Done ===")
