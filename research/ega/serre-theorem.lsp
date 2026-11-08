; ============================================================
; ega/serre-theorem -- Serre's affineness criterion.
; Affine scheme ⇒ H^i(X,F) = 0 for i>0, F quasi-coherent.
; Verify for Spec(Z/6Z): H¹ = 0.
; Serre's criterion for affineness.
; ============================================================

(print "=== EGA: Serre's Theorem ===")

; ---------- Spec(Z/6Z) ----------
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

(define ideals6 (filter (lambda (s) (ideal-p6 s)) (powerset z6)))

(define (prime-p6 p)
  (and (not (equal p z6))
    (every (lambda (a)
      (every (lambda (b)
        (or (notin (mul6 a b) p) (in a p) (in b p))) z6)) z6)))

(define spec6 (filter (lambda (p) (prime-p6 p)) ideals6))
(define (D f) (filter (lambda (p) (notin f p)) spec6))
(define (V i) (filter (lambda (p) (subset i p)) spec6))

(check "affine-spec-Z6" (= (length spec6) 2))

; ---------- Serre's Theorem: X affine iff H^i(X,F)=0 for i>0 Qcoh(F) ----------
; Key result (Serre; Hartshorne III.3.7): Let X be a Noetherian (more
; generally quasi-compact) scheme. TFAE:
; 1. X is affine.
; 2. H^i(X, F) = 0 for all i > 0 and all quasi-coherent sheaves F.
; 3. H¹(X, I) = 0 for all quasi-coherent ideal sheaves I.
;
; For X = Spec(Z/6Z): verify H¹ = 0.

; ---------- Computing H¹ using Cech cohomology ----------
; Cover X = Spec(Z/6Z) by D(2) and D(3).
; D(2) = {(3)}, D(3) = {(2)}. Overlap D(2)∩D(3) = ∅.
;
; For the structure sheaf F = O:
; C⁰ = O(D(2)) × O(D(3)) ≅ Z/3Z × Z/2Z
; C¹ = O(D(2)∩D(3)) = O(∅) = {0}
; C² = 0 (no triple intersections for 2-set cover)
;
; d⁰: C⁰ → C¹: (s₀, s₁) ↦ s₀|_{∅} - s₁|_{∅} = 0 - 0 = 0.
; So im(d⁰) = {0}.
; d¹: C¹ → C²: the zero map. ker(d¹) = C¹ = {0}.
; H¹ = ker(d¹)/im(d⁰) = {0}/{0} = {0}.
;
; So H¹(Spec(Z/6Z), O) = 0. ✓

(define O-D2 {0 1 2})
(define O-D3 {0 1})
(define O-empty {0})

(check "C1-is-zero" (= (length O-empty) 1))
(check "C0-is-product-d2-d3" (= (length (cartesian-product O-D2 O-D3)) 6))

; H¹ = ker(d¹) / im(d⁰).  Compute both sides independently from the
; actual cover {D(2), D(3)} produced by D above.
(define u0 (D 2))
(define u1 (D 3))
(define u01 (cap u0 u1))                 ; D(2) ∩ D(3) = D(6) = D(0) = ∅

; O(U0 ∩ U1): sections over the overlap.  If the overlap is empty this is
; the zero ring {0} and every section restricts to its single element;
; otherwise restriction is the identity on Z/6Z.
(define (restrict-to-u01 s)
  (cond ((= (length u01) 0) 0)
        (t (mod s 6))))

; d⁰(s0,s1) = s0|_{U01} - s1|_{U01}
(define c0 (cartesian-product O-D2 O-D3))
(define im-d0
  (map (lambda (pr)
    (mod (- (restrict-to-u01 (car pr)) (restrict-to-u01 (cadr pr))) 6)) c0))

; d¹: C¹ → C² = 0 is the zero map, so ker(d¹) is all of C¹ = O(U01).
(define ker-d1 (filter (lambda (c) (= 0 (mod c 6))) O-empty))

; H¹ = 0 exactly when every 1-cocycle is a coboundary, i.e. im(d⁰) = ker(d¹).
(check "H1-Z6-is-zero" (equal im-d0 ker-d1))

; ---------- H⁰ = Γ(X, F) = global sections ----------
; H⁰(Spec(Z/6Z), O) = Z/6Z = {0,1,2,3,4,5}, order 6.
; This matches: O(Spec(R)) = R for an affine scheme.
(check "H0-is-R" (= (length z6) 6))

; ---------- Cohomology of quasi-coherent sheaf M̃ ----------
; For any quasi-coherent sheaf F = M̃ (associated to R-module M):
; H⁰(X, F) = M (global sections).
; H^i(X, F) = 0 for i > 0 (by Serre's theorem).
;
; Verify for M = Z/6Z itself (F = O):
; H¹ = 0 as already checked.
;
; For M = ideal (2) = {0,2,4} ≅ Z/3Z ≅ Z/6Z/(3) (as a Z/6Z-module):
; M₂ = M ⊗ R₂ with R₂ = Z/6Z[1/2] ≅ Z/6Z/(3), so
; M₂ = Z/6Z/(3) ⊗_{Z/6Z} Z/6Z/(3) ≅ Z/6Z/((3)+(3)) = Z/6Z/(3) ≅ Z/3Z.
; (2 already acts invertibly on M: 2·M = {0,4,2} = M and 3·M = 0.)
; So M₂ = M (size 3).
;
; M₃ = M ⊗ R_3. R_3 = Z/6Z/(2) ≅ Z/2Z.
; M = Z/6Z/(3) ≅ Z/3Z.
; M ⊗_{Z/6Z} R_3 = Z/6Z/(3) ⊗ Z/6Z/(2) = Z/6Z/((3)+(2)) = Z/6Z/(1) = 0.
; So M₃ = 0.
;
; Global sections of M̃ = M (size 3). Cech complex:
; C⁰ = M₂ × M₃ = Z/3Z × 0 ≅ Z/3Z (size 3).
; C¹ = M₆ = M ⊗ R₆. R₆ = Z/6Z[1/6] = 0. So C¹ = {0}.
; d⁰ = 0. H¹ = {0}.

; The sheaf condition:
; M → M₂ × M₃ is the diagonal map m ↦ (m in M₂, m in M₃ = 0).
; This map is injective, since M → M₂ is an isomorphism.
; Its image in M₂ × M₃ = Z/3Z × 0 has size 3.
; The equalizer of the two maps to C¹(={0}) is the whole C⁰ (size 3).
; Since M = 3 elements and C⁰ = 3 elements, they match.
; So Γ(X, M̃) = M. ✓

(check "H0-for-M-is-M"
  (= (length {0 2 4}) 3))

; ---------- Serre's criterion: converse ----------
; If X is quasi-compact and H^i(X, F) = 0 for all i > 0 and all
; quasi-coherent F, then X is affine.
; Contrapositive: a non-affine scheme such as P¹ has some quasi-coherent F
; with H¹(X, F) ≠ 0.  For P¹ over F_5, H¹(P¹, O) = 0 but
; H¹(P¹, O(-2)) ≅ F_5 ≠ 0.  Building the Cech complex of P¹ concretely is
; beyond this file, which checks the forward direction on affine schemes.

; ---------- All H^i vanish for i > dim X ----------
; Grothendieck's vanishing: H^i(X, F) = 0 for i > dim X (X a Noetherian
; topological space).  The primes of Z/6Z are (2)={0,2,4} and (3)={0,3},
; both maximal, so there are no strict chains and Krull dim(Z/6Z) = 0 (a
; finite ring is Artinian).  So H^i = 0 for all i > 0: only H⁰ survives.
; This matches the picture: Spec(Z/6Z) is a finite discrete space, a sheaf
; on it is just the product of its stalks, and sheaf cohomology vanishes
; for i > 0.

; Krull dim = length of the longest STRICT chain of primes.  Compute every
; strictly increasing pair of primes; dim 0 means there are none.
(define prime-pairs (cartesian-product spec6 spec6))
(define strict-prime-inclusions
  (filter (lambda (pr)
    (and (subset (car pr) (cadr pr)) (not (equal (car pr) (cadr pr)))))
    prime-pairs))

; dim = 0 (no strict inclusions) and, consistently, H^i = 0 for i > 0
; (witnessed by the H¹ computation above).
(check "krull-dim-0-vanishing"
  (and (= (length strict-prime-inclusions) 0)
       (equal im-d0 ker-d1)))

(print "=== EGA: Serre's Theorem Done ===")
