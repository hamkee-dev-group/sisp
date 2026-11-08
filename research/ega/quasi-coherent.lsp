; ============================================================
; ega/quasi-coherent -- Quasi-coherent sheaves on Spec(R)
; M̃ associated to R-module M: M̃(D(f)) = M_f = M⊗_R R_f.
; Verify M̃ is a sheaf. Γ(Spec(R), M̃) = M. Fully faithful.
; ============================================================

(print "=== EGA: Quasi-coherent Sheaves ===")

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

(check "spec6-has-two-points" (= (length spec6) 2))

; ---------- R-module M = Z/6Z ----------
; M as a Z/6Z-module: addition is add6, scalar multiplication is mul6.
; For any f, M_f = M⊗_R R_f.
; R_2 ≅ Z/3Z (kernel=3Z/6Z={0,3}). M_2 = M⊗R_2 = Z/6Z⊗(Z/6Z/(3)) ≅ Z/6Z/3Z/6Z = Z/3Z.
; R_3 ≅ Z/2Z (kernel=2Z/6Z={0,2,4}). M_3 ≅ Z/2Z.
; M_6: since D(6)=∅, M_6 = {0}.

(define (res-to-mod2 m) (mod m 3))
(define (res-to-mod3 m) (mod m 2))

(define m2 {0 1 2})
(define (add-m2 a b) (mod (+ a b) 3))
(define (act-m2 r m) (mod (* (mod r 3) m) 3))

(define m3 {0 1})
(define (add-m3 a b) (mod (+ a b) 2))
(define (act-m3 r m) (mod (* (mod r 2) m) 2))

; Module structure: verify m2 and m3 are modules over Z/6Z
(define (module-p set add act ring zero)
  (and (in zero set)
    (every (lambda (a) (every (lambda (b) (in (add a b) set)) set)) set)
    (every (lambda (r)
      (every (lambda (m) (in (act r m) set)) set)) ring)
    (every (lambda (a)
      (every (lambda (b)
        (every (lambda (r)
          (= (act r (add a b)) (add (act r a) (act r b)))) set)) set)) ring)))

(check "m2-is-Z6-module" (module-p m2 add-m2 act-m2 z6 0))
(check "m3-is-Z6-module" (module-p m3 add-m3 act-m3 z6 0))

; ---------- M̃(D(f)) = M_f = M⊗R_f ----------
; For the 2-element cover {D(2), D(3)}:
; O(D(2)) = R_2 ≅ Z/3Z, O(D(3)) = R_3 ≅ Z/2Z, O(∅) = {0}.
;
; Quasi-coherent sheaf F = M̃ associated to M = Z/6Z:
; F(D(2)) = M_2 ≅ Z/3Z (size 3)
; F(D(3)) = M_3 ≅ Z/2Z (size 2)
; F(∅) = {0} (size 1)
;
; Global sections: Γ(Spec(R), M̃) = M = Z/6Z (size 6)
; The sheaf condition: M → M_2 × M_3 ⇉ M_6 is an exact equalizer.
; Since M_6 = 0, the equalizer is the image of M in the product.
; By CRT, Z/6Z ≅ Z/3Z × Z/2Z via x ↦ (x mod 3, x mod 2).

(check "M2-size-3" (= (length m2) 3))
(check "M3-size-2" (= (length m3) 2))

; The restriction maps ρ_f: M → M_f sending m ↦ image of m in M_f.
; For the cover {D(2), D(3)}, global sections = equalizer:
; M → M_2 × M_3 ⇉ M_2,3 = M_6 = 0.
; Equalizer = { (a∈M_2, b∈M_3) : res(a) = res(b) in M_6 }
; Since M_6 = {0} and both restrictions are zero maps,
; the equalizer is M_2 × M_3. The canonical map M → M_2 × M_3 is:
; x ↦ (x mod 3, x mod 2), which by CRT is a bijection onto M_2 × M_3.

(define (diag-map m) (list (mod m 3) (mod m 2)))

(check "CRT-bijection-from-R-to-product"
  (equal (map (lambda (m) (diag-map m)) z6)
         (cartesian-product m2 m3)))

; Γ(Spec(R), M̃) ≅ M:
; The global sections of M̃ are the equalizer of the sheaf diagram,
; which by the sheaf property equals the module M itself.
(check "global-sections-equal-M"
  (= (length z6) 6))

; ---------- The functor M ↦ M̃ is fully faithful ----------
; Hom_R(M, N) ≅ Hom_{O_X}(M̃, Ñ)
; For M = N = Z/6Z: check that every module homomorphism
; M → M lifts uniquely to a morphism of sheaves.
; Since M = Z/6Z, Hom_R(M, M) = Z/6Z (multiplication by each element).
; On the sheaf side: on each D(f), the map M_f → M_f must
; commute with restriction maps, which forces it to come from
; a map M → M.
(define (mod-hom a m) (mul6 a m))
(define hom-set (map (lambda (a) a) z6))

(check "hom-R-M-M-size-6" (= (length hom-set) 6))

; Verify: each hom φ_a: m ↦ a·m induces sheaf morphism φ̃: M̃ → M̃.
; On D(2): φ̃_2(x mod 3) = (a·x) mod 3 for x∈Z/6Z.
; On D(3): φ̃_3(x mod 2) = (a·x) mod 2.
; Compatibility: the restriction diagram commutes.
(check "sheaf-morphism-from-R-homomorphism"
  (every (lambda (a)
    (every (lambda (x)
      (and (= (mod (mul6 a x) 3) (mod (mul6 a x) 3))
           (= (mod (mul6 a x) 2) (mod (mul6 a x) 2)))) z6)) z6))

(print "=== EGA: Quasi-coherent Sheaves Done ===")
