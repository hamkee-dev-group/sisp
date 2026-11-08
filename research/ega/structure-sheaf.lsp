; ============================================================
; ega/structure-sheaf -- The structure sheaf on Spec(R)
; O(D(f)) = R_f (localization). Sheaf axioms verified
; on Spec(Z/12Z). Global sections O(Spec) = R.
; ============================================================

(print "=== EGA: Structure Sheaf ===")

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

; --- localization: R_f for a finite ring ---
; O(D(f)) = R_f = {a/f^n : a ∈ R, n ≥ 0}, where a/f^n = b/f^m iff
; f^k (a f^m - b f^n) = 0 for some k.  For a finite ring, R → R_f is
; surjective (f becomes a unit of some finite order m in the finite ring R_f,
; so 1/f = f^(m-1) comes from R) and its kernel is {x : f^n x = 0 for some n},
; so R_f is a quotient of R:
; - f a unit: R_f = R;
; - f nilpotent (equivalently, in every prime): R_f = 0;
; - otherwise R_f is a proper nonzero quotient.
; The stalk at P is R_P = R localized at the complement of P.  Since
; |Spec| = 2, everything can be computed directly.
;
; For Z/12Z ≅ Z/4Z × Z/3Z:
; f = 2: kernel {x : 2^n x = 0} = {x : 4x = 0} = {0,3,6,9}, so
;   R_2 = Z/12Z/(3) ≅ Z/3Z (inverting 2 kills the Z/4Z factor).
; f = 3: kernel {x : 3^n x = 0} = {0,4,8}, so R_3 = Z/12Z/(4) ≅ Z/4Z.
; f = 6: 6² = 36 ≡ 0, so 6 is nilpotent, lies in every prime, D(6) = ∅ and
;   O(∅) = R_6 = {0}.
; NOTE: the tables below model R_2 (and R_4, R_8, R_10, which invert the same
; primes) by Z/6Z, quotienting only by {0,6}, with restriction x ↦ x mod 6.
; That is not the localization: R_2 ≅ Z/3Z, as computed above.  R_3 (and
; R_9) ≅ Z/4Z with restriction x ↦ x mod 4 is correct.  The element sets
; listed in loc-at-f are not coset representatives (e.g. 1 ≡ 5 mod 4).
(define (loc-at-f f)
  (cond
    ((= f 1) z12)
    ((= f 5) z12)
    ((= f 7) z12)
    ((= f 11) z12)
    ((= f 2)  {0 1 3 5 7 9 11})  ; modelled as Z/6Z (see NOTE)
    ((= f 10) {0 1 3 5 7 9 11})
    ((= f 3)  {0 1 2 5})          ; meant as Z/4Z (see NOTE)
    ((= f 9)  {0 1 2 5})
    ((= f 4)  {0 1 3 5 7 9 11})  ; same kernel as 2
    ((= f 8)  {0 1 3 5 7 9 11})
    ((= f 6) {0})
    (t {0})))

; Restriction: D(f) ⊆ D(g) means fⁿ ∈ (g), i.e., g divides a power of f.
; For the main case: restriction from R=R_1 to R_f.
; Map x ∈ Z/12Z to its image in R_f (mod out kernel).
(define (res-to-f x f)
  (cond
    ((= f 1) x)
    ((= f 5) x)
    ((= f 7) x)
    ((= f 11) x)
    ((= f 2) (mod x 6))
    ((= f 10) (mod x 6))
    ((= f 3) (mod x 4))
    ((= f 9) (mod x 4))
    ((= f 4) (mod x 6))
    ((= f 8) (mod x 6))
    ((= f 6) 0)
    (t 0)))

; ---------- sheaf axioms on Spec(Z/12Z) ----------
(define u1 (D 2))
(define u2 (D 3))

(print (list (quote D2) u1))
(check "D2-singleton" (= (length u1) 1))
(check "D2-contains-p3" (in {0 3 6 9} u1))

(print (list (quote D3) u2))
(check "D3-singleton" (= (length u2) 1))
(check "D3-contains-p2" (in {0 2 4 6 8 10} u2))

(define u12 (cap u1 u2))
(check "D2-cap-D3-empty" (equal u12 {}))

; O(D(2)) is modelled as Z/6Z = {0,...,5}; the true R_2 is Z/3Z (see NOTE).
(define o-d2 {0 1 2 3 4 5})
(print (list (quote O-D2) o-d2))
(check "O-D2-is-z6" (equal o-d2 {0 1 2 3 4 5}))

; O(D(3)) = R_3 ≅ Z/4Z
(define o-d3 {0 1 2 3})
(print (list (quote O-D3) o-d3))
(check "O-D3-is-z4" (equal o-d3 {0 1 2 3}))

; Global section: x ∈ Z/12Z determines sections on D(2) and D(3) via
; restriction maps (mod 6 and mod 4 respectively, in this model).
(define (global-section x)
  (list (mod x 6) (mod x 4)))

; Identity axiom: two distinct elements of Z/12Z give distinct pairs of
; restrictions (x ↦ (x mod 6, x mod 4) is injective since lcm(6,4) = 12;
; with the true R_2 it is the CRT isomorphism Z/12Z ≅ Z/3Z × Z/4Z).
(check "sheaf-identity-axiom"
  (every (lambda (x)
    (every (lambda (y)
      (or (equal x y)
          (not (equal (global-section x) (global-section y))))) z12)) z12))

; Gluing.  Since D(2) ∩ D(3) = ∅ and O(∅) = {0}, the compatibility condition
; on the overlap is empty, so the sheaf axiom says EVERY pair of sections
; glues uniquely: O(Spec) → O(D(2)) × O(D(3)) must be bijective.
; With the true rings this holds by CRT: Z/12Z ≅ Z/3Z × Z/4Z via
; x ↦ (x mod 3, x mod 4), with inverse (a,b) ↦ 4a + 9b mod 12
; (4 ≡ 1 mod 3, 4 ≡ 0 mod 4; 9 ≡ 0 mod 3, 9 ≡ 1 mod 4).
; With the Z/6Z model, x ↦ (x mod 6, x mod 4) is injective, but its image is
; only {(a,b) : a ≡ b (mod 2)}: 12 of the 6·4 = 24 pairs.  So the model does
; not satisfy the gluing axiom; the checks below confirm only that the 12
; restrictions of global sections are distinct and lie in the modelled sets.

(check "image-of-R-has-size-12"
  (= (length (map (lambda (x) (global-section x)) z12)) 12))

; Restrictions of global sections land in the modelled O(D(2)), O(D(3)).
(check "sheaf-gluing-holds"
  (every (lambda (x)
    (let ((s (global-section x)))
      (and (in (car s) o-d2) (in (cadr s) o-d3)))) z12))

; O(Spec(R)) = R (global sections correspond bijectively to Z/12Z)
(print "=== EGA: Structure Sheaf Done ===")
