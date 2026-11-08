; ============================================================
; EGA -- Formal Schemes: Formal Completion Along Closed
; Subscheme, I-adic Completion, Formal Spectrum Spf.
; ============================================================

(print "=== EGA: Formal Schemes ===")

; ---------- pow ----------
(define (pow b e)
  (cond ((= e 0) 1)
        (t (* b (pow b (- e 1))))))

; ============================================================
; 1. FORMAL COMPLETION ALONG CLOSED SUBSCHEME
; ============================================================
; For a scheme X and a closed subscheme Y defined by ideal I ⊂ O_X,
; the formal completion of X along Y is the formal scheme X̂_Y = Spf(lim O_X/I^n).
; This is the I-adic completion of the structure sheaf.

; For affine X = Spec(R) with I ⊂ R ideal:
; Formal completion along V(I): X̂ = Spf(lim_{←n} R/I^n)
; As a topological space: same as V(I).
; Structure sheaf: on the formal scheme, O_X̂ = lim O_X/I^n.

(print "--- 1. Formal completion of Spec(Z/8Z) along V(2) ---")

; R = Z/8Z, I = (2) = {0,2,4,6}
; R/I^n for n=1,2,3,... :
; n=1: R/I ≅ Z/8Z / (2) ≅ Z/2Z (size 2)
; n=2: R/I^2 = Z/8Z / (4) = Z/8Z / {0,4} ≅ Z/4Z (size 4)
; n=3: R/I^3 = Z/8Z / (8) = Z/8Z / {0} ≅ Z/8Z (size 8)
; n≥4: R/I^n = Z/8Z (since I^3 = (8) = (0))

; For Z/8Z, I = (2) = {0,2,4,6}
; I^1 = {0,2,4,6}, I^2 = {0,4}, I^3 = {0}
(define ideal-2-in-8 {0 2 4 6})
(define ideal-4-in-8 {0 4})
(define ideal-8-in-8 {0})

(check "I1-in-Z8-size-4" (= (length ideal-2-in-8) 4))
(check "I2-in-Z8-size-2" (= (length ideal-4-in-8) 2))
(check "I3-in-Z8-size-1" (= (length ideal-8-in-8) 1))

; Size of R/I^n:
; n=1: |R/I| = 8/4 = 2
; n=2: |R/I^2| = 8/2 = 4
; n=3: |R/I^3| = 8/1 = 8
; So the quotient stabilizes at n=3.

(print "Spec(Z/8Z / I^n) sizes for n=1,2,3...:")
(print "  n=1: |R/I| = 2 (Z/2Z)")
(print "  n=2: |R/I^2| = 4 (Z/4Z)")
(print "  n=3: |R/I^3| = 8 (Z/8Z), stabilizes")

(check "R-mod-I1-size-2" (= (/ 8 (length ideal-2-in-8)) 2))
(check "R-mod-I2-size-4" (= (/ 8 (length ideal-4-in-8)) 4))
(check "R-mod-I3-size-8" (= (/ 8 (length ideal-8-in-8)) 8))

; ============================================================
; 2. FORMAL SPECTRUM Spf OF I-ADIC COMPLETION
; ============================================================
; The formal spectrum Spf(A) where A is a complete topological ring
; (I-adic topology). Spf(R̂) has the same underlying space as Spec(R/I).
; But with a richer structure sheaf coming from the inverse limit.

; For R = Z/8Z, R̂ = lim R/I^n = Z/8Z (already complete).
; Spf(Z/8Z) with respect to I=(2) has:
; - Underlying space: Spec(Z/8Z / (2)) = Spec(Z/2Z) = 1 point.
; - Structure sheaf: the inverse limit.

(print "--- 2. Formal spectrum Spf ---")
(print "Spf(Z/8Z) w.r.t I=(2): 1-point space, structure lim Z/8Z/(2)^n")
(print "lim R/(2)^n = Z/8Z (finite ring, filtration terminates)")

; Count the points of Spf(Z/8Z) directly: its underlying space is V(I) ⊂ Spec(R),
; the set of primes of R = Z/8Z containing I = (2).  Build Spec(Z/8Z) from
; scratch (ideals, then primes) and take the ones containing I.
(define z8 {0 1 2 3 4 5 6 7})
(define (add8 a b) (mod (+ a b) 8))
(define (mul8 a b) (mod (* a b) 8))
(define (neg8 a) (mod (- 0 a) 8))

(define (ideal-p8 s)
  (and (in 0 s)
    (every (lambda (a)
      (every (lambda (b) (in (add8 a b) s)) s)) s)
    (every (lambda (a) (in (neg8 a) s)) s)
    (every (lambda (a)
      (every (lambda (rr) (in (mul8 rr a) s)) z8)) s)))

(define ideals8 (filter (lambda (s) (ideal-p8 s)) (powerset z8)))

(define (prime-p8 p)
  (and (not (equal p z8))
    (every (lambda (a)
      (every (lambda (b)
        (or (notin (mul8 a b) p) (in a p) (in b p))) z8)) z8)))

(define spec8 (filter (lambda (p) (prime-p8 p)) ideals8))
(define spf-z8 (filter (lambda (p) (subset ideal-2-in-8 p)) spec8))

; Exactly one point, and that point is the prime (2) itself.
(check "spf-z8-1-point"
  (and (= (length spf-z8) 1) (in ideal-2-in-8 spf-z8)))

; ============================================================
; 3. Spec(R/I^n) FOR n=1,2,3
; ============================================================
; For Z/8Z and I=(2):
; V(I) = 1 point (the prime containing 2)
; The nilpotent thickenings:
;   Spec(R/I) = Spec(Z/2Z): 1 point, reduced
;   Spec(R/I^2) = Spec(Z/4Z): 1 point, non-reduced (nilpotent 2)
;   Spec(R/I^3) = Spec(Z/8Z): 1 point, non-reduced (nilpotent 2,4)

(print "--- 3. Spec(R/I^n) for n=1,2,3 ---")
(print "n=1: Spec(Z/2Z) -- reduced")
(print "n=2: Spec(Z/4Z) -- nilpotent 2")
(print "n=3: Spec(Z/8Z) -- nilpotent 2,4")

; Verification: Z/4Z has a nilpotent element (2^2 = 0 mod 4)
(define (nilpotent-p x mod-rr)
  (labels (((try-k k)
    (cond ((> k 10) nil)
          ((= (mod (pow x k) mod-rr) 0) t)
          (t (try-k (+ k 1))))))
    (try-k 1)))

(check "2-is-nilpotent-in-Z4" (nilpotent-p 2 4))
(check "2-is-nilpotent-in-Z8" (nilpotent-p 2 8))
(check "4-is-nilpotent-in-Z8" (nilpotent-p 4 8))

; ============================================================
; 4. FORMAL COMPLETION OF Spec(Z/12Z) ALONG V(2)
; ============================================================
(print "--- 4. Formal completion of Spec(Z/12Z) along V(2) ---")

; R = Z/12Z, I = (2) = {0,2,4,6,8,10}
; V(2) = {(2)} (the prime containing 2).
; I-adic filtration:
; R/I = Z/12Z / (2) ≅ Z/2Z (size 2)
; R/I^2 = Z/12Z / (4) = Z/12Z / {0,4,8} ≅ Z/4Z (size 4)
; R/I^3 = Z/12Z / (8): in Z/12Z, (8) = (gcd(8,12)) = (4) = {0,4,8}
;   (2*4=8, 2*8=16≡4 mod 12), so I^3 = I^2 and R/I^3 ≅ Z/4Z, with the
;   4 cosets {0,4,8}, {1,5,9}, {2,6,10}, {3,7,11}.
; R/I^k for k≥2: stays Z/4Z (the filtration stabilizes at I^2).  So the
; completion along V(2) is Z/4Z, the 2-primary factor of Z/12Z ≅ Z/4Z × Z/3Z.

(define ideal-2z12 {0 2 4 6 8 10})
(check "I2-in-Z12-size-6" (= (length ideal-2z12) 6))

; R/(2): reps = {0,1} => Z/2Z size 2
; R/(2)^2 = (4) = {0,4,8}, |R/I^2| = 12/3 = 4
; R/(2)^n for n≥2: stays size 4

(check "R-mod-2-in-Z12-size-2" (= (/ 12 6) 2))

; ============================================================
; 5. I-ADIC COMPLETION FOR Dedekind DOMAIN
; ============================================================
; For a Dedekind domain R and a maximal ideal p:
; R̂_p = lim R/p^n. This is a complete DVR.
; For Z localized at (p): the completion of Z_{(p)} is Z_p (p-adic integers).
; The finite quotient Z/p^nZ is the truncation of the completion.

(print "--- 5. I-adic completion for Dedekind domain ---")
(print "Completion of Z at (p): Z_p = lim Z/p^n")
(print "Finite truncation: Z/p^nZ gives the first n levels")
; (No check here: this section is narrative.  The inverse limit lim Z/p^n is an
;  infinite object with no finite representation in this file, so there is
;  nothing to compute.  The finite-level content is checked in section 6.)

; ============================================================
; 6. COUNTING FORMAL THICKENINGS
; ============================================================
; For Z/8Z with I=(2):
;   Spec(R/I) = Spec(Z/2Z): 1 point
;   Spec(R/I^2) = Spec(Z/4Z): 1 point (thicker)
;   Spec(R/I^3) = Spec(Z/8Z): 1 point (thickest)
; The formal scheme X̂ = colim of these thickenings.

(print "--- 6. Formal thickening count ---")
(print "Z/8Z along (2): 3 levels of thickening")
(print "n=1-3 then stabilizes at Z/8Z")

; Compute the I-adic filtration explicitly: I^(n+1) = I^n · I, where the product
; of two ideals is the additive closure of the set of pairwise products.
(define (add-close8 s)
  (let ((bigger (list-to-set
      (foldr append nil
        (map (lambda (a) (map (lambda (b) (add8 a b)) (set-to-list s)))
             (set-to-list s))))))
    (cond ((= (length bigger) (length s)) s)
          (t (add-close8 bigger)))))

(define (ideal-mul8 a b)
  (add-close8 (list-to-set
    (foldr append nil
      (map (lambda (x) (map (lambda (y) (mul8 x y)) (set-to-list b)))
           (set-to-list a))))))

(define ipow1 ideal-2-in-8)
(define ipow2 (ideal-mul8 ipow1 ipow1))
(define ipow3 (ideal-mul8 ipow2 ipow1))
(define ipow4 (ideal-mul8 ipow3 ipow1))

(print (list (quote i-adic-filtration) ipow1 ipow2 ipow3 ipow4))

; The distinct terms of the filtration are I, I^2, I^3 = (0): three levels,
; and the hardcoded ideals above are exactly what the multiplication produces.
(check "three-thickenings-z8"
  (and (= (length (list-to-set (list ipow1 ipow2 ipow3 ipow4))) 3)
       (equal ipow2 ideal-4-in-8)
       (equal ipow3 ideal-8-in-8)
       (equal ipow4 ipow3)))

(print "=== Formal Schemes Done ===")
