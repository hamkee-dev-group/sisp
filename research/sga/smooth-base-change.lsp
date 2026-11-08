; ============================================================
; SGA -- Smooth Base Change Theorem.
; Etale cohomology commutes with smooth base change.
; Verify for etale maps.
; ============================================================

(print "=== Smooth Base Change ===")

; ============================================================
; 0. COMPUTATIONAL TOOLKIT: THE FIELD F_64 AND ITS SUBFIELDS
; ============================================================
; Everything below is checked by honest brute force inside one
; explicitly constructed field
;   F_64 = F_2[x]/(x^6 + x + 1),
; whose elements are coefficient vectors of length 6 over F_2
; (low degree first). F_64 contains F_2, F_4, F_8 as the fixed
; fields of the Frobenius powers a -> a^2, a -> a^4, a -> a^8,
; so it is a large enough "geometric point" for every degree
; dividing 6 that appears in this file.

(define gfmod (list 1 1 0 0 0 0 1))          ; 1 + x + x^6

(define (zerolist k) (cond ((<= k 0) nil) (t (cons 0 (zerolist (- k 1))))))
(define (coefat p i) (cond ((< i 0) 0) ((>= i (length p)) 0) (t (nth i p))))
(define (padz p k) (cond ((>= (length p) k) p) (t (append p (zerolist (- k (length p)))))))
(define (padd a b)
  (map (lambda (k) (mod (+ (coefat a k) (coefat b k)) 2))
       (seq 0 (- (max (length a) (length b)) 1))))
(define (pmul a b)
  (cond ((or (eq a nil) (eq b nil)) nil)
        (t (map (lambda (k)
                  (mod (foldl + 0 (map (lambda (i) (* (coefat a i) (coefat b (- k i))))
                                       (seq 0 k)))
                       2))
                (seq 0 (- (+ (length a) (length b)) 2))))))
(define (pdeg-h p i) (cond ((< i 0) -1) ((= (coefat p i) 1) i) (t (pdeg-h p (- i 1)))))
(define (pdeg p) (pdeg-h p (- (length p) 1)))
(define (preduce p)
  (cond ((< (pdeg p) 6) p)
        (t (preduce (padd p (append (zerolist (- (pdeg p) 6)) gfmod))))))
(define (fnorm p) (take (padz (preduce p) 6) 6))

(define gf0 (zerolist 6))
(define gf1 (fnorm (list 1)))
(define (fadd a b) (fnorm (padd a b)))
(define (fmul a b) (fnorm (pmul a b)))
; binary powering, so that a^(2^d) stays cheap
(define (fpow a k)
  (cond ((= k 0) gf1)
        ((evenp k) (let ((h (fpow a (truncate (/ k 2))))) (fmul h h)))
        (t (fmul a (fpow a (- k 1))))))

(define (bitvecs k)
  (cond ((= k 0) (list nil))
        (t (foldr append nil
                  (map (lambda (v) (list (cons 0 v) (cons 1 v))) (bitvecs (- k 1)))))))
(define gfall (bitvecs 6))                    ; all 64 elements

; Frobenius of F_{2^d}: a -> a^(2^d), computed as d successive squarings.
(define (frobd a d) (cond ((= d 0) a) (t (frobd (fmul a a) (- d 1)))))
(define (frob a) (frobd a 1))
; the subfield F_{2^d} = fixed points of a -> a^(2^d), found by brute force
(define (subfield d) (filter (lambda (a) (equal (frobd a d) a)) gfall))

; evaluate an F_2-polynomial (coefficient list, low degree first) at a in F_64
(define (peval cs a)
  (foldl (lambda (acc i) (cond ((= (nth i cs) 1) (fadd acc (fpow a i))) (t acc)))
         gf0 (seq 0 (- (length cs) 1))))
(define (roots-of cs) (filter (lambda (a) (equal (peval cs a) gf0)) gfall))

; orbits of a map phi acting on a finite list
(define (orb-h phi cur acc)
  (cond ((contains cur acc) acc) (t (orb-h phi (phi cur) (append acc (list cur))))))
(define (orbit-of phi a) (orb-h phi a nil))
(define (orbs-h phi rest acc)
  (cond ((eq rest nil) (reverse acc))
        (t (let ((o (orbit-of phi (car rest))))
             (orbs-h phi (filter (lambda (x) (not (contains x o))) rest) (cons o acc))))))
(define (orbits phi lst) (orbs-h phi lst nil))

; The number of connected components of X (x)_{F_2} F_{2^d}, where
; X = Spec(F_2[x]/(cs)) and cs is irreducible: the irreducible factors of
; cs over F_{2^d} are exactly the orbits of a -> a^(2^d) on the roots.
(define (components-over cs d) (length (orbits (lambda (a) (frobd a d)) (roots-of cs))))

(define poly-f4 (list 1 1 1))                 ; x^2 + x + 1,   F_4 = F_2[x]/(...)
(define poly-f8 (list 1 1 0 1))               ; x^3 + x + 1,   F_8 = F_2[x]/(...)

(print "--- 0. Toolkit: F_64 built, subfield sizes ---")
(print (map (lambda (d) (length (subfield d))) (list 1 2 3 6)))
(check "F64-has-64-elements" (= (length gfall) 64))
; brute-forced fixed-point counts on the left, the field-theory prediction
; |F_{2^d}| = 2^d on the right
(check "subfield-size-equals-2-to-the-d"
       (equal (map (lambda (d) (length (subfield d))) (list 1 2 3 6))
              (map (lambda (d) (^ 2 d)) (list 1 2 3 6))))
; for d NOT dividing 6 the fixed field is F_{2^gcd(d,6)}, strictly smaller --
; so the brute-force test really discriminates
(check "fixed-field-of-frobenius-power-is-2-to-the-gcd"
       (equal (map (lambda (d) (length (subfield d))) (list 4 5 7 9))
              (map (lambda (d) (^ 2 (gcd d 6))) (list 4 5 7 9))))

; ============================================================
; 1. SMOOTH BASE CHANGE: STATEMENT
; ============================================================
; Let f: X → S be a quasi-compact, quasi-separated morphism and F a torsion
; etale sheaf on X whose torsion is prime to the residue characteristics
; (e.g., a finite constant sheaf Z/mZ with m invertible on S).
; For any cartesian diagram in which g: S' → S is SMOOTH:
;
;   X' —→ X
;   |      |
;   f'     f
;   ↓      ↓
;   S' —→ S
;
; The base change morphism:
;   g^* R^i f_* F → R^i f'_* (g'^* F)
; is an isomorphism for all i.  (Compare proper base change, where f is
; proper and g is arbitrary.)
;
; For g etale (smooth of relative dimension 0) the statement is elementary:
; g^* is then just restriction to S'.  For f finite (e.g. finite etale) the
; higher direct images R^i f_* vanish for i > 0.

(print "--- 1. Smooth base change statement ---")
(print "For smooth f: X→S, g^* R^i f_* F ≅ R^i f'_* g'^* F")
(print "In particular: etale cohomology commutes with any base change")
(print "For etale f: R^i f_* = 0 for i > 0")
; (section heading only -- the statement is verified concretely in 2..7 below)

; ============================================================
; 2. ETALE MAPS AND BASE CHANGE
; ============================================================
; A finite etale morphism f: X → S = Spec(F_q) means X is a finite
; disjoint union of Spec(F_{q^{d_i}}) with d_i > 0.
; f is finite, flat, and unramified => etale => smooth of relative dim 0.

; For f: Spec(F_{q^n}) → Spec(F_q):
; For any base change S' = Spec(K) where K is a separable extension of F_q:
;   X' = X ×_S S' = Spec(F_{q^n} ⊗_{F_q} K).
; If K contains F_{q^n}: X' splits into n copies of Spec(K).
;
; For F = Z/mZ constant sheaf, R^0 f_* F is NOT the constant sheaf Z/mZ on
; Spec(F_q): it is the etale sheaf U ↦ F(X ×_S U).  Its global sections are
; Z/mZ (X is connected), but its sections over an etale extension K/F_q are
; F(X ⊗_{F_q} K), one copy of Z/mZ for each connected component.
;
; Example: X = Spec(F_4), S = Spec(F_2), S' = Spec(F_4).
;   X' = Spec(F_4 ⊗_{F_2} F_4) = Spec(F_4 × F_4) (two points).
;   R^0 f'_* g'^* F on S' has sections (Z/mZ)^2, one per component of X'.
;   g^* R^0 f_* F, evaluated on S' (an etale S-scheme), has sections
;   F(X ⊗_{F_2} F_4) = F(Spec(F_4 × F_4)) = (Z/mZ)^2 as well.
; So the base change map is an isomorphism ✓.  (Comparing instead with the
; GLOBAL sections Z/mZ of R^0 f_* F over S would wrongly suggest a mismatch.)

(print "--- 2. Smooth base change for etale morphisms ---")
(print "For f etale and F constant: R^0 f_* F(K) = F(X ⊗_F K)")
(print "This is indeed the etale sheaf. Base change holds ✓")

; ------------------------------------------------------------
; CONCRETE TEST: X = Spec(F_4), S = Spec(F_2), S' = Spec(F_4).
; Both sides of the base change morphism are ranks of free Z/mZ-modules,
; i.e. connected-component counts of X' = Spec(F_4 (x)_{F_2} F_4).
; We compute that ONE number by TWO genuinely different routes.
;
; ROUTE A -- ring theory.  Build the 16-element ring
;   A = F_4 (x)_{F_2} F_4 = F_4[y]/(y^2 + y + 1)
; explicitly and count its IDEMPOTENTS by brute force. A finite
; commutative ring with c connected components has exactly 2^c
; idempotents (it is a product of c local rings, here c fields).
;
; ROUTE B -- Galois theory.  The irreducible factors of x^2+x+1 over F_4
; are the orbits of the F_4-Frobenius a -> a^4 on its roots inside F_64.
; ------------------------------------------------------------

; elements of A = F_4[y]/(y^2+y+1) as pairs (c0 c1) with ci in F_4; y^2 = y+1
(define f4elts (subfield 2))
(define aelts (foldr append nil
                     (map (lambda (c0) (map (lambda (c1) (list c0 c1)) f4elts)) f4elts)))
(define (amul u v)
  (let ((u0 (car u)) (u1 (cadr u)) (v0 (car v)) (v1 (cadr v)))
    (list (fadd (fmul u0 v0) (fmul u1 v1))
          (fadd (fadd (fmul u0 v1) (fmul u1 v0)) (fmul u1 v1)))))
(define aidems (filter (lambda (u) (equal (amul u u) u)) aelts))

; recover c from 2^c = number of idempotents, by exact integer search
(define (log2-exact nn) (car (filter (lambda (c) (= (^ 2 c) nn)) (seq 0 8))))
(define comps-route-a (log2-exact (length aidems)))
(define comps-route-b (components-over poly-f4 2))    ; Frobenius orbits

(print "F_4 (x)_F2 F_4 : ring has this many elements, idempotents, components:")
(print (list (length aelts) (length aidems) comps-route-a))
(print "Frobenius-orbit count of the same base change:")
(print comps-route-b)

(check "F4-tensor-F4-has-16-elements" (= (length aelts) 16))
; the two independent computations of the SAME rank must agree
(check "smooth-base-change-etale" (= comps-route-a comps-route-b))
; ... and the common value is 2, NOT 1: the base change genuinely splits,
; so the check is not vacuous
(check "smooth-base-change-etale-splits-into-two" (= comps-route-b 2))
; contrast: F_8 (x)_{F_2} F_4 does NOT split (gcd(3,2)=1), so the same
; machinery returns 1 there -- it discriminates
(check "F8-tensor-F4-stays-connected" (= (components-over poly-f8 2) 1))

; ============================================================
; 3. ETALE SHEAF SECTIONS OVER A GEOMETRIC POINT
; ============================================================
; For f: Spec(L) → Spec(K) with L/K finite separable extension.
; Let s-bar: Spec(K-bar) → Spec(K) be a geometric point.
; The stalk (R^0 f_* F)_{s-bar} = lim_{U → s-bar} F(X ×_S U).
; = F(X ×_S Spec(K-bar)) = F(Spec(L ⊗_K K-bar)).
;
; Since K-bar is algebraically closed, L ⊗_K K-bar ≅ ∏_{σ: L → K-bar} K-bar
; (product over all embeddings). So X_{s-bar} has [L:K] points.
; Then (R^0 f_* F)_{s-bar} = F^{⊕ [L:K]} (direct sum of [L:K] copies of F).
;
; H^0(X_{s-bar}, F) = F^{⊕ [L:K]} (functions from [L:K]-point set to F).
; These match! ✓

(print "--- 3. Stalk at geometric point ---")
(print "(R^0 f_* F)_{s-bar} = F(X_{s-bar}) = F^{⊕n} where n = [L:K]")
(print "H^0(X_{s-bar}, F) = F^{⊕n}. ✓")

; ------------------------------------------------------------
; CONCRETE TEST for L = F_4 and L = F_8 over K = F_2.
; ROUTE A -- the DEGREE [L:K], obtained by counting the elements of L
;   inside F_64 by brute force and solving |L| = 2^[L:K].
; ROUTE B -- the NUMBER OF POINTS of the geometric fibre X_{s-bar},
;   obtained by listing the distinct roots of the defining polynomial
;   in F_64 (= the distinct embeddings L -> F_2-bar).
; Smooth base change at a geometric point says these agree.
; ------------------------------------------------------------

(define (deg-by-cardinality d) (log2-exact (length (subfield d))))
(define (fibre-points cs) (length (roots-of cs)))

(print "degrees by counting field elements:")
(print (list (deg-by-cardinality 2) (deg-by-cardinality 3)))
(print "geometric fibre point counts by root finding:")
(print (list (fibre-points poly-f4) (fibre-points poly-f8)))

(check "smooth-base-change-geometric-point"
       (equal (list (deg-by-cardinality 2) (deg-by-cardinality 3))
              (list (fibre-points poly-f4) (fibre-points poly-f8))))
; not vacuous: the two degrees are different from each other, and neither is 1
(check "geometric-fibres-have-2-and-3-points"
       (equal (list (fibre-points poly-f4) (fibre-points poly-f8)) (list 2 3)))

; ============================================================
; 4. VERIFICATION FOR SPEC(F_8) → SPEC(F_2)
; ============================================================
; L = F_8, K = F_2, n = [F_8:F_2] = 3.
; F = Z/5Z constant sheaf.
; 
; On Spec(F_2): R^0 f_* Z/5Z is the sheaf whose sections over
; an etale extension E/F_2 are H^0(Spec(F_8 ⊗_{F_2} E), Z/5Z).
;
; For E = F_2: F_8 ⊗ F_2 = F_8 (1 point), sections = Z/5Z.
; For E = F_4: since gcd(3,2) = 1, F_8 ⊗_{F_2} F_4 is a field, the
;   compositum F_{2^lcm(3,2)} = F_64 (x^3+x+1 stays irreducible over F_4).
;   So it is a single point, and the sections are Z/5Z.
;
; For E = F_2-bar: F_8 ⊗ F_2-bar = F_2-bar × F_2-bar × F_2-bar (3 copies).
;   Sections = Z/5Z × Z/5Z × Z/5Z = (Z/5Z)^3.

(print "--- 4. F_8/F_2 with constant sheaf Z/5Z ---")
(print "Fiber over F_2-bar: 3 copies of F_2-bar, H^0 = (Z/5Z)^3")
(print "(R^0 f_* Z/5Z)_{s-bar} = (Z/5Z)^3. Match! ✓")

; ------------------------------------------------------------
; CONCRETE TEST.  X = Spec(F_8), S = Spec(F_2), F = Z/5Z.
;
; LEFT SIDE  H^0(X_{s-bar}, Z/5Z): the geometric fibre is the set of
;   distinct roots of x^3+x+1 in F_2-bar; H^0 of a constant sheaf on a
;   finite discrete set of r points is (Z/5Z)^r, of order 5^r.
; RIGHT SIDE (R^0 f_* Z/5Z)_{s-bar}: the stalk is the colimit over etale
;   neighbourhoods E/F_2 of H^0(X (x)_{F_2} E, Z/5Z) = (Z/5Z)^{c(E)},
;   where c(E) is the number of connected components. We compute c(E) for
;   E = F_2, F_4, F_8, F_64 by orbit counting and take the stable value.
; These are different computations: root finding vs orbit counting.
; ------------------------------------------------------------

(define comp-table (map (lambda (d) (components-over poly-f8 d)) (list 1 2 3 6)))
(define stalk-rank (last comp-table))          ; stabilised value over F_2-bar
(define fibre-rank (fibre-points poly-f8))

(print "components of Spec(F_8) x_{F_2} E for E = F_2, F_4, F_8, F_64:")
(print comp-table)
(print "stalk rank / fibre rank / order of the group:")
(print (list stalk-rank fibre-rank (^ 5 fibre-rank)))

(check "smooth-base-change-F8-F2" (= stalk-rank fibre-rank))
(check "F8-F2-stalk-is-Z5-cubed" (= (^ 5 stalk-rank) 125))
; the table is NOT constant -- over F_2 and F_4 the cover stays connected and
; the rank is 1, only over F_8 and beyond does it split into 3
(check "F8-component-table-is-1-1-3-3" (equal comp-table (list 1 1 3 3)))
; and it matches the Galois prediction: c(F_{2^d}) = gcd(3, d)
(check "F8-components-equal-gcd-3-d"
       (equal comp-table (map (lambda (d) (gcd 3 d)) (list 1 2 3 6))))

; ============================================================
; 5. SMOOTH BASE CHANGE FOR PROJECTIVE LINES
; ============================================================
; P^1_S → S is smooth and proper. Base change applies for both.
; For P^1 over S = Spec(F_q), the geometric cohomology (over F_q-bar) is
; H^0(P^1, Z/mZ) = Z/mZ, H^1(P^1, Z/mZ) = 0, H^2(P^1, Z/mZ) = Z/mZ(-1)
; (a Tate twist; equivalently H^2(P^1, μ_m) ≅ Z/mZ).
;
; After base change to S' = Spec(F_{q^n}) the geometric cohomology groups
; are the same; only Frobenius is replaced by its n-th power.
; (Cohomology is invariant under separable base change for the projective
; line.) ✓

(print "--- 5. Smooth base change for P^1 ---")
(print "H^i(P^1_{F_q}, Z/mZ) = H^i(P^1_{F_{q^n}}, Z/mZ)")
(print "Cohomology is invariant under base field extension for P^1")

; ------------------------------------------------------------
; CONCRETE TEST by BRUTE-FORCE POINT COUNTING.
; We enumerate P^1(F_{2^d}) honestly: all pairs (a b) in F x F minus (0 0),
; grouped into orbits under scaling by F^*. No formula is used.
;
; The Lefschetz trace formula reads
;   #P^1(F_{q^n}) = tr(Fr^n | H^0) - tr(Fr^n | H^1) + tr(Fr^n | H^2)
;                 = 1 - 0 + q^n
; using H^0 = Q_l (eigenvalue 1), H^1 = 0, H^2 = Q_l(-1) (eigenvalue q).
; SMOOTH BASE CHANGE says these H^i do not change when we replace the base
; F_q by F_{q^m}: only Fr is replaced by Fr^m. So the SAME eigenvalues,
; extracted from the F_2 point count alone, must predict the point counts
; over F_4 and F_8. We check that against the independent brute force.
; ------------------------------------------------------------

(define (p1-points d)
  (let* ((fld (subfield d))
         (units (filter (lambda (c) (not (equal c gf0))) fld))
         (pairs (filter (lambda (pr) (not (and (equal (car pr) gf0) (equal (cadr pr) gf0))))
                        (foldr append nil
                               (map (lambda (a) (map (lambda (b) (list a b)) fld)) fld)))))
    (length
      (dedup
        (map (lambda (pr)
               (sort (lambda (u v) (< (index-of u pairs) (index-of v pairs)))
                     (map (lambda (c) (list (fmul c (car pr)) (fmul c (cadr pr)))) units)))
             pairs)))))

(define p1-counts (map p1-points (list 1 2 3)))     ; over F_2, F_4, F_8
(print "brute-force #P^1(F_2), #P^1(F_4), #P^1(F_8):")
(print p1-counts)

; read q off the SMALLEST base field only: #P^1(F_q) = 1 + q
(define q-from-h2 (- (car p1-counts) 1))
(print "Frobenius eigenvalue on H^2, read off from the F_2 count only:")
(print q-from-h2)

; now PREDICT the counts over the extensions using the unchanged H^i
(define p1-predicted (map (lambda (n) (+ 1 (^ q-from-h2 n))) (list 1 2 3)))
(print "prediction from the base-changed cohomology:")
(print p1-predicted)

(check "smooth-base-change-P1" (equal p1-counts p1-predicted))
; not vacuous: the three counts are distinct and grow
(check "P1-counts-are-3-5-9" (equal p1-counts (list 3 5 9)))
; contrast -- A^1 (not proper) has q^n points, so the P^1 answer is a real
; statement about P^1 and not about every curve
(check "P1-count-differs-from-A1-count"
       (equal (map (lambda (d) (- (p1-points d) (length (subfield d)))) (list 1 2 3))
              (list 1 1 1)))

; ============================================================
; 6. BASE CHANGE FOR UNRAMIFIED MORPHISMS
; ============================================================
; Etale morphisms are smooth of relative dimension 0, so base change along
; an etale g is covered by the theorem.
; Unramified + flat (+ locally of finite presentation) = etale = smooth of dim 0.
;
; For f: X → S finite etale, any base change preserves the
; fact that X' → S' is finite etale. The direct image f_*
; sends sheaves on X to sheaves on S in a way compatible with
; base change.

(print "--- 6. Etale = smooth of relative dimension 0 ---")
(print "For finite etale f: any base change preserves etaleness")

; ------------------------------------------------------------
; CONCRETE TEST.  Over a perfect field, Spec(k[x]/(g)) -> Spec(k) is etale
; exactly when g is SEPARABLE, i.e. has deg(g) DISTINCT roots in k-bar.
; We test that by counting distinct roots in F_64 -- and we run the test
; both before the base change (over F_2) and after it (over F_4), on a
; list of four polynomials, two etale and two ramified.
; The root count is intrinsic, so the verdict vectors must coincide;
; the point is that the vector is (t t nil nil), NOT all-true.
; ------------------------------------------------------------

(define testpolys (list poly-f8                ; x^3 + x + 1   separable
                        poly-f4                ; x^2 + x + 1   separable
                        (list 0 0 1)           ; x^2 = x*x     repeated root
                        (list 1 0 1)))         ; x^2 + 1 = (x+1)^2 in char 2

(define (separable-p cs) (= (length (roots-of cs)) (pdeg cs)))
; after base change to F_4 the roots live in the same F_2-bar; what changes is
; the factorisation, tracked by the F_4-Frobenius orbits. Etaleness is the
; statement that the orbit sizes sum to deg with no repetition.
(define (etale-after-base-change-p cs)
  (= (foldl + 0 (map length (orbits (lambda (a) (frobd a 2)) (roots-of cs))))
     (pdeg cs)))

(define verdicts-before (map separable-p testpolys))
(define verdicts-after (map etale-after-base-change-p testpolys))
(print "etale verdicts over F_2 / after base change to F_4:")
(print (list verdicts-before verdicts-after))

(check "smooth-base-change-finite-etale" (equal verdicts-before verdicts-after))
; the criterion discriminates: two yes, two no
(check "etale-verdicts-are-not-all-true"
       (equal verdicts-before (list t t nil nil)))
; base change also preserves the DEGREE: sum of the degrees of the F_4-factors
(check "base-change-preserves-degree-3"
       (= (foldl + 0 (map length (orbits (lambda (a) (frobd a 2)) (roots-of poly-f8)))) 3))

; ============================================================
; 7. APPLICATIONS: COHOMOLOGY PULLBACK
; ============================================================
; For f: X → S smooth, for any etale sheaf F on X:
; The cohomology groups H^i(X, F) can be studied by base-changing
; to a geometric point: this reduces to the cohomology of a
; smooth variety over an algebraically closed field.
;
; This is the key technique in etale cohomology: properties of
; cohomology (purity, Lefschetz trace formula, etc.) can be
; checked on geometric fibers.

(print "--- 7. Application: cohomology via geometric fibers ---")
(print "Smooth base change reduces cohomology to fibers")
(print "Key in Deligne's proof of Weil conjectures via purity")

; ------------------------------------------------------------
; CONCRETE TEST: recovering the ARITHMETIC cohomology from the GEOMETRIC one.
; For X finite etale over F_q,
;   H^0(X, Z/mZ) = H^0(X_{s-bar}, Z/mZ)^{Frobenius}.
; LEFT SIDE, brute force: enumerate ALL m^r functions from the r geometric
;   fibre points to Z/mZ and keep the ones fixed by the Frobenius permutation.
; RIGHT SIDE, independently: the invariants are the functions constant on
;   Frobenius orbits, so there are m^(number of orbits) of them, and the
;   number of orbits is the number of connected components of X over F_q.
; ------------------------------------------------------------

(define mm 5)
(define (funcs r) (cond ((= r 0) (list nil))
                        (t (foldr append nil
                                  (map (lambda (v) (map (lambda (c) (cons c v)) (seq 0 (- mm 1))))
                                       (funcs (- r 1)))))))
; the permutation of the fibre induced by the d-th Frobenius power
(define (fibre-perm cs d)
  (let ((rs (roots-of cs)))
    (map (lambda (i) (index-of (frobd (nth i rs) d) rs)) (seq 0 (- (length rs) 1)))))
(define (invariant-count cs d)
  (let ((sg (fibre-perm cs d)))
    (length (filter (lambda (fn)
                      (every (lambda (i) (= (nth (nth i sg) fn) (nth i fn)))
                             (seq 0 (- (length sg) 1))))
                    (funcs (length sg))))))

; three situations: F_8 over F_2, F_8 over F_4, F_4 over F_4
(define situations (list (list poly-f8 1) (list poly-f8 2) (list poly-f4 2)))
(define invariants-brute (map (lambda (s) (invariant-count (car s) (cadr s))) situations))
(define invariants-predicted
  (map (lambda (s) (^ mm (components-over (car s) (cadr s)))) situations))

(print "Frobenius-invariant sections, counted one by one:")
(print invariants-brute)
(print "m^(number of connected components), computed independently:")
(print invariants-predicted)

(check "smooth-base-change-application" (equal invariants-brute invariants-predicted))
; not vacuous: the third situation genuinely has more invariants than the others
(check "invariant-counts-are-5-5-25" (equal invariants-brute (list 5 5 25)))
; and the full geometric H^0 of Spec(F_8) is much bigger than its invariants
(check "geometric-H0-is-125-not-5" (= (^ mm (fibre-points poly-f8)) 125))

(print "=== Smooth Base Change Done ===")
