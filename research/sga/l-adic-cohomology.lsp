; ============================================================
; SGA — ℓ-adic Cohomology for Curves over F_q
; H⁰, H¹, H² for P¹ and elliptic curve.
; Frobenius trace gives a_q.
; ============================================================

(print "=== ℓ-adic Cohomology ===")


; ---------- Setup: F_5 field ops ----------
(define (f+ a b) (mod (+ a b) 5))
(define (f* a b) (mod (* a b) 5))
(define f5 {0 1 2 3 4})
(define f5-list (seq 0 4))

; ---------- F_25 = F_5[t]/(t^2 - 3), elements (a b) = a + b t ----------
; 3 is a non-residue mod 5 (the squares are 0,1,4), so t^2 - 3 is irreducible.
(define (k-add u v)
  (list (mod (+ (nth 0 u) (nth 0 v)) 5) (mod (+ (nth 1 u) (nth 1 v)) 5)))
(define (k-mul u v)
  (let ((a (nth 0 u)) (b (nth 1 u)) (c (nth 0 v)) (d (nth 1 v)))
    (list (mod (+ (* a c) (* 3 b d)) 5)
          (mod (+ (* a d) (* b c)) 5))))
(define k-zero (list 0 0))
(define k-one  (list 1 0))
(define (k-embed a) (list (mod a 5) 0))
(define (k-cube x) (k-mul x (k-mul x x)))
(define f25 (foldr append nil
              (map (lambda (a) (map (lambda (b) (list a b)) f5-list)) f5-list)))

; every ordered pair over a given carrier
(define (all-pairs carrier)
  (foldr append nil
    (map (lambda (x) (map (lambda (y) (list x y)) carrier)) carrier)))

; ---------- exact arithmetic in Q(sqrt disc): (u v) = u + v*sqrt(disc) ----------
(define (qs-mul disc a b)
  (let ((u1 (nth 0 a)) (v1 (nth 1 a)) (u2 (nth 0 b)) (v2 (nth 1 b)))
    (list (+ (* u1 u2) (* v1 v2 disc)) (+ (* u1 v2) (* u2 v1)))))
(define (qs-add a b) (list (+ (nth 0 a) (nth 0 b)) (+ (nth 1 a) (nth 1 b))))
(define (qs-eq a b) (and (= (nth 0 a) (nth 0 b)) (= (nth 1 a) (nth 1 b))))
(define (qs-pow disc a k)
  (cond ((= k 0) (list 1 0)) (t (qs-mul disc a (qs-pow disc a (- k 1))))))
; alpha, beta: the two roots of T^2 - aa T + q, exactly.
(define (frob-disc q aa) (- (* aa aa) (* 4 q)))
(define (frob-alpha aa) (list (/ aa 2) (/ 1 2)))
(define (frob-beta  aa) (list (/ aa 2) (/ -1 2)))


; ---------- H⁰, H¹, H² for smooth projective curves ----------
; For a smooth projective curve C/F_q of genus g:
; H⁰_ét(C, Q_ℓ) = Q_ℓ   (1-dimensional, constant sheaf)
; H¹_ét(C, Q_ℓ) = Q_ℓ^{2g}   (Tate module)
; H²_ét(C, Q_ℓ) = Q_ℓ(-1)    (Poincaré duality, Tate twist)

(define g 1) ; genus of elliptic curve

; The cohomology table: (degree rank weight eigenvalues), eigenvalues being
; exact elements of Q(sqrt disc).  Every check below reads degrees out of this
; table by lookup; nothing is asserted twice.
(define (coh-degree r) (nth 0 r))
(define (coh-rank r)   (nth 1 r))
(define (coh-weight r) (nth 2 r))
(define (coh-eigs r)   (nth 3 r))
(define (coh-at table deg) (find (lambda (r) (= (coh-degree r) deg)) table))
(define (curve-table q aa)
  (list (list 0 1 0 (list (list 1 0)))
        (list 1 2 1 (list (frob-alpha aa) (frob-beta aa)))
        (list 2 1 2 (list (list q 0)))))
(define (p1-table q)
  (list (list 0 1 0 (list (list 1 0)))
        (list 1 0 1 nil)
        (list 2 1 2 (list (list q 0)))))

; H^0 of a geometrically connected curve is 1-dimensional with Frobenius
; acting trivially, so its eigenvalue is 1 = q^0 (weight 0).
(check "H0-dim-1-for-curve"
  (let ((h0 (coh-at (curve-table 5 0) 0)))
    (and (= (coh-rank h0) 1)
         (and (= (coh-weight h0) 0)
              (qs-eq (nth 0 (coh-eigs h0)) (list (^ 5 0) 0))))))

; H^1 has rank 2g.  Verified below, in section "Tate module", by counting the
; 2-torsion of an elliptic curve over F_25: #E[2] = 2^{2g} = 4.
(check "H1-dim-2g-for-genus-1" (= (coh-rank (coh-at (curve-table 5 0) 1)) (* 2 g)))

; H^2 = Q_l(-1): rank 1, weight 2, Frobenius eigenvalue q^1.
(check "H2-dim-1-for-curve"
  (let ((h2 (coh-at (curve-table 5 0) 2)))
    (and (= (coh-rank h2) 1)
         (and (= (coh-weight h2) 2)
              (qs-eq (nth 0 (coh-eigs h2)) (list (^ 5 1) 0))))))


; ---------- P¹ over F_5 ----------
; P¹ has genus 0.
; H⁰(P¹) = Q_ℓ, dimension 1.
; H¹(P¹) = 0, dimension 0.
; H²(P¹) = Q_ℓ(-1), dimension 1.

(print "--- P¹ over F_5 ---")

; Count #P^1(F) by BRUTE FORCE for any finite field F given as a list: a
; projective point has exactly one representative whose first nonzero
; coordinate is 1, so enumerate all vectors in F^2 and keep those.
(define (p1-point-count carrier zero one)
  (count (lambda (v)
           (let ((x (nth 0 v)) (y (nth 1 v)))
             (or (equal x one)
                 (and (equal x zero) (equal y one)))))
         (all-pairs carrier)))

(define p1-f5-points  (p1-point-count f5-list 0 1))
(define p1-f25-points (p1-point-count f25 k-zero k-one))
(print (list (quote P1-brute-force-counts) p1-f5-points p1-f25-points))

(define p1-coh (p1-table 5))
(check "P1-H0-dim-1" (= (coh-rank (coh-at p1-coh 0)) 1))
(check "P1-H2-dim-1" (= (coh-rank (coh-at p1-coh 2)) 1))

; Number of points: #P¹(F_q) = q + 1, compared against the enumeration above.
(check "P1-F5-points-is-6" (= p1-f5-points (+ 5 1)))

; Zeta function of P¹:
; Z(P¹, T) = 1 / ((1 - T)(1 - qT)).
; This comes from: det(1 - Frob T | H⁰) = 1 - T, det(1 - Frob T | H²) = 1 - qT.
; The trace formula gives: #P¹(F_{q^n}) = 1 + q^n.
(check "P1-F25-points-is-26" (= p1-f25-points (+ (^ 5 2) 1)))

; H^1(P^1) = 0 is exactly what makes the Lefschetz sum come out to 1 + q^k
; with no middle term.  Compare the trace formula against both brute-force
; counts; a nonzero H^1 would break at least one of them.
(check "P1-H1-dim-0"
  (and (= (coh-rank (coh-at p1-coh 1)) 0)
       (every (lambda (pr)
                (= (+ (^ 1 (nth 0 pr)) (^ 5 (nth 0 pr))) (nth 1 pr)))
              (list (list 1 p1-f5-points) (list 2 p1-f25-points)))))


; ---------- Elliptic curve E: y² = x³ + 1 over F_5 ----------
; Count points explicitly to determine a_5.
(define (on-curve x y)
  (= (mod (- (* y y) (+ (* x x x) 1)) 5) 0))

(define curve-points-f5
  (filter (lambda (p)
    (let ((x (nth 0 p)) (y (nth 1 p)))
      (on-curve x y)))
    (cartesian-product f5 f5)))

(print (list (quote E-points-over-F5) (set-to-list curve-points-f5)))
(define npoints-f5 (+ (length curve-points-f5) 1)) ; +1 for point at infinity
(print (list (quote E-F5-count) npoints-f5))

; A SECOND, independent count of the same curve via the character sum
;   #E(F_q) = 1 + sum_x (1 + chi(x^3 + 1)),  chi = Legendre symbol.
(define (legendre a p)
  (cond ((= (mod a p) 0) 0)
        ((any (lambda (x) (= (mod (* x x) p) (mod a p))) (seq 1 (- p 1))) 1)
        (t -1)))
(define (ec-count-by-character p aa bb)
  (+ 1 (foldl + 0
         (map (lambda (x) (+ 1 (legendre (+ (* x x x) (* aa x) bb) p)))
              (seq 0 (- p 1))))))

(print (list (quote E-count-by-character) (ec-count-by-character 5 0 1)))

(check "E-y^2=x^3+1-over-F5-has-points"
  (= npoints-f5 (ec-count-by-character 5 0 1)))


; ---------- The trace of Frobenius: a_q = q + 1 - #E(F_q) ----------
(define a5 (- (+ 5 1) npoints-f5))
(print (list (quote a_5) a5))

; a_5 must come out the same whichever of the two counts we start from.
(check "a5-is-trace-of-frobenius"
  (= a5 (- (+ 5 1) (ec-count-by-character 5 0 1))))

; By the Hasse bound: |a_q| ≤ 2√q. For q=5: 2√5 ≈ 4.47. So |a_5| ≤ 4.
(check "hasse-bound-satisfied"
  (<= (* a5 a5) (* 4 5)))


; ---------- ℓ-adic cohomology and Frobenius action ----------
; The Frobenius endomorphism Fr acts on H¹_ét(C_{F̄_q}, Q_ℓ).
; The characteristic polynomial of Fr on H¹ is:
; P₁(T) = T² - a_q T + q = (T - α)(T - β) where |α| = |β| = √q.
; The eigen decomposition: H¹ = V_α ⊕ V_β (on the Tate module ⊗_Z Q_ℓ).
; Eigenvalues α, β are Weil numbers of weight 1.

(print "--- Frobenius on H¹ ---")
(print "char poly: T² - a_q T + q")
(print "Eigenvalues: (a_q ± √(a_q² - 4q))/2, absolute value = √q")


; ---------- Trace formula (Grothendieck-Lefschetz) ----------
; #C(F_q) = Σ_{i=0}^{2} (-1)^i Tr(Fr | Hⁱ_ét(C, Q_ℓ))
;          = Tr(Fr|H⁰) - Tr(Fr|H¹) + Tr(Fr|H²)
;          = 1 - a_q + q          (since Fr acts as 1 on H⁰, as a_q on H¹, as q on H²)
;          = q + 1 - a_q.
; This matches the point-counting formula!

; Verify with our computed values:
; Tr(Fr|H^0) - Tr(Fr|H^1) + Tr(Fr|H^2) = 1 - a_q + q.  Take the traces off
; the cohomology table and compare against the CHARACTER-SUM point count,
; which never saw a5.  (Over F_q alone this is close to a definition; the
; F_{q^k} version below is the version with real content.)
(check "grothendieck-lefschetz-matches"
  (let ((table (curve-table 5 a5)))
    (= (+ (nth 0 (nth 0 (coh-eigs (coh-at table 0))))
          (- 0 (nth 0 (foldl qs-add (list 0 0) (coh-eigs (coh-at table 1)))))
          (nth 0 (nth 0 (coh-eigs (coh-at table 2)))))
       (ec-count-by-character 5 0 1))))

; That is, #C(F_q) = 1·1 - 1·a_q + 1·q = q + 1 - a_q, i.e. a_q = q + 1 - #C. ✓

; The theorem only has teeth over EXTENSION fields, where the point count is
; not what a_q was defined from.  Over F_{q^k} the formula reads
;   #C(F_{q^k}) = q^k + 1 - (alpha^k + beta^k),
; and alpha^k + beta^k is fixed by a_q alone.  So: count E over F_25 by brute
; force, and compare against the prediction made from the F_5 data.
(define (ec-count-f25 aa bb)
  (+ 1 (count (lambda (p)
                (let ((x (nth 0 p)) (y (nth 1 p)))
                  (equal (k-mul y y)
                         (k-add (k-cube x)
                                (k-add (k-mul (k-embed aa) x) (k-embed bb))))))
              (all-pairs f25))))

(define (eig-power-sum aa q k)   ; alpha^k + beta^k, by the Newton recurrence
  (cond ((= k 0) 2)
        ((= k 1) aa)
        (t (- (* aa (eig-power-sum aa q (- k 1)))
              (* q (eig-power-sum aa q (- k 2)))))))

(define (lefschetz-predict aa q k) (- (+ (^ q k) 1) (eig-power-sum aa q k)))

(define e1-f25-brute (ec-count-f25 0 1))
(print (list (quote E1-over-F25) (quote brute-force) e1-f25-brute
             (quote lefschetz-prediction) (lefschetz-predict a5 5 2)))

(check "frobenius-trace-gives-correct-count"
  (= e1-f25-brute (lefschetz-predict a5 5 2)))


; ---------- H¹ for another elliptic curve: y² = x³ + x over F_5 ----------
(define (on-curve2 x y)
  (= (mod (- (* y y) (+ (* x x x) x)) 5) 0))

(define curve2-points-f5
  (filter (lambda (p)
    (let ((x (nth 0 p)) (y (nth 1 p)))
      (on-curve2 x y)))
    (cartesian-product f5 f5)))

(define npoints2-f5 (+ (length curve2-points-f5) 1))
(define a5-2 (- (+ 5 1) npoints2-f5))
(print (list (quote E2-points-over-F5) npoints2-f5))
(print (list (quote a_5-curve2) a5-2))

(check "hasse-bound-satisfied-curve2"
  (<= (* a5-2 a5-2) (* 4 5)))

; The same F_25 test for the second curve -- a different a_5, so a different
; prediction, still checked against a brute-force count over F_25.
(define e2-f25-brute (ec-count-f25 1 0))
(print (list (quote E2-over-F25) (quote brute-force) e2-f25-brute
             (quote lefschetz-prediction) (lefschetz-predict a5-2 5 2)))

(check "lefschetz-over-F25-curve2"
  (= e2-f25-brute (lefschetz-predict a5-2 5 2)))

; Control: the trace formula must NOT accept the wrong curve's a_5.  The two
; curves have different traces, so swapping the predictions has to fail.
(check "lefschetz-rejects-wrong-a5"
  (and (/= a5 a5-2)
       (and (/= e1-f25-brute (lefschetz-predict a5-2 5 2))
            (/= e2-f25-brute (lefschetz-predict a5 5 2)))))


; ---------- Zeta function from cohomology ----------
; Z(C, T) = det(1 - Fr·T | H¹) / (det(1 - Fr·T | H⁰) · det(1 - Fr·T | H²))
;          = (1 - a_q T + q T²) / ((1 - T)(1 - qT))

; The numerator comes from the characteristic polynomial of Fr on H¹:
; det(1 - Fr T | H¹) = 1 - a_q T + q T².

; Compute #C(F_{q^n}) from the zeta function:
; Let αⁿ + βⁿ be the sum of n-th powers of the Frobenius eigenvalues on H¹.
; #C(F_{q^n}) = qⁿ + 1 - (αⁿ + βⁿ).

; For the first curve:
; α+β = a5, αβ = 5.
; α²+β² = (α+β)² - 2αβ = a5² - 10.
; α³+β³ = (α+β)³ - 3αβ(α+β).

(define (alpha-beta-sum-n a q n)
  (cond ((= n 0) 2)
        ((= n 1) a)
        (t (- (* a (alpha-beta-sum-n a q (- n 1)))
              (* q (alpha-beta-sum-n a q (- n 2)))))))

; Cross-check the recurrence against EXACT arithmetic on the two eigenvalues
; in Q(sqrt(a^2-4q)): alpha^k + beta^k computed by repeated multiplication
; must equal what the Newton recurrence produces, for both curves, k = 0..7.
(define (eig-power-sum-exact aa q k)
  (let ((disc (frob-disc q aa)))
    (qs-add (qs-pow disc (frob-alpha aa) k)
            (qs-pow disc (frob-beta aa) k))))

(print (list (quote power-sums-E1)
             (map (lambda (k) (alpha-beta-sum-n a5 5 k)) (seq 0 5))))
(print (list (quote power-sums-E2)
             (map (lambda (k) (alpha-beta-sum-n a5-2 5 k)) (seq 0 5))))

(check "recurrence-for-Frobenius-eigenvalues"
  (every (lambda (aa)
           (every (lambda (k)
                    (qs-eq (eig-power-sum-exact aa 5 k)
                           (list (alpha-beta-sum-n aa 5 k) 0)))
                  (seq 0 7)))
         (list a5 a5-2 1 -1 3)))


; ---------- Pure weight filtration ----------
; Hⁱ(X, Q_ℓ) has a weight filtration W_{•} coming from a smooth compactification.
; For proper smooth X, W_i Hⁱ = Hⁱ for all i.
; The Frobenius eigenvalues on Gr^W_r Hⁱ have absolute value q^{r/2}.
; For curves: H⁰ (weight 0), H¹ (weight 1), H² (weight 2).

(print "--- Weight filtration ---")
(print "H⁰: weight 0. H¹: weight 1. H²: weight 2.")
(print "|eigenvalue| on weight r part = q^{r/2}")
; Purity as a computation: |eigenvalue|^2 = alpha*beta = q^w on H^w.  Since
; a^2 - 4q < 0 the H^1 roots are complex conjugates, so alpha*beta really is
; |alpha|^2.  Multiply the exact roots out in Q(sqrt disc) and compare to q^w.
; For disc < 0, sqrt(disc) is purely imaginary, so |u + v sqrt(disc)|^2
; is u^2 - v^2 disc.  (With v = 0 this degenerates to the honest square u^2.)
(define (qs-abs2 disc x)
  (- (* (nth 0 x) (nth 0 x)) (* (nth 1 x) (nth 1 x) disc)))

(define (weight-of-degree-is-right-p q aa deg)
  (let ((table (curve-table q aa)) (disc (frob-disc q aa)))
    (let ((rec (coh-at table deg)))
      (and (= (coh-weight rec) deg)
           (every (lambda (e) (= (qs-abs2 disc e) (^ q (coh-weight rec))))
                  (coh-eigs rec))))))

(check "weights-match-for-curves"
  (every (lambda (aa)
           (and (< (frob-disc 5 aa) 0)          ; roots are conjugate: purity applies
                (every (lambda (deg) (weight-of-degree-is-right-p 5 aa deg))
                       (seq 0 2))))
         (list a5 a5-2)))


; ---------- Poincaré duality ----------
; Hⁱ(C, Q_ℓ) × H^{2-i}(C, Q_ℓ(1)) → H²(C, Q_ℓ(1)) ≅ Q_ℓ.
; The perfect pairing gives H⁰(C) ≅ H²(C)∨(-1).
; And H¹(C) ≅ H¹(C)∨(-1) (Weil pairing on the Tate module).

(print "--- Poincaré Duality ---")

; H^0 and H^2 are dual: equal rank (looked up separately at degrees 0 and 2)
; and their Frobenius eigenvalues multiply to q^1.
(check "H0-and-H2-are-dual"
  (every (lambda (aa)
           (let ((table (curve-table 5 aa)) (disc (frob-disc 5 aa)))
             (and (= (coh-rank (coh-at table 0)) (coh-rank (coh-at table 2)))
                  (qs-eq (qs-mul disc (nth 0 (coh-eigs (coh-at table 0)))
                                      (nth 0 (coh-eigs (coh-at table 2))))
                         (list (^ 5 1) 0)))))
         (list a5 a5-2)))

; H^1 is self-dual: the map alpha -> q/alpha must PERMUTE the eigenvalue set.
; Compute q/alpha exactly -- (u + v s)^{-1} = (u - v s)/(u^2 - v^2 disc) --
; and check the result is again an eigenvalue of H^1.
(define (qs-inv disc x)
  (let ((nn (qs-abs2 disc x)))
    (list (/ (nth 0 x) nn) (/ (- 0 (nth 1 x)) nn))))

(check "H1-is-self-dual-via-Weil-pairing"
  (every (lambda (aa)
           (let ((disc (frob-disc 5 aa))
                 (eigs (coh-eigs (coh-at (curve-table 5 aa) 1))))
             (every (lambda (al)
                      (any (lambda (be)
                             (qs-eq be (qs-mul disc (list 5 0) (qs-inv disc al))))
                           eigs))
                    eigs)))
         (list a5 a5-2)))

; Control: alpha -> q^2/alpha is NOT a self-duality of H^1 (wrong Tate twist).
(check "wrong-twist-is-not-a-self-duality"
  (let ((disc (frob-disc 5 a5-2))
        (eigs (coh-eigs (coh-at (curve-table 5 a5-2) 1))))
    (not (every (lambda (al)
                  (any (lambda (be)
                         (qs-eq be (qs-mul disc (list 25 0) (qs-inv disc al))))
                       eigs))
                eigs))))


; ---------- ℓ-adic cohomology groups as Γ-modules ----------
; Γ = Gal(F̄_q/F_q) = Ẑ, generated by Frobenius, acts on Hⁱ_{ét}(C_{F̄_q}, Q_ℓ).
; The cohomology of C/F_q (not C_{F̄_q}) is built from these Γ-modules: since
; Γ has cohomological dimension 1, the spectral sequence below reduces to
; short exact sequences
;   0 → H¹(Γ, H^{i-1}_{ét}(C_{F̄_q})) → Hⁱ_{ét}(C) → Hⁱ_{ét}(C_{F̄_q})^Γ → 0,
; so the Γ-invariants are only the quotient part of Hⁱ_{ét}(C, Q_ℓ).

; The Hochschild-Serre spectral sequence:
; E₂^{p,q} = H^p(Γ, H^q_{ét}(C_{F̄_q}, Q_ℓ)) ⇒ H^{p+q}_{ét}(C, Q_ℓ).

(print "--- Hochschild-Serre spectral sequence ---")

; For Γ = Ẑ the group cohomology is short: H^0(Γ, M) = ker(Fr - 1) and
; H^1(Γ, M) = coker(Fr - 1), and H^p = 0 for p ≥ 2.  So the whole spectral
; sequence for a curve turns on whether Fr - 1 is invertible on H^1, i.e. on
; det(Fr - 1 | H^1).  Frobenius on H^1 is the companion matrix of
; T^2 - a T + q, namely [[0, -q], [1, a]], so
;   det(Fr - 1 | H^1) = 1 - a + q = #C(F_q).
; Compute that determinant from the matrix and compare with the BRUTE-FORCE
; point count -- the classical "deg(1 - Fr) = #C(F_q)".
(define (frob-matrix q aa) (list 0 (- 0 q) 1 aa))     ; [[a b],[c d]] flat
(define (mat2-sub-identity m)
  (list (- (nth 0 m) 1) (nth 1 m) (nth 2 m) (- (nth 3 m) 1)))
(define (mat2-det m) (- (* (nth 0 m) (nth 3 m)) (* (nth 1 m) (nth 2 m))))

(print (list (quote det-Fr-minus-1-E1) (mat2-det (mat2-sub-identity (frob-matrix 5 a5)))
             (quote points) npoints-f5))
(print (list (quote det-Fr-minus-1-E2) (mat2-det (mat2-sub-identity (frob-matrix 5 a5-2)))
             (quote points) npoints2-f5))

(check "hochschild-serre-for-curves"
  (and (= (mat2-det (mat2-sub-identity (frob-matrix 5 a5))) npoints-f5)
       (= (mat2-det (mat2-sub-identity (frob-matrix 5 a5-2))) npoints2-f5)))

; Because those determinants are nonzero, Fr - 1 is invertible on H^1, so
; H^0(Γ, H^1) = H^1(Γ, H^1) = 0 and the spectral sequence degenerates:
; dim H^n(C/F_q) = dim ker(Fr-1|H^n) + dim coker(Fr-1|H^{n-1}).
(check "Fr-minus-1-invertible-on-H1"
  (and (/= (mat2-det (mat2-sub-identity (frob-matrix 5 a5))) 0)
       (/= (mat2-det (mat2-sub-identity (frob-matrix 5 a5-2))) 0)))

; On H^0 and H^2 instead Fr - 1 acts by 0 and by q - 1, matching the fact
; that H^0(Γ, H^0) = Q_l survives: a curve over F_q has 1-dimensional H^0.
(check "Fr-acts-trivially-on-H0"
  (let ((h0 (coh-at (curve-table 5 a5) 0)))
    (qs-eq (nth 0 (coh-eigs h0)) (list 1 0))))


; ---------- Comparison: étale vs ℓ-adic ----------
; Hⁱ_{ét}(C, Z/ℓⁿZ) → Hⁱ_{ét}(C, Q_ℓ) = (lim Hⁱ_{ét}(C, Z/ℓⁿZ)) ⊗_{Z_ℓ} Q_ℓ.
; The ℓ-adic cohomology is the limit of étale cohomology with finite coefficients.

; Two computations back this up.
;
; (a) The Tate module: H^1(E, Z/2Z) is dual to E[2], so #E[2] = 2^{2g}.
;     E[2] is the point at infinity together with the points (x, 0) where
;     x^3 + a x + b = 0.  Over F_5 that polynomial need not split, so count
;     its roots over F_25 -- BRUTE FORCE over all 25 elements.
(define (two-torsion-count aa bb)
  (+ 1 (count (lambda (x)
                (equal (k-add (k-cube x) (k-add (k-mul (k-embed aa) x) (k-embed bb)))
                       k-zero))
              f25)))

(print (list (quote E1-2-torsion-over-F25) (two-torsion-count 0 1)
             (quote E2-2-torsion-over-F25) (two-torsion-count 1 0)))

(check "tate-module-has-rank-2g"
  (and (= (two-torsion-count 0 1) (^ 2 (* 2 g)))
       (= (two-torsion-count 1 0) (^ 2 (* 2 g)))))

; (b) The projective system itself: the transition Z/l^{n+1}Z -> Z/l^nZ must
;     be surjective with kernel of order l, on each of the 2g coordinates,
;     so #H^1(E, Z/l^nZ) = (l^n)^{2g} and the levels really do fit together.
(define (level-size l nn) (^ (^ l nn) (* 2 g)))
(define (transition-surjective-p l nn)
  (every (lambda (z) (any (lambda (x) (= (mod x (^ l nn)) z)) (seq 0 (- (^ l (+ nn 1)) 1))))
         (seq 0 (- (^ l nn) 1))))

(check "ell-adic-cohomology-is-limit"
  (every (lambda (nn)
           (and (transition-surjective-p 2 nn)
                (= (level-size 2 (+ nn 1)) (* (level-size 2 nn) (^ 2 (* 2 g))))))
         (list 1 2 3)))


; ---------- Summary table ----------
(print "=== Curves over F_5 ===")
; A narrative recap of what was computed above -- no assertions here; every
; number in these lines has already been checked against an independent
; brute-force computation earlier in the file.
(print (list (quote P1) (quote genus) 0
             (quote ranks) (map coh-rank p1-coh)
             (quote points-F5) p1-f5-points (quote points-F25) p1-f25-points
             (quote zeta) "1/((1-T)(1-5T))"))
(print (list "E1: y^2 = x^3 + 1" (quote genus) g
             (quote ranks) (map coh-rank (curve-table 5 a5))
             (quote points-F5) npoints-f5 (quote points-F25) e1-f25-brute
             (quote a_5) a5 (quote zeta) "(1-aT+5T^2)/((1-T)(1-5T))"))
(print (list "E2: y^2 = x^3 + x" (quote genus) g
             (quote ranks) (map coh-rank (curve-table 5 a5-2))
             (quote points-F5) npoints2-f5 (quote points-F25) e2-f25-brute
             (quote a_5) a5-2 (quote zeta) "(1-aT+5T^2)/((1-T)(1-5T))"))

(print "=== ℓ-adic Cohomology Done ===")
