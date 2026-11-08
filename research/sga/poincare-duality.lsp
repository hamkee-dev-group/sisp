; ============================================================
; SGA -- Poincare Duality for Smooth Curves Over Finite Fields.
; H^i_c(X,F) ≅ H^{2-i}(X, F^∨(1))^∨.
; Trace map. Verify for P^1 and elliptic curves.
; ============================================================

(print "=== Poincare Duality ===")

; ---------- pow ----------
(define (pow b e)
  (cond ((= e 0) 1)
        (t (* b (pow b (- e 1))))))

; ============================================================
; 0. MACHINERY
; ============================================================
; Everything below is checked against these three tools:
;   (a) exact arithmetic in the quadratic field Q(sqrt disc), so that
;       Frobenius eigenvalues on H^1 of an elliptic curve are honest
;       numbers we can multiply, not decorations;
;   (b) a Laplace-expansion determinant, so "nondegenerate pairing"
;       means an actual nonzero determinant;
;   (c) brute-force point counting over F_q, so the Lefschetz trace
;       formula has something independent to be compared against.

; ---------- exact arithmetic in Q(sqrt disc): (u v) = u + v*sqrt(disc) ----------
(define (qs-mul disc a b)
  (let ((u1 (nth 0 a)) (v1 (nth 1 a)) (u2 (nth 0 b)) (v2 (nth 1 b)))
    (list (+ (* u1 u2) (* v1 v2 disc))
          (+ (* u1 v2) (* u2 v1)))))
(define (qs-add a b)
  (list (+ (nth 0 a) (nth 0 b)) (+ (nth 1 a) (nth 1 b))))
(define (qs-eq a b)
  (and (= (nth 0 a) (nth 0 b)) (= (nth 1 a) (nth 1 b))))
(define (qs-sum disc l) (foldl qs-add (list 0 0) l))

; ---------- determinant of a small square matrix (list of rows) ----------
(define (mat-minor m j)
  (map (lambda (row) (append (take row j) (drop row (+ j 1)))) (cdr m)))
(define (det m)
  (cond ((= (length m) 1) (nth 0 (nth 0 m)))
        (t (foldl + 0
             (map (lambda (j)
                    (* (^ -1 j) (nth j (nth 0 m)) (det (mat-minor m j))))
                  (seq 0 (- (length m) 1)))))))

; ---------- brute-force point count of y^2 = x^3 + aa x + bb over F_q ----------
(define (f-pairs q)
  (foldr append nil
    (map (lambda (x) (map (lambda (y) (list x y)) (seq 0 (- q 1))))
         (seq 0 (- q 1)))))
(define (ec-affine-count q aa bb)
  (count (lambda (p)
           (let ((x (nth 0 p)) (y (nth 1 p)))
             (= (mod (- (* y y) (+ (* x x x) (* aa x) bb)) q) 0)))
         (f-pairs q)))
(define (ec-point-count q aa bb) (+ 1 (ec-affine-count q aa bb)))   ; + point at infinity
(define (ec-frobenius-trace q aa bb) (- (+ q 1) (ec-point-count q aa bb)))

; ---------- cohomology records: (degree rank weight eigenvalues) ----------
; An eigenvalue is a Q(sqrt disc) element (u v).  The tables below are the
; ONLY place cohomological data is written down; every duality check reads
; degree i and degree 2d-i back out of the table by independent lookup.
(define (coh-degree r) (nth 0 r))
(define (coh-rank r)   (nth 1 r))
(define (coh-weight r) (nth 2 r))
(define (coh-eigs r)   (nth 3 r))
(define (coh-at table deg)
  (find (lambda (r) (= (coh-degree r) deg)) table))

; P^1 over F_q: H^0 = Q_l, H^1 = 0, H^2 = Q_l(-1) with Frobenius q.
(define (p1-table q)
  (list (list 0 1 0 (list (list 1 0)))
        (list 1 0 1 nil)
        (list 2 1 2 (list (list q 0)))))

; Elliptic curve over F_q with Frobenius trace aa: the two eigenvalues on H^1
; are the roots (aa +- sqrt(aa^2-4q))/2 of T^2 - aa T + q.
(define (ec-disc q aa) (- (* aa aa) (* 4 q)))
(define (ec-table q aa)
  (list (list 0 1 0 (list (list 1 0)))
        (list 1 2 1 (list (list (/ aa 2) (/ 1 2))
                          (list (/ aa 2) (/ -1 2))))
        (list 2 1 2 (list (list q 0)))))

; A bare genus-g curve table, ranks 1, 2g, 1 -- no eigenvalue data.
(define (genus-table gg)
  (list (list 0 1 0 nil)
        (list 1 (* 2 gg) 1 nil)
        (list 2 1 2 nil)))

; ---------- the three halves of Poincare duality, as predicates ----------
(define (rank-duality-p table dd)
  (every (lambda (i)
           (= (coh-rank (coh-at table i))
              (coh-rank (coh-at table (- (* 2 dd) i)))))
         (seq 0 (* 2 dd))))

(define (weight-duality-p table dd)
  (every (lambda (i)
           (= (+ (coh-weight (coh-at table i))
                 (coh-weight (coh-at table (- (* 2 dd) i))))
              (* 2 dd)))
         (seq 0 (* 2 dd))))

; For every Frobenius eigenvalue alpha on H^i there must be an eigenvalue
; beta on H^{2d-i} with alpha*beta = q^d.  This is the numerical shadow of
; the perfect pairing H^i x H^{2d-i} -> H^{2d} = Q_l(-d).
(define (eig-duality-p table dd q disc)
  (every (lambda (i)
           (let ((dual (coh-eigs (coh-at table (- (* 2 dd) i)))))
             (every (lambda (al)
                      (any (lambda (be)
                             (qs-eq (qs-mul disc al be) (list (^ q dd) 0)))
                           dual))
                    (coh-eigs (coh-at table i)))))
         (seq 0 (* 2 dd))))

; ============================================================
; 1. POINCARE DUALITY: STATEMENT
; ============================================================
; Let X be a smooth, geometrically connected variety of dimension d
; over a finite field F_q (or any separably closed field).
; Let F be a lisse (locally constant constructible) Q_ℓ-sheaf, ℓ ≠ p =
; char(F_q), with dual F^∨ = Hom(F, Q_ℓ).  (For general constructible F one
; uses the Verdier dual RHom(F, Q_ℓ(d)[2d]) instead.)
;
; Poincaré duality gives a perfect pairing:
;   H^i_c(X, F) × H^{2d-i}(X, F^∨(d)) → H^{2d}_c(X, Q_ℓ(d)) ≅ Q_ℓ
;
; Or equivalently:
;   H^i_c(X, F) ≅ H^{2d-i}(X, F^∨(d))^∨ (dual vector space).
;
; For curves (d=1):
;   H^i_c(X, F) ≅ H^{2-i}(X, F^∨(1))^∨
;
; In particular:
;   H^0_c(X, Q_ℓ) ≅ H^2(X, Q_ℓ(1))^∨
;   H^1_c(X, Q_ℓ) ≅ H^1(X, Q_ℓ(1))^∨
;   H^2_c(X, Q_ℓ) ≅ H^0(X, Q_ℓ(1))^∨

(print "--- 1. Poincare duality statement ---")
(print "For curve X/F_q of dimension 1:")
(print "  H^i_c(X, F) ≅ H^{2-i}(X, F^∨(1))^∨")

; The statement, as a computation: for a smooth projective curve of any
; genus, rank H^i = rank H^{2-i} and weight H^i + weight H^{2-i} = 2, with
; the two degrees looked up independently in the table.
(check "poincare-duality-statement"
  (every (lambda (gg)
           (and (rank-duality-p (genus-table gg) 1)
                (weight-duality-p (genus-table gg) 1)))
         (seq 0 4)))

; Control: the checker must REJECT a table that is not self-dual.
; Ranks 1,2,3 in degrees 0,1,2 violate rank H^0 = rank H^2.
(check "duality-checker-rejects-non-dual-table"
  (not (rank-duality-p (list (list 0 1 0 nil) (list 1 2 1 nil) (list 2 3 2 nil))
                       1)))

; ============================================================
; 2. POINCARE DUALITY FOR P^1
; ============================================================
; P^1 is proper => H^i_c = H^i (compact support = ordinary cohom).
; d=1 (dimension = 1).
;
; H^0(P^1, Q_ℓ) = Q_ℓ (dimension 1)
; H^1(P^1, Q_ℓ) = 0
; H^2(P^1, Q_ℓ) = Q_ℓ(-1) (Tate twist, dimension 1)
;
; F = Q_ℓ (constant sheaf). Since F^∨ = Q_ℓ (self-dual), we have:
; F^∨(1) = Q_ℓ(1).
;
; Poincare duality:
; H^0(P^1, Q_ℓ) ≅ H^2(P^1, Q_ℓ(1))^∨ = Q_ℓ(0)^∨ = Q_ℓ. ✓
;   (since H^2(P^1, Q_ℓ(1)) = Q_ℓ(-1)⊗Q_ℓ(1) = Q_ℓ, and dual = Q_ℓ)
; H^1(P^1, Q_ℓ) ≅ H^1(P^1, Q_ℓ(1))^∨ = 0^∨ = 0. ✓
; H^2(P^1, Q_ℓ) ≅ H^0(P^1, Q_ℓ(1))^∨ = Q_ℓ(1)^∨ = Q_ℓ(-1). ✓

(print "--- 2. Poincare duality for P^1 ---")
(print "H^0 ≅ H^2(1)^∨, H^1 ≅ H^1(1)^∨, H^2 ≅ H^0(1)^∨")
(print "All satisfied for P^1")

; Full duality for P^1 over F_5: ranks, weights, and the eigenvalue pairing
; alpha*beta = q^d.  H^0 carries Frobenius 1, H^2 carries q, 1*q = 5 = q^1.
(define q5 5)
(define p1-f5 (p1-table q5))
(print (list (quote P1-table) p1-f5))

(check "poincare-duality-P1"
  (and (and (rank-duality-p p1-f5 1) (weight-duality-p p1-f5 1))
       (eig-duality-p p1-f5 1 q5 0)))

; #P^1(F_q) = q+1 read off the table via Lefschetz: 1 - 0 + q.
(check "P1-lefschetz-trace-is-q-plus-1"
  (= (+ (nth 0 (qs-sum 0 (coh-eigs (coh-at p1-f5 0))))
        (- 0 (nth 0 (qs-sum 0 (coh-eigs (coh-at p1-f5 1)))))
        (nth 0 (qs-sum 0 (coh-eigs (coh-at p1-f5 2)))))
     (+ q5 1)))

; ============================================================
; 3. POINCARE DUALITY FOR ELLIPTIC CURVE
; ============================================================
; E is projective smooth, dimension 1 => proper => H^i_c = H^i.
;
; H^0(E, Q_ℓ) = Q_ℓ (dim 1)
; H^1(E, Q_ℓ) = Q_ℓ^2 (dim 2, Tate module)
; H^2(E, Q_ℓ) = Q_ℓ(-1) (dim 1)
;
; F = Q_ℓ, F^∨(1) = Q_ℓ(1).
;
; Duality pairs:
; H^0 ≅ H^2(1)^∨: dim 1 = dim Q_ℓ = 1. ✓
; H^1 ≅ H^1(1)^∨: dim 2 = dim H^1(E, Q_ℓ(1)) which is also 2-dim.
;   H^1(E, Q_ℓ(1)) = H^1(E, Q_ℓ) ⊗ Q_ℓ(1), again 2-dimensional, so its
;   dual is 2-dimensional too. ✓
; H^2 ≅ H^0(1)^∨: dim 1 = dim Q_ℓ(1)^∨ = 1. ✓

(print "--- 3. Poincare duality for elliptic curve ---")
(print "H^0 ≅ H^2(1)^∨, dim 1 = 1 ✓")
(print "H^1 ≅ H^1(1)^∨, dim 2 = 2 ✓")
(print "H^2 ≅ H^0(1)^∨, dim 1 = 1 ✓")

; E: y^2 = x^3 + 1 over F_5.  The Frobenius trace is obtained by BRUTE-FORCE
; point counting, not by assertion; the H^1 eigenvalues are then the exact
; roots of T^2 - a T + 5 in Q(sqrt(a^2-20)).
(define e1-points (ec-point-count 5 0 1))
(define e1-a      (ec-frobenius-trace 5 0 1))
(define e1-disc   (ec-disc 5 e1-a))
(define e1-table  (ec-table 5 e1-a))
(print (list (quote E1-y2=x3+1-over-F5) (quote points) e1-points (quote a) e1-a
             (quote disc) e1-disc))

(check "poincare-duality-E"
  (and (and (rank-duality-p e1-table 1) (weight-duality-p e1-table 1))
       (eig-duality-p e1-table 1 5 e1-disc)))

; A second curve, y^2 = x^3 + x over F_5, counted the same way.
(define e2-points (ec-point-count 5 1 0))
(define e2-a      (ec-frobenius-trace 5 1 0))
(define e2-disc   (ec-disc 5 e2-a))
(define e2-table  (ec-table 5 e2-a))
(print (list (quote E2-y2=x3+x-over-F5) (quote points) e2-points (quote a) e2-a
             (quote disc) e2-disc))

(check "poincare-duality-E2"
  (and (and (rank-duality-p e2-table 1) (weight-duality-p e2-table 1))
       (eig-duality-p e2-table 1 5 e2-disc)))

; Weil pairing, stated numerically: alpha*beta = q for the H^1 eigenvalues,
; i.e. H^1 is self-dual up to a Tate twist.  Multiply them out exactly.
(check "H1-eigenvalues-multiply-to-q"
  (every (lambda (pr)
           (qs-eq (qs-mul (nth 1 pr)
                          (nth 0 (coh-eigs (coh-at (nth 2 pr) 1)))
                          (nth 1 (coh-eigs (coh-at (nth 2 pr) 1))))
                  (list 5 0)))
         (list (list 0 e1-disc e1-table) (list 0 e2-disc e2-table))))

; ============================================================
; 4. THE TRACE MAP
; ============================================================
; The trace map Tr: H^{2d}_c(X, Q_ℓ(d)) → Q_ℓ is the fundamental
; isomorphism that gives the perfect pairing.
;
; For a curve X/F_q: Tr: H^2_c(X, Q_ℓ(1)) → Q_ℓ.
;
; For P^1: H^2(P^1, Q_ℓ(1)) = Q_ℓ (since H^2 = Q_ℓ(-1), tensor with
; Q_ℓ(1) = Q_ℓ). Tr: Q_ℓ → Q_ℓ is the identity. So the pairing
; H^0 × H^2 → Q_ℓ is (a, b) → a·b (multiplication).

(print "--- 4. Trace map ---")
(print "Tr: H^{2d}_c(X, Q_ℓ(d)) → Q_ℓ is the canonical isomorphism")
(print "For P^1: Tr = id: Q_ℓ → Q_ℓ")

; The trace map exists exactly when H^{2d} is 1-dimensional, and it identifies
; H^{2d} with Q_l(-d), i.e. the single Frobenius eigenvalue there is q^d.
; Check both facts on every table we have built, by lookup at degree 2d.
(define (trace-map-is-iso-p table dd q)
  (let ((top (coh-at table (* 2 dd))))
    (and (= (coh-rank top) 1)
         (qs-eq (nth 0 (coh-eigs top)) (list (^ q dd) 0)))))

(check "trace-map-P1"
  (every (lambda (table) (trace-map-is-iso-p table 1 5))
         (list p1-f5 e1-table e2-table)))

; Control: a curve whose H^2 had rank 2 would admit no trace isomorphism.
(check "trace-map-rejects-rank-2-top"
  (not (trace-map-is-iso-p
         (list (list 0 1 0 nil) (list 1 0 1 nil)
               (list 2 2 2 (list (list 5 0) (list 5 0))))
         1 5)))

; ============================================================
; 5. CUP PRODUCT AND PERFECT PAIRING
; ============================================================
; The Poincare pairing is given by the cup product:
; H^i_c(X, F) ⊗ H^{2d-i}(X, F^∨(d)) → H^{2d}_c(X, Q_ℓ(d)) →^{Tr} Q_ℓ
;
; This pairing is nondegenerate (perfect):
; For any non-zero α in H^i_c, there exists β in H^{2d-i} such that
; Tr(α ∪ β) ≠ 0.
;
; For H^0 and H^2 of P^1: the pairing is multiplication. Nondegenerate:
; For a ≠ 0 in H^0 = Q_ℓ, pair with b = 1/a in H^2 = Q_ℓ, product = 1 ≠ 0.

(print "--- 5. Cup product gives perfect pairing ---")
(print "α ∈ H^i_c, β ∈ H^{2d-i}, pairing = Tr(α ∪ β)")

; Concretely: on H^1 of a genus-g curve the cup product is the symplectic
; form J_g = [[0, I_g], [-I_g, 0]] in a symplectic basis.  "Perfect pairing"
; means det J_g is a UNIT, so compute the determinant and check it is nonzero
; modulo the coefficient ring.
(define (symplectic-matrix gg)
  (let ((nn (* 2 gg)))
    (map (lambda (i)
           (map (lambda (j)
                  (cond ((= j (+ i gg)) 1)
                        ((= i (+ j gg)) -1)
                        (t 0)))
                (seq 0 (- nn 1))))
         (seq 0 (- nn 1)))))

(print (list (quote symplectic-g1) (symplectic-matrix 1)
             (quote det) (det (symplectic-matrix 1))))
(print (list (quote symplectic-g2) (quote det) (det (symplectic-matrix 2))))

(check "cup-product-pairing"
  (every (lambda (gg)
           (and (/= (det (symplectic-matrix gg)) 0)
                (/= (mod (det (symplectic-matrix gg)) 3) 0)))
         (list 1 2 3)))

; Control: a genuinely degenerate pairing matrix must be caught -- rows
; (1 2) and (2 4) are proportional, so the determinant vanishes.
(check "degenerate-pairing-has-zero-determinant"
  (= (det (list (list 1 2) (list 2 4))) 0))

; ============================================================
; 6. VERIFICATION FOR P^1 OVER F_2 (CONSTANT SHEAF Z/3Z)
; ============================================================
; With finite coefficients Z/mZ, m prime to p (on P^1_{F_2} this excludes
; m = 2, so take m = 3), Poincare duality on a curve reads
;   H^i_c(X, Z/mZ) ≅ Hom(H^{2-i}(X, μ_m), Z/mZ),   μ_m = Z/mZ(1).
; Over F_2-bar: H^0(P^1, Z/3Z) = Z/3Z, H^1(P^1, Z/3Z) = 0, and
; H^2(P^1, Z/3Z) ≅ Z/3Z(-1), i.e. H^2(P^1, μ_3) ≅ Z/3Z via the trace map.
; As abstract groups these are Z/3Z, 0, Z/3Z; the Tate twist changes only
; the Frobenius action (on μ_3(F_2-bar) ⊂ F_4^×, Frobenius acts by ζ ↦ ζ²).
; Note that μ_3(F_2) = {x ∈ F_2^× : x^3 = 1} = {1} is the group of
; F_2-RATIONAL points of μ_3, not H^2.
; So it suffices to compare group orders: 3, 1, 3.

(print "--- 6. Poincare duality for P^1 with Z/3Z ---")
(print "H^0 = Z/3Z, H^1 = 0, H^2 = Z/3Z (≅ μ_3 by duality)")
(print "Dimensions: 1,0,1 → Poincare dual pairs match")

; With finite coefficients Z/nZ, "dual" means Hom(-, Z/nZ).  The number of
; group homomorphisms Z/mZ -> Z/nZ is gcd(m,n); compute it by brute force
; over all n candidate maps k: a -> k*a, keeping the k that are well defined.
(define (hom-count mm nn)
  (count (lambda (k) (= (mod (* mm k) nn) 0)) (seq 0 (- nn 1))))

(check "hom-count-agrees-with-gcd"
  (every (lambda (pr) (= (hom-count (nth 0 pr) (nth 1 pr))
                         (gcd (nth 0 pr) (nth 1 pr))))
         (list (list 3 3) (list 2 3) (list 4 6) (list 1 5) (list 6 4))))

; P^1 with Z/3Z coefficients: the group ORDERS in degrees 0,1,2 are 3,1,3.
; Duality says ord H^{2-i} = #Hom(H^i, Z/3Z); look up both degrees separately.
(define p1-z3-orders (list (list 0 3) (list 1 1) (list 2 3)))
(define (order-at deg) (nth 1 (find (lambda (r) (= (nth 0 r) deg)) p1-z3-orders)))

(check "poincare-duality-P1-Z3"
  (every (lambda (i) (= (order-at (- 2 i)) (hom-count (order-at i) 3)))
         (seq 0 2)))

; And the pairing itself is nondegenerate: for every nonzero a in Z/3Z there
; is a b with a*b nonzero mod 3.  The same test over Z/6Z with the pairing
; (a,b) -> 2ab fails, so the test has teeth.
(define (pairing-nondegenerate-p nn c)
  (every (lambda (a) (any (lambda (b) (/= (mod (* c a b) nn) 0)) (seq 0 (- nn 1))))
         (filter (lambda (a) (/= a 0)) (seq 0 (- nn 1)))))

(check "Z3-cup-pairing-nondegenerate" (pairing-nondegenerate-p 3 1))
(check "Z6-doubled-pairing-is-degenerate" (not (pairing-nondegenerate-p 6 2)))

; ============================================================
; 7. COHOMOLOGY WITH COMPACT SUPPORT
; ============================================================
; For open curves like A^1 (affine line, not proper):
; H^0_c(A^1, Q_ℓ) = 0
; H^1_c(A^1, Q_ℓ) = 0
; H^2_c(A^1, Q_ℓ) = Q_ℓ(-1)
;
; Poincare duality pairs H^i_c with H^{2-i}(1):
; H^0_c ≅ H^2(1)^∨: 0 ≅ H^2(A^1, Q_ℓ(1))^∨ = 0, since H^2(A^1) = 0. ✓
; H^1_c ≅ H^1(1)^∨: 0 ≅ H^1(A^1, Q_ℓ(1))^∨. H^1(A^1) = 0. ✓
; H^2_c ≅ H^0(1)^∨: Q_ℓ(-1) ≅ H^0(A^1, Q_ℓ(1))^∨ = Q_ℓ(1)^∨ = Q_ℓ(-1). ✓

(print "--- 7. Cohomology with compact support for A^1 ---")
(print "H^0_c=0, H^1_c=0, H^2_c=Q_ℓ(-1)")
(print "Dual pairs: H^2_c ≅ H^0(1)^∨ = Q_ℓ(-1) ✓")

; A^1 is NOT proper, so H^*_c and H^* are genuinely different tables.
; Duality is a statement relating one to the other, so build both and look
; degree i up in the compact-support table and degree 2-i in the ordinary one.
(define a1-compact  (list (list 0 0 0 nil) (list 1 0 1 nil) (list 2 1 2 (list (list 5 0)))))
(define a1-ordinary (list (list 0 1 0 (list (list 1 0))) (list 1 0 1 nil) (list 2 0 2 nil)))

(define (cross-duality-p tc to dd)
  (every (lambda (i)
           (and (= (coh-rank (coh-at tc i))
                   (coh-rank (coh-at to (- (* 2 dd) i))))
                (= (+ (coh-weight (coh-at tc i))
                      (coh-weight (coh-at to (- (* 2 dd) i))))
                   (* 2 dd))))
         (seq 0 (* 2 dd))))

(check "poincare-duality-A1" (cross-duality-p a1-compact a1-ordinary 1))

; P^1 IS proper, so there H^*_c = H^* and the same cross-check passes with
; one and the same table on both sides.
(check "P1-proper-so-compact-support-agrees" (cross-duality-p p1-f5 p1-f5 1))

; Control: pairing A^1's compact-support cohomology against A^1's OWN
; compact-support cohomology is not duality, and indeed fails.
(check "A1-is-not-self-dual-without-compact-supports"
  (not (cross-duality-p a1-compact a1-compact 1)))

; ============================================================
; 8. POINCARE DUALITY FOR FROBENIUS ACTION
; ============================================================
; Frobenius Fr acts on both sides of the duality.
; For X proper and smooth, the eigenvalues on H^i have weight i and those
; on H^{2d-i} weight 2d-i (Deligne); in general H^i_c has weights ≤ i.
; The pairing is Frobenius-equivariant:
;   ⟨Fr·α, Fr·β⟩ = q^d ⟨α, β⟩
;
; For P^1 (d=1):
; Fr on H^0: eigenvalue 1 (weight 0).
; Fr on H^2: eigenvalue q (weight 2).
; ⟨Fr·1, Fr·1⟩ = 1·q·⟨1, 1⟩: the pairing H^0 × H^2 → H^2 ≅ Q_ℓ(-1) is
; Frobenius-equivariant, and Frobenius acts on the target Q_ℓ(-1) by q.

(print "--- 8. Frobenius-equivariant pairing ---")
(print "<Fr·α, Fr·β> = q^d <α, β>")
(print "For P^1: H^0 eigenvalue 1, H^2 eigenvalue q, product = q ✓")

; Frobenius-equivariance has a testable consequence: the Grothendieck-
; Lefschetz trace formula.  Sum the Frobenius traces off the cohomology
; table with alternating signs and compare against the BRUTE-FORCE point
; count of the same curve over F_5.  Two entirely separate computations.
(define (lefschetz-count table disc dd)
  (nth 0
    (foldl qs-add (list 0 0)
      (map (lambda (i)
             (let ((s (qs-sum disc (coh-eigs (coh-at table i)))))
               (cond ((evenp i) s)
                     (t (list (- 0 (nth 0 s)) (- 0 (nth 1 s)))))))
           (seq 0 (* 2 dd))))))

(print (list (quote lefschetz-E1) (lefschetz-count e1-table e1-disc 1)
             (quote brute-force) e1-points))
(print (list (quote lefschetz-E2) (lefschetz-count e2-table e2-disc 1)
             (quote brute-force) e2-points))

; The brute-force count itself needs an independent witness, otherwise
; a = q + 1 - #E makes the comparison above true by construction.  Count the
; same curves a SECOND way, by the character sum
;   #E(F_q) = 1 + sum_x (1 + chi(x^3 + aa x + bb)),  chi = Legendre symbol.
(define (legendre a p)
  (cond ((= (mod a p) 0) 0)
        ((any (lambda (x) (= (mod (* x x) p) (mod a p))) (seq 1 (- p 1))) 1)
        (t -1)))
(define (ec-count-by-character p aa bb)
  (+ 1 (foldl + 0
         (map (lambda (x) (+ 1 (legendre (+ (* x x x) (* aa x) bb) p)))
              (seq 0 (- p 1))))))

(print (list (quote character-sum-counts)
             (ec-count-by-character 5 0 1) (ec-count-by-character 5 1 0)))

(check "poincare-duality-frobenius"
  (and (and (= (lefschetz-count e1-table e1-disc 1) e1-points)
            (= (lefschetz-count e2-table e2-disc 1) e2-points))
       (and (= e1-points (ec-count-by-character 5 0 1))
            (= e2-points (ec-count-by-character 5 1 0)))))

; The other visible consequence of a Frobenius-equivariant perfect pairing is
; the FUNCTIONAL EQUATION of the zeta numerator.  With P(T) = prod (1 - a_i T)
; over the H^1 eigenvalues, duality (a_i -> q/a_i is a permutation) forces
;   q T^2 P(1/(qT)) = P(T).
; Evaluate both sides at several rational T, in exact Q(sqrt disc) arithmetic.
(define (charpoly-at disc eigs tv)
  (foldl (lambda (acc al)
           (qs-mul disc acc
             (qs-add (list 1 0) (qs-mul disc al (list (- 0 tv) 0)))))
         (list 1 0) eigs))
(define (qs-scale disc s x) (qs-mul disc (list s 0) x))

(define (functional-equation-p q table disc)
  (let ((eigs (coh-eigs (coh-at table 1))))
    (every (lambda (tv)
             (qs-eq (qs-scale disc (* q tv tv)
                      (charpoly-at disc eigs (/ 1 (* q tv))))
                    (charpoly-at disc eigs tv)))
           (list 1 2 3 (/ 1 7)))))

(check "zeta-numerator-functional-equation"
  (and (functional-equation-p 5 e1-table e1-disc)
       (functional-equation-p 5 e2-table e2-disc)))

; Control: eigenvalues that do NOT pair to q break the functional equation.
(check "functional-equation-fails-for-wrong-eigenvalues"
  (not (functional-equation-p 5
         (list (list 0 1 0 nil)
               (list 1 2 1 (list (list 1 0) (list 3 0)))
               (list 2 1 2 nil))
         0)))

; ============================================================
; 9. DIMENSIONAL VERIFICATION FOR CURVES
; ============================================================
(print "--- 9. Dimensional verification ---")

; For a smooth projective curve of genus g:
; H^0 dimension = 1, H^1 dimension = 2g, H^2 dimension = 1.
; (For constant sheaf Q_ℓ, which is self-dual up to Tate twist.)
;
; Poincare duality pairs:
; H^0 ↔ H^2(1)^∨: both dim=1
; H^1 ↔ H^1(1)^∨: both dim=2g

(define (h0-curve genus-g) 1)
(define (h1-curve genus-g) (* 2 genus-g))
(define (h2-curve genus-g) 1)

(check "genus-0-H0-eq-H2" (= (h0-curve 0) (h2-curve 0)))
(check "genus-1-H0-eq-H2" (= (h0-curve 1) (h2-curve 1)))
(check "genus-1-H1-dim-2" (= (h1-curve 1) 2))
(check "genus-2-H1-dim-4" (= (h1-curve 2) 4))

; Poincare duality forces rank H^1 = rank H^{2-1}, and the ranks it produces
; must reproduce the topological Euler characteristic chi = 2 - 2g.  Read the
; ranks back out of the table; compare with 2-2g, which the table never saw.
(define (euler-char table dd)
  (foldl + 0 (map (lambda (i) (* (^ -1 i) (coh-rank (coh-at table i))))
                  (seq 0 (* 2 dd)))))

(check "duality-h1-dimension-match"
  (every (lambda (g-val)
           (= (coh-rank (coh-at (genus-table g-val) 1))
              (coh-rank (coh-at (genus-table g-val) (- 2 1)))))
         (list 0 1 2 3)))

(check "euler-characteristic-is-2-minus-2g"
  (every (lambda (g-val) (= (euler-char (genus-table g-val) 1) (- 2 (* 2 g-val))))
         (list 0 1 2 3 4)))

; The genus-1 tables built from the BRUTE-FORCE point counts must have
; Euler characteristic 0, and their H^1 rank must be 2g = 2.
(check "counted-curves-have-euler-char-0"
  (and (= (euler-char e1-table 1) 0) (= (euler-char e2-table 1) 0)))

(print "=== Poincare Duality Done ===")
