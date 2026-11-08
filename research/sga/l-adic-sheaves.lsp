; ============================================================
; SGA — ℓ-adic Sheaves
; ℓ-adic sheaves = projective limits of Z/ℓ^nZ-sheaves.
; For finite field computations: work with Z/ℓ^nZ coefficients.
; Smooth sheaf on Spec(F_q): determined by Frobenius action on stalk.
; Purity: eigenvalues are Weil numbers of weight w.
; ============================================================

(print "=== ℓ-adic Sheaves ===")

; ============================================================
; 0. MACHINERY
; ============================================================
; 2x2 matrices over Z/pZ, stored flat as (a b c d) = [[a b],[c d]].
(define (m2mul p m1 m2)
  (let ((a1 (nth 0 m1)) (b1 (nth 1 m1)) (c1 (nth 2 m1)) (d1 (nth 3 m1))
        (a2 (nth 0 m2)) (b2 (nth 1 m2)) (c2 (nth 2 m2)) (d2 (nth 3 m2)))
    (list (mod (+ (* a1 a2) (* b1 c2)) p)
          (mod (+ (* a1 b2) (* b1 d2)) p)
          (mod (+ (* c1 a2) (* d1 c2)) p)
          (mod (+ (* c1 b2) (* d1 d2)) p))))
(define (m2det p m)
  (mod (- (* (nth 0 m) (nth 3 m)) (* (nth 1 m) (nth 2 m))) p))
(define m2id (list 1 0 0 1))
(define (m2pow p m k)
  (cond ((= k 0) m2id) (t (m2mul p m (m2pow p m (- k 1))))))
(define (all-m2 p)
  (foldr append nil
    (map (lambda (a)
      (foldr append nil
        (map (lambda (b)
          (foldr append nil
            (map (lambda (c)
              (map (lambda (d) (list a b c d)) (seq 0 (- p 1))))
              (seq 0 (- p 1)))))
          (seq 0 (- p 1)))))
      (seq 0 (- p 1)))))
(define (gl2 p) (filter (lambda (m) (/= (m2det p m) 0)) (all-m2 p)))

; integer powers, including negative ones (exact rationals)
(define (qpow b nn)
  (cond ((>= nn 0) (^ b nn)) (t (/ 1 (^ b (- 0 nn))))))

; ============================================================
; 1. ℓ-adic sheaf = projective system
; ============================================================
; Fix prime ℓ ≠ char(F_q) = p.
; An ℓ-adic sheaf F on X is a projective system {F_n}_{n≥1} where:
;   F_n is a constructible Z/ℓ^nZ-sheaf,
;   F_n ≅ F_{n+1} ⊗ Z/ℓ^nZ (compatibility).
;
; For a point Spec(F_q): the étale topos is the classifying topos of
; the absolute Galois group Γ = Gal(\bar{F_q}/F_q) ≅ \hat{Z}.
; An ℓ-adic sheaf on Spec(F_q) is a continuous representation
; ρ: Γ -> GL_d(Z_ℓ) (or Q_ℓ-vector space with Γ-action).

(print "--- 1. ℓ-adic sheaf definition ---")
(print "F = {F_n} projective system of Z/ℓ^nZ-sheaves.")
(print "On Spec(F_q): F corresponds to continuous Γ-action on Z_ℓ-module.")
; The content of "projective system" is the transition map
;   r_n: Z/l^{n+1}Z -> Z/l^nZ,  x |-> x mod l^n.
; It must be a surjective ring homomorphism with kernel of order l.
; Check all three by brute force over every element, for l = 2, 3 and n = 1,2.
(define (zmod nn) (seq 0 (- nn 1)))

(define (transition-is-ring-hom-p l nn)
  (let ((big (^ l (+ nn 1))) (small (^ l nn)))
    (every (lambda (x)
             (every (lambda (y)
                      (and (= (mod (mod (+ x y) big) small)
                              (mod (+ (mod x small) (mod y small)) small))
                           (= (mod (mod (* x y) big) small)
                              (mod (* (mod x small) (mod y small)) small))))
                    (zmod big)))
           (zmod big))))

(define (transition-is-surjective-p l nn)
  (let ((big (^ l (+ nn 1))) (small (^ l nn)))
    (every (lambda (z)
             (any (lambda (x) (= (mod x small) z)) (zmod big)))
           (zmod small))))

(define (transition-kernel-order l nn)
  (let ((big (^ l (+ nn 1))) (small (^ l nn)))
    (count (lambda (x) (= (mod x small) 0)) (zmod big))))

(check "l-adic-definition"
  (every (lambda (pr)
           (let ((l (nth 0 pr)) (nn (nth 1 pr)))
             (and (and (transition-is-ring-hom-p l nn)
                       (transition-is-surjective-p l nn))
                  (= (transition-kernel-order l nn) l))))
         (list (list 2 1) (list 2 2) (list 3 1))))

; Control: a projective system needs the RIGHT modulus at each level.
; Reducing Z/8Z mod 3 is not a map of the l=2 system: its kernel is not 2.
(check "wrong-modulus-is-not-a-transition-map"
  (/= (count (lambda (x) (= (mod x 3) 0)) (zmod 8)) 2))

; ============================================================
; 2. Smooth sheaf on Spec(F_q) = Frobenius module
; ============================================================
; For X = Spec(F_q) (0-dimensional), étale sheaf = discrete Γ-module.
; A smooth (= lisse) ℓ-adic sheaf on Spec(F_q) of rank r:
; Continuous homomorphism Γ = \hat{Z} -> GL_r(Z_ℓ).
;
; This is determined by the image of Frobenius Fr = 1 ∈ \hat{Z}:
; One matrix Φ ∈ GL_r(Z_ℓ).
; Conversely, any Φ determines a representation by Fr^k ↦ Φ^k.
;
; So: smooth ℓ-adic sheaf on Spec(F_q) of rank r
;     ⟺ conjugacy class in GL_r(Z_ℓ).
;
; For computations: work modulo ℓ^n. The Frobenius action mod ℓ^n
; is just Φ mod ℓ^n ∈ GL_r(Z/ℓ^nZ).

(print "--- 2. Smooth sheaf = Frobenius module ---")
(print "On Spec(F_q): smooth ℓ-adic sheaf ≅ Φ ∈ GL_r(Z_ℓ).")
(print "Concrete: work mod ℓ^n, Φ_n ∈ GL_r(Z/ℓ^nZ).")
; A rank-2 smooth sheaf mod l is a single matrix Phi in GL_2(Z/lZ), with the
; Galois action Fr^k |-> Phi^k.  Enumerate GL_2(F_2) by brute force and check
;   (i)  |GL_2(F_2)| = (2^2-1)(2^2-2) = 6,   |GL_2(F_3)| = 48,
;   (ii) it is closed under multiplication,
;   (iii) k |-> Phi^k really is a homomorphism: Phi^{j+k} = Phi^j Phi^k,
;   (iv) Phi^6 = I for every Phi, so the action factors through Z/6Z.
(define gl2-f2 (gl2 2))
(print (list (quote GL2-F2) (quote order) (length gl2-f2)))
(print (list (quote GL2-F3) (quote order) (length (gl2 3))))

(check "smooth-sheaf-frob-module"
  (and (and (= (length gl2-f2) (* (- 4 1) (- 4 2)))
            (= (length (gl2 3)) (* (- 9 1) (- 9 3))))
       (and (every (lambda (m1)
                     (every (lambda (m2) (contains (m2mul 2 m1 m2) gl2-f2))
                            gl2-f2))
                   gl2-f2)
            (every (lambda (m)
                     (and (every (lambda (j)
                                   (every (lambda (k)
                                            (equal (m2pow 2 m (+ j k))
                                                   (m2mul 2 (m2pow 2 m j) (m2pow 2 m k))))
                                          (seq 0 3)))
                                 (seq 0 3))
                          (equal (m2pow 2 m 6) m2id)))
                   gl2-f2))))

; Control: a singular matrix is not in GL_2 and does not give a Galois action
; (no power of it is the identity).
(check "singular-matrix-is-not-a-frobenius-module"
  (and (not (contains (list 1 1 1 1) gl2-f2))
       (not (any (lambda (k) (equal (m2pow 2 (list 1 1 1 1) k) m2id)) (seq 1 8)))))

; ============================================================
; 3. Tate twists Q_ℓ(n)
; ============================================================
; For n ∈ Z, the Tate twist Q_ℓ(n) on Spec(F_q) is the n-th tensor power of
; Q_ℓ(1) = (lim μ_{ℓ^k}) ⊗ Q_ℓ (negative n: of its dual).  The arithmetic
; Frobenius x ↦ x^q acts on μ_{ℓ^k} by ζ ↦ ζ^q, hence on Q_ℓ(1) by q.  The
; GEOMETRIC Frobenius Φ, its inverse and the one used in the Lefschetz trace
; formula, acts on Q_ℓ(n) by q^{-n}.  So twisting a sheaf by n multiplies its
; Frobenius eigenvalues by q^{-n}.
;
; L-function factor: For a smooth sheaf F with Frobenius Φ on stalk:
; det(1 - Φ·T | F)^{-1}.
; For Q_ℓ(n): Φ = q^{-n}, so factor = (1 - q^{-n} T)^{-1}.
;
; Example: Q_ℓ(0) on Spec(F_q) gives factor (1 - T)^{-1}.
; Q_ℓ(1) gives (1 - q^{-1} T)^{-1}, and Q_ℓ(-1) gives (1 - qT)^{-1}.
; H^2(P¹) ≅ Q_ℓ(-1): geometric Frobenius acts there by q.

(print "--- 3. Tate twists ---")
(print "Q_ℓ(n): Frobenius acts by q^n on stalk.")
(print "Q_ℓ(-1): Frob acts by q. (H^2 of P¹)")
(print "Q_ℓ(0) = constant sheaf: Frob acts by 1.")
; A Frobenius module is (rank weight eigenvalues).  Twisting by n divides
; every eigenvalue by q^n and drops the weight by 2n.
(define (fm-rank m)   (nth 0 m))
(define (fm-weight m) (nth 1 m))
(define (fm-eigs m)   (nth 2 m))
(define (tate-twist q m nn)
  (list (fm-rank m)
        (- (fm-weight m) (* 2 nn))
        (map (lambda (lam) (/ lam (qpow q nn))) (fm-eigs m))))

(define q5 5)
(define ql0 (list 1 0 (list 1)))          ; the constant sheaf Q_l = Q_l(0)

(print (list (quote Ql-minus-1) (tate-twist q5 ql0 -1)))
(print (list (quote Ql-plus-1)  (tate-twist q5 ql0 1)))

(check "tate-twists"
  (and
    ; twisting by n and then by -n is the identity
    (every (lambda (nn) (equal (tate-twist q5 (tate-twist q5 ql0 nn) (- 0 nn)) ql0))
           (list -2 -1 1 2 3))
    ; Q_l(-1) is exactly the Frobenius module of H^2(P^1): weight 2, eigenvalue q
    (and (= (fm-weight (tate-twist q5 ql0 -1)) 2)
         (equal (fm-eigs (tate-twist q5 ql0 -1)) (list q5)))))

; The twist must genuinely MOVE the weight -- otherwise the round-trip check
; above would be empty.  A nonzero twist always changes the weight by 2n.
(check "nonzero-tate-twist-changes-weight"
  (every (lambda (nn)
           (and (/= (fm-weight (tate-twist q5 ql0 nn)) (fm-weight ql0))
                (= (- (fm-weight ql0) (fm-weight (tate-twist q5 ql0 nn))) (* 2 nn))))
         (list -2 -1 1 2 3)))

; L-factor of a Tate twist: det(1 - Phi T | Q_l(n)) = 1 - q^{-n} T.
(define (l-factor-at m tv)
  (foldl (lambda (acc lam) (* acc (- 1 (* lam tv)))) 1 (fm-eigs m)))
(check "tate-twist-L-factor"
  (every (lambda (nn)
           (every (lambda (tv)
                    (= (l-factor-at (tate-twist q5 ql0 nn) tv)
                       (- 1 (* (qpow q5 (- 0 nn)) tv))))
                  (list 1 2 (/ 1 3))))
         (list -2 -1 0 1 2)))

; ============================================================
; 4. Smooth sheaf rank 1 on Spec(F_q): Kummer sheaf
; ============================================================
; For a character χ: F_q^× -> Q_ℓ^×, the Kummer sheaf L_χ is the rank-1
; smooth sheaf on G_m obtained from the Lang torsor x ↦ x^{q-1} (a finite
; etale cover G_m → G_m with group F_q^×) by pushing out along χ.
; Its stalk at a rational point a ∈ F_q^× is a rank-1 smooth sheaf on
; Spec(F_q), on which Frobenius acts by χ(a) (or χ(a)^{-1}, depending on
; the normalisation).
;
; For a rank-1 smooth sheaf on Spec(F_q) with Frobenius eigenvalue λ:
; L(T) = (1 - λ T)^{-1}.
;
; Example: constant sheaf Q_ℓ: λ = 1, L(T) = (1 - T)^{-1}.
; Quadratic character sheaf: λ = χ(a) = ±1.

(print "--- 4. Rank-1 smooth sheaves ---")
(print "Kummer sheaf L_χ: Frobenius eigenvalue = Gauss sum / χ-value.")
(print "L-function factor: (1 - λ T)^{-1} where λ = Frob eigenvalue.")
; Take chi = the quadratic (Legendre) character of F_5^x, computed by brute
; force: chi(a) = 1 if a is a square, -1 otherwise.  The stalk of the Kummer
; sheaf L_chi at a generator g of F_5^x is rank 1 with Frobenius eigenvalue
; lambda = chi(g).
(define (legendre a p)
  (cond ((= (mod a p) 0) 0)
        ((any (lambda (x) (= (mod (* x x) p) (mod a p))) (seq 1 (- p 1))) 1)
        (t -1)))
(define f5-units (seq 1 4))
(print (list (quote legendre-symbols-mod-5) (map (lambda (a) (legendre a 5)) f5-units)))

; multiplicative order of a mod p, by brute force
(define (mult-order a p)
  (find (lambda (k) (= (mod (^ a k) p) 1)) (seq 1 (- p 1))))

(check "rank-1-smooth-sheaf"
  (and
    ; chi is a character: chi(ab) = chi(a)chi(b) on every pair in F_5^x
    (every (lambda (a)
             (every (lambda (b)
                      (= (legendre (* a b) 5) (* (legendre a 5) (legendre b 5))))
                    f5-units))
           f5-units)
    ; it is nontrivial and of order 2, and 2 generates F_5^x
    (and (and (any (lambda (a) (= (legendre a 5) -1)) f5-units)
              (every (lambda (a) (= (* (legendre a 5) (legendre a 5)) 1)) f5-units))
         (= (mult-order 2 5) 4))))

; The L-factor of that stalk is (1 - lambda T)^{-1} with lambda = chi(2) = -1,
; so det(1 - Phi T) = 1 + T.  Compute lambda from the character, not by hand.
(define lchi (list 1 0 (list (legendre 2 5))))
(check "kummer-sheaf-L-factor-is-1-plus-T"
  (and (equal (fm-eigs lchi) (list -1))
       (every (lambda (tv) (= (l-factor-at lchi tv) (+ 1 tv)))
              (list 1 2 3 (/ 1 4)))))

; Control: the trivial character would give 1 - T, a different L-factor.
(check "trivial-character-has-different-L-factor"
  (/= (l-factor-at lchi 2) (l-factor-at (list 1 0 (list 1)) 2)))

; ============================================================
; 5. Computation: Frobenius matrix mod ℓ² for ℓ = 2
; ============================================================
; Let q = 5, ℓ = 2 (ℓ ≠ char F_5 = 5, OK).
; Smooth sheaf on Spec(F_5) of rank 2: pick Φ ∈ GL_2(Z_2).
; Reduce mod 4: |GL_2(Z/4Z)| = 2^4·|GL_2(F_2)| = 16·6 = 96 (the kernel of
; reduction mod 2 is {I + 2M}, of size 2^4; the field formula (q²-1)(q²-q)
; does not apply to Z/4Z).  Mod 2: GL_2(F_2) ≅ S_3, order 6.
;
; A specific choice: Φ = [[1,0],[0,1]] (identity).
; This is the trivial rank-2 sheaf (constant, Frob acts trivially).
; Trace(Frob) = 2. L-function factor: det(1 - Φ·T) = (1-T)².

(print "--- 5. Constant sheaf rank 2 ---")
(print "Φ = I_2, Tr(Φ) = 2.")
(print "det(1 - I_2·T) = (1-T)².")

; det(1 - I_2*val) = (1-val)^2 = 1 - 2*val + val^2, evaluated at val
(define (det-1-minus-I2 val)
  (+ 1 (* -2 val) (^ val 2)))

(print "(1-2)^2 = 1-4+4=") (print (det-1-minus-I2 2))
(check "det-const-rank2" (= (det-1-minus-I2 2) 1))

; Cross-check the hand-expanded formula against an honest 2x2 determinant of
; the matrix I - T*I = [[1-T, 0], [0, 1-T]], computed over the rationals.
(define (det2-rational m)
  (- (* (nth 0 m) (nth 3 m)) (* (nth 1 m) (nth 2 m))))
(check "det-1-minus-I2-agrees-with-2x2-determinant"
  (every (lambda (tv)
           (= (det-1-minus-I2 tv)
              (det2-rational (list (- 1 tv) 0 0 (- 1 tv)))))
         (list 0 1 2 3 (/ 1 2) -1)))

; ============================================================
; 6. Weil numbers and weights
; ============================================================
; For a smooth sheaf F on X/F_q that is pure of weight w:
; For each closed point x of degree d, eigenvalues α of Frob_x
; satisfy |α| = (q^d)^{w/2} (complex absolute value).
;
; For ℓ-adic sheaf on Spec(F_q) (X is a point!):
; Every eigenvalue of Frobenius Φ is a Weil number of some weight.
; The "point" Spec(F_q) has dimension 0, so its cohomology is H^0 only.
;
; More interesting: For a smooth proper variety X of dimension d,
; eigenvalues of Frob on H^i(X⊗\bar{F_q}, Q_ℓ) are q-Weil numbers
; of weight i (Deligne).

(print "--- 6. Purity and Weil numbers ---")
(print "For eigenvalue α of Φ on H^i: |α| = q^{i/2}.")
(print "Weight w sheaf: Frobenius eigenvalues have |α| = q^{w/2}.")
; Purity says |alpha|^2 = q^w.  Test it on H^1 of a real elliptic curve:
; count E: y^2 = x^3 + b over F_5 by BRUTE FORCE, form a = q + 1 - #E, and
; let alpha, beta be the roots of T^2 - aT + q.  Since a^2 - 4q < 0 the roots
; are complex conjugates, so |alpha|^2 = alpha*beta = q = q^1: weight 1.
(define (ec-point-count q aa bb)
  (+ 1 (count (lambda (p)
                (let ((x (nth 0 p)) (y (nth 1 p)))
                  (= (mod (- (* y y) (+ (* x x x) (* aa x) bb)) q) 0)))
              (foldr append nil
                (map (lambda (x) (map (lambda (y) (list x y)) (seq 0 (- q 1))))
                     (seq 0 (- q 1)))))))
(define (ec-trace q aa bb) (- (+ q 1) (ec-point-count q aa bb)))

; alpha = (a + sqrt(disc))/2 and beta = (a - sqrt(disc))/2 with disc = a^2-4q.
; Their product, expanded as (a^2 - disc)/4, is |alpha|^2 when disc < 0.
; Compute it from a and disc; purity of weight w asserts it equals q^w.
(define (root-product q aa)
  (let ((disc (- (* aa aa) (* 4 q))))
    (- (/ (* aa aa) 4) (/ disc 4))))
(define (purity-weight-p q aa w)
  (and (< (- (* aa aa) (* 4 q)) 0)      ; roots really are complex conjugates
       (= (root-product q aa) (^ q w))))

; Five smooth curves y^2 = x^3 + aa x + bb over F_5, given as (aa bb).
(define test-curves (list (list 0 1) (list 1 0) (list 1 1) (list 2 1) (list 3 2)))
(define (curve-trace c) (ec-trace 5 (nth 0 c) (nth 1 c)))
(print (list (quote E-traces-over-F5) (map curve-trace test-curves)))
(print (list (quote root-products) (map (lambda (c) (root-product 5 (curve-trace c)))
                                        test-curves)))

(check "purity-weights"
  (and
    ; every one of these curves has weight-1 H^1: |alpha|^2 = q^1
    (every (lambda (c) (purity-weight-p 5 (curve-trace c) 1)) test-curves)
    ; and none of them is pure of weight 0 or 2 -- the weight is pinned down
    (every (lambda (c)
             (and (not (purity-weight-p 5 (curve-trace c) 0))
                  (not (purity-weight-p 5 (curve-trace c) 2))))
           test-curves)))

; H^0 and H^2 of P^1 are the Tate twists Q_l(0) and Q_l(-1); their weights
; and eigenvalues must satisfy the same purity relation |lambda|^2 = q^w,
; with both numbers produced by the twisting machinery of section 3.
(check "purity-of-tate-twists"
  (every (lambda (pr)
           (let ((w (nth 0 pr)) (m (nth 1 pr)))
             (and (= (fm-weight m) w)
                  (= (* (nth 0 (fm-eigs m)) (nth 0 (fm-eigs m))) (qpow q5 w)))))
         (list (list 0 (tate-twist q5 ql0 0))
               (list 2 (tate-twist q5 ql0 -1))
               (list -2 (tate-twist q5 ql0 1)))))

; Purity is equivalent to the Hasse bound |a| <= 2 sqrt(q); verify a^2 <= 4q
; for every curve we counted, and note that a^2 = 4q would break conjugacy.
(check "hasse-bound-from-brute-force-counts"
  (every (lambda (c) (< (* (curve-trace c) (curve-trace c)) (* 4 5))) test-curves))

; Control: a would-be Frobenius with a = 5 violates the bound, so no rank-2
; smooth sheaf of weight 1 over F_5 with determinant q can have that trace.
(check "purity-rejects-trace-outside-hasse-bound"
  (not (purity-weight-p 5 5 1)))

(print "=== ℓ-adic Sheaves Done ===")
