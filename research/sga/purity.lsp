; ============================================================
; SGA — Deligne's Purity Theorem (Weil II)
; For smooth proper X/F_q: eigenvalues of Frob on H^i are
; q-Weil numbers of weight i.
; Verify for elliptic curves E/F_q: H¹ has weight 1 (|eigenvalues|=√q).
; ============================================================

(print "=== Deligne's Purity Theorem ===")

; ------------------------------------------------------------
; Machinery — F_{p^d} = Z/pZ[x]/(f) as coefficient lists LOW to HIGH.
; Taking f = x (degree 1) gives the prime field F_p itself, so ONE point-counting
; routine serves both F_q and F_{q^2}.
; ------------------------------------------------------------
(define (zn a m) (mod (+ (mod a m) m) m))
(define (zeros k) (cond ((<= k 0) nil) (t (cons 0 (zeros (- k 1))))))
(define (padto p dd) (append p (zeros (- dd (length p)))))
(define (coefat p i) (cond ((< i 0) 0) ((>= i (length p)) 0) (t (nth i p))))
(define (vsub a b m) (map (lambda (pr) (zn (- (car pr) (cadr pr)) m)) (zip a b)))
(define (vadd a b m) (map (lambda (pr) (zn (+ (car pr) (cadr pr)) m)) (zip a b)))
(define (scale-poly c p m) (map (lambda (co) (zn (* c co) m)) p))
(define (poly-mul a b m)
  (map (lambda (k)
         (zn (foldl + 0 (map (lambda (i) (* (coefat a i) (coefat b (- k i)))) (seq 0 k))) m))
       (seq 0 (- (+ (length a) (length b)) 2))))
(define (poly-reduce p f m)
  (cond ((< (length p) (length f)) p)
        (t (let ((cc (last p)) (shift (- (length p) (length f))))
             (poly-reduce (butlast (vsub p (append (zeros shift) (scale-poly cc f m)) m)) f m)))))
(define (fdeg f) (- (length f) 1))
(define (fzero f) (zeros (fdeg f)))
(define (fmul a b f m) (padto (poly-reduce (poly-mul a b m) f m) (fdeg f)))
(define (felts dd m)
  (cond ((= dd 0) (list nil))
        (t (foldr append nil
                  (map (lambda (v) (map (lambda (c) (cons c v)) (seq 0 (- m 1))))
                       (felts (- dd 1) m))))))
(define (femb c f m) (padto (list (zn c m)) (fdeg f)))
(define (fcard f m) (^ m (fdeg f)))

; ------------------------------------------------------------
; BRUTE-FORCE POINT COUNTING on E: y² = x³ + a x + b.
; For each x, the number of y with y² = rhs is 1 if rhs = 0, 2 if rhs is a
; nonzero square, 0 otherwise. Plus the single point at infinity.
; ------------------------------------------------------------
(define (squares-of f m) (dedup (map (lambda (y) (fmul y y f m)) (felts (fdeg f) m))))
(define (ec-count ai bi f m)
  (let ((sqs (squares-of f m)) (pts (felts (fdeg f) m)))
    (+ 1 (foldl + 0
          (map (lambda (x)
                 (let ((rhs (vadd (vadd (fmul x (fmul x x f m) f m)
                                        (fmul (femb ai f m) x f m) m)
                                  (femb bi f m) m)))
                   (cond ((equal rhs (fzero f)) 1)
                         ((contains rhs sqs) 2)
                         (t 0))))
               pts)))))
(define (nonsingular-p ai bi p) (/= (zn (+ (* 4 (^ ai 3)) (* 27 (^ bi 2))) p) 0))
(define (a-trace ai bi f m) (- (+ (fcard f m) 1) (ec-count ai bi f m)))

; the prime fields, and quadratic extensions given by irreducible quadratics
(define fp5 (list 0 1))          ; F_5   = F_5[x]/(x)
(define fp7 (list 0 1))          ; F_7   = F_7[x]/(x)
(define f25 (list 2 0 1))        ; F_25  = F_5[x]/(x²+2)
(define f49 (list 1 0 1))        ; F_49  = F_7[x]/(x²+1)
; confirm those quadratics really are irreducible (no root in the prime field)
(define (quad-has-root c1 c0 p)
  (any (lambda (x) (= (zn (+ (^ x 2) (+ (* c1 x) c0)) p) 0)) (seq 0 (- p 1))))
(check "F25-and-F49-moduli-are-irreducible"
       (and (not (quad-has-root 0 2 5)) (not (quad-has-root 0 1 7))
            (= (fcard f25 5) 25) (= (fcard f49 7) 49)))

; ============================================================
; 1. Purity: statement
; ============================================================
; X: smooth proper variety over F_q, dimension d.
; H^i(X⊗\bar{F_q}, Q_ℓ): ℓ-adic cohomology.
; Frobenius F acts on H^i.
;
; Deligne (Weil II): For each i, every eigenvalue α of F on H^i is
; an algebraic integer whose complex absolute value under every
; embedding is q^{i/2}.
;
; Consequence: The characteristic polynomial P_i(T) = det(1 - F·T | H^i)
; has roots α^{-1} with |α^{-1}| = q^{-i/2}, i.e., |α| = q^{i/2} for
; the zeros of P_i(1/α) = 0.
;
; For i=0: eigenvalues = 1, weight 0. ✓
; For i=2d: eigenvalues = q^d, weight 2d. ✓ (Poincaré dual to H^0).

(print "--- 1. Purity theorem statement ---")
(print "For smooth proper X/F_q: eigenvalues of Frob on H^i")
(print "  are algebraic integers with |α| = q^{i/2} in all embeddings.")

; For a curve, purity in weight 1 says the two eigenvalues α, β on H¹ satisfy
; |α| = |β| = √q. Since α+β = a and αβ = q are real, that is EXACTLY the
; assertion a² ≤ 4q (the roots are complex conjugates of modulus √q).
; So brute-force count the points of EVERY nonsingular Weierstrass curve over
; F_5 and over F_7, read off a = q + 1 - #E(F_q), and test a² ≤ 4q.
; This is falsifiable: a wrong point count breaks it.
(define (all-curves p)
  (filter (lambda (pr) (nonsingular-p (car pr) (cadr pr) p))
          (foldr append nil
                 (map (lambda (ai) (map (lambda (bi) (list ai bi)) (seq 0 (- p 1))))
                      (seq 0 (- p 1))))))
(define (traces-over p f) (map (lambda (pr) (a-trace (car pr) (cadr pr) f p)) (all-curves p)))
(define traces5 (traces-over 5 fp5))
(define traces7 (traces-over 7 fp7))
(print "number of nonsingular curves over F_5, F_7:")
(print (list (length (all-curves 5)) (length (all-curves 7))))
(print "Frobenius traces a over F_5:") (print (sort < (dedup traces5)))
(print "Frobenius traces a over F_7:") (print (sort < (dedup traces7)))
(check "purity-statement"
       (and (every (lambda (a) (<= (* a a) (* 4 5))) traces5)
            (every (lambda (a) (<= (* a a) (* 4 7))) traces7)
            ; the bound is not vacuous — some curve very nearly attains it
            (any (lambda (a) (> (* a a) 5)) traces5)
            (any (lambda (a) (> (* a a) 7)) traces7)
            ; a structural cross-check on the counting itself: the quadratic
            ; twist by the non-residue d=2 (y² = x³ + a d² x + b d³) must have
            ; trace exactly -a. Two independent point counts per curve.
            (every (lambda (pr)
                     (= (a-trace (zn (* 4 (car pr)) 5) (zn (* 3 (cadr pr)) 5) fp5 5)
                        (- 0 (a-trace (car pr) (cadr pr) fp5 5))))
                   (all-curves 5))))

; ============================================================
; 2. Verify for elliptic curves: H¹ has weight 1
; ============================================================
; For elliptic curve E/F_q:
; H¹(E, Q_ℓ) is 2-dimensional over Q_ℓ.
; Frobenius F has characteristic polynomial: T² - a_q T + q.
; (where a_q = q+1 - #E(F_q) = trace of Frob).
;
; Eigenvalues α, β are roots of T² - a_q T + q = 0.
; α + β = a_q, α·β = q.
; From Hasse: |a_q| ≤ 2√q, so |α| = |β| = √q. (Weight 1!)
;
; Verification for concrete curves:

(print "--- 2. Weight 1 for H¹ of elliptic curves ---")

; Example 1: E: y² = x³ + x over F_5
; #E(F_5) = 4, a_5 = 5+1-4 = 2.
; Char poly: T² - 2T + 5.
; Roots: T = 1 ± 2i. |T| = √(1+4) = √5 = √q. ✓ (weight 1)
(print "E: y²=x³+x/F_5: a_5=2, |roots|=√5. Weight 1. ✓")

; Example 2: E: y² = x³ + x + 1 over F_5
; #E(F_5) = 9, a_5 = -3.
; Char poly: T² + 3T + 5.
; Roots: T = (-3 ± i√11)/2.
; Product: ( (-3)² + 11 )/4 = (9+11)/4 = 5 = q. ✓
; Sum: -3 = a_q. ✓
; |roots| = √(9+11)/2 = √20/2 = √5 = √q. ✓

(print "E: y²=x³+x+1/F_5: a_5=-3, |roots|=√5. Weight 1. ✓")

; Example 3: E: y² = x³ + 1 over F_7
; Brute-force counting gives #E(F_7) = 12, so a_7 = 7+1-12 = -4.
; (An earlier draft of this file claimed #E(F_7) = 6 and a_7 = 2; that is wrong —
;  the affine solutions are x=0,1,2,4 with two y each and x=3,5,6 with one y each,
;  11 affine points plus the point at infinity.)
; Char poly: T² + 4T + 7. Roots: T = -2 ± i√3. |roots| = √(4+3) = √7 = √q. ✓

(print "E: y²=x³+1/F_7: a_7=-4, |roots|=√7. Weight 1. ✓")

; THE REAL TEST. Purity plus the Lefschetz trace formula pin down the point
; count over EVERY extension F_{q^r}, not just F_q:
;   #E(F_{q^r}) = q^r + 1 - (α^r + β^r),
; and for r=2, α²+β² = (α+β)² - 2αβ = a² - 2q.
; So a alone — obtained from counting over F_q — PREDICTS the count over F_{q²}.
; Count over F_{q²} independently, by brute force in F_25 and F_49, and compare.
(define (n2-predicted ai bi f m)
  (let ((a (a-trace ai bi f m)) (q (fcard f m)))
    (- (+ (* q q) 1) (- (* a a) (* 2 q)))))
; each entry is (a b base-field p quadratic-extension):
;   y²=x³+x /F_5 ; y²=x³+x+1 /F_5 ; y²=x³+1 /F_7
(define named-curves2 (list (list 1 0 fp5 5 f25) (list 1 1 fp5 5 f25) (list 0 1 fp7 7 f49)))
(define (n1-of cv) (ec-count (nth 0 cv) (nth 1 cv) (nth 2 cv) (nth 3 cv)))
(define (a-of cv) (a-trace (nth 0 cv) (nth 1 cv) (nth 2 cv) (nth 3 cv)))
(define (n2-brute cv) (ec-count (nth 0 cv) (nth 1 cv) (nth 4 cv) (nth 3 cv)))
(define (n2-pred cv) (n2-predicted (nth 0 cv) (nth 1 cv) (nth 2 cv) (nth 3 cv)))
(print "brute-force #E(F_q) for the three curves:") (print (map n1-of named-curves2))
(print "traces a:") (print (map a-of named-curves2))
(print "predicted #E(F_q²) from a:") (print (map n2-pred named-curves2))
(print "brute-force #E(F_q²):") (print (map n2-brute named-curves2))
(check "purity-elliptic-curves"
       (and (equal (map n1-of named-curves2) (list 4 9 12))
            (equal (map a-of named-curves2) (list 2 -3 -4))
            ; the Lefschetz/purity prediction over F_{q²} matches brute force
            (equal (map n2-pred named-curves2) (map n2-brute named-curves2))
            ; weight 1: |α| = √q, i.e. a² ≤ 4q, for each of them
            (every (lambda (cv) (<= (* (a-of cv) (a-of cv)) (* 4 (fcard (nth 2 cv) (nth 3 cv)))))
                   named-curves2)))

; ============================================================
; 3. SISP: compute eigenvalue magnitudes numerically
; ============================================================
; For E/F_5: y² = x³ + x.
; a_5 = 2, q = 5.
; |eigenvalue|² = product = q = 5.
; For weight 1 we need |α| = √q.

; NOTE: the identity a²/4 + (4q - a²)/4 = q holds for ANY a and q, so it verifies
; nothing on its own. The content of purity is that the discriminant 4q - a² is
; NON-NEGATIVE, i.e. α and β really are a complex-conjugate pair of modulus √q
; rather than two real numbers of different sizes. That is a genuine constraint
; on the point count, and it is what is tested below.
(define (purity-verify a q) (>= (- (* 4 q) (* a a)) 0))

(print "--- 3. Numerical purity verification ---")
(print "4q - a² (must be >= 0) for the three curves:")
(print (map (lambda (cv) (- (* 4 (fcard (nth 2 cv) (nth 3 cv))) (* (a-of cv) (a-of cv))))
            named-curves2))
; Each check recomputes a from an honest point count, then tests purity on it.
(check "purity-E1-F5" (and (= (a-of (nth 0 named-curves2)) 2)
                           (purity-verify (a-of (nth 0 named-curves2)) 5)))
(check "purity-E2-F5" (and (= (a-of (nth 1 named-curves2)) -3)
                           (purity-verify (a-of (nth 1 named-curves2)) 5)))
(check "purity-E3-F7" (and (= (a-of (nth 2 named-curves2)) -4)
                           (purity-verify (a-of (nth 2 named-curves2)) 7)))
; and the test can fail: a hypothetical curve over F_5 with trace 5 would violate
; purity, and purity-verify says so.
(check "purity-verify-rejects-impossible-trace" (not (purity-verify 5 5)))

; ============================================================
; 4. Weight structure for P¹
; ============================================================
; H⁰(P¹) = Q_ℓ: eigenvalue 1 (weight 0). |1| = q^{0/2} = 1. ✓
; H¹(P¹) = 0: (weight 1 trivially).
; H²(P¹) = Q_ℓ(-1): eigenvalue q (weight 2). |q| = q^{2/2} = q. ✓
;
; These match the purity theorem: H^i has weight i.

(print "--- 4. Purity for P¹ ---")
(print "H^0: weight 0, H^2: weight 2. ✓")

; The weight decomposition of P¹ is completely pinned down by point counts:
; Lefschetz gives #P¹(F_{q^r}) = tr(F^r|H⁰) - tr(F^r|H¹) + tr(F^r|H²)
;                              = 1 - 0 + q^r.
; Count P¹ by brute force instead — one point per field element plus the point
; at infinity — over F_q AND over F_{q²}, and compare.
(define (p1-count f m) (+ 1 (length (felts (fdeg f) m))))
(print "brute-force #P¹ over F_5, F_25, F_7, F_49:")
(print (list (p1-count fp5 5) (p1-count f25 5) (p1-count fp7 7) (p1-count f49 7)))
(check "purity-P1"
       (and (= (p1-count fp5 5) (+ 1 5))          ; weight 0 gives 1, weight 2 gives q
            (= (p1-count f25 5) (+ 1 (* 5 5)))    ; over F_{q²}: 1 + q²
            (= (p1-count fp7 7) (+ 1 7))
            (= (p1-count f49 7) (+ 1 (* 7 7)))
            ; H¹(P¹) = 0: no weight-1 term. If H¹ had dimension h with
            ; eigenvalues of modulus √q, the counts would deviate from 1 + q^r
            ; by a nonzero amount for some r; they do not.
            (= (- (p1-count f25 5) 1) (* (- (p1-count fp5 5) 1) (- (p1-count fp5 5) 1)))))

; ============================================================
; 5. Purity for a product: X × Y
; ============================================================
; Kunneth: H^k(X×Y) = ⊕_{i+j=k} H^i(X) ⊗ H^j(Y).
; Frobenius eigenvalues = products of eigenvalues from factors.
; If H^i(X) has weight i and H^j(Y) has weight j,
; then H^k(X×Y) has eigenvalues of weight i+j = k. ✓
;
; Example: E × E over F_5:
; H^2(E×E) contains H^1(E)⊗H^1(E) of weight 1+1=2.
; This has dimension 4, eigenvalues are α_i·β_j where α_i,β_j
; are eigenvalues on H¹(E). Each |α_i·β_j| = √q·√q = q = q^{2/2}. ✓

(print "--- 5. Purity for products (Kunneth) ---")
(print "E×E: H¹⊗H¹ contributes to H², weight 1+1=2. ✓")

; Künneth on the level Frobenius traces: the eigenvalues on H^*(X×Y) are the
; products of eigenvalues, hence for every r
;     #(X×Y)(F_{q^r}) = #X(F_{q^r}) · #Y(F_{q^r}).
; Test that for X = E, Y = P¹, at r = 1 and r = 2, with the E-counts coming from
; brute force and the P¹-counts from brute force too.
; Then test the weight-2 statement for E×E: the eigenvalues on H¹(E)⊗H¹(E) are
; α², αβ = q, βα = q, β², all of modulus q = q^{2/2}. For α², β² that says
; the pair (α², β²) is again conjugate of modulus q, i.e. their sum a² - 2q
; satisfies (a² - 2q)² ≤ 4q² — a real inequality on the brute-forced a.
; α^r + β^r by Newton's recurrence from the characteristic polynomial
; T² - aT + q, using nothing but a — no point counting.
(define (trace-r a q r)
  (cond ((= r 0) 2)
        ((= r 1) a)
        (t (- (* a (trace-r a q (- r 1))) (* q (trace-r a q (- r 2)))))))
; LEFT ROUTE: assemble Σ(-1)^k tr(F^r | H^k(E×P¹)) piece by graded piece —
;   H⁰⊗H⁰ : 1            (degree 0)
;   H¹⊗H⁰ : α^r+β^r      (degree 1, subtracted)
;   H²⊗H⁰ : q^r          (degree 2)
;   H⁰⊗H² : q^r          (degree 2)
;   H¹⊗H² : (α^r+β^r)q^r (degree 3, subtracted)
;   H²⊗H² : q^{2r}       (degree 4)
(define (kunneth-alt-sum a q r)
  (let ((tr (trace-r a q r)) (qr (^ q r)))
    (+ (- 1 tr) (+ qr (+ qr (+ (- 0 (* tr qr)) (* qr qr)))))))
; RIGHT ROUTE: the product of two honest brute-force point counts.
(define (kunneth-ok cv)
  (let ((a (a-of cv)) (q (fcard (nth 2 cv) (nth 3 cv))))
    (and (= (kunneth-alt-sum a q 1) (* (n1-of cv) (p1-count (nth 2 cv) (nth 3 cv))))
         (= (kunneth-alt-sum a q 2) (* (n2-brute cv) (p1-count (nth 4 cv) (nth 3 cv)))))))
(define (weight2-ok cv)
  (let ((a (a-of cv)) (q (fcard (nth 2 cv) (nth 3 cv))))
    (<= (* (- (* a a) (* 2 q)) (- (* a a) (* 2 q))) (* 4 (* q q)))))
(print "weight-2 traces α²+β² = a²-2q for the three curves:")
(print (map (lambda (cv) (- (* (a-of cv) (a-of cv)) (* 2 (fcard (nth 2 cv) (nth 3 cv)))))
            named-curves2))
(check "purity-product-kunneth"
       (and (every kunneth-ok named-curves2)
            (every weight2-ok named-curves2)
            ; and the weight-2 bound is a real constraint: a hypothetical
            ; α²+β² of size 3q would violate it
            (not (<= (* (* 3 5) (* 3 5)) (* 4 (* 5 5))))))

(print "=== Purity Theorem Done ===")
