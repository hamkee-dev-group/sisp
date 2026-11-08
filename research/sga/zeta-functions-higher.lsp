; ============================================================
; SGA — Zeta Functions of Higher-Dimensional Varieties
; For X/F_q of dimension d:
; Z(X,T) = ∏_{i=0}^{2d} P_i(T)^{(-1)^{i+1}}
; where P_i(T) = det(1 - Frob·T | H^i_c(X⊗\bar{F_q}, Q_ℓ)).
; Compute Z(P²(F_2), T) = 1/((1-T)(1-2T)(1-4T)).
; Verify functional equation.
; ============================================================

(print "=== Zeta Functions of Higher-Dimensional Varieties ===")
(define (pow b e) (cond ((= e 0) 1) (t (* b (pow b (- e 1))))))

; ------------------------------------------------------------
; ROUTE A — brute force. Enumerate P^n(F_{q^m}) point by point.
; A point of P^n is a nonzero (n+1)-tuple up to scaling; the
; canonical representative is the one whose FIRST NONZERO
; coordinate equals 1. So we list every tuple over the field and
; keep the canonical ones. No formula for #P^n is used.
; Field elements of F_{q^m} are coefficient lists of length m
; over Z/qZ; for counting we only need zero, one and equality.
; ------------------------------------------------------------

(define (fq-elts q m)
  (cond ((= m 0) (list nil))
        (t (foldr append nil
             (map (lambda (v) (map (lambda (c) (cons c v)) (seq 0 (- q 1))))
                  (fq-elts q (- m 1)))))))
(define (fq-zero m) (map (lambda (i) 0) (seq 1 m)))
(define (fq-one m) (cons 1 (map (lambda (i) 0) (seq 2 m))))

(define (tuples els k)
  (cond ((= k 0) (list nil))
        (t (foldr append nil
             (map (lambda (v) (map (lambda (e) (cons e v)) els))
                  (tuples els (- k 1)))))))

(define (canonical-p vv zz oo)
  (let ((nz (filter (lambda (e) (not (equal e zz))) vv)))
    (cond ((eq nz nil) nil) (t (equal (car nz) oo)))))

(define (proj-count q m n)            ; #P^n(F_{q^m})
  (let* ((els (fq-elts q m)) (zz (fq-zero m)) (oo (fq-one m)))
    (count (lambda (vv) (canonical-p vv zz oo)) (tuples els (+ n 1)))))

; ------------------------------------------------------------
; ROUTE B — cohomology. The zeta function of P^n is the
; alternating product of the P_i(T) = det(1 - Frob T | H^i).
; For P^n only even degrees survive, H^{2i} = Q_l(-i), b_{2i} = 1,
; so Frob acts by the single eigenvalue q^i and
;   Z(P^n, T) = 1 / prod_{i=0}^{n} (1 - q^i T).
; We expand that product as a power series in T by actual
; polynomial arithmetic, then recover the point counts N_k from
; the series via  k z_k = sum_{m=1..k} N_m z_{k-m}
; (the coefficient form of T Z'/Z = sum N_k T^k).
; ------------------------------------------------------------

(define (frob-eigenvalues q n) (map (lambda (i) (^ q i)) (seq 0 n)))

; multiply a coefficient list by the geometric series 1/(1-cT):
; w_0 = z_0, w_k = z_k + c*w_{k-1}
(define (mul-geom coeffs c)
  (reverse (foldl (lambda (acc x)
                    (cons (+ x (* c (cond ((eq acc nil) 0) (t (car acc))))) acc))
                  nil coeffs)))
(define (zseries cs nmax)
  (foldl mul-geom (cons 1 (map (lambda (i) 0) (seq 1 nmax))) cs))

; acc holds (N_{k-1} ... N_1); pair it against z_1 ... z_{k-1}
(define (ext-go zs nmax k acc)
  (cond ((> k nmax) (reverse acc))
        (t (let ((sm (foldl + 0
                       (map (lambda (pr) (* (nth 0 pr) (nth 1 pr)))
                            (zip acc (map (lambda (j) (nth j zs)) (seq 1 (- k 1))))))))
             (ext-go zs nmax (+ k 1) (cons (- (* k (nth k zs)) sm) acc))))))

(define (counts-from-zeta q n nmax)
  (ext-go (zseries (frob-eigenvalues q n) nmax) nmax 1 nil))

; ============================================================
; 1. Zeta function definition for varieties of dimension d
; ============================================================
; For X/F_q smooth proper of dimension d:
; Z(X, T) = exp(Σ_{n≥1} #X(F_{q^n}) T^n / n).
;
; Grothendieck's cohomological formula:
; Z(X, T) = ∏_{i=0}^{2d} det(1 - Frob·T | H^i(X⊗\bar{F_q}, Q_ℓ))^{(-1)^{i+1}}
;         = P_1·P_3·... / (P_0·P_2·...)
;
; P_i(T) = det(1 - Frob·T | H^i).
; Degrees: deg P_i = dim H^i (Betti number b_i).

(print "--- 1. Zeta function definition ---")
(print "Z(X,T) = P_1 P_3 P_5 ... / (P_0 P_2 P_4 ...)")
(print "P_i(T) = det(1 - Frob·T | H^i).")

; The definition is only worth anything if the cohomological product
; really does reproduce exp(Σ N_n T^n / n). Test exactly that for P²/F_2:
; take the eigenvalue list from cohomology, expand the product as a power
; series, pull N_1,N_2,N_3 back out of the series, and compare against
; points counted one at a time in P²(F_2), P²(F_4), P²(F_8).

(print "Frobenius eigenvalues on H^0,H^2,H^4 of P²/F_2:")
(print (frob-eigenvalues 2 2))
(print "power series of Z(P²/F_2, T) to T^4:") (print (zseries (frob-eigenvalues 2 2) 4))
(print "N_1..N_3 extracted from that series:") (print (counts-from-zeta 2 2 3))
(print "N_1..N_3 by brute-force enumeration of P²(F_2), P²(F_4), P²(F_8):")
(print (list (proj-count 2 1 2) (proj-count 2 2 2) (proj-count 2 3 2)))

(check "zeta-definition"
       (equal (counts-from-zeta 2 2 3)
              (list (proj-count 2 1 2) (proj-count 2 2 2) (proj-count 2 3 2))))
(check "zeta-definition-P1-over-F3"
       (equal (counts-from-zeta 3 1 2)
              (list (proj-count 3 1 1) (proj-count 3 2 1))))
; and it is discriminating: dropping H^4 from the product changes the answer
(check "zeta-definition-needs-all-cohomology"
       (not (equal (counts-from-zeta 2 2 3) (counts-from-zeta 2 1 3))))

; ============================================================
; 2. Z(P²(F_q), T) computation
; ============================================================
; H⁰(P²) = Q_ℓ, dim 1. Frob acts as 1. P_0 = 1 - T.
; H¹(P²) = 0. P_1 = 1.
; H²(P²) = Q_ℓ(-1), dim 1. Frob acts as q. P_2 = 1 - qT.
; H³(P²) = 0. P_3 = 1.
; H⁴(P²) = Q_ℓ(-2), dim 1. Frob acts as q². P_4 = 1 - q²T.
;
; Z(P², T) = P_1 P_3 / (P_0 P_2 P_4)
;          = 1·1 / ((1-T)·(1-qT)·(1-q²T))
;          = 1 / ((1-T)(1-qT)(1-q²T)).
;
; For q=2: Z(P²(F_2), T) = 1 / ((1-T)(1-2T)(1-4T)).

(print "--- 2. Z(P², T) ---")
(print "H^0: P_0 = 1-T")
(print "H^2: P_2 = 1-qT")
(print "H^4: P_4 = 1-q^2T")
(print "Z(P²,T) = 1/((1-T)(1-qT)(1-q²T))")

; The formula asserts precisely that the H^{2i} contribute eigenvalues
; q^i and nothing else contributes. Its first consequence is
; #P^n(F_q) = 1 + q + ... + q^n, the trace of Frobenius summed over the
; even cohomology. Compare that sum against the point-by-point count
; for a spread of q, m and n — a single wrong Betti number breaks it.

(define (poincare-count q m n)
  (foldl + 0 (map (lambda (i) (^ (^ q m) i)) (seq 0 n))))

(define zeta-cases
  (list (list 2 1 2) (list 2 2 2) (list 2 3 2) (list 3 1 2) (list 5 1 2)
        (list 2 1 1) (list 3 2 1) (list 2 1 3) (list 2 2 3) (list 3 1 3)))

(print "brute-force #P^n(F_{q^m}) for (q m n) in")
(print zeta-cases)
(print (map (lambda (cs) (proj-count (nth 0 cs) (nth 1 cs) (nth 2 cs))) zeta-cases))
(print "trace of Frobenius over H^0,H^2,...,H^{2n}:")
(print (map (lambda (cs) (poincare-count (nth 0 cs) (nth 1 cs) (nth 2 cs))) zeta-cases))

(check "zeta-P2-formula"
       (every (lambda (cs)
                (let ((q (nth 0 cs)) (m (nth 1 cs)) (nn (nth 2 cs)))
                  (= (proj-count q m nn) (poincare-count q m nn))))
              zeta-cases))
; the cases really are distinct, so the quantifier above is not vacuous
(check "zeta-P2-formula-cases-distinct"
       (> (length (dedup (map (lambda (cs)
                                (proj-count (nth 0 cs) (nth 1 cs) (nth 2 cs)))
                              zeta-cases)))
          5))

; ============================================================
; 3. Compute #P²(F_q) from zeta function
; ============================================================
; Expand Z(P², T) = 1 / ((1-T)(1-qT)(1-q²T)) as power series.
; log Z(P², T) = Σ_{n≥1} #P²(F_{q^n}) T^n / n.
;
; log Z = -log(1-T) - log(1-qT) - log(1-q²T)
;        = Σ_{n≥1} T^n/n + Σ_{n≥1} q^n T^n/n + Σ_{n≥1} q^{2n} T^n/n
;        = Σ_{n≥1} (1 + q^n + q^{2n}) T^n / n.
;
; So: #P²(F_{q^n}) = 1 + q^n + q^{2n}.
;
; Check direct count for P²(F_q): points in projective plane over F_q.
; P²(F_q) = {(x:y:z) | (x,y,z) ≠ (0,0,0)} / F_q^×.
; Count: (q³ - 1)/(q-1) = q² + q + 1. ✓ (matches 1 + q + q²).

(define (P2-count q n)
  (+ 1 (^ q n) (^ q (* 2 n))))

(print "--- 3. Point counts from zeta ---")
(print "q=2, n=1: #P²(F_2) =") (print (P2-count 2 1))
(print "  Direct: (2³-1)/(2-1) = 7. ✓")
(print "q=2, n=2: #P²(F_4) =") (print (P2-count 2 2))
(print "q=3, n=1: #P²(F_3) =") (print (P2-count 3 1))
(print "  Direct: (27-1)/2 = 13. ✓")
(check "P2-F2-count" (= (P2-count 2 1) 7))
(check "P2-F2-count-n2" (= (P2-count 2 2) 21))
(check "P2-F3-count" (= (P2-count 3 1) 13))

; ============================================================
; 4. Functional equation for smooth proper varieties
; ============================================================
; Weil conjectures (proved by Deligne):
; Z(X, 1/(q^d T)) = ± q^{d χ/2} T^χ Z(X, T)
; where χ = Σ (-1)^i b_i is the Euler characteristic.
;
; For P²: d=2, b_0=b_2=b_4=1, b_1=b_3=0. χ = 1+0+1+0+1 = 3.
; For q=2: q^{d·χ/2} = 2^{2·3/2} = 2³ = 8.
; Direct computation, with Z(P²,T) = 1/((1-T)(1-2T)(1-4T)):
; Z(P², 1/4T) = 1/((1-1/4T)(1-2/4T)(1-4/4T))
;              = 1/((1 - 1/4T)(1 - 1/2T)(1 - 1/T))
; Multiply numerator and denominator by 4T·2T·T = 8T³:
;              = 8T³ / ((4T-1)(2T-1)(T-1))
;              = -8T³ / ((1-4T)(1-2T)(1-T))
;              = -8T³ Z(P², T).
; So Z(P², 1/4T) = -8T³ Z(P², T): the factor is q^{d·χ/2} = 8, with sign -1.
; The sign comes from the middle cohomology: H² carries the eigenvalue
; q = q^{d/2} once, giving (-1)^1.  For P^n in general it is (-1)^{n+1}.

(print "--- 4. Functional equation for P² ---")
(print "d=2, χ=3, q=2. q^{d·χ/2} = 2^3 = 8.")
(print "Z(P², 1/4T) = -8 T³ Z(P², T). ✓")

; Z is a rational function, and SISP divides exactly, so just evaluate
; both sides at concrete rational T and demand equality.
; Z(P^n, T) = 1 / prod_{i=0}^{n} (1 - q^i T).
(define (zeta-pn q nn tt)
  (/ 1 (foldl * 1 (map (lambda (i) (- 1 (* (^ q i) tt))) (seq 0 nn)))))

; χ = n+1 (one class in each even degree 0,2,...,2n), d = n.
; Weil: Z(X, 1/(q^d T)) = ± q^{d·χ/2} T^χ Z(X, T).
(define (chi-pn nn) (+ nn 1))
(define (fe-factor q nn)
  ; sign (-1)^{n+1} times q^{n(n+1)/2}
  (* (^ -1 (+ nn 1)) (^ q (truncate (/ (* nn (+ nn 1)) 2)))))

; Poles of Z(P^n,T) sit at T = q^{-i}, and T -> 1/(q^n T) permutes them,
; so drop any test value that lands on one — on either side.
(define fe-raw (list (/ 1 3) (/ 2 3) (/ 3 7) (/ 5 7) (/ 1 11) 3 5 (/ 7 2)))

(define (fe-safe q nn tt)
  (and (every (lambda (i) (/= (* (^ q i) tt) 1)) (seq 0 nn))
       (every (lambda (i) (/= (* (^ q i) (/ 1 (* (^ q nn) tt))) 1)) (seq 0 nn))))

(define (fe-tvals q nn) (filter (lambda (tt) (fe-safe q nn tt)) fe-raw))

(define (fe-pn-holds q nn)
  (every (lambda (tt)
           (= (zeta-pn q nn (/ 1 (* (^ q nn) tt)))
              (* (fe-factor q nn) (^ tt (chi-pn nn)) (zeta-pn q nn tt))))
         (fe-tvals q nn)))

(print "χ(P²) = 3, factor for q=2 (expect -8):") (print (fe-factor 2 2))
(print "Z(P²/F_2, 1/3) =") (print (zeta-pn 2 2 (/ 1 3)))
(print "Z(P²/F_2, 1/(4·(1/3))) = Z(P²/F_2, 3/4) =") (print (zeta-pn 2 2 (/ 3 4)))
(print "-8·(1/3)³·Z(P²/F_2,1/3) =") (print (* -8 (/ 1 27) (zeta-pn 2 2 (/ 1 3))))

(check "functional-equation-P2" (fe-pn-holds 2 2))
(check "functional-equation-P2-q3" (fe-pn-holds 3 2))
(check "functional-equation-P2-q5" (fe-pn-holds 5 2))
(check "functional-equation-P1" (fe-pn-holds 2 1))
(check "functional-equation-P3" (fe-pn-holds 2 3))
(check "functional-equation-factor-for-P2-F2-is-minus-8" (= (fe-factor 2 2) -8))
; the sign matters: without it the identity fails
(check "functional-equation-sign-is-needed"
       (not (every (lambda (tt)
                     (= (zeta-pn 2 2 (/ 1 (* 4 tt)))
                        (* 8 (^ tt 3) (zeta-pn 2 2 tt))))
                   (fe-tvals 2 2))))
; and there is something left to test after dropping the poles
(check "functional-equation-has-test-points"
       (every (lambda (cs) (>= (length (fe-tvals (nth 0 cs) (nth 1 cs))) 5))
              (list (list 2 1) (list 2 2) (list 2 3) (list 3 2) (list 5 2))))

; ============================================================
; 5. SISP: verify the functional equation numerically
; ============================================================
; Z(P², T) = 1/((1-T)(1-2T)(1-4T)).
; Pick T = 1/8:
; Z(P², 1/8) = 1/((7/8)(6/8)(4/8)) = 1/(21/64) = 64/21.
; Then 1/(4T) = 2, and Z(P², 2) = 1/((1-2)(1-4)(1-8)) = 1/((-1)(-3)(-7)) = -1/21.
; The functional equation predicts -8·(1/8)³·(64/21) = -1/21. ✓
; (T = 2 lies outside the disc of convergence of the power series, but Z is a
; rational function, so evaluating it there is legitimate; section 4 checks
; the identity at many such points.)
;
; Here we verify the point counts via the power series expansion of
; Z(P², T) = (1-T)^{-1} (1-2T)^{-1} (1-4T)^{-1} to a few terms.

(define (P2-zeta-n q n)
  ; #P²(F_{q^n}) genuinely read off the zeta power series coefficients
  (nth (- n 1) (counts-from-zeta q 2 n)))

(print "--- 5. Numerical verification ---")
(print "Z(P²(F_2),T) series => point counts:")
(print "n=1: # = 1+2+4 = 7")
(print "n=2: # = 1+4+16 = 21")
(print "n=3: # = 1+8+64 = 73")
(check "P2-F2-n3" (= (P2-zeta-n 2 3) 73))

; Independent confirmation: enumerate P²(F_{q^n}) point by point.
(define (verify-series q n) (proj-count q n 2))

(print "#P²(F_2) from series n=1:") (print (verify-series 2 1))
(print "#P²(F_2) from series n=2:") (print (verify-series 2 2))
(print "#P²(F_2) from series n=3:") (print (verify-series 2 3))
(check "P2-series-n1" (= (verify-series 2 1) 7))
(check "P2-series-n2" (= (verify-series 2 2) 21))
(check "P2-series-n3" (= (verify-series 2 3) 73))

(print "=== Zeta Functions Higher Done ===")
