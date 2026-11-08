; ============================================================
; SGA — Weil Conjectures for Curves over F_q
; Zeta function, functional equation, Riemann hypothesis (Weil).
; For elliptic curves: Z(C,T) = (1 - a_q T + q T²)/((1-T)(1-qT)).
; ============================================================

(print "=== Weil Conjectures for Curves ===")

; ------------------------------------------------------------
; ROUTE A — brute force. Count E(F_{q^k}) by iterating over the
; whole field. F_q is Z/qZ; F_{q^2} = F_q[u]/(u²-nr) for a
; non-residue nr; F_{q^3} = F_q[u]/(u³+pu+s) for an irreducible
; depressed cubic. Nothing about zeta functions is assumed.
; ------------------------------------------------------------

(define (ec-affine aa bb q)
  (foldl + 0
    (map (lambda (x)
           (let ((rhs (mod (+ (* x x x) (* aa x) bb) q)))
             (count (lambda (y) (= (mod (* y y) q) rhs)) (seq 0 (- q 1)))))
         (seq 0 (- q 1)))))
(define (ec-count aa bb q) (+ 1 (ec-affine aa bb q)))
(define (ec-trace aa bb q) (- (+ q 1) (ec-count aa bb q)))

; --- F_{q^2} ---
(define (nonsquare q)
  (car (filter (lambda (c)
                 (= 0 (count (lambda (y) (= (mod (* y y) q) c)) (seq 0 (- q 1)))))
               (seq 1 (- q 1)))))
(define (f2mul u v nr q)
  (list (mod (+ (* (nth 0 u) (nth 0 v)) (* nr (nth 1 u) (nth 1 v))) q)
        (mod (+ (* (nth 0 u) (nth 1 v)) (* (nth 1 u) (nth 0 v))) q)))
(define (f2add u v q)
  (list (mod (+ (nth 0 u) (nth 0 v)) q) (mod (+ (nth 1 u) (nth 1 v)) q)))
(define (f2scal c u q)
  (list (mod (* c (nth 0 u)) q) (mod (* c (nth 1 u)) q)))
(define (f2elts q)
  (foldr append nil
    (map (lambda (c0) (map (lambda (c1) (list c0 c1)) (seq 0 (- q 1))))
         (seq 0 (- q 1)))))
(define (ec-count-2 aa bb q)
  (let* ((nr (nonsquare q)) (els (f2elts q)))
    (+ 1 (foldl + 0
      (map (lambda (x)
             (let ((rhs (f2add (f2add (f2mul x (f2mul x x nr q) nr q)
                                      (f2scal aa x q) q)
                               (list (mod bb q) 0) q)))
               (count (lambda (y) (equal (f2mul y y nr q) rhs)) els)))
           els)))))

; --- F_{q^3} ---
(define (cubic-has-root p s q)
  (any (lambda (x) (= 0 (mod (+ (* x x x) (* p x) s) q))) (seq 0 (- q 1))))
(define (irr-cubic q)
  (car (filter (lambda (pr) (not (cubic-has-root (nth 0 pr) (nth 1 pr) q)))
               (foldr append nil
                 (map (lambda (p) (map (lambda (s) (list p s)) (seq 0 (- q 1))))
                      (seq 0 (- q 1)))))))
(define (f3mul u v pp ss q)
  (let* ((a0 (nth 0 u)) (a1 (nth 1 u)) (a2 (nth 2 u))
         (b0 (nth 0 v)) (b1 (nth 1 v)) (b2 (nth 2 v))
         (c0 (* a0 b0)) (c1 (+ (* a0 b1) (* a1 b0)))
         (c2 (+ (* a0 b2) (* a1 b1) (* a2 b0)))
         (c3 (+ (* a1 b2) (* a2 b1))) (c4 (* a2 b2)))
    ; reduce u³ = -p·u - s, u⁴ = -p·u² - s·u
    (list (mod (- c0 (* ss c3)) q)
          (mod (- c1 (* pp c3) (* ss c4)) q)
          (mod (- c2 (* pp c4)) q))))
(define (f3add u v q)
  (list (mod (+ (nth 0 u) (nth 0 v)) q) (mod (+ (nth 1 u) (nth 1 v)) q)
        (mod (+ (nth 2 u) (nth 2 v)) q)))
(define (f3scal c u q)
  (list (mod (* c (nth 0 u)) q) (mod (* c (nth 1 u)) q) (mod (* c (nth 2 u)) q)))
(define (f3elts q)
  (foldr append nil (map (lambda (c0)
    (foldr append nil (map (lambda (c1)
      (map (lambda (c2) (list c0 c1 c2)) (seq 0 (- q 1)))) (seq 0 (- q 1)))))
    (seq 0 (- q 1)))))
(define (ec-count-3 aa bb q)
  (let* ((ic (irr-cubic q)) (pp (nth 0 ic)) (ss (nth 1 ic)) (els (f3elts q)))
    (+ 1 (foldl + 0
      (map (lambda (x)
             (let ((rhs (f3add (f3add (f3mul x (f3mul x x pp ss q) pp ss q)
                                      (f3scal aa x q) q)
                               (list (mod bb q) 0 0) q)))
               (count (lambda (y) (equal (f3mul y y pp ss q) rhs)) els)))
           els)))))

; ------------------------------------------------------------
; ROUTE B — the zeta function. From a_q alone, the Weil
; conjectures PREDICT every N_k with no further counting:
;   N_k = q^k + 1 - s_k,  s_k = α^k + β^k,
;   s_0 = 2, s_1 = a_q, s_k = a_q·s_{k-1} - q·s_{k-2}.
; (Newton's identity for the roots of t² - a_q t + q.)
; ------------------------------------------------------------

; tail-recursive walk: at step i, prev = s_{i-1} and cur = s_i
(define (s-go aa q k i prev cur)
  (cond ((= i k) cur)
        (t (s-go aa q k (+ i 1) cur (- (* aa cur) (* q prev))))))
(define (frob-s aa q k)
  (cond ((= k 0) 2) (t (s-go aa q k 1 2 aa))))
(define (weil-n aa q k) (+ (^ q k) 1 (* -1 (frob-s aa q k))))

; numerator P(T) = 1 - a T + q T², and Z(T) = P(T)/((1-T)(1-qT)).
(define (pnum aa q tt) (+ 1 (* -1 aa tt) (* q tt tt)))
(define (zeta aa q tt) (/ (pnum aa q tt) (* (- 1 tt) (- 1 (* q tt)))))

; ============================================================
; 1. Zeta function for E: y² = x³ + x over F_5
; ============================================================
; We counted #C(F_5) = 4, so a_5 = q+1-#C = 5+1-4 = 2.
; Z(C,T) = (1 - 2T + 5T²) / ((1-T)(1-5T)).
;
; Compute #C(F_{5^n}) for n=1,2,3 via zeta formula:
; The numerator 1 - 2T + 5T² = (1 - αT)(1 - βT) where α,β are algebraic integers.
; α + β = 2, αβ = 5.
; #C(F_{q^n}) = qⁿ + 1 - (αⁿ + βⁿ).
;
; α,β are roots of t² - 2t + 5 = 0.
; t = (2 ± √(4-20))/2 = (2 ± √(-16))/2 = 1 ± 2i.
; α = 1 + 2i, β = 1 - 2i.
; |α| = |β| = √(1² + 2²) = √5 = q^{1/2}. ✓ (Weil RH).

(print "--- 1. Zeta of E: y² = x³ + x / F_5 ---")
(print "a_5 = 2 => Z(C,T) = (1 - 2T + 5T²)/((1-T)(1-5T))")
(print "α = 1+2i, β = 1-2i. |α| = |β| = √5 = q^{1/2}. (Riemann Hypothesis)")

; --- #C(F_{5^n}) for n=1,2,3,4 ---
; n=1: #C = q+1-(α+β) = 5+1-2 = 4. ✓
; n=2: α²+β² = (1+2i)²+(1-2i)² = (1+4i-4)+(1-4i-4) = (-3+4i)+(-3-4i) = -6.
;       q² = 25. #C = 25+1-(-6) = 32.
; n=3: α³ = (1+2i)³ = (1+2i)(1+4i-4) = (1+2i)(-3+4i) = -3+4i-6i+8i² = -3-2i-8 = -11-2i.
;       β³ = -11+2i. α³+β³ = -22.
;       q³ = 125. #C = 125+1-(-22) = 148.
; n=4: α⁴+β⁴. α⁴ = (-3+4i)² = 9-24i+16i² = 9-24i-16 = -7-24i.
;       β⁴ = -7+24i. α⁴+β⁴ = -14.
;       q⁴ = 625. #C = 625+1-(-14) = 640.

(print "")
; a_5 is not asserted — it is counted, then everything below follows from it.
(define a-e1 (ec-trace 1 0 5))
(print "counted a_5 =") (print a-e1)
(check "E-x^3+x-F5-trace-is-2" (= a-e1 2))

(print "predicted N_k (k=1..4) from the zeta numerator:")
(print (map (lambda (k) (weil-n a-e1 5 k)) (seq 1 4)))
(print "brute-force N_1, N_2, N_3 by exhaustive search in F_5, F_25, F_125:")
(print (list (ec-count 1 0 5) (ec-count-2 1 0 5) (ec-count-3 1 0 5)))

; The two sides never touch: the left is a linear recursion in a_5,
; the right walks every point of an explicitly constructed field.
(check "E-x^3+x-F5-pt-counts"    (= (weil-n a-e1 5 1) (ec-count   1 0 5)))
(check "E-x^3+x-F25-pt-counts"   (= (weil-n a-e1 5 2) (ec-count-2 1 0 5)))
(check "E-x^3+x-F125-pt-counts"  (= (weil-n a-e1 5 3) (ec-count-3 1 0 5)))
(check "E-x^3+x-F5-N2-is-32"     (= (ec-count-2 1 0 5) 32))
(check "E-x^3+x-F5-N3-is-148"    (= (ec-count-3 1 0 5) 148))
(check "E-x^3+x-F5-N4-is-640"    (= (weil-n a-e1 5 4) 640))
; s_k = α^k+β^k must satisfy Newton's identity independently of the walk
(check "E-x^3+x-F5-newton-s4"
       (= (frob-s a-e1 5 4)
          (- (* a-e1 (frob-s a-e1 5 3)) (* 5 (frob-s a-e1 5 2)))))

; ============================================================
; 2. Functional equation
; ============================================================
; For a curve of genus g: Z(C, 1/(qT)) = q^{1-g} T^{2-2g} Z(C, T).
; For an elliptic curve (g=1) this is simply Z(C, 1/(qT)) = Z(C, T).
;
; Check by hand, for a = 2:
; Numerator of Z(C, 1/(qT)): 1 - 2/(qT) + q/(q²T²) = 1 - 2/(qT) + 1/(qT²).
; Denominator: (1 - 1/(qT))(1 - q/(qT)) = (1 - 1/(qT))(1 - 1/T).
; Multiply both by qT²: the numerator becomes qT² - 2T + 1 and the
; denominator (qT - 1)(T - 1) = (1 - qT)(1 - T).
; So Z(C, 1/(qT)) = (1 - 2T + qT²)/((1-T)(1-qT)) = Z(C, T). ✓

(print "--- 2. Functional Equation ---")
(print "Z(C, 1/qT) = Z(C, T) for elliptic curves (genus 1).")

; SISP division is exact rational arithmetic, so we can simply EVALUATE
; both sides at several T and demand equality on the nose.
; Avoid the poles T = 1, 1/q (and their images under T -> 1/(qT)).
(define fe-raw (list 2 3 (/ 1 3) (/ 2 3) (/ 1 7) (/ 5 7) 10))

; T = 1 and T = 1/q are poles of Z, and T -> 1/(qT) swaps them,
; so those two values (and only those) must be dropped.
(define (fe-points q)
  (filter (lambda (tt) (and (/= tt 1) (/= tt (/ 1 q)))) fe-raw))

(define (fe-holds aa q)
  (every (lambda (tt) (= (zeta aa q tt) (zeta aa q (/ 1 (* q tt))))) (fe-points q)))

(print "Z(C,T) at T=1/3:") (print (zeta a-e1 5 (/ 1 3)))
(print "Z(C,1/(5T)) at T=1/3, i.e. Z(C,3/5):") (print (zeta a-e1 5 (/ 3 5)))
(check "functional-equation-elliptic" (fe-holds a-e1 5))

; the numerator alone is self-reciprocal: q T² P(1/(qT)) = P(T)
(check "numerator-self-reciprocal"
       (every (lambda (tt)
                (= (* 5 tt tt (pnum a-e1 5 (/ 1 (* 5 tt)))) (pnum a-e1 5 tt)))
              (fe-points 5)))

; ...and this is a real constraint: a numerator with the WRONG constant
; term (q replaced by q+1) fails the same test.
(check "functional-equation-detects-a-wrong-numerator"
       (not (every (lambda (tt)
                     (= (* 5 tt tt (+ 1 (* -1 a-e1 (/ 1 (* 5 tt))) (* 6 (/ 1 (* 25 tt tt)))))
                        (+ 1 (* -1 a-e1 tt) (* 6 tt tt))))
                   (fe-points 5))))

; ============================================================
; 3. Riemann Hypothesis (Weil): |roots| = q^{-1/2}
; ============================================================
; The numerator: P(T) = 1 - a_q T + q T².
; Zeros: T satisfying 1 - 2T + 5T² = 0.
; T = [2 ± √(4-20)]/(2·5) = [2 ± 4i]/10 = (1 ± 2i)/5.
; |T| = √(1² + 2²)/5 = √5/5 = 1/√5 = q^{-1/2}.
; Thus |T| = q^{-1/2} = 1/√5 ≈ 0.447. ✓
; This is exactly the Riemann Hypothesis for curves.

(print "--- 3. Weil Riemann Hypothesis ---")
(print "Roots of 1 - 2T + 5T²: T = (1 ± 2i)/5")

; The roots of P(T) = 1 - aT + qT² are T = (a ± i√(4q-a²))/(2q),
; PROVIDED the discriminant a²-4q is negative (a conjugate pair).
; That negativity is the Hasse bound, and it is checked, not assumed.
(define (conjugate-pair aa q) (< (* aa aa) (* 4 q)))

; Quadratic formula on qT² - aT + 1 = 0:
;   Re T = a/(2q),  (Im T)² = (4q - a²)/(4q²).
; |T|² = (Re T)² + (Im T)².
(define (root-re aa q) (/ aa (* 2 q)))
(define (root-im-sq aa q) (/ (- (* 4 q) (* aa aa)) (* 4 q q)))
(define (root-modulus-sq aa q)
  (+ (* (root-re aa q) (root-re aa q)) (root-im-sq aa q)))

; Independent route to the same number: Vieta on P(T) = 1 - aT + qT²
; gives T₁T₂ = 1/q, and the roots are complex conjugates, so |T|² = T₁T₂.
(define (vieta-root-product q) (/ 1 q))

(print "Re T =") (print (root-re a-e1 5))
(print "(Im T)² =") (print (root-im-sq a-e1 5))
(print "|T|² from Re/Im:") (print (root-modulus-sq a-e1 5))
(print "|T|² from Vieta:") (print (vieta-root-product 5))
(check "weil-rh-conjugate-pair" (conjugate-pair a-e1 5))
(check "weil-rh-roots-q^{-1/2}" (= (root-modulus-sq a-e1 5) (vieta-root-product 5)))
; the same must hold for the two other curves in this file
(check "weil-rh-roots-all-curves"
       (every (lambda (pr)
                (let ((aa (nth 0 pr)) (q (nth 1 pr)))
                  (and (conjugate-pair aa q)
                       (= (root-modulus-sq aa q) (vieta-root-product q)))))
              (list (list (ec-trace 1 0 5) 5)
                    (list (ec-trace 1 1 5) 5)
                    (list (ec-trace 0 1 7) 7))))

; A second, wholly different route to the same statement: the Weil RH is
; equivalent to |s_k| ≤ 2 q^{k/2} for every k, i.e. s_k² ≤ 4 q^k. Test it
; against the recursion out to k=8 (no floats: everything is squared).
(define (rh-bound-k aa q k) (<= (* (frob-s aa q k) (frob-s aa q k)) (* 4 (^ q k))))
(print "s_1..s_8 =") (print (map (lambda (k) (frob-s a-e1 5 k)) (seq 1 8)))
(check "weil-rh-s_k-bound" (every (lambda (k) (rh-bound-k a-e1 5 k)) (seq 1 8)))
; the bound has teeth: a curve with the (Hasse-violating) trace 5 over F_5
; would break it already at k=1.
(check "weil-rh-s_k-bound-is-sharp" (not (rh-bound-k 5 5 1)))

; ============================================================
; 4. Another example: E: y² = x³ + x + 1 over F_5
; ============================================================
; #C(F_5) = 9, a_5 = 5+1-9 = -3.
; Z(C,T) = (1 + 3T + 5T²)/((1-T)(1-5T)).
; Roots of numerator: T = [-3 ± √(9-20)]/10 = [-3 ± √(-11)]/10.
; |T| = √(9+11)/10 = √20/10 = (2√5)/10 = √5/5 = 1/√5. ✓
;
; #C(F_{25}): α+β = -3, αβ = 5. α²+β² = (α+β)² - 2αβ = 9 - 10 = -1.
; #C(F_{25}) = 25 + 1 - (-1) = 27.
;
; #C(F_{125}): α³+β³ = (α+β)³ - 3αβ(α+β) = -27 - 3·5·(-3) = -27 + 45 = 18.
; #C(F_{125}) = 125 + 1 - 18 = 108.

(print "--- 4. E: y² = x³ + x + 1 over F_5 ---")
(print "a_5 = -3. Z(C,T) = (1 + 3T + 5T²)/((1-T)(1-5T)).")

(define a-e2 (ec-trace 1 1 5))
(print "counted a_5 =") (print a-e2)
(print "predicted N_1,N_2,N_3 =")
(print (map (lambda (k) (weil-n a-e2 5 k)) (seq 1 3)))
(print "brute-forced N_1,N_2,N_3 =")
(print (list (ec-count 1 1 5) (ec-count-2 1 1 5) (ec-count-3 1 1 5)))

(check "E-x^3+x+1-F5-weil"      (= (weil-n a-e2 5 1) (ec-count   1 1 5)))
(check "E-x^3+x+1-F25-weil"     (= (weil-n a-e2 5 2) (ec-count-2 1 1 5)))
(check "E-x^3+x+1-F125-weil"    (= (weil-n a-e2 5 3) (ec-count-3 1 1 5)))
(check "E-x^3+x+1-F25-is-27"    (= (ec-count-2 1 1 5) 27))
(check "E-x^3+x+1-F125-is-108"  (= (ec-count-3 1 1 5) 108))
(check "E-x^3+x+1-F5-func-eq"   (fe-holds a-e2 5))
; this curve is NOT isogenous to the one in section 1 — different trace,
; so the two predictions genuinely differ.
(check "E-x^3+x+1-differs-from-E-x^3+x" (/= a-e2 a-e1))

; ============================================================
; 5. Quadratic twist example over F_7
; ============================================================
; E: y² = x³ + 1 over F_7.
; x³ + 1 takes the value 2 at x = 1, 2, 4, and 2 is a square mod 7
; (3² = 4² = 2), so each of these x gives two points.  Exhaustive search
; finds 11 affine points, so #C(F_7) = 12 and a_7 = 7 + 1 - 12 = -4.
; Z(C,T) = (1 + 4T + 7T²)/((1-T)(1-7T)).
; Roots: T = (-4 ± √(16-28))/14 = (-4 ± 2i√3)/14 = (-2 ± i√3)/7.
; |T|² = (4+3)/49 = 7/49 = 1/7. ✓ (RH still holds, as it must.)

(print "--- 5. E: y² = x³ + 1 over F_7 ---")
(define a-e3 (ec-trace 0 1 7))
(print "counted #C(F_7) =") (print (ec-count 0 1 7))
(print "counted a_7 =") (print a-e3)
(check "E-x^3+1-F7-count-is-12" (= (ec-count 0 1 7) 12))
(check "E-x^3+1-F7-trace-is-minus-4" (= a-e3 -4))

(print "predicted N_1, N_2 =") (print (list (weil-n a-e3 7 1) (weil-n a-e3 7 2)))
(print "brute-forced N_1, N_2 =") (print (list (ec-count 0 1 7) (ec-count-2 0 1 7)))
(check "E-x^3+1-F7-weil"  (= (weil-n a-e3 7 1) (ec-count   0 1 7)))
(check "E-x^3+1-F49-weil" (= (weil-n a-e3 7 2) (ec-count-2 0 1 7)))
(check "E-x^3+1-F49-is-48" (= (ec-count-2 0 1 7) 48))
(check "E-x^3+1-F7-func-eq" (fe-holds a-e3 7))
(print "|T|² =") (print (root-modulus-sq a-e3 7))
(check "E-x^3+1-F7-rh" (= (root-modulus-sq a-e3 7) (vieta-root-product 7)))

(print "=== Weil Conjectures Done ===")
