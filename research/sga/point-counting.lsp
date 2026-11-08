; ============================================================
; SGA 5 — Point Counting on Curves over Finite Fields
; Elliptic curve: y² = x³ + ax + b over F_q.
; Count #C(F_q) by exhaustive search. Hasse-Weil bound.
; ============================================================

(print "=== Point Counting on Curves ===")

; ------------------------------------------------------------
; Brute-force point counting on y² = x³ + ax + b over F_q.
; ec-affine iterates x AND y over all of F_q and tests the
; congruence directly — no formula is assumed anywhere.
; The projective count adds the single point at infinity O=(0:1:0).
; ------------------------------------------------------------

(define (ec-affine aa bb q)
  (foldl + 0
    (map (lambda (x)
           (let ((rhs (mod (+ (* x x x) (* aa x) bb) q)))
             (count (lambda (y) (= (mod (* y y) q) rhs)) (seq 0 (- q 1)))))
         (seq 0 (- q 1)))))

(define (ec-count aa bb q) (+ 1 (ec-affine aa bb q)))

; trace of Frobenius a_q = q + 1 - #E(F_q)
(define (ec-trace aa bb q) (- (+ q 1) (ec-count aa bb q)))

; Hasse bound |a_q| ≤ 2√q, squared so we stay in exact integers.
(define (hasse-ok aa bb q)
  (let ((tr (ec-trace aa bb q))) (<= (* tr tr) (* 4 q))))

; Δ = -16(4a³ + 27b²) ≠ 0 in F_q  <=>  the curve is nonsingular.
(define (ec-nonsingular aa bb q)
  (/= 0 (mod (* -16 (+ (* 4 aa aa aa) (* 27 bb bb))) q)))

; the actual x-fibre sizes, for narration and for fibre-by-fibre checks
(define (ec-fibres aa bb q)
  (map (lambda (x)
         (let ((rhs (mod (+ (* x x x) (* aa x) bb) q)))
           (count (lambda (y) (= (mod (* y y) q) rhs)) (seq 0 (- q 1)))))
       (seq 0 (- q 1))))

; ============================================================
; 1. Counting points on y² = x³ + x over F_5
; ============================================================
; Curve: y² = x³ + x over F_5.
; Discriminant: Δ = -16(4·1³ + 27·0²) = -64 = 1 mod 5 (since 64 ≡ 4, -64 ≡ 1).
; Δ ≠ 0 => not singular (elliptic curve).
;
; Count all (x,y) ∈ F_5 × F_5 satisfying y² ≡ x³ + x (mod 5).
; Points F_5 = {0,1,2,3,4}.
; x=0: x³+x = 0. y²=0 => y=0. Point: (0,0).
; x=1: x³+x = 1+1 = 2. y²=2 => no solution in F_5 (squares: 0,1,4).
; x=2: x³+x = 8+2 = 10 ≡ 0. y²=0 => y=0. Point: (2,0).
; x=3: x³+x = 27+3 = 30 ≡ 0. y²=0 => y=0. Point: (3,0).
; x=4: x³+x = 64+4 = 68 ≡ 3. y²=3 => no solution.
; Finite points: {(0,0), (2,0), (3,0)}. Plus point at infinity O.
; Total: #C(F_5) = 4.

(print "--- 1. E: y² = x³ + x over F_5 ---")
(print "x=0: y²=0 => (0,0)")
(print "x=1: y²=2 => no solution")
(print "x=2: y²=10 ≡ 0 => (2,0)")
(print "x=3: y²=30 ≡ 0 => (3,0)")
(print "x=4: y²=3 => no solution")
(print "Plus point at infinity O.")
(print "#C(F_5) = 3 + 1 = 4")

; n1 is not written down — it is counted.
(define n1 (ec-count 1 0 5))
(print "brute-force #C(F_5) =") (print n1)
(print "x-fibre sizes over x=0..4:") (print (ec-fibres 1 0 5))
(check "e1-F5-nonsingular" (ec-nonsingular 1 0 5))
(check "count-e1-F5-is-4" (= n1 4))
; the hand table above says the fibres are 1,0,1,1,0 — compare against the count
(check "e1-F5-fibres-match-hand-table" (equal (ec-fibres 1 0 5) (list 1 0 1 1 0)))

; ============================================================
; 2. Trace of Frobenius and Hasse-Weil bound
; ============================================================
; For elliptic curve over F_q:
; a_q = q + 1 - #C(F_q) (trace of Frobenius).
; Hasse bound: |a_q| ≤ 2√q.
;
; For q=5, #C=4: a_5 = 5 + 1 - 4 = 2.
; Check: |2| ≤ 2√5 ≈ 4.47. ✓

(define a5 (- (+ 5 1) n1))
(print "--- 2. Trace of Frobenius ---")
(print "a_5 = q + 1 - #C(F_5) = 5 + 1 - 4 =") (print a5)
(print "|a_5| = 2 ≤ 2√5 ≈ 4.472 => Hasse-Weil holds.")
(check "trace-of-frob-2" (= a5 2))
; Hasse tested as a_q² ≤ 4q, exactly: 4 ≤ 20.
(check "hasse-bound-e1-F5" (hasse-ok 1 0 5))
; and it is a real bound, not a tautology: it is tight only for a² = 4q,
; so a hypothetical trace of 5 here would have to violate it.
(check "hasse-bound-is-restrictive" (not (<= (* 5 5) (* 4 5))))

; ============================================================
; 3. Count on y² = x³ + 1 over F_7
; ============================================================
; F_7 = {0,1,2,3,4,5,6}. Squares in F_7: 0,1,4,2,2,4,1 => {0,1,2,4}.
; NOTE: 2 IS a square mod 7 (3²=9≡2, 4²=16≡2), so every x with x³+1 ≡ 2
; gives two points.
; x=0: x³+1 = 1. y²=1 => y=1,6. Points: (0,1), (0,6).
; x=1: x³+1 = 2. y²=2 => y=3,4. Points: (1,3), (1,4).
; x=2: x³+1 = 8+1 = 9 ≡ 2. y²=2 => y=3,4. Points: (2,3), (2,4).
; x=3: x³+1 = 27+1 = 28 ≡ 0. y²=0 => y=0. Point: (3,0).
; x=4: x³+1 = 64+1 = 65 ≡ 2. y²=2 => y=3,4. Points: (4,3), (4,4).
; x=5: x³+1 = 125+1 = 126 ≡ 0. y²=0 => y=0. Point: (5,0).
; x=6: x³+1 = 216+1 = 217 ≡ 0. y²=0 => y=0. Point: (6,0).
; Finite points: 2+2+2+1+2+1+1 = 11. Plus ∞.
; #C(F_7) = 11 + 1 = 12.
; a_7 = 7 + 1 - 12 = -4. |-4| ≤ 2√7 ≈ 5.29. ✓ (16 ≤ 28)

(print "--- 3. E: y² = x³ + 1 over F_7 ---")
(print "x-fibre sizes over x=0..6:") (print (ec-fibres 0 1 7))
(print "#C(F_7) =") (print (ec-count 0 1 7))
(print "a_7 =") (print (ec-trace 0 1 7))
(check "E-x^3+1-over-F7-nonsingular" (ec-nonsingular 0 1 7))
(check "E-x^3+1-over-F7" (= (ec-count 0 1 7) 12))
(check "E-x^3+1-over-F7-fibres" (equal (ec-fibres 0 1 7) (list 2 2 2 1 2 1 1)))
(check "E-x^3+1-over-F7-trace" (= (ec-trace 0 1 7) -4))
(check "E-x^3+1-over-F7-hasse" (hasse-ok 0 1 7))

; ============================================================
; 4. Count on y² = x³ + 2x over F_3
; ============================================================
; F_3 = {0,1,2}. Squares in F_3: {0,1} (0²=0, 1²=1, 2²=4≡1).
; x=0: x³+2x = 0. y²=0 => y=0. (0,0).
; x=1: x³+2x = 1+2 = 3 ≡ 0. y²=0 => y=0. (1,0).
; x=2: x³+2x = 8+4 = 12 ≡ 0. y²=0 => y=0. (2,0).
; Finite: {(0,0), (1,0), (2,0)} + ∞ = 4.
; a_3 = 3+1-4 = 0. |0| ≤ 2√3 ≈ 3.46. ✓

(print "--- 4. E: y² = x³ + 2x over F_3 ---")
(print "x-fibre sizes over x=0..2:") (print (ec-fibres 2 0 3))
(print "#C(F_3) =") (print (ec-count 2 0 3))
(print "a_3 =") (print (ec-trace 2 0 3))
(check "E-x^3+2x-over-F3-nonsingular" (ec-nonsingular 2 0 3))
(check "E-x^3+2x-over-F3" (= (ec-count 2 0 3) 4))
(check "E-x^3+2x-over-F3-fibres" (equal (ec-fibres 2 0 3) (list 1 1 1)))
; a_3 = 0: this curve is supersingular over F_3.
(check "E-x^3+2x-over-F3-supersingular" (= (ec-trace 2 0 3) 0))
(check "E-x^3+2x-over-F3-hasse" (hasse-ok 2 0 3))

; ============================================================
; 5. Count on y² = x³ + x + 1 over F_5
; ============================================================
; F_5 = {0,1,2,3,4}
; x=0: 0³+0+1 = 1. y²=1 => y=1,4. (0,1), (0,4).
; x=1: 1+1+1 = 3. y²=3 => no.
; x=2: 8+2+1 = 11 ≡ 1. y²=1 => y=1,4. (2,1), (2,4).
; x=3: 27+3+1 = 31 ≡ 1. y²=1 => y=1,4. (3,1), (3,4).
; x=4: 64+4+1 = 69 ≡ 4. y²=4 => y=2,3. (4,2), (4,3).
; Finite: 2 + 0 + 2 + 2 + 2 = 8. + ∞ = 9.
; #C(F_5) = 9.
; a_5 = q+1-#C = 5+1-9 = -3. |-3| ≤ 2√5 ≈ 4.47. ✓

(print "--- 5. E: y² = x³ + x + 1 over F_5 ---")
(print "x-fibre sizes over x=0..4:") (print (ec-fibres 1 1 5))
(print "#C(F_5) =") (print (ec-count 1 1 5))
(print "a_5 =") (print (ec-trace 1 1 5))
(check "E-x^3+x+1-over-F5-nonsingular" (ec-nonsingular 1 1 5))
(check "E-x^3+x+1-over-F5" (= (ec-count 1 1 5) 9))
(check "E-x^3+x+1-over-F5-fibres" (equal (ec-fibres 1 1 5) (list 2 0 2 2 2)))
(check "E-x^3+x+1-over-F5-trace" (= (ec-trace 1 1 5) -3))
(check "E-x^3+x+1-over-F5-hasse" (hasse-ok 1 1 5))

; ============================================================
; 6. Hasse holds for EVERY nonsingular curve over the small F_q
; ============================================================
; Not one curve: sweep all (a,b) over F_3, F_5, F_7 and demand
; a_q² ≤ 4q for every nonsingular one. This is the real theorem,
; and any error in ec-affine would break it.

(define (all-pairs q)
  (foldr append nil
    (map (lambda (aa) (map (lambda (bb) (list aa bb)) (seq 0 (- q 1))))
         (seq 0 (- q 1)))))

(define (hasse-all q)
  (every (lambda (pr)
           (let ((aa (nth 0 pr)) (bb (nth 1 pr)))
             (cond ((ec-nonsingular aa bb q) (hasse-ok aa bb q)) (t t))))
         (all-pairs q)))

(define (nonsing-count q)
  (count (lambda (pr) (ec-nonsingular (nth 0 pr) (nth 1 pr) q)) (all-pairs q)))

(print "--- 6. Hasse bound over all curves ---")
(print "nonsingular (a,b) over F_3, F_5, F_7:")
(print (list (nonsing-count 3) (nonsing-count 5) (nonsing-count 7)))
(check "hasse-all-curves-F3" (hasse-all 3))
(check "hasse-all-curves-F5" (hasse-all 5))
(check "hasse-all-curves-F7" (hasse-all 7))
; there really are curves in the sweep (the quantifier is not vacuous)
(check "sweep-is-nonempty" (> (nonsing-count 7) 0))

(print "=== Point Counting Done ===")
