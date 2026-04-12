; ============================================================
; Pi Approximation via Leibniz Series (Exact Fractions)
; pi/4 = 1 - 1/3 + 1/5 - 1/7 + 1/9 - ...
; Each partial sum is computed as an EXACT rational.
; ============================================================

(print "=== Pi Approximation (Exact Rational Arithmetic) ===")

(define (sub a b) (+ a (* -1 b)))

; Compute the Nth partial sum of the Leibniz series as an exact fraction.
; S_n = sum_{k=0}^{n-1} (-1)^k / (2k+1)
; This gives pi/4 approximately.
(define (leibniz-partial n)
  (labels (((compute k sign acc)
    (if (= k n) acc
      (let* ((term (/ sign (+ (* 2 k) 1))))
        (compute (+ k 1) (* -1 sign) (+ acc term))))))
  (compute 0 1 0)))

; The partial sum approximates pi/4, so multiply by 4 for pi
(define (pi-approx n) (* 4 (leibniz-partial n)))

; --- Show convergence ---
(print "--- Leibniz partial sums (pi/4) ---")
(print "S_1  = 1 term:")    (print (leibniz-partial 1))
(print "S_2  = 2 terms:")   (print (leibniz-partial 2))
(print "S_3  = 3 terms:")   (print (leibniz-partial 3))
(print "S_4  = 4 terms:")   (print (leibniz-partial 4))
(print "S_5  = 5 terms:")   (print (leibniz-partial 5))
(print "S_6  = 6 terms:")   (print (leibniz-partial 6))
(print "S_8  = 8 terms:")   (print (leibniz-partial 8))
(print "S_10 = 10 terms:")  (print (leibniz-partial 10))

; --- Multiply by 4 for pi approximations ---
(print "--- Pi approximations (4 * partial sum) ---")
(print "5 terms:") (print (pi-approx 5))
(print "10 terms:") (print (pi-approx 10))
(print "15 terms:") (print (pi-approx 15))

; --- Compare with 355/113 ---
(print "--- Comparison with 355/113 ---")
(print "355/113 =") (print 355/113)
(print "Pi approx (15 terms) =") (print (pi-approx 15))

; Which is closer to pi?
; pi = 3.14159265...
; 355/113 = 3.14159292... (accurate to 6 decimal places)
; The Leibniz series converges very slowly.

; --- Compare with 22/7 ---
(print "--- Comparison with 22/7 ---")
(print "22/7 =") (print 22/7)

; --- Show exact rational nature ---
(print "--- Exact fractions (no floating point!) ---")
(print "After 5 terms, pi/4 is exactly:")
(print (leibniz-partial 5))
(print "That is: 1 - 1/3 + 1/5 - 1/7 + 1/9")
; Verify manually
(define manual (+ 1 (* -1 1/3) 1/5 (* -1 1/7) 1/9))
(print "Manual computation:") (print manual)
(if (= manual (leibniz-partial 5))
  (print "VERIFIED: exact match")
  (print "MISMATCH"))

; --- Show the alternating nature ---
(print "--- Alternating convergence ---")
(print "S_1 (upper):") (print (pi-approx 1))
(print "S_2 (lower):") (print (pi-approx 2))
(print "S_3 (upper):") (print (pi-approx 3))
(print "S_4 (lower):") (print (pi-approx 4))
(print "Odd partial sums are above pi, even are below")

(print "=== Pi Approximation Done ===")
