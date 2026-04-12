; ============================================================
; Exact Harmonic Numbers
; H_n = 1 + 1/2 + 1/3 + ... + 1/n as exact fractions.
; Shows partial sums growing like ln(n).
; ============================================================

(print "=== Exact Harmonic Numbers ===")

; Compute H_n = sum of 1/k for k=1..n, as an exact rational
(define (harmonic n)
  (labels (((h-sum k acc)
    (if (> k n) acc
      (h-sum (+ k 1) (+ acc (/ 1 k))))))
  (h-sum 1 0)))

; --- Display H_1 through H_10 ---
(print "--- Harmonic numbers H_1 through H_10 ---")

(print "H_1 =") (print (harmonic 1))
(print "H_2 =") (print (harmonic 2))
(print "H_3 =") (print (harmonic 3))
(print "H_4 =") (print (harmonic 4))
(print "H_5 =") (print (harmonic 5))
(print "H_6 =") (print (harmonic 6))
(print "H_7 =") (print (harmonic 7))
(print "H_8 =") (print (harmonic 8))
(print "H_9 =") (print (harmonic 9))
(print "H_10 =") (print (harmonic 10))

; --- Verify some values ---
(print "--- Verification ---")
; H_1 = 1
(if (= (harmonic 1) 1)
  (print "H_1 = 1 VERIFIED")
  (print "H_1 FAILED"))

; H_2 = 3/2
(if (= (harmonic 2) 3/2)
  (print "H_2 = 3/2 VERIFIED")
  (print "H_2 FAILED"))

; H_3 = 11/6
(if (= (harmonic 3) 11/6)
  (print "H_3 = 11/6 VERIFIED")
  (print "H_3 FAILED"))

; H_4 = 25/12
(if (= (harmonic 4) 25/12)
  (print "H_4 = 25/12 VERIFIED")
  (print "H_4 FAILED"))

; --- Differences H_n - H_{n-1} = 1/n ---
(print "--- Differences (should be 1/n) ---")
(define (sub a b) (+ a (* -1 b)))
(print "H_5 - H_4 =") (print (sub (harmonic 5) (harmonic 4)))
(print "H_8 - H_7 =") (print (sub (harmonic 8) (harmonic 7)))
(print "H_10 - H_9 =") (print (sub (harmonic 10) (harmonic 9)))

; --- Partial sums grow slowly (like ln n) ---
; H_n is approximately ln(n) + 0.5772... (Euler-Mascheroni)
; We can compare ratios of consecutive harmonic numbers
(print "--- Growth pattern ---")
(print "H_1 =") (print (harmonic 1))
(print "H_2 =") (print (harmonic 2))
(print "H_4 =") (print (harmonic 4))
(print "H_8 =") (print (harmonic 8))
(print "Each doubling adds roughly ln(2) ~ 0.693")
(print "H_2 - H_1 =") (print (sub (harmonic 2) (harmonic 1)))
(print "H_4 - H_2 =") (print (sub (harmonic 4) (harmonic 2)))
(print "H_8 - H_4 =") (print (sub (harmonic 8) (harmonic 4)))

; --- Larger harmonic number ---
(print "--- H_12 (exact) ---")
(print (harmonic 12))

(print "=== Harmonic Numbers Done ===")
