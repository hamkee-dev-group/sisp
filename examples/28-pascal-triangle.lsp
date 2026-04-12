; ============================================================
; Pascal's Triangle
; Generate rows, compute C(n,k), verify sum = 2^n,
; and show connection to powerset cardinalities.
; ============================================================

(print "=== Pascal's Triangle ===")

(define (sub a b) (+ a (* -1 b)))

; Factorial
(define (fact n) (if (<= n 1) 1 (* n (fact (sub n 1)))))

; Binomial coefficient C(n, k)
(define (binom n k)
  (if (> k n) 0
    (/ (fact n) (* (fact k) (fact (sub n k))))))

; Generate row n of Pascal's triangle: (C(n,0) C(n,1) ... C(n,n))
(define (pascal-row n)
  (labels (((build k acc)
    (if (< k 0) acc
      (build (sub k 1) (cons (binom n k) acc)))))
  (build n nil)))

; Sum a list of numbers
(define (sum-list lst)
  (if (equal lst nil) 0
    (+ (car lst) (sum-list (cdr lst)))))

; --- Display Pascal's triangle rows 0..7 ---
(print "--- Pascal's Triangle rows 0-7 ---")
(print "Row 0:") (print (pascal-row 0))
(print "Row 1:") (print (pascal-row 1))
(print "Row 2:") (print (pascal-row 2))
(print "Row 3:") (print (pascal-row 3))
(print "Row 4:") (print (pascal-row 4))
(print "Row 5:") (print (pascal-row 5))
(print "Row 6:") (print (pascal-row 6))
(print "Row 7:") (print (pascal-row 7))

; --- Verify row sum = 2^n ---
(print "--- Row sums (should be 2^n) ---")
(define (verify-row-sum n)
  (let* ((row (pascal-row n))
         (s (sum-list row))
         (expected (^ 2 n)))
    (print "Row") (print n)
    (print "Sum =") (print s)
    (if (= s expected)
      (print "VERIFIED")
      (print "FAILED"))))

(verify-row-sum 0)
(verify-row-sum 3)
(verify-row-sum 5)
(verify-row-sum 7)

; --- Symmetry: C(n,k) = C(n,n-k) ---
(print "--- Symmetry verification ---")
(if (= (binom 7 2) (binom 7 5))
  (print "C(7,2) = C(7,5) VERIFIED")
  (print "FAILED"))
(if (= (binom 6 1) (binom 6 5))
  (print "C(6,1) = C(6,5) VERIFIED")
  (print "FAILED"))

; --- Connection to powerset cardinalities ---
; |P(S)| = 2^|S|, and C(n,k) counts subsets of size k
(print "--- Powerset connection ---")
(print "Set {a b c} has 2^3 = 8 subsets")
(print "Breakdown by size: C(3,0) C(3,1) C(3,2) C(3,3)")
(print (pascal-row 3))
(print "Powerset of {a b c}:")
(print (pow {a b c}))
(print "Powerset size:") (print (ord (pow {a b c})))
(print "Sum C(3,k):") (print (sum-list (pascal-row 3)))

(print "Set {a b c d} has 2^4 = 16 subsets")
(print "Breakdown by size:") (print (pascal-row 4))
(print "Powerset size:") (print (ord (pow {a b c d})))

; --- Binomial coefficients as exact rationals ---
; C(n,k) = n! / (k!(n-k)!) always produces integers, proving they divide evenly
(print "--- Large binomial coefficients (exact) ---")
(print "C(10,3) =") (print (binom 10 3))
(print "C(10,5) =") (print (binom 10 5))
(print "C(12,4) =") (print (binom 12 4))
(print "C(12,6) =") (print (binom 12 6))

(print "=== Pascal's Triangle Done ===")
