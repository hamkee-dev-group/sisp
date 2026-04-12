; ============================================================
; 31-combinations.lsp
; Generate all k-element subsets of a set using powerset
; and cardinality filtering. Verify C(n,k) = |{S in P(X) : |S|=k}|.
; ============================================================

(print "=== Combinations via Powerset ===")

; --- Factorial ---
(define (fact n) (if (= n 0) 1 (* n (fact (+ n -1)))))

; --- Binomial coefficient C(n,k) = n! / (k! * (n-k)!) ---
(define (binom n k)
  (/ (fact n) (* (fact k) (fact (+ n (* -1 k))))))

; --- Filter subsets of cardinality k from a powerset ---
; Works on sets: car/cdr traverse the set elements
(define (filter-card s k)
  (if (equal s nil) nil
    (if (= (ord (car s)) k)
        (cons (car s) (filter-card (cdr s) k))
        (filter-card (cdr s) k))))

; --- Count subsets of cardinality k ---
(define (count-card s k)
  (if (equal s nil) 0
    (+ (if (= (ord (car s)) k) 1 0)
       (count-card (cdr s) k))))

; ============================================================
; Test 1: C(5,2) = 10
; ============================================================
(print "--- C(5,2) ---")
(define X5 {1 2 3 4 5})
(define P5 (pow X5))

(print "Set X:")
(print X5)
(print "Total subsets |P(X)|:")
(print (ord P5))

(define k2-subsets (filter-card P5 2))
(print "2-element subsets:")
(print k2-subsets)

(define counted (count-card P5 2))
(print "Number of 2-element subsets found:")
(print counted)

(print "C(5,2) by formula:")
(print (binom 5 2))

(if (= counted (binom 5 2))
    (print "VERIFIED: C(5,2) = 10")
    (print "FAILED: C(5,2) mismatch"))

; ============================================================
; Test 2: C(4,2) = 6
; ============================================================
(print "--- C(4,2) ---")
(define X4 {1 2 3 4})
(define P4 (pow X4))

(define k2-of-4 (filter-card P4 2))
(print "2-element subsets of {1,2,3,4}:")
(print k2-of-4)

(define counted4 (count-card P4 2))
(print "Number of 2-element subsets found:")
(print counted4)

(print "C(4,2) by formula:")
(print (binom 4 2))

(if (= counted4 (binom 4 2))
    (print "VERIFIED: C(4,2) = 6")
    (print "FAILED: C(4,2) mismatch"))

; ============================================================
; Additional: verify all C(5,k) sum to 2^5 = 32
; ============================================================
(print "--- Verify sum of C(5,k) for k=0..5 ---")
(define (sum-binom n k)
  (if (> k n) 0
    (+ (count-card P5 k) (sum-binom n (+ k 1)))))

(define total (sum-binom 5 0))
(print "Sum of C(5,0)+C(5,1)+...+C(5,5):")
(print total)
(if (= total 32)
    (print "VERIFIED: Sum = 2^5 = 32")
    (print "FAILED: Sum mismatch"))

(print "=== Combinations Complete ===")
