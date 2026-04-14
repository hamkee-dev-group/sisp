; ============================================================
; Program 47: Matrix Chain Multiplication (Memoized DP)
; Compute optimal parenthesization using association list memo.
; Demonstrates closures for memoization, dynamic programming.
; ============================================================

(print "=== Matrix Chain Multiplication ===")

; Dimensions: matrix i has dimensions dims[i] x dims[i+1]
; We want to minimize total scalar multiplications.

; --- Memoization table (global association list) ---
(define memo-table nil)

(define (memo-key i j) (+ (* i 100) j))

(define (memo-get i j)
  (let ((k (memo-key i j)))
    (assoc k memo-table)))

(define (memo-put i j val)
  (let ((k (memo-key i j)))
    (define memo-table (cons (list k val) memo-table))
    val))

; --- Get dimension from dims list ---
(define (get-dim dims i)
  (nth i dims))

; --- Find minimum of cost over split points ---
(define (find-min-split dims i j best-cost)
  (find-min-k dims i j i best-cost))

(define (find-min-k dims i j k best)
  (if (= k j) best
    (let* ((left-cost (mcm dims i k))
           (right-cost (mcm dims (+ k 1) j))
           (split-cost (+ (+ left-cost right-cost)
                         (* (* (get-dim dims (+ i -1))
                               (get-dim dims k))
                            (get-dim dims j))))
           (new-best (if (= best -1) split-cost
                       (if (< split-cost best) split-cost best))))
      (find-min-k dims i j (+ k 1) new-best))))

; --- Matrix Chain Multiplication with memoization ---
(define (mcm dims i j)
  (if (= i j) 0
    (let ((cached (memo-get i j)))
      (if (not (null cached))
        (car (cdr cached))
        (let ((result (find-min-split dims i j -1)))
          (memo-put i j result))))))

; --- Driver: reset memo and solve ---
(define (matrix-chain dims)
  (define memo-table nil)
  (let ((n (+ (ord dims) -1)))
    (mcm dims 1 n)))

; --- Test Case 1: dims = (10 30 5 60) ---
; Matrices: A1=10x30, A2=30x5, A3=5x60
; Optimal: (A1*A2)*A3 = 10*30*5 + 10*5*60 = 1500+3000 = 4500
(print "-- Test 1: dims = (10 30 5 60) --")
(print "Optimal cost:")
(print (matrix-chain (list 10 30 5 60)))

; --- Test Case 2: dims = (40 20 30 10 30) ---
; Matrices: A1=40x20, A2=20x30, A3=30x10, A4=10x30
(print "-- Test 2: dims = (40 20 30 10 30) --")
(print "Optimal cost:")
(print (matrix-chain (list 40 20 30 10 30)))

; --- Test Case 3: dims = (10 20 30 40 30) ---
(print "-- Test 3: dims = (10 20 30 40 30) --")
(print "Optimal cost:")
(print (matrix-chain (list 10 20 30 40 30)))

; --- Test Case 4: Two matrices (trivial) ---
(print "-- Test 4: dims = (10 20 30) --")
(print "Optimal cost:")
(print (matrix-chain (list 10 20 30)))

; --- Demonstrate memoization effect ---
(print "-- Memo table size after test 2 --")
(define memo-table nil)
(define result2 (matrix-chain (list 40 20 30 10 30)))
(print "Entries cached:")
(print (ord memo-table))

; --- Simple cost calculator without memo (for comparison) ---
(define (naive-mcm dims i j)
  (if (= i j) 0
    (find-min-naive dims i j i -1)))

(define (find-min-naive dims i j k best)
  (if (= k j) best
    (let* ((left (naive-mcm dims i k))
           (right (naive-mcm dims (+ k 1) j))
           (cost (+ (+ left right)
                    (* (* (get-dim dims (+ i -1))
                          (get-dim dims k))
                       (get-dim dims j))))
           (new-best (if (= best -1) cost
                       (if (< cost best) cost best))))
      (find-min-naive dims i j (+ k 1) new-best))))

(print "-- Naive (no memo) verification --")
(print (naive-mcm (list 10 30 5 60) 1 3))

(print "=== Matrix Chain Done ===")
