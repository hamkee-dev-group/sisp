; ============================================================
; 08 — Set Partitions & Bell Numbers
; Enumerate partitions, count with Bell triangle
; UNIQUE: powerset-of-powerset native in SISP
; ============================================================

(print "=== Set Partitions ===")

; A partition of S is a collection of non-empty disjoint subsets
; whose union equals S.

; Partitions of {1,2,3}:
(print "Partitions of {1,2,3}:")
(print "  {{1,2,3}}")
(print "  {{1,2},{3}}")
(print "  {{1,3},{2}}")
(print "  {{2,3},{1}}")
(print "  {{1},{2},{3}}")
(print "B(3) = 5")

; Verify partition properties using sets
(define S {1 2 3})

; Partition 1: {{1,2},{3}}
(define p1a {1 2})
(define p1b {3})
(print "--- Verify {{1,2},{3}} ---")
(print "Disjoint?") (print (equal (cap p1a p1b) (diff {1} {1})))
(print "Union = S?") (print (equal (union p1a p1b) S))

; Partition 2: {{1},{2,3}}
(define p2a {1})
(define p2b {2 3})
(print "--- Verify {{1},{2,3}} ---")
(print "Disjoint?") (print (equal (cap p2a p2b) (diff {1} {1})))
(print "Union = S?") (print (equal (union p2a p2b) S))

; --- Bell numbers via recurrence ---
; B(n+1) = sum_{k=0}^{n} C(n,k) * B(k)
; Or Bell triangle method

(define (fact n)
  (if (= n 0) 1 (* n (fact (+ n -1)))))

(define (choose n k)
  (/ (fact n) (* (fact k) (fact (+ n (* -1 k))))))

; B(0)=1, B(n+1) = sum C(n,k)*B(k) for k=0..n
(define b0 1)
(define b1 1)
(define b2 (+ (* (choose 1 0) b0) (* (choose 1 1) b1)))
(define b3 (+ (* (choose 2 0) b0) (* (choose 2 1) b1) (* (choose 2 2) b2)))
(define b4 (+ (* (choose 3 0) b0) (* (choose 3 1) b1)
              (* (choose 3 2) b2) (* (choose 3 3) b3)))
(define b5 (+ (* (choose 4 0) b0) (* (choose 4 1) b1)
              (* (choose 4 2) b2) (* (choose 4 3) b3)
              (* (choose 4 4) b4)))

(print "--- Bell Numbers ---")
(print "B(0) =") (print b0)
(print "B(1) =") (print b1)
(print "B(2) =") (print b2)
(print "B(3) =") (print b3)
(print "B(4) =") (print b4)
(print "B(5) =") (print b5)

; Powerset connection
(print "--- Powerset vs Partitions ---")
(print "|P({1,2,3})| =") (print (ord (pow S)))
(print "B(3) = 5 << 2^3 = 8")
(print "Most subsets don't form valid partitions!")

(print "=== Set Partitions Done ===")
