; ============================================================
; 37-sieve-eratosthenes.lsp
; Implement the Sieve of Eratosthenes using set difference.
; Start with {2,...,n}, remove multiples using set diff.
; Output the set of primes <= n.
; ============================================================

(print "=== Sieve of Eratosthenes via Set Difference ===")

; --- Build the set of multiples of p up to limit ---
(define (multiples-set p limit)
  (labels (((build k)
    (if (> k limit) nil
      (cons k (build (+ k p))))))
    (build (* p 2))))

; --- Convert list to a set by iterating and using union ---
; We work with lists internally and use set diff at the end
(define (list-to-set lst)
  (if (equal lst nil) nil
    (if (equal (cdr lst) nil)
        (cons (car lst) nil)
        (cons (car lst) (list-to-set (cdr lst))))))

; --- Remove all elements of `to-remove` from `lst` ---
(define (remove-all lst to-remove)
  (if (equal lst nil) nil
    (if (my-memberp (car lst) to-remove)
        (remove-all (cdr lst) to-remove)
        (cons (car lst) (remove-all (cdr lst) to-remove)))))

(define (my-memberp x lst)
  (if (equal lst nil) nil
    (if (= (car lst) x) t
        (my-memberp x (cdr lst)))))

; --- Sieve: starting from current prime p, remove multiples ---
(define (sieve candidates p limit)
  (if (> (* p p) limit) candidates
    (if (my-memberp p candidates)
        (let ((mults (multiples-set p limit)))
          (sieve (remove-all candidates mults) (+ p 1) limit))
        (sieve candidates (+ p 1) limit))))

; --- Main sieve function ---
(define (primes-up-to n)
  (sieve (seq 2 n) 2 n))

; ============================================================
; Compute primes up to 30
; ============================================================
(print "--- Primes up to 30 ---")
(define p30 (primes-up-to 30))
(print p30)

; Verify against known primes <= 30
(define expected '(2 3 5 7 11 13 17 19 23 29))
(if (equal p30 expected)
    (print "VERIFIED: Primes up to 30 correct")
    (print "FAILED: Mismatch"))

; ============================================================
; Count primes up to 50
; ============================================================
(print "--- Primes up to 50 ---")
(define p50 (primes-up-to 50))
(print p50)
(print "Number of primes up to 50:")
(print (ord p50))
(if (= (ord p50) 15)
    (print "VERIFIED: pi(50) = 15")
    (print "FAILED"))

; ============================================================
; Now demonstrate using actual SISP set difference
; Build the candidate set and remove multiples with set ops
; ============================================================
(print "--- Set-difference approach for primes up to 20 ---")

(define candidates {2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20})

; Remove multiples of 2
(define step1 (\ candidates {4 6 8 10 12 14 16 18 20}))
(print "After removing multiples of 2:")
(print step1)

; Remove multiples of 3
(define step2 (\ step1 {6 9 12 15 18}))
(print "After removing multiples of 3:")
(print step2)

; Remove multiples of 4 (already handled, but safe)
; 4 is already gone, skip. Next prime is 5: 5^2=25 > 20, done.
(print "Primes up to 20 via set difference:")
(print step2)

; Verify count
(print "Count:")
(print (ord step2))
(if (= (ord step2) 8)
    (print "VERIFIED: pi(20) = 8")
    (print "FAILED"))

(print "=== Sieve Complete ===")
