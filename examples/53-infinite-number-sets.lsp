; ============================================================
; 53 — Infinite Number-Theoretic Sets
; Primes, squares, triangulars as INFINITE comprehension sets
; TRULY IMPOSSIBLE elsewhere: test membership in infinite sets
; ============================================================

(print "=== Infinite Number-Theoretic Sets ===")

(define (primep n)
  (cond ((<= n 1) nil)
        ((= n 2) t)
        ((= (mod n 2) 0) nil)
        (t (labels (((go d)
              (cond ((> (* d d) n) t)
                    ((= (mod n d) 0) nil)
                    (t (go (+ d 2))))))
             (go 3)))))

; --- Infinite set of primes ---
(define primes {tau : (primep tau)})
(print "--- Primes (infinite set) ---")
(print "2 prime?")  (print (in 2 primes))
(print "7 prime?")  (print (in 7 primes))
(print "10 prime?") (print (in 10 primes))
(print "97 prime?") (print (in 97 primes))
(print "100 prime?")(print (in 100 primes))

; --- Perfect squares ---
(define (isqrt n)
  (labels (((go x)
    (cond ((> (* x x) n) (+ x -1))
          ((= (* x x) n) x)
          (t (go (+ x 1))))))
    (if (< n 0) -1 (go 0))))

(define squares {tau : (and (>= tau 0) (= (* (isqrt tau) (isqrt tau)) tau))})
(print "--- Perfect Squares (infinite set) ---")
(print "0?")   (print (in 0 squares))
(print "4?")   (print (in 4 squares))
(print "9?")   (print (in 9 squares))
(print "10?")  (print (in 10 squares))
(print "144?") (print (in 144 squares))

; --- Triangular numbers: T(n) = n(n+1)/2 ---
(define triangulars {tau : (and (>= tau 0)
  (let ((disc (+ (* 8 tau) 1)))
    (= (* (isqrt disc) (isqrt disc)) disc)))})
(print "--- Triangular Numbers (infinite set) ---")
(print "0?")  (print (in 0 triangulars))
(print "1?")  (print (in 1 triangulars))
(print "3?")  (print (in 3 triangulars))
(print "6?")  (print (in 6 triangulars))
(print "10?") (print (in 10 triangulars))
(print "11?") (print (in 11 triangulars))
(print "55?") (print (in 55 triangulars))

; --- Set operations on infinite sets ---
(print "--- Primes ∩ (4k+1): Fermat primes ---")
(define primes4k1 (cap primes {tau : (equal (mod tau 4) 1)}))
(print "5?")  (print (in 5 primes4k1))
(print "13?") (print (in 13 primes4k1))
(print "7?")  (print (in 7 primes4k1))
(print "29?") (print (in 29 primes4k1))

(print "--- Composite = N>1 \\ Primes ---")
(define composites (diff {tau : (> tau 1)} primes))
(print "4?")  (print (in 4 composites))
(print "9?")  (print (in 9 composites))
(print "7?")  (print (in 7 composites))
(print "15?") (print (in 15 composites))

(print "--- Squares ∩ Triangulars ---")
(define sq-tri (cap squares triangulars))
(print "0?")  (print (in 0 sq-tri))
(print "1?")  (print (in 1 sq-tri))
(print "36?") (print (in 36 sq-tri))
(print "9?")  (print (in 9 sq-tri))

(print "=== Infinite Number Sets Done ===")
