; ============================================================
; 48 — Number Theory via Set Comprehension
; GCD, totient, divisors, primes — infinite sets
; UNIQUE: define primes/coprimes as infinite comprehension sets
; ============================================================

(print "=== Number Theory ===")

; --- GCD ---
(define (gcd a b)
  (if (= b 0) a (gcd b (mod a b))))

(print "--- GCD ---")
(print "gcd(12,8) =") (print (gcd 12 8))
(print "gcd(35,14) =") (print (gcd 35 14))
(print "gcd(100,75) =") (print (gcd 100 75))

; --- LCM ---
(define (lcm a b) (/ (* a b) (gcd a b)))
(print "--- LCM ---")
(print "lcm(4,6) =") (print (lcm 4 6))
(print "lcm(3,5) =") (print (lcm 3 5))

; --- Euler's totient ---
(define (phi n)
  (labels (((go k acc)
    (cond ((> k n) acc)
          ((= (gcd k n) 1) (go (+ k 1) (+ acc 1)))
          (t (go (+ k 1) acc)))))
    (go 1 0)))

(print "--- Euler's Totient ---")
(print "phi(1) =") (print (phi 1))
(print "phi(6) =") (print (phi 6))
(print "phi(7) = 7-1 =") (print (phi 7))
(print "phi(10) =") (print (phi 10))
(print "phi(12) =") (print (phi 12))
(print "phi(13) = 13-1 =") (print (phi 13))

; --- Perfect numbers ---
(define (sum-divisors n)
  (labels (((go d acc)
    (cond ((>= d n) acc)
          ((= (mod n d) 0) (go (+ d 1) (+ acc d)))
          (t (go (+ d 1) acc)))))
    (go 1 0)))

(print "--- Perfect Numbers ---")
(print "sigma(6) =") (print (sum-divisors 6))
(print "6 perfect?") (print (= 6 (sum-divisors 6)))
(print "sigma(28) =") (print (sum-divisors 28))
(print "28 perfect?") (print (= 28 (sum-divisors 28)))
(print "12 perfect?") (print (= 12 (sum-divisors 12)))

; --- Infinite coprime set via comprehension ---
(print "--- Coprime to 12 (infinite set) ---")
(define coprime12 {tau : (and (> tau 0) (= (gcd tau 12) 1))})
(print "1 coprime to 12?") (print (in 1 coprime12))
(print "5 coprime to 12?") (print (in 5 coprime12))
(print "6 coprime to 12?") (print (in 6 coprime12))
(print "7 coprime to 12?") (print (in 7 coprime12))
(print "11 coprime to 12?") (print (in 11 coprime12))

; --- Primality ---
(define (primep n)
  (cond ((<= n 1) nil)
        ((= n 2) t)
        ((= (mod n 2) 0) nil)
        (t (labels (((go d)
              (cond ((> (* d d) n) t)
                    ((= (mod n d) 0) nil)
                    (t (go (+ d 2))))))
             (go 3)))))

(print "--- Primes ---")
(print "2?") (print (primep 2))
(print "17?") (print (primep 17))
(print "15?") (print (primep 15))
(print "97?") (print (primep 97))
(print "100?") (print (primep 100))

; --- Twin primes via infinite set ops ---
(define primes-inf {tau : (primep tau)})
(define twin-candidates {tau : (and (primep tau) (primep (+ tau 2)))})
(print "--- Twin Primes (infinite set) ---")
(print "3 twin?") (print (in 3 twin-candidates))
(print "5 twin?") (print (in 5 twin-candidates))
(print "11 twin?") (print (in 11 twin-candidates))
(print "7 twin?") (print (in 7 twin-candidates))
(print "9 twin?") (print (in 9 twin-candidates))

(print "=== Number Theory Done ===")
