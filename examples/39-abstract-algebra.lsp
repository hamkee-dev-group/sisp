; ============================================================
; 39 — Rings & Fields: Z_n Multiplication
; Multiplicative structure, zero divisors, units, fields
; UNIQUE: algebraic structures verified via set comprehensions
; ============================================================

(print "=== Abstract Algebra: Rings ===")

; Z_6 under multiplication mod 6
(define Z6 {0 1 2 3 4 5})
(print "Z_6 =") (print Z6)

; Multiplication mod 6
(define (mulmod a b n) (mod (* a b) n))

; --- Cayley Table for × mod 6 ---
(print "--- × mod 6 (partial) ---")
(print "2*3 mod 6 =") (print (mulmod 2 3 6))
(print "3*4 mod 6 =") (print (mulmod 3 4 6))
(print "5*5 mod 6 =") (print (mulmod 5 5 6))

; --- Zero divisors: a*b = 0 but a≠0 and b≠0 ---
(print "--- Zero Divisors in Z_6 ---")
(print "2*3 = 0 mod 6!  2 and 3 are zero divisors")
(print "4*3 = 0 mod 6!  4 and 3 are zero divisors")
(define zero-divisors {2 3 4})
(print "Zero divisors:") (print zero-divisors)

; --- Units: elements with multiplicative inverses ---
(print "--- Units in Z_6 ---")
(print "1*1=1") (print "5*5=") (print (mulmod 5 5 6))
(define units {1 5})
(print "Units:") (print units)
(print "Units and zero divisors disjoint?")
(print (equal (cap units zero-divisors) (diff {1} {1})))

; --- Z_5 is a FIELD (every nonzero element is a unit) ---
(print "--- Z_5: A Field ---")
(define Z5 {0 1 2 3 4})
(print "1*1 mod 5 =") (print (mulmod 1 1 5))
(print "2*3 mod 5 =") (print (mulmod 2 3 5))
(print "3*2 mod 5 =") (print (mulmod 3 2 5))
(print "4*4 mod 5 =") (print (mulmod 4 4 5))
(print "Every nonzero element has inverse:")
(print "  inv(1)=1, inv(2)=3, inv(3)=2, inv(4)=4")
(define units5 {1 2 3 4})
(define nonzero5 (diff Z5 {0}))
(print "Units = Z_5\\{0}?") (print (equal units5 nonzero5))
(print "Z_5 is a field! (5 is prime)")

; --- Z_4: has zero divisors, not a field ---
(print "--- Z_4: Not a Field ---")
(print "2*2 mod 4 =") (print (mulmod 2 2 4))
(print "2 is a zero divisor in Z_4")

; --- Euler's theorem: |units| = phi(n) ---
(print "--- Euler's Totient ---")
(print "phi(6) = |units of Z_6| =") (print (ord units))
(print "phi(5) = |units of Z_5| =") (print (ord units5))

(print "=== Abstract Algebra Done ===")
