; ============================================================
; Program 43: Y Combinator — Recursion Without Named Functions
; Demonstrates fixed-point combinators for anonymous recursion.
; Uses the Z combinator (applicative-order Y) via helper patterns.
; ============================================================

(print "=== Y Combinator ===")

; In a strict language, we use the Z combinator pattern:
; Instead of self-application (which SISP doesn't support as
; first-class lambdas), we pass the recursive function as
; an explicit argument to itself.

; --- Pattern: pass self as first argument ---
; We define a "template" that takes itself as argument,
; then use a driver that feeds it to itself.

; --- Factorial via self-passing ---
(define (fact-self self n)
  (if (= n 0) 1
    (* n (self self (+ n -1)))))

(define (factorial n) (fact-self fact-self n))

(print "-- Factorial via self-passing --")
(print "0! =")
(print (factorial 0))
(print "5! =")
(print (factorial 5))
(print "10! =")
(print (factorial 10))

; --- Fibonacci via self-passing ---
(define (fib-self self n)
  (if (<= n 1) n
    (+ (self self (+ n -1)) (self self (+ n -2)))))

(define (fibonacci n) (fib-self fib-self n))

(print "-- Fibonacci via self-passing --")
(print "fib(0) =")
(print (fibonacci 0))
(print "fib(1) =")
(print (fibonacci 1))
(print "fib(5) =")
(print (fibonacci 5))
(print "fib(10) =")
(print (fibonacci 10))

; --- Generic fixed-point driver ---
; fix takes a "template" function f that expects (self, args...)
; and returns a callable version

(define (fix2 f)
  (define (fixed x) (f fixed x))
  fixed)

; --- Factorial using fix2 ---
(define (fact-template self n)
  (if (= n 0) 1
    (* n (self (+ n -1)))))

(define fact-fixed (fix2 fact-template))

(print "-- Factorial via fix2 --")
(print "7! =")
(print (fact-fixed 7))

; --- Fibonacci using fix2 ---
(define (fib-template self n)
  (if (<= n 1) n
    (+ (self (+ n -1)) (self (+ n -2)))))

(define fib-fixed (fix2 fib-template))

(print "-- Fibonacci via fix2 --")
(print "fib(8) =")
(print (fib-fixed 8))

; --- Labels as anonymous recursion ---
; labels provides local recursive scope, equivalent to Y in effect

(print "-- Factorial via labels --")
(print "6! =")
(print (labels (((f n) (if (= n 0) 1 (* n (f (+ n -1)))))) (f 6)))

(print "-- Sum 1..n via labels --")
(print "sum(100) =")
(print (labels (((s n) (if (= n 0) 0 (+ n (s (+ n -1)))))) (s 100)))

; --- GCD via self-passing (Euclidean algorithm) ---
(define (gcd-self self a b)
  (if (= b 0) a
    (self self b (mod a b))))

(define (my-gcd a b) (gcd-self gcd-self a b))

(print "-- GCD via self-passing --")
(print "gcd(48,18) =")
(print (my-gcd 48 18))
(print "gcd(100,75) =")
(print (my-gcd 100 75))

(print "=== Y Combinator Done ===")
