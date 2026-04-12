; ============================================================
; 12 — Infinite Set Operations
; IMPOSSIBLE in any other language: operate on INFINITE sets
; Union, intersection, complement, diff of infinite sets
; Test membership on lazily-defined mathematical sets
; ============================================================

(print "=== Infinite Set Operations ===")

; Define infinite sets by comprehension
(define evens    {tau : (equal (mod tau 2) 0)})
(define odds     {tau : (not (equal (mod tau 2) 0))})
(define mult3    {tau : (equal (mod tau 3) 0)})
(define mult5    {tau : (equal (mod tau 5) 0)})
(define primes-small {2 3 5 7 11 13 17 19 23 29})

; --- Union of infinite sets ---
(print "--- evens ∪ odds = all integers ---")
(define all-ints (union evens odds))
(print "42 in evens∪odds?") (print (in 42 all-ints))
(print "77 in evens∪odds?") (print (in 77 all-ints))
(print "0 in evens∪odds?")  (print (in 0 all-ints))

; --- Intersection of infinite sets ---
(print "--- evens ∩ mult3 = multiples of 6 ---")
(define mult6 (cap evens mult3))
(print "6 in evens∩mult3?")  (print (in 6 mult6))
(print "12 in evens∩mult3?") (print (in 12 mult6))
(print "9 in evens∩mult3?")  (print (in 9 mult6))
(print "4 in evens∩mult3?")  (print (in 4 mult6))

; --- Complement of infinite sets ---
(print "--- comp(evens) = odds ---")
(define not-even (comp evens))
(print "3 in comp(evens)?") (print (in 3 not-even))
(print "4 in comp(evens)?") (print (in 4 not-even))

; --- Difference of infinite sets ---
(print "--- mult3 \\ evens = odd multiples of 3 ---")
(define odd-mult3 (diff mult3 evens))
(print "3 in mult3\\evens?")  (print (in 3 odd-mult3))
(print "9 in mult3\\evens?")  (print (in 9 odd-mult3))
(print "6 in mult3\\evens?")  (print (in 6 odd-mult3))
(print "15 in mult3\\evens?") (print (in 15 odd-mult3))

; --- Symmetric difference ---
(print "--- mult3 △ mult5 ---")
(define sd35 (symdiff mult3 mult5))
(print "3 in symdiff?")  (print (in 3 sd35))
(print "5 in symdiff?")  (print (in 5 sd35))
(print "15 in symdiff?") (print (in 15 sd35))
(print "7 in symdiff?")  (print (in 7 sd35))

; --- FizzBuzz via infinite sets! ---
(print "--- FizzBuzz via Set Theory ---")
(define fizzbuzz (cap mult3 mult5))
(define fizz-only (diff mult3 mult5))
(define buzz-only (diff mult5 mult3))
(print "15 = FizzBuzz?") (print (in 15 fizzbuzz))
(print "9 = Fizz only?") (print (in 9 fizz-only))
(print "10 = Buzz only?") (print (in 10 buzz-only))

; --- Cartesian product of infinite with finite ---
(print "--- prod({1,2}, evens) ---")
(define cp (prod {1 2} evens))
(print "(1.4) in prod?")  (print (in '(1 . 4) cp))
(print "(2.6) in prod?")  (print (in '(2 . 6) cp))
(print "(1.3) in prod?")  (print (in '(1 . 3) cp))
(print "(3.4) in prod?")  (print (in '(3 . 4) cp))

(print "=== Infinite Set Operations Done ===")
