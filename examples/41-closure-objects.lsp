; ============================================================
; Program 41: Closure-Based Objects with Mutable State
; Message-passing OOP style using closures and global state.
; Demonstrates: closures, define, cond, recursion.
; ============================================================

(print "=== Closure-Based Objects ===")

; --- Counter Object ---
; Uses global state with a unique prefix per "instance"

(define counter-val 0)

(define (counter-new) (define counter-val 0))

(define (counter-inc)
  (define counter-val (+ counter-val 1))
  counter-val)

(define (counter-dec)
  (define counter-val (+ counter-val -1))
  counter-val)

(define (counter-get) counter-val)

(print "-- Counter --")
(counter-new)
(print "Initial:")
(print (counter-get))
(print "After 3 increments:")
(counter-inc)
(counter-inc)
(counter-inc)
(print (counter-get))
(print "After 1 decrement:")
(counter-dec)
(print (counter-get))

; --- Stack Object ---
; Stack as a global list with push/pop operations

(define stack-data nil)

(define (stack-new) (define stack-data nil))

(define (stack-push-val x)
  (define stack-data (cons x stack-data))
  stack-data)

(define (stack-pop-val)
  (if (null stack-data) nil
    (let ((top (car stack-data)))
      (define stack-data (cdr stack-data))
      top)))

(define (stack-peek)
  (if (null stack-data) nil (car stack-data)))

(define (stack-size)
  (if (null stack-data) 0 (ord stack-data)))

(define (stack-empty) (null stack-data))

(print "-- Stack --")
(stack-new)
(print "Push 10, 20, 30:")
(stack-push-val 10)
(stack-push-val 20)
(stack-push-val 30)
(print (cat "Size: " (cat "" stack-data)))
(print (stack-size))
(print "Peek:")
(print (stack-peek))
(print "Pop:")
(print (stack-pop-val))
(print "Pop:")
(print (stack-pop-val))
(print "Remaining size:")
(print (stack-size))

; --- Accumulator ---
; Returns running sum of all values added

(define accum-total 0)

(define (accum-new) (define accum-total 0))

(define (accum-add x)
  (define accum-total (+ accum-total x))
  accum-total)

(define (accum-get) accum-total)

(print "-- Accumulator --")
(accum-new)
(print "Add 5:")
(print (accum-add 5))
(print "Add 10:")
(print (accum-add 10))
(print "Add 1/2:")
(print (accum-add 1/2))
(print "Total:")
(print (accum-get))

; --- Closure Factory: make-adder ---
; True closures that capture their environment

(define (make-adder n)
  (define (adder x) (+ x n))
  adder)

(define add3 (make-adder 3))
(define add10 (make-adder 10))

(print "-- Closure Factory --")
(print "add3(7):")
(print (add3 7))
(print "add10(7):")
(print (add10 7))
(print "add3(add10(0)):")
(print (add3 (add10 0)))

(print "=== Closure Objects Done ===")
