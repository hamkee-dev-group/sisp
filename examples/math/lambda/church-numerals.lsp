; ============================================================
; 42 — Church Numerals in SISP
; Numbers encoded as Peano-style nested lists
; Church numeral n = n applications of successor to zero
; ============================================================

(print "=== Church Numerals ===")

; Represent Church numeral n as n nested cons:
; 0 = nil, 1 = (nil), 2 = (nil nil), 3 = (nil nil nil)
; This is equivalent to "apply succ n times"

(define c0 nil)
(define c1 (cons nil nil))
(define c2 (cons nil (cons nil nil)))
(define c3 (cons nil (cons nil (cons nil nil))))
(define c4 (cons nil (cons nil (cons nil (cons nil nil)))))
(define c5 (cons nil (cons nil (cons nil (cons nil (cons nil nil))))))

; Church to integer
(define (church-to-int n)
  (if (null n) 0 (+ 1 (church-to-int (cdr n)))))

(print "--- Church to Integer ---")
(print "c0 =") (print (church-to-int c0))
(print "c1 =") (print (church-to-int c1))
(print "c2 =") (print (church-to-int c2))
(print "c3 =") (print (church-to-int c3))
(print "c5 =") (print (church-to-int c5))

; Church successor
(define (church-succ n) (cons nil n))

(print "--- Successor ---")
(print "succ(3) =") (print (church-to-int (church-succ c3)))
(print "succ(5) =") (print (church-to-int (church-succ c5)))

; Church addition: add a b = apply succ b times to a
(define (church-add a b)
  (if (null b) a (church-add (church-succ a) (cdr b))))

(print "--- Addition ---")
(print "2+3 =") (print (church-to-int (church-add c2 c3)))
(print "0+5 =") (print (church-to-int (church-add c0 c5)))
(print "4+1 =") (print (church-to-int (church-add c4 c1)))

; Church multiplication: mul a b = add a to itself b times
(define (church-mul a b)
  (if (null b) nil (church-add a (church-mul a (cdr b)))))

(print "--- Multiplication ---")
(print "2*3 =") (print (church-to-int (church-mul c2 c3)))
(print "3*4 =") (print (church-to-int (church-mul c3 c4)))
(print "0*5 =") (print (church-to-int (church-mul c0 c5)))

; Church predecessor
(define (church-pred n)
  (if (null n) nil (cdr n)))

(print "--- Predecessor ---")
(print "pred(5) =") (print (church-to-int (church-pred c5)))
(print "pred(1) =") (print (church-to-int (church-pred c1)))
(print "pred(0) =") (print (church-to-int (church-pred c0)))

; Church subtraction (monus)
(define (church-sub a b)
  (if (null b) a (church-sub (church-pred a) (cdr b))))

(print "--- Subtraction ---")
(print "5-2 =") (print (church-to-int (church-sub c5 c2)))
(print "3-3 =") (print (church-to-int (church-sub c3 c3)))

; Church isZero
(define (church-zero-p n) (null n))
(print "--- isZero ---")
(print "isZero(0)?") (print (church-zero-p c0))
(print "isZero(3)?") (print (church-zero-p c3))

; Verify arithmetic law: a*(b+c) = a*b + a*c
(print "--- Distributive law ---")
(print "2*(3+4) =") (print (church-to-int (church-mul c2 (church-add c3 c4))))
(print "2*3+2*4 =") (print (church-to-int (church-add (church-mul c2 c3) (church-mul c2 c4))))

(print "=== Church Numerals Done ===")
