; ============================================================
; 52 — Peano Arithmetic
; Natural numbers as nested cons: 0=nil, S(n)=(cons nil n)
; Arithmetic from first principles — UNIQUE foundation
; ============================================================

(print "=== Peano Arithmetic ===")

; Peano representation: 0 = nil, S(n) = (nil . n)
(define p-zero nil)
(define (p-succ n) (cons nil n))

; Build naturals
(define p0 p-zero)
(define p1 (p-succ p0))
(define p2 (p-succ p1))
(define p3 (p-succ p2))
(define p4 (p-succ p3))
(define p5 (p-succ p4))

; Convert Peano → integer
(define (p-to-int n)
  (if (null n) 0 (+ 1 (p-to-int (cdr n)))))

; Convert integer → Peano
(define (int-to-p n)
  (if (= n 0) nil (p-succ (int-to-p (+ n -1)))))

(print "--- Representation ---")
(print "P(0) =") (print p0)
(print "P(1) =") (print p1)
(print "P(2) =") (print p2)
(print "P(3) =") (print p3)
(print "P(3) → int:") (print (p-to-int p3))
(print "P(5) → int:") (print (p-to-int p5))

; --- Peano addition: a + 0 = a, a + S(b) = S(a+b) ---
(define (p-add a b)
  (if (null b) a (p-succ (p-add a (cdr b)))))

(print "--- Addition ---")
(print "2+3 =") (print (p-to-int (p-add p2 p3)))
(print "0+5 =") (print (p-to-int (p-add p0 p5)))
(print "4+1 =") (print (p-to-int (p-add p4 p1)))

; --- Peano multiplication: a * 0 = 0, a * S(b) = a + (a*b) ---
(define (p-mul a b)
  (if (null b) nil (p-add a (p-mul a (cdr b)))))

(print "--- Multiplication ---")
(print "2*3 =") (print (p-to-int (p-mul p2 p3)))
(print "3*4 =") (print (p-to-int (p-mul p3 p4)))
(print "5*0 =") (print (p-to-int (p-mul p5 p0)))
(print "1*5 =") (print (p-to-int (p-mul p1 p5)))

; --- Peano predecessor ---
(define (p-pred n) (if (null n) nil (cdr n)))

(print "--- Predecessor ---")
(print "pred(3) =") (print (p-to-int (p-pred p3)))
(print "pred(1) =") (print (p-to-int (p-pred p1)))
(print "pred(0) =") (print (p-to-int (p-pred p0)))

; --- Peano comparison ---
(define (p-less a b)
  (cond ((null b) nil)
        ((null a) t)
        (t (p-less (cdr a) (cdr b)))))

(print "--- Comparison ---")
(print "2 < 3?") (print (p-less p2 p3))
(print "3 < 2?") (print (p-less p3 p2))
(print "2 < 2?") (print (p-less p2 p2))

; --- Verify arithmetic laws ---
(print "--- Verify Laws ---")
(print "Commutative: 2+3 = 3+2?")
(print (= (p-to-int (p-add p2 p3)) (p-to-int (p-add p3 p2))))
(print "Associative: (1+2)+3 = 1+(2+3)?")
(print (= (p-to-int (p-add (p-add p1 p2) p3))
          (p-to-int (p-add p1 (p-add p2 p3)))))
(print "Distributive: 2*(3+4) = 2*3+2*4?")
(print (= (p-to-int (p-mul p2 (p-add p3 p4)))
          (p-to-int (p-add (p-mul p2 p3) (p-mul p2 p4)))))

(print "=== Peano Arithmetic Done ===")
