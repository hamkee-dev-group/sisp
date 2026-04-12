; ============================================================
; Egyptian Fraction Decomposition (Fibonacci-Sylvester Greedy)
; Decompose p/q into a sum of distinct unit fractions 1/n.
; Uses exact rational arithmetic throughout.
; ============================================================

(print "=== Egyptian Fraction Decomposition ===")

; --- Helpers ---
(define (sub a b) (+ a (* -1 b)))

(define (gcd-fn a b) (if (= b 0) a (gcd-fn b (mod a b))))

(define (floor-div a b) (/ (sub a (mod a b)) b))
(define (ceil-div a b) (if (= (mod a b) 0) (/ a b) (+ 1 (floor-div a b))))

; Egyptian decomposition: given integers p, q with 0 < p/q < 1,
; return list of denominators (n1 n2 ...) such that p/q = 1/n1 + 1/n2 + ...
(define (egyptian p q)
  (if (= p 0) nil
    (if (= p 1) (list q)
      (let* ((n (ceil-div q p))
             (new-p (sub (* n p) q))
             (new-q (* n q))
             (g (gcd-fn new-p new-q)))
        (cons n (egyptian (/ new-p g) (/ new-q g)))))))

; Sum a list of unit fractions 1/n to verify decomposition
(define (sum-units lst)
  (if (equal lst nil) 0
    (+ (/ 1 (car lst)) (sum-units (cdr lst)))))

; Pretty-print a decomposition
(define (show-egyptian p q)
  (let ((denoms (egyptian p q)))
    (print "---")
    (print "Fraction:")
    (print (/ p q))
    (print "Unit fraction denominators:")
    (print denoms)
    (print "Verification sum:")
    (print (sum-units denoms))
    (if (= (sum-units denoms) (/ p q))
      (print "VERIFIED: sum matches original")
      (print "ERROR: sum does not match"))))

; --- Decompose several fractions ---

; 5/7 = 1/2 + 1/5 + 1/70
(show-egyptian 5 7)

; 3/7 = 1/3 + 1/11 + 1/231
(show-egyptian 3 7)

; 2/3 = 1/2 + 1/6
(show-egyptian 2 3)

; 4/5 = 1/2 + 1/4 + 1/20
(show-egyptian 4 5)

; 7/11 = ?
(show-egyptian 7 11)

; 5/13 = ?
(show-egyptian 5 13)

; 8/15 = 1/2 + 1/30
(show-egyptian 8 15)

; --- Show that exact arithmetic keeps everything precise ---
(print "---")
(print "Exact arithmetic showcase:")
(print "1/2 + 1/5 + 1/70 computed exactly:")
(print (+ 1/2 1/5 1/70))

(print "=== Egyptian Fractions Done ===")
