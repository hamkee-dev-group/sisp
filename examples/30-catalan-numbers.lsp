; ============================================================
; Catalan Numbers via Exact Rational Arithmetic
; C_n = (2n)! / ((n+1)! * n!) = C(2n, n) / (n+1)
; Counts valid parenthesizations, binary trees, etc.
; ============================================================

(print "=== Catalan Numbers ===")

(define (sub a b) (+ a (* -1 b)))

; Factorial
(define (fact n) (if (<= n 1) 1 (* n (fact (sub n 1)))))

; Catalan number via the formula C_n = (2n)! / ((n+1)! * n!)
; This uses exact rational division -- intermediate result is rational,
; but the final answer is always an integer.
(define (catalan n)
  (/ (fact (* 2 n)) (* (fact (+ n 1)) (fact n))))

; --- Compute C_0 through C_10 ---
(print "--- Catalan numbers C_0 through C_10 ---")
(print "C_0 =") (print (catalan 0))
(print "C_1 =") (print (catalan 1))
(print "C_2 =") (print (catalan 2))
(print "C_3 =") (print (catalan 3))
(print "C_4 =") (print (catalan 4))
(print "C_5 =") (print (catalan 5))
(print "C_6 =") (print (catalan 6))
(print "C_7 =") (print (catalan 7))
(print "C_8 =") (print (catalan 8))
(print "C_9 =") (print (catalan 9))
(print "C_10 =") (print (catalan 10))

; --- Verify known values ---
(print "--- Verification ---")
(if (= (catalan 0) 1)  (print "C_0 = 1 VERIFIED")  (print "C_0 FAILED"))
(if (= (catalan 1) 1)  (print "C_1 = 1 VERIFIED")  (print "C_1 FAILED"))
(if (= (catalan 2) 2)  (print "C_2 = 2 VERIFIED")  (print "C_2 FAILED"))
(if (= (catalan 3) 5)  (print "C_3 = 5 VERIFIED")  (print "C_3 FAILED"))
(if (= (catalan 4) 14) (print "C_4 = 14 VERIFIED") (print "C_4 FAILED"))

; --- Recurrence relation ---
; C_{n+1} = C_n * 2(2n+1) / (n+2)
; This is an exact rational multiplication that always yields an integer
(print "--- Recurrence verification ---")
(print "C_5 via recurrence from C_4:")
(define c4-next (/ (* (catalan 4) (* 2 9)) 6))
(print c4-next)
(if (= c4-next (catalan 5))
  (print "Recurrence VERIFIED")
  (print "Recurrence FAILED"))

; --- Intermediate rationals that simplify to integers ---
; The formula (2n)! / ((n+1)! * n!) always gives integers,
; but the intermediate computation goes through rationals.
(print "--- Exact rational intermediates ---")
(print "(2*5)! =") (print (fact 10))
(print "(5+1)! * 5! =") (print (* (fact 6) (fact 5)))
(print "Ratio =") (print (/ (fact 10) (* (fact 6) (fact 5))))

; --- Count valid parenthesizations ---
; C_n counts the number of valid arrangements of n pairs of parentheses
(print "--- Valid parenthesizations ---")
(print "n=0: 1 way (empty)")
(print "n=1: 1 way -> ()")
(print "n=2: 2 ways -> (()), ()()")
(print "n=3: 5 ways -> ((())), (()()), (())(), ()(()), ()()()")
(print "n=4: 14 ways")
(print "These counts match Catalan numbers:")
(print (list (catalan 0) (catalan 1) (catalan 2) (catalan 3) (catalan 4)))

; --- Generate C_n using recursive formula ---
; C_n = sum_{i=0}^{n-1} C_i * C_{n-1-i}
(define (catalan-rec n)
  (if (<= n 1) 1
    (labels (((cat-sum i acc)
      (if (= i n) acc
        (cat-sum (+ i 1) (+ acc (* (catalan-rec i)
                                    (catalan-rec (sub (sub n 1) i))))))))
    (cat-sum 0 0))))

(print "--- Recursive formula verification ---")
(print "C_4 recursive:") (print (catalan-rec 4))
(print "C_5 recursive:") (print (catalan-rec 5))

(print "=== Catalan Numbers Done ===")
