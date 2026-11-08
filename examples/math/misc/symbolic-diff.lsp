; ============================================================
; Program 44: Symbolic Differentiation
; Differentiate symbolic expressions with +, *, ^, constants.
; Includes algebraic simplification of results.
; ============================================================

(print "=== Symbolic Differentiation ===")

; --- Simplification rules ---
(define (simplify expr)
  (if (atom expr) expr
    (let* ((op (car expr))
           (a (simplify (car (cdr expr))))
           (b (simplify (car (cdr (cdr expr))))))
      (cond
        ; 0 + x = x, x + 0 = x
        ((and (equal op '+) (equal a 0)) b)
        ((and (equal op '+) (equal b 0)) a)
        ; x + x = (* 2 x)
        ((and (equal op '+) (numberp a) (numberp b)) (+ a b))
        ; 0 * x = 0, x * 0 = 0
        ((and (equal op '*) (equal a 0)) 0)
        ((and (equal op '*) (equal b 0)) 0)
        ; 1 * x = x, x * 1 = x
        ((and (equal op '*) (equal a 1)) b)
        ((and (equal op '*) (equal b 1)) a)
        ; numeric * numeric
        ((and (equal op '*) (numberp a) (numberp b)) (* a b))
        ; x ^ 0 = 1, x ^ 1 = x
        ((and (equal op '^) (equal b 0)) 1)
        ((and (equal op '^) (equal b 1)) a)
        (t (list op a b))))))

; --- Differentiation with respect to variable var ---
(define (deriv expr var)
  (cond
    ; d/dx(constant) = 0
    ((numberp expr) 0)
    ; d/dx(x) = 1, d/dx(y) = 0
    ((atom expr)
     (if (equal expr var) 1 0))
    ; d/dx(a + b) = da + db
    ((equal (car expr) '+)
     (simplify (list '+
       (deriv (car (cdr expr)) var)
       (deriv (car (cdr (cdr expr))) var))))
    ; d/dx(a * b) = a*db + da*b  (product rule)
    ((equal (car expr) '*)
     (simplify (list '+
       (simplify (list '* (car (cdr expr))
                          (deriv (car (cdr (cdr expr))) var)))
       (simplify (list '* (deriv (car (cdr expr)) var)
                          (car (cdr (cdr expr))))))))
    ; d/dx(a ^ n) = n * a^(n-1) * da  (power rule)
    ((equal (car expr) '^)
     (let ((base (car (cdr expr)))
           (exp  (car (cdr (cdr expr)))))
       (simplify (list '*
         (simplify (list '* exp (simplify (list '^ base (+ exp -1)))))
         (deriv base var)))))
    (t 0)))

; --- Test cases ---
(print "-- d/dx(x) --")
(print (deriv 'x 'x))

(print "-- d/dx(5) --")
(print (deriv 5 'x))

(print "-- d/dx(x + 3) --")
(print (deriv '(+ x 3) 'x))

(print "-- d/dx(x * x) --")
(print (deriv '(* x x) 'x))

(print "-- d/dx(x^3) --")
(print (deriv '(^ x 3) 'x))

(print "-- d/dx(x^2 + x) --")
(print (deriv '(+ (^ x 2) x) 'x))

(print "-- d/dx(3 * x) --")
(print (deriv '(* 3 x) 'x))

(print "-- d/dx(x * (x + 3)) --")
(print (deriv '(* x (+ x 3)) 'x))

(print "-- d/dx(5 * x^2) --")
(print (deriv '(* 5 (^ x 2)) 'x))

(print "-- d/dy(x + y) with respect to y --")
(print (deriv '(+ x y) 'y))

(print "-- Second derivative: d^2/dx^2(x^3) --")
(define first-d (deriv '(^ x 3) 'x))
(print first-d)
(print (deriv first-d 'x))

(print "=== Symbolic Differentiation Done ===")
