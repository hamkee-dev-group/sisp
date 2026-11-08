; ============================================================
; Newton's Method for Systems of Nonlinear Equations
; f(x,y) = x^2 + y^2 - 4 = 0
; g(x,y) = x^2 - y^2 - 1 = 0
; Jacobian matrix, iterate from (2,1). Verify convergence.
; Exact solution: x = sqrt(5/2), y = sqrt(3/2)
; ============================================================

(print "=== Newton's Method for Systems ===")

(define (ck name ok)
  (cond (ok (print (list 'pass name)))
        (t (print (list 'FAIL name)))))

; Newton step using inverse Jacobian
; J = [[2x, 2y], [2x, -2y]], det = -8xy
; step = J^{-1} * F
;   dx = (f+g)/(4x),  dy = (g-f)/(4y)

; Non-integral float constant to keep arithmetic in floats
(define FOUR 4.000000000000001)

(define (newton-step x y)
  (let* ((f (- (+ (* x x) (* y y)) FOUR))
         (g (- (- (* x x) (* y y)) 1.000000000000001))
         (dx (/ (+ f g) (* 4 x)))
         (dy (/ (- g f) (* 4 y))))
    (list (- x dx) (+ y dy))))

(define (newton-iterate x y max-iter tol)
  (labels
    (((iter xx yy i)
      (cond ((>= i max-iter) (list xx yy i))
            (t (let* ((f (abs (- (+ (* xx xx) (* yy yy)) 4.000000000000001)))
                      (g (abs (- (- (* xx xx) (* yy yy)) 1.000000000000001)))
                      (err (+ f g)))
              (cond ((< err tol) (list xx yy i))
                    (t (let ((np (newton-step xx yy)))
                      (iter (car np) (cadr np) (+ i 1))))))))))
    (iter x y 0)))

(print "Solving: x^2 + y^2 = 4,  x^2 - y^2 = 1")
(print "Starting from (2, 1)")

(define result (newton-iterate 2.000000000000001 1.000000000000001 30 0.00000001))
(define sol-x (car result))
(define sol-y (cadr result))
(define sol-iters (caddr result))

(print "Solution x =") (print sol-x)
(print "Solution y =") (print sol-y)
(print "Iterations =") (print sol-iters)

(define f-val (- (+ (* sol-x sol-x) (* sol-y sol-y)) 4.0))
(define g-val (- (- (* sol-x sol-x) (* sol-y sol-y)) 1.0))
(print "f(x,y) = x^2+y^2-4 =") (print f-val)
(print "g(x,y) = x^2-y^2-1 =") (print g-val)

(ck "converged-f" (< (abs f-val) 0.000001))
(ck "converged-g" (< (abs g-val) 0.000001))

; Exact solution
(define exact-x (sqrt (/ 5 2)))
(define exact-y (sqrt (/ 3 2)))
(print "Exact x = sqrt(5/2) =") (print exact-x)
(print "Exact y = sqrt(3/2) =") (print exact-y)
(ck "x-near-exact" (< (abs (- sol-x exact-x)) 0.000001))
(ck "y-near-exact" (< (abs (- sol-y exact-y)) 0.000001))

; Rational approximations
(define rat-x (rationalize sol-x 0.000001))
(define rat-y (rationalize sol-y 0.000001))
(print "Rational approx x =") (print rat-x)
(print "Rational approx y =") (print rat-y)

; Test another starting point
(define result2 (newton-iterate 3.000000000000001 2.000000000000001 30 0.00000001))
(define sol-x2 (car result2))
(define sol-y2 (cadr result2))
(ck "x2-near-exact" (< (abs (- sol-x2 exact-x)) 0.000001))
(ck "y2-near-exact" (< (abs (- sol-y2 exact-y)) 0.000001))

(print "=== Newton's Method for Systems Done ===")
