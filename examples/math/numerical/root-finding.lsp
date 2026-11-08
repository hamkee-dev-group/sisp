; ============================================================
; root-finding.lsp — Newton, bisection, secant. Convergence.
; Roots of x^3-2x-5, sin(x)-x/2. Deflation.
; ============================================================

(print "=== Root-Finding ===")


; float-converter to keep arithmetic in float domain
(define (asf x) (+ x 0.000000000000001))

; --- f(x)=x^3-2x-5   g(x)=sin(x)-x/2 ---
(define (cubicf x) (- (* x x x) (* 2 x) 5))
(define (cubic-df x) (- (* 3 x x) 2))
(define (sinef x) (- (sin x) (* 0.5 x)))
(define (sine-df x) (- (cos x) 0.5))

; Newton's method — all arithmetic kept in float by asf
(define (newton f df x0 tol maxit)
  (let ((x0f (+ x0 0.000000000000001)))
    (labels
      (((iter x i)
        (cond ((>= i maxit) (list x i nil))
              (t (let ((fx (f x)) (dx (df x)))
                (cond ((< (abs fx) tol) (list x i t))
                      ((< (abs dx) 0.000000000000001) (list x i nil))
                      (t (iter (- x (/ fx dx)) (+ i 1)))))))))
      (iter x0f 0))))

; Bisection method
(define (bisection f a b tol maxit)
  (let ((af (+ a 0.000000000000001)) (bf (+ b 0.000000000000001)))
    (labels
      (((iter a b fa fb i)
        (cond ((>= i maxit) (list (/ (+ a b) 2) i nil))
              (t (let* ((m (/ (+ a b) 2))
                         (fm (f m)))
                (cond ((< (abs fm) tol) (list m i t))
                      ((< (* fa fm) 0) (iter a m fa fm (+ i 1)))
                      (t (iter m b fm fb (+ i 1)))))))))
      (let ((fa (f af)) (fb (f bf)))
        (cond ((< (* fa fb) 0) (iter af bf fa fb 0))
              (t (list af 0 nil)))))))

; Secant method
(define (secant f x0 x1 tol maxit)
  (let ((x0f (+ x0 0.000000000000001)) (x1f (+ x1 0.000000000000001)))
    (labels
      (((iter x0 x1 f0 f1 i)
        (cond ((>= i maxit) (list x1 i nil))
              ((< (abs (- f1 f0)) 0.000000000000001) (list x1 i nil))
              (t (let* ((x2 (- x1 (* f1 (/ (- x1 x0) (- f1 f0)))))
                         (f2 (f x2)))
                (cond ((< (abs f2) tol) (list x2 i t))
                      (t (iter x1 x2 f1 f2 (+ i 1)))))))))
      (let ((f0 (f x0f)) (f1 (f x1f)))
        (iter x0f x1f f0 f1 0)))))

; --- Find root of x^3-2x-5 ---
(print "f(x)=x^3-2x-5, actual root ~2.0945514815")
(define r1 (newton cubicf cubic-df 2.0 0.00000001 50))
(print "Newton root =") (print (car r1))
(print "Newton iters =") (print (cadr r1))
(check "newton-cubic" (< (abs (- (car r1) 2.094551481542327)) 0.0001))

(define r2 (bisection cubicf 2.0 3.0 0.00000001 50))
(print "Bisection root =") (print (car r2))
(print "Bisection iters =") (print (cadr r2))
(check "bisection-cubic" (< (abs (- (car r2) 2.094551481542327)) 0.0001))

(define r3 (secant cubicf 2.0 3.0 0.00000001 50))
(print "Secant root =") (print (car r3))
(print "Secant iters =") (print (cadr r3))
(check "secant-cubic" (< (abs (- (car r3) 2.094551481542327)) 0.0001))

; --- Root of sin(x)-x/2 ---
(print "g(x)=sin(x)-x/2")
(define r4 (newton sinef sine-df 1.0 0.00000001 50))
(print "Newton root =") (print (car r4))
(check "newton-sine" (< (abs (sinef (car r4))) 0.0001))

(define r5 (bisection sinef 1.5 2.0 0.00000001 50))
(print "Bisection root =") (print (car r5))
(check "bisection-sine" (< (abs (sinef (car r5))) 0.0001))

; Convergence comparison
(print "Convergence: Newton-cubic=" (cadr r1) " Bisection-cubic=" (cadr r2))
(check "newton-fewer-iters" (< (cadr r1) (cadr r2)))

; --- Deflation for cubic ---
(define root-cubic (car r1))
(define (deflated x) (/ (cubicf x) (- x root-cubic)))
(define dl1 (deflated 1.0))
(define dl3 (deflated 3.0))
(print "Deflated f(1)=" dl1 " f(3)=" dl3)
(check "deflation-works" (< (abs dl1) 20))
(check "deflation-works2" (< (abs dl3) 20))

(print "=== Root-Finding Done ===")
