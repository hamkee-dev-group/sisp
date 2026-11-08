; ============================================================
; lib/math/numerical.lsp -- root finding, quadrature, ODE stepping
;
; Public:   (bisect-root f a b tol maxit) -> (root iterations converged)
;           (newton-root f df x0 tol maxit) -> (root iterations converged)
;           (secant-root f x0 x1 tol maxit) -> (root iterations converged)
;           (simpson f a b n) -> integral estimate (n even)
;           (gauss5 f a b) -> integral estimate (5-point Gauss-Legendre)
;           (rk4 f y0 t0 t1 h) -> y(t1) estimate, f = (lambda (t y) ...)
; Private:  num-as-float, num-sign-change-p, num-bisect-walk,
;           num-newton-walk, num-secant-walk, num-simpson-walk,
;           num-gauss5-walk, num-rk4-walk, num-rk4-step
; Depends:  none (uses builtins sin, exp, abs, sqrt only in tests)
; Derived-from: standard numerical methods; test vectors follow
;               examples/math/numerical/root-finding.lsp and quadrature.lsp,
;               with values recomputed independently
; Assumes:  bisect-root requires f(a) and f(b) to have opposite signs
;           (checked, else error); converged is t when |f(root)| < tol
;           (a residual criterion, not a distance criterion); simpson
;           requires even n (checked, else error).
;
; Numeric policy: the root finders coerce their starting points to
;           floats at entry -- Newton/secant iteration in EXACT
;           rationals explodes 64-bit numerators in a few steps.
;           Quadrature and rk4 are left in the caller's domain: exact
;           inputs give exact rational answers, float inputs give floats.
;           (num-as-float adds 1e-15: SISP normalizes integral-valued
;           floats back to integers, so x*1.0 collapses; an additive
;           non-integral perturbation is the only float-forcing that
;           works.  The 1e-15 distortion is far below any root
;           tolerance this module is meant for.)
;
; ============================================================

(define (num-as-float x) (+ x 0.000000000000001))

; ---- root finding: uniform (root iterations converged) return ----

(define (num-sign-change-p fa fb)
  (cond ((= fa 0) nil)
        ((= fb 0) nil)
        ((< fa 0) (> fb 0))
        (t (< fb 0))))

(define (num-bisect-walk f a b fa fb tol maxit iters)
  (cond ((>= iters maxit) (list (/ (+ a b) 2) iters nil))
        (t (let* ((m (/ (+ a b) 2))
                  (fm (f m)))
             (cond ((< (abs fm) tol) (list m (+ iters 1) t))
                   ((num-sign-change-p fa fm)
                    (num-bisect-walk f a m fa fm tol maxit (+ iters 1)))
                   (t (num-bisect-walk f m b fm fb tol maxit (+ iters 1))))))))

(define (bisect-root f a b tol maxit)
  "root of f on [a b] by bisection; f(a) and f(b) must straddle zero"
  (let ((fa (f a)) (fb (f b)))
    (cond ((= fa 0) (list a 0 t))
          ((= fb 0) (list b 0 t))
          ((num-sign-change-p fa fb)
           (num-bisect-walk f (num-as-float a) (num-as-float b)
                            (num-as-float fa) (num-as-float fb) tol maxit 0))
          (t (error "BISECT-ROOT: NO SIGN CHANGE ON INTERVAL" (list a b))))))

(define (num-newton-walk f df x tol maxit iters)
  (cond ((>= iters maxit) (list x iters nil))
        (t (let ((fx (f x)))
             (cond ((< (abs fx) tol) (list x iters t))
                   (t (num-newton-walk f df (- x (/ fx (df x)))
                                       tol maxit (+ iters 1))))))))

(define (newton-root f df x0 tol maxit)
  "root of f by Newton-Raphson from x0; df is the derivative of f"
  (num-newton-walk f df (num-as-float x0) tol maxit 0))

(define (num-secant-walk f x0 x1 f0 f1 tol maxit iters)
  (cond ((>= iters maxit) (list x1 iters nil))
        (t (let ((x2 (- x1 (/ (* f1 (- x1 x0)) (- f1 f0)))))
             (cond ((< (abs (f x2)) tol) (list x2 (+ iters 1) t))
                   (t (num-secant-walk f x1 x2 f1 (f x2)
                                       tol maxit (+ iters 1))))))))

(define (secant-root f x0 x1 tol maxit)
  "root of f by the secant method from x0 and x1"
  (num-secant-walk f (num-as-float x0) (num-as-float x1)
                   (num-as-float (f x0)) (num-as-float (f x1)) tol maxit 0))

; ---- quadrature ----

(define (num-simpson-walk f a h n i acc)
  (cond ((= i n) acc)
        (t (num-simpson-walk f a h n (+ i 1)
                             (+ acc (* (f (+ a (* i h)))
                                       (cond ((= (mod i 2) 1) 4) (t 2))))))))

(define (simpson f a b n)
  "composite Simpson's rule over [a b] with n panels; n must be even"
  (cond ((not (= (mod n 2) 0)) (error "SIMPSON: N MUST BE EVEN" n))
        (t (let ((h (/ (- b a) n)))
             (* (/ h 3)
                (+ (+ (f a) (f b))
                   (num-simpson-walk f a h n 1 0)))))))

; 5-point Gauss-Legendre nodes and weights on [-1 1]
(define (num-gauss5-walk f m h xs ws acc)
  (cond ((null xs) acc)
        (t (num-gauss5-walk f m h (cdr xs) (cdr ws)
                            (+ acc (* (car ws) (f (+ m (* h (car xs))))))))))

(define (gauss5 f a b)
  "5-point Gauss-Legendre quadrature over [a b]; exact for deg <= 9"
  (let ((m (/ (+ a b) 2))
        (h (/ (- b a) 2)))
    (* h (num-gauss5-walk f m h
          (list 0.0
                (- 0.5384693101056831) 0.5384693101056831
                (- 0.906179845938664) 0.906179845938664)
          (list 0.5688888888888889
                0.4786286704993665 0.4786286704993665
                0.2369268850561891 0.2369268850561891)
          0))))

; ---- ODE: classical Runge-Kutta 4 ----

(define (num-rk4-step f tt y h)
  (let* ((k1 (f tt y))
         (k2 (f (+ tt (/ h 2)) (+ y (* (/ h 2) k1))))
         (k3 (f (+ tt (/ h 2)) (+ y (* (/ h 2) k2))))
         (k4 (f (+ tt h) (+ y (* h k3)))))
    (+ y (* (/ h 6) (+ k1 (+ (* 2 k2) (+ (* 2 k3) k4)))))))

(define (num-rk4-walk f tt y h n)
  (cond ((= n 0) y)
        (t (num-rk4-walk f (+ tt h) (num-rk4-step f tt y h) h (- n 1)))))

(define (rk4 f y0 t0 t1 h)
  "integrate y' = f(t,y) from t0 to t1 with classical RK4, step h;
   the number of steps is round((t1-t0)/h) -- time never accumulates
   across steps, so float drift cannot add or drop a step"
  (num-rk4-walk f t0 y0 h
                (truncate (/ (+ (- t1 t0) (/ h 2)) h))))
