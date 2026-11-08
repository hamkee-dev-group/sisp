; ============================================================
; ode-solvers.lsp — Euler, Midpoint, RK4, system solver.
; y'=y, y'=-2xy (Gaussian), Lotka-Volterra. Step-size control.
; ============================================================

(print "=== ODE Solvers ===")


(define EPS 0.000000000000001)

; --- Euler ---
(define (euler f y0 tv0 tv1 h)
  (cond ((>= tv0 tv1) y0)
        (t (euler f (+ y0 (* h (f tv0 y0))) (+ tv0 h) tv1 h))))

; --- Midpoint (RK2) ---
(define (midpoint f y0 tv0 tv1 h)
  (cond ((>= tv0 tv1) y0)
        (t (let* ((h2 (/ h 2.0))
                   (k1 (f tv0 y0))
                   (k2 (f (+ tv0 h2) (+ y0 (* h2 k1)))))
          (midpoint f (+ y0 (* h k2)) (+ tv0 h) tv1 h)))))

; --- RK4 ---
(define (rk4 f y0 tv0 tv1 h)
  (cond ((>= tv0 tv1) y0)
        (t (let* ((h2 (/ h 2.0))
                   (h6 (/ h 6.0))
                   (k1 (f tv0 y0))
                   (k2 (f (+ tv0 h2) (+ y0 (* h2 k1))))
                   (k3 (f (+ tv0 h2) (+ y0 (* h2 k2))))
                   (k4 (f (+ tv0 h) (+ y0 (* h k3)))))
          (rk4 f (+ y0 (* h6 (+ k1 (* 2.0 k2) (* 2.0 k3) k4))) (+ tv0 h) tv1 h)))))

; --- Test 1: y'=y, y(0)=1 → y(x)=e^x. Evaluate at x=1. ---
(define (ode1 x y) y)

(print "y'=y, y(0)=1, x=1, exact=" (exp 1.0))
(define exact-exp (exp 1.0))
(define y1-eu (euler ode1 (+ 1 EPS) 0.0 1.0 0.2))
(define y1-mp (midpoint ode1 (+ 1 EPS) 0.0 1.0 0.2))
(define y1-rk (rk4 ode1 (+ 1 EPS) 0.0 1.0 0.2))

(print "Euler(h=0.2)=" y1-eu " err=" (abs (- y1-eu exact-exp)))
(print "Midpoint(h=0.2)=" y1-mp " err=" (abs (- y1-mp exact-exp)))
(print "RK4(h=0.2)=" y1-rk " err=" (abs (- y1-rk exact-exp)))
(check "rk4-best" (< (abs (- y1-rk exact-exp)) 0.001))

; --- Test 2: y'=-2xy, y(0)=1 → y(x)=e^{-x^2}. Evaluate at x=1. ---
(define (ode2 x y) (* -2.0 x y))
(define exact-gauss (exp -1.0))

(print "y'=-2xy, y(0)=1, x=1, exact=" exact-gauss)
(define y2-rk (rk4 ode2 (+ 1 EPS) 0.0 1.0 0.05))
(print "RK4(h=0.05)=" y2-rk " err=" (abs (- y2-rk exact-gauss)))
(check "gaussian-ode" (< (abs (- y2-rk exact-gauss)) 0.0001))

; --- Test 3: Lotka-Volterra (2D system) ---
; dx/dt = 1.5x - xy, dy/dt = -3y + xy
(define (lv st)
  (let ((x (car st)) (y (cadr st)))
    (list (- (* 1.5 x) (* x y))
          (+ (* -3.0 y) (* x y)))))

(define (rk4-sys rhs st0 tv0 tv1 h)
  (cond ((>= tv0 tv1) st0)
        (t (let* ((h2 (/ h 2.0)) (h6 (/ h 6.0))
                   (k1 (rhs st0))
                   (s1 (list (+ (car st0) (* h2 (car k1)))
                            (+ (cadr st0) (* h2 (cadr k1)))))
                   (k2 (rhs s1))
                   (s2 (list (+ (car st0) (* h2 (car k2)))
                            (+ (cadr st0) (* h2 (cadr k2)))))
                   (k3 (rhs s2))
                   (s3 (list (+ (car st0) (* h (car k3)))
                            (+ (cadr st0) (* h (cadr k3)))))
                   (k4 (rhs s3))
                   (nx (+ (car st0) (* h6 (+ (car k1) (* 2.0 (car k2))
                                            (* 2.0 (car k3)) (car k4)))))
                   (ny (+ (cadr st0) (* h6 (+ (cadr k1) (* 2.0 (cadr k2))
                                              (* 2.0 (cadr k3)) (cadr k4))))))
          (rk4-sys rhs (list nx ny) (+ tv0 h) tv1 h)))))

(print "Lotka-Volterra: x0=3, y0=1, t=10")
(define lv-end (rk4-sys lv (list 3.0 1.0) 0.0 10.0 0.2))
(define lv-x (car lv-end)) (define lv-y (cadr lv-end))
(print "x(10)=" lv-x " y(10)=" lv-y)
(check "lv-prey" (> lv-x 0))
(check "lv-pred" (> lv-y 0))

; --- Step-size control: halve step and compare ---
(define y2-coarse (rk4 ode2 (+ 1 EPS) 0.0 1.0 0.05))
(define y2-fine (rk4 ode2 (+ 1 EPS) 0.0 1.0 0.025))
(define step-delta (abs (- y2-coarse y2-fine)))
(print "Step-halving diff (0.05 vs 0.025):" step-delta)
(check "step-converges" (< step-delta 0.001))

(print "=== ODE Solvers Done ===")
