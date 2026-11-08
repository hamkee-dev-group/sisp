; optimization.lsp — Gradient descent, Newton opt, line search.
; Rosenbrock function. Conjugate gradient.
(print "=== Optimization ===")


(define EPS 0.000000000000001)

; Rosenbrock: f(x,y) = (1-x)^2 + 100*(y-x^2)^2
; Gradient: df/dx = -2(1-x) - 400x(y-x^2)
;            df/dy = 200(y-x^2)
(define (rosenbrock x y)
  (+ (* (- 1.0 x) (- 1.0 x)) (* 100.0 (* (- y (* x x)) (- y (* x x))))))

(define (rosenbrock-grad x y)
  (list (+ (* -2.0 (- 1.0 x)) (* -400.0 x (- y (* x x))))
        (* 200.0 (- y (* x x)))))

; Gradient descent
(define (grad-descent f grad x0 y0 lr iters)
  (grad-desc-iter f grad x0 y0 lr 0 iters))

(define (grad-desc-iter f grad x y lr k maxit)
  (cond ((>= k maxit) (list x y (f x y)))
        ; let* : gx/gy depend on g, so the bindings must be sequential
        (t (let* ((g (grad x y))
                  (gx (car g)) (gy (cadr g)))
          (grad-desc-iter f grad (- x (* lr gx)) (- y (* lr gy)) lr (+ k 1) maxit)))))

(define gd (grad-descent rosenbrock rosenbrock-grad -0.5 1.0 0.0001 500))
(define gx (car gd)) (define gy (cadr gd)) (define gf (caddr gd))
(print "GD: x=" gx " y=" gy " f=" gf)
(check "grad-descent" (and (> gx -1) (< gx 2) (> gy -1) (< gy 3)))

; Newton's method for optimization: x_new = x - H^{-1} * grad
; For Rosenbrock: H = [[2 - 400(y-3x^2), -400x], [-400x, 200]]
(define (rosenbrock-hessian x y)
  (let* ((h11 (- 2.0 (* 400.0 (- y (* 3.0 x x)))))
         (h12 (* -400.0 x))
         (h21 h12)
         (h22 200.0))
    (list h11 h12 h21 h22)))

(define (newton-opt f grad hess x0 y0 iters)
  (newton-opt-iter f grad hess x0 y0 0 iters))

(define (newton-opt-iter f grad hess x y k maxit)
  (cond ((>= k maxit) (list x y (f x y)))
        (t (let* ((g (grad x y)) (gx (car g)) (gy (cadr g))
                  (h (hess x y))
                  (h11 (car h)) (h12 (cadr h))
                  (h21 (caddr h)) (h22 (cadddr h))
                  (det (+ (* h11 h22) (* -1.0 h12 h21) EPS))
                  (dx (/ (- (* h22 gx) (* h12 gy)) det))
                  (dy (/ (- (* h11 gy) (* h21 gx)) det)))
          (newton-opt-iter f grad hess (- x dx) (- y dy) (+ k 1) maxit)))))

(define nt (newton-opt rosenbrock rosenbrock-grad rosenbrock-hessian -0.5 1.0 20))
(define nx (car nt)) (define ny (cadr nt)) (define nf (caddr nt))
(print "Newton: x=" nx " y=" ny " f=" nf)
(check "newton-opt" (< nf gf))

; Line search: Armijo backtracking
(define (armijo f grad x y alpha c rho)
  (let ((g (grad x y)))
    (let ((gx (car g)) (gy (cadr g))
          (fx (f x y)))
      (armijo-iter f fx grad x y gx gy alpha c rho 0))))

(define (armijo-iter f fx grad x y gx gy alpha c rho iter)
  (cond ((>= iter 20) alpha)
        (t (let* ((nx (- x (* alpha gx)))
                  (ny (- y (* alpha gy)))
                  (nf (f nx ny)))
          ; Armijo sufficient-decrease: f(x - a*g) <= f(x) - c*a*||g||^2
          (cond ((<= nf (- fx (* c alpha (+ (* gx gx) (* gy gy))))) alpha)
                (t (armijo-iter f fx grad x y gx gy (* alpha rho) c rho (+ iter 1))))))))

(define als (armijo rosenbrock rosenbrock-grad 0.0 2.0 1.0 0.0001 0.5))
(print "Armijo step size =" als)
(check "line-search" (> als 0.00001))

; Conjugate gradient (Fletcher-Reeves)
(define (cg-opt f grad x0 y0 iters)
  (let ((g (grad x0 y0)))
    (let ((gx (- 0.0 (car g))) (gy (- 0.0 (cadr g))))
      (cg-iter f grad x0 y0 gx gy gx gy gx gy 0 iters))))

(define (cg-iter f grad x y px py gx gy gx0 gy0 k maxit)
  (cond ((>= k maxit) (list x y (f x y)))
        (t (let* ((alpha 0.001)
                   (nx (+ x (* alpha px)))
                   (ny (+ y (* alpha py)))
                   (ng (grad nx ny))
                   (ngx (car ng)) (ngy (cadr ng))
                   (gdot-new (+ (* ngx ngx) (* ngy ngy)))
                   (gdot-old (+ (* gx0 gx0) (* gy0 gy0)))
                   (beta (/ gdot-new (+ gdot-old EPS)))
                   (npx (- (* -1.0 ngx) (* beta px)))
                   (npy (- (* -1.0 ngy) (* beta py))))
          (cg-iter f grad nx ny npx npy ngx ngy gx0 gy0 (+ k 1) maxit)))))

(define cg (cg-opt rosenbrock rosenbrock-grad -0.5 1.0 200))
(define cgx (car cg)) (define cgy (cadr cg)) (define cgf (caddr cg))
(print "CG: x=" cgx " y=" cgy " f=" cgf)
(check "cg-opt" (< cgf 5))

(print "=== Optimization Done ===")
