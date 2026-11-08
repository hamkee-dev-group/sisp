; ============================================================
; interpolation.lsp — Lagrange, Newton divided differences,
; cubic splines (natural). Runge's phenomenon.
; ============================================================

(print "=== Interpolation ===")


; --- Lagrange interpolation ---
(define (lagrange pts x)
  (let ((n (length pts)))
    (lagrange-sum pts x n 0 0.0)))

(define (lagrange-sum pts x n i acc)
  (cond ((>= i n) acc)
        (t (let* ((pt (nth i pts))
                  (xi (car pt)) (yi (cadr pt))
                  (li (lagrange-basis pts x n i)))
          (lagrange-sum pts x n (+ i 1) (+ acc (* yi li)))))))

(define (lagrange-basis pts x n i)
  (let* ((xi (car (nth i pts))))
    (lagrange-basis-prod pts x n i 0 1.0 xi)))

(define (lagrange-basis-prod pts x n i j acc xi)
  (cond ((>= j n) acc)
        ((= j i) (lagrange-basis-prod pts x n i (+ j 1) acc xi))
        (t (let ((xj (car (nth j pts))))
          (lagrange-basis-prod pts x n i (+ j 1)
                               (* acc (/ (- x xj) (- xi xj))) xi)))))

; --- Newton divided differences ---
(define (newton-interp pts x)
  (let* ((n (length pts))
         (coefs (div-diffs pts n)))
    (newton-eval pts coefs x n)))

(define (div-diffs pts n)
  (let ((dd (make-matrix n n)))
    (dd-init dd pts n 0)
    (dd-table dd pts n)
    dd))

(define (dd-init dd pts n i)
  (cond ((>= i n) nil)
        (t (let ((pt (nth i pts)))
          (mat-set dd i 0 (cadr pt))
          (dd-init dd pts n (+ i 1))))))

(define (dd-table dd pts n)
  (dd-table-j dd pts n 1))

(define (dd-table-j dd pts n j)
  (cond ((>= j n) nil)
        (t (dd-table-ji dd pts n j 0)
           (dd-table-j dd pts n (+ j 1)))))

; f[x_i..x_{i+j}] = (f[x_{i+1}..x_{i+j}] - f[x_i..x_{i+j-1}]) / (x_{i+j} - x_i).
; The denominator uses the ABSCISSAE from pts; column 0 of dd holds the
; y-values, so reading the denominator out of dd was wrong.
(define (dd-table-ji dd pts n j i)
  (cond ((>= (+ i j) n) nil)
        (t (let ((num (- (mat-ref dd (+ i 1) (- j 1)) (mat-ref dd i (- j 1))))
                 (den (- (car (nth (+ i j) pts)) (car (nth i pts)))))
          (mat-set dd i j (/ num den))
          (dd-table-ji dd pts n j (+ i 1))))))

(define (newton-eval pts dd x n)
  (let ((xvals (map car pts)))
    (newton-eval-iter dd xvals x n 0 1.0 0.0)))

(define (newton-eval-iter dd xvals x n i term acc)
  (cond ((>= i n) acc)
        (t (newton-eval-iter dd xvals x n (+ i 1)
                             (* term (- x (nth i xvals)))
                             (+ acc (* (mat-ref dd 0 i) term))))))

; --- Natural cubic spline ---
(define (natural-spline pts x)
  (let* ((n (length pts))
         (h (spline-h pts n))
         (M (spline-moments pts h n))
         (i (spline-find-interval pts x n)))
    (spline-eval pts h M n i x)))

(define (spline-h pts n)
  (let ((hv nil) (i 0))
    (spline-h-iter pts n i hv)))

(define (spline-h-iter pts n i acc)
  (cond ((>= i (- n 1)) (reverse acc))
        (t (let ((xi (car (nth i pts)))
                 (xj (car (nth (+ i 1) pts))))
          (spline-h-iter pts n (+ i 1) (cons (- xj xi) acc))))))

; The moments M[i] = S''(x_i) of a natural cubic spline are NOT a local
; formula: they solve the tridiagonal system, for i = 1 .. n-2,
;   h[i-1]*M[i-1] + 2*(h[i-1]+h[i])*M[i] + h[i]*M[i+1]
;     = 6*((y[i+1]-y[i])/h[i] - (y[i]-y[i-1])/h[i-1])
; with the natural end conditions M[0] = M[n-1] = 0.  Solved by the Thomas
; algorithm below.  (The previous local difference quotient was not a spline.)
(define (spline-moments pts h n)
  (let ((nin (- n 2)))
    (cond ((< nin 1) (spline-zeros n))
          (t (let* ((fwd (thomas-fwd pts h nin 0 nil nil))
                    (inner (thomas-back (car fwd) (cadr fwd) 0.0 nil)))
               (cons 0.0 (append inner (list 0.0))))))))

(define (spline-zeros k)
  (cond ((<= k 0) nil)
        (t (cons 0.0 (spline-zeros (- k 1))))))

; Forward sweep. cps/dps are accumulated in REVERSE order, so (car cps) is
; always the previous row's coefficient.
(define (thomas-fwd pts h nin k cps dps)
  (cond ((>= k nin) (list cps dps))
        (t (let* ((ak (nth k h))
                  (ck (nth (+ k 1) h))
                  (bk (* 2 (+ ak ck)))
                  (yi (cadr (nth k pts)))
                  (yj (cadr (nth (+ k 1) pts)))
                  (yk (cadr (nth (+ k 2) pts)))
                  (dk (* 6 (- (/ (- yk yj) ck) (/ (- yj yi) ak))))
                  (pc (if (null cps) 0.0 (car cps)))
                  (pd (if (null dps) 0.0 (car dps)))
                  (den (- bk (* ak pc))))
             (thomas-fwd pts h nin (+ k 1)
                         (cons (/ ck den) cps)
                         (cons (/ (- dk (* ak pd)) den) dps))))))

; Back substitution over the reversed lists: M[k] = dp[k] - cp[k]*M[k+1],
; consing rebuilds the interior moments in forward order.
(define (thomas-back cps dps prev acc)
  (cond ((null cps) acc)
        (t (let ((mk (- (car dps) (* (car cps) prev))))
             (thomas-back (cdr cps) (cdr dps) mk (cons mk acc))))))

(define (spline-find-interval pts x n)
  (spline-find-iter pts x n 0))

(define (spline-find-iter pts x n i)
  (cond ((>= i (- n 2)) i)
        ((<= x (car (nth (+ i 1) pts))) i)
        (t (spline-find-iter pts x n (+ i 1)))))

(define (spline-eval pts h M n i x)
  (let* ((xi (car (nth i pts))) (xj (car (nth (+ i 1) pts)))
         (yi (cadr (nth i pts))) (yj (cadr (nth (+ i 1) pts)))
         (hi (nth i h))
         (Mi (nth i M)) (Mj (nth (+ i 1) M))
         (dx (- x xi)))
    (+ yi
       (* (- (/ (- yj yi) hi) (/ (* hi (+ (* 2 Mi) Mj)) 6.0)) dx)
       (* 0.5 Mi dx dx)
       (* (/ (- Mj Mi) (* 6.0 hi)) dx dx dx))))

; --- Test: f(x) = 1/(1+25x^2) (Runge) vs f(x) = sin(x) ---
(define (runge x) (/ 1.0 (+ 1.0 (* 25.0 x x))))
(define (sinfn x) (sin x))
(define PI 3.141592653589793)

; n equally spaced points on [a,b], endpoints included
(define (uniform-pts-n n a b)
  (unif-iter 0 n a b nil))

(define (unif-iter i n a b acc)
  (cond ((>= i n) (reverse acc))
        (t (let ((x (+ a (* (- b a) (/ i (- n 1))))))
             (unif-iter (+ i 1) n a b (cons (list x (runge x)) acc))))))

; Chebyshev points
(define (cheb-pts n a b)
  (let ((pts nil) (i 0))
    (cheb-iter i n a b pts)))

(define (cheb-iter i n a b acc)
  (cond ((>= i n) (reverse acc))
        (t (let ((x (* 0.5 (+ (+ a b) (* (- b a) (cos (* PI (/ (+ (* 2 i) 1) (* 2 n)))))))))
          (cheb-iter (+ i 1) n a b (cons (list x (runge x)) acc))))))

; Runge's phenomenon is a statement about the error over the WHOLE interval:
; with equally spaced nodes the interpolant oscillates wildly near the
; endpoints, while Chebyshev nodes keep the error bounded.  It does not show
; up at a single interior point such as x=0.5, and 6 nodes is far too few to
; exhibit it at all — so sample the sup-norm error with 12 nodes.
(define n-nodes 12)
(define uniform-pts (uniform-pts-n n-nodes -1.0 1.0))
(define cheb-pts-n (cheb-pts n-nodes -1.0 1.0))

; max |p(x) - runge(x)| over the midpoints of n samples sub-intervals of [-1,1].
; Sampling the MIDPOINTS (x0 = -0.975, step 0.05) matters twice over: the error
; is identically zero at the nodes themselves, and every x stays a genuine
; float.  SISP collapses exact results, so a grid point like (* 20 0.05) would
; come back as the integer 1; the uniform nodes are exact rationals (i/11), and
; an all-exact degree-11 Lagrange sum overflows the integer type.
(define (max-err pts nsamples x0 step)
  (max-err-iter pts nsamples x0 step 0 0.0))

(define (max-err-iter pts nsamples x0 step k acc)
  (cond ((>= k nsamples) acc)
        (t (let* ((x (+ x0 (* k step)))
                  (e (abs (- (lagrange pts x) (runge x)))))
             (max-err-iter pts nsamples x0 step (+ k 1) (if (> e acc) e acc))))))

(define lg-runge (lagrange uniform-pts 0.5))
(define lg-cheb (lagrange cheb-pts-n 0.5))
(define true-r05 (runge 0.5))
(print "Runge at 0.5: true=" true-r05 " Lagrange(unif)=" lg-runge " Lagrange(cheb)=" lg-cheb)

(check "lagrange-works" (< (abs (- lg-runge true-r05)) 1.0))

(define err-unif (max-err uniform-pts 40 -0.975 0.05))
(define err-cheb (max-err cheb-pts-n 40 -0.975 0.05))
(print "Max error on [-1,1]: uniform=" err-unif " chebyshev=" err-cheb)
(check "cheb-better-runge" (< err-cheb err-unif))

; Also test with sin function (well-behaved)
(define sin-pts (list (list 0.0 0.0) (list 1.0 0.841471) (list 2.0 0.909297)
                      (list 3.0 0.141120) (list 4.0 -0.756802)))
(define lg-sin (lagrange sin-pts 1.5))
(print "Lagrange sin(1.5)=" lg-sin " true=" (sin 1.5))
(check "lagrange-sine" (< (abs (- lg-sin (sin 1.5))) 0.05))

; Newton divided differences at same point
(define nd-sin (newton-interp sin-pts 1.5))
(print "Newton sin(1.5)=" nd-sin)
(check "newton-sine" (< (abs (- nd-sin (sin 1.5))) 0.05))

; Spline
(define sp-sin (natural-spline sin-pts 1.5))
(print "Spline sin(1.5)=" sp-sin)
(check "spline-sine" (< (abs (- sp-sin (sin 1.5))) 0.1))

(print "=== Interpolation Done ===")
