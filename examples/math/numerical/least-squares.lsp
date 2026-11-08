; ============================================================
; least-squares.lsp — Linear regression, polynomial fit.
; Normal equations. The A^T A part of the pseudo-inverse.
; Overdetermined systems.
; ============================================================

(print "=== Least Squares ===")


; --- Linear regression via normal equations ---
; Fit y = a + b*x to data points
; Accumulate the sums, then solve the 2x2 normal equations for (intercept, slope).
(define (linear-regression xvals yvals)
  (let ((n (length xvals)))
    (lin-solve (lin-reg-sum xvals yvals n 0 0 0 0 0))))

(define (lin-reg-sum xvals yvals n i sx sy sxx sxy)
  (cond ((>= i n) (list sx sy sxx sxy n))
        (t (let ((xi (nth i xvals)) (yi (nth i yvals)))
          (lin-reg-sum xvals yvals n (+ i 1)
                       (+ sx xi) (+ sy yi)
                       (+ sxx (* xi xi)) (+ sxy (* xi yi)))))))

; Cramer's rule on a 2x2 system  mat * x = rhs  (rhs is a 2x1 matrix).
(define (solve2 mat rhs)
  (let* ((a (mat-ref mat 0 0)) (b (mat-ref mat 0 1))
         (c (mat-ref mat 1 0)) (d (mat-ref mat 1 1))
         (e (mat-ref rhs 0 0)) (f (mat-ref rhs 1 0))
         (det (- (* a d) (* b c))))
    (list (/ (- (* d e) (* b f)) det)
          (/ (- (* a f) (* c e)) det))))

(define (lin-solve stats)
  (let* ((sx (car stats)) (sy (cadr stats))
         (sxx (caddr stats)) (sxy (cadddr stats))
         (nn (nth 4 stats))
         (nf nn)
         (det (- (* nf sxx) (* sx sx)))
         (a (/ (- (* sxx sy) (* sx sxy)) (+ det 0.000000000000001)))
         (b (/ (- (* nf sxy) (* sx sy)) (+ det 0.000000000000001))))
    (list a b)))

; Data: y = 2 + 0.5*x + noise
(define x-dat (list 0.0 1.0 2.0 3.0 4.0 5.0))
(define y-dat (list 2.1 2.4 3.2 3.3 3.9 4.7))
(define coefs (linear-regression x-dat y-dat))
(print "Linear fit: y=" (car coefs) "+" (cadr coefs) "*x")
(check "lin-reg-slope-positive" (> (cadr coefs) 0))
(check "lin-reg-intercept" (> (car coefs) 1.5))
; The data were generated from y = 2 + 0.5x plus small noise, so the fit must
; recover those parameters to within the noise level.
(check "lin-reg-recovers-intercept" (< (abs (- (car coefs) 2)) 0.05))
(check "lin-reg-recovers-slope" (< (abs (- (cadr coefs) 0.5)) 0.05))

; --- Polynomial fit via normal equations ---
; Fit y = c0 + c1*x + c2*x^2
(define (poly-fit xvals yvals deg)
  (let* ((n (length xvals))
         (p (+ deg 1))
         (AtA (make-matrix p p))
         (Aty (make-matrix p 1)))
    (build-poly-ata xvals n AtA deg)
    (build-poly-aty xvals yvals n Aty deg)
    (list AtA Aty)))

(define (build-poly-ata xvals n AtA deg)
  (build-poly-ata-ij xvals n AtA deg 0 0))

(define (build-poly-ata-ij xvals n AtA deg i j)
  (cond ((> i deg) nil)
        ((> j deg) (build-poly-ata-ij xvals n AtA deg (+ i 1) 0))
        (t (poly-sum xvals n AtA deg i j 0 0))))

; (A^T A)[i][j] = sum_k x_k^(i+j).  deg is passed as an argument because it
; is not in lexical scope here.
(define (poly-sum xvals n AtA deg i j k sum)
  (cond ((>= k n) (mat-set AtA i j sum)
                  (build-poly-ata-ij xvals n AtA deg i (+ j 1)))
        (t (let ((xk (nth k xvals)))
             (poly-sum xvals n AtA deg i j (+ k 1)
                       (+ sum (^ xk (+ i j))))))))

(define (build-poly-aty xvals yvals n Aty deg)
  (build-poly-aty-i xvals yvals n Aty deg 0))

(define (build-poly-aty-i xvals yvals n Aty deg i)
  (cond ((> i deg) nil)
        (t (poly-aty-sum xvals yvals n Aty deg i 0 0))))

; (A^T y)[i] = sum_k y_k * x_k^i
(define (poly-aty-sum xvals yvals n Aty deg i k sum)
  (cond ((>= k n) (mat-set Aty i 0 sum)
                  (build-poly-aty-i xvals yvals n Aty deg (+ i 1)))
        (t (let ((xk (nth k xvals)) (yk (nth k yvals)))
             (poly-aty-sum xvals yvals n Aty deg i (+ k 1)
                           (+ sum (* yk (^ xk i))))))))

; Test polynomial fit of degree 1 (same as linear regression)
(define pf (poly-fit x-dat y-dat 1))
(define pf-ata (car pf))
(define pf-aty (cadr pf))
(print "Poly-fit normal eqns built, AtA size =" (mat-rows pf-ata))
(check "poly-fit-ata-shape" (= (mat-rows pf-ata) 2))
; With x = 0..5: sum 1 = 6, sum x = 15, sum x^2 = 55, so AtA = [[6,15],[15,55]].
(check "poly-fit-ata-entries"
       (and (and (= (mat-ref pf-ata 0 0) 6) (= (mat-ref pf-ata 0 1) 15))
            (and (= (mat-ref pf-ata 1 0) 15) (= (mat-ref pf-ata 1 1) 55))))
; A^T y = [sum y, sum x*y] = [19.6, 57.8]
(check "poly-fit-aty"
       (and (< (abs (- (mat-ref pf-aty 0 0) 19.6)) 0.000001)
            (< (abs (- (mat-ref pf-aty 1 0) 57.8)) 0.000001)))
; Degree-1 normal equations must reproduce the closed-form linear regression.
(define pf-coefs (solve2 pf-ata pf-aty))
(print "Poly-fit degree 1 coefficients: " pf-coefs)
(check "poly-fit-matches-linreg"
       (and (< (abs (- (car pf-coefs) (car coefs))) 0.000001)
            (< (abs (- (cadr pf-coefs) (cadr coefs))) 0.000001)))

; --- Pseudo-inverse (normal-equations form, no SVD) ---
; A^+ = (A^T A)^{-1} A^T for full column rank.
; Only A^T A is built; it is returned so the caller can check it.
(define (pseudo-inverse-simple A-rows)
  (let* ((nr (length A-rows)) (nc (length (car A-rows)))
         (At (transpose A-rows nr nc))
         (AtA (matmul At (rows2mat A-rows nr nc))))
    (print "Pseudo-inverse: AtA is " (mat-rows AtA) "x" (mat-cols AtA))
    AtA))

; NB: the accumulator cannot be called T — identifiers are case-insensitive,
; so T is the reserved boolean t and the define would be rejected outright.
(define (transpose A m n)
  (let ((tm (make-matrix n m)))
    (transpose-fill A tm m n 0 0)))

(define (transpose-fill A tm m n i j)
  (cond ((>= i n) tm)
        ((>= j m) (transpose-fill A tm m n (+ i 1) 0))
        (t (mat-set tm i j (row-ref A j i))
           (transpose-fill A tm m n i (+ j 1)))))

(define (row-ref A i j)
  (nth j (nth i A)))

; Likewise the matrix cannot be called M: that is the same identifier as the
; row count m.
(define (rows2mat A m n)
  (let ((mm (make-matrix m n)))
    (rows2mat-fill A mm m n 0 0)))

(define (rows2mat-fill A mm m n i j)
  (cond ((>= i m) mm)
        ((>= j n) (rows2mat-fill A mm m n (+ i 1) 0))
        (t (mat-set mm i j (row-ref A i j))
           (rows2mat-fill A mm m n i (+ j 1)))))

; C would collide with the column count c, same case-insensitivity trap.
(define (matmul A B)
  (let* ((r (mat-rows A)) (c (mat-cols B)) (k (mat-cols A))
         (cm (make-matrix r c)))
    (matmul-fill A B cm r c k 0 0)))

(define (matmul-fill A B cm r c k i j)
  (cond ((>= i r) cm)
        ((>= j c) (matmul-fill A B cm r c k (+ i 1) 0))
        (t (matmul-sum A B cm k i j 0 0 r c))))

(define (matmul-sum A B cm k i j p sum r c)
  (cond ((>= p k) (mat-set cm i j sum)
                  (matmul-fill A B cm r c k i (+ j 1)))
        (t (matmul-sum A B cm k i j (+ p 1)
                       (+ sum (* (mat-ref A i p) (mat-ref B p j))) r c))))

; For A = [[1,0],[1,1],[1,2]],  A^T A = [[3,3],[3,5]]  (by hand).
(define pi-ata (pseudo-inverse-simple (list (list 1 0) (list 1 1) (list 1 2))))
(check "pseudo-inv-shape" (= (mat-rows pi-ata) 2))
(check "pseudo-inv-ata"
       (and (and (= (mat-ref pi-ata 0 0) 3) (= (mat-ref pi-ata 0 1) 3))
            (and (= (mat-ref pi-ata 1 0) 3) (= (mat-ref pi-ata 1 1) 5))))

; --- Overdetermined system solution via normal equations ---
; Ax = b with A 3x2
; Solves A^T A x = A^T b by Cramer's rule and returns x.
(define (overdet-solve A-rows b)
  (let* ((nr (length A-rows)) (nc (length (car A-rows)))
         (Am (rows2mat A-rows nr nc))
         (At (transpose A-rows nr nc))
         (AtA (matmul At Am))
         (Atb (matvec At b)))
    (print "Overdetermined: AtA=" (mat-rows AtA) "x" (mat-cols AtA))
    (solve2 AtA Atb)))

(define (matvec A v)
  (let* ((r (mat-rows A)) (c (mat-cols A))
         (res (make-matrix r 1)))
    (matvec-fill A v res r c 0)))

(define (matvec-fill A v res r c i)
  (cond ((>= i r) res)
        (t (matvec-sum A v 0 res r c i 0))))

(define (matvec-sum A v sum res r c i j)
  (cond ((>= j c) (mat-set res i 0 sum)
                  (matvec-fill A v res r c (+ i 1)))
        (t (matvec-sum A v (+ sum (* (mat-ref A i j) (nth j v))) res r c i (+ j 1)))))

; The three points (0,1),(1,2),(2,3) are exactly collinear (y = 1 + 1*x), so
; the least-squares solution is the exact interpolant x = [1, 1] with zero
; residual.  A^T b = [6, 8] and [[3,3],[3,5]] [1,1]^T = [6,8]. Verified by hand.
(define over-x (overdet-solve (list (list 1 0) (list 1 1) (list 1 2)) (list 1 2 3)))
(print "Overdetermined solution: " over-x)
(check "overdet-intercept" (= (car over-x) 1))
(check "overdet-slope" (= (cadr over-x) 1))

(print "=== Least Squares Done ===")
