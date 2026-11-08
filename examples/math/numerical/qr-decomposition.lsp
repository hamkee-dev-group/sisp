; ============================================================
; qr-decomposition.lsp — Gram-Schmidt, Householder, QR.
; Least squares: fit polynomial to noisy data. QR algorithm.
; ============================================================

(print "=== QR Decomposition ===")


(define EPS 0.000000000000001)

; --- Gram-Schmidt orthogonalization ---
(define (gram-schmidt A-rows)
  (let* ((m (length A-rows)) (n (length (car A-rows)))
         (Q (make-matrix m n))
         (R (make-matrix n n)))
    (gs-main A-rows Q R m n 0)))

(define (gs-main A-rows Q R m n j)
  (cond ((>= j n) (list Q R))
        (t (let ((v (copy-column A-rows m j)))
          (gs-subtract R Q v A-rows m n j 0)
          (let ((vn (vec-norm v m)))
            (cond ((< vn 0.000000000000001) (gs-main A-rows Q R m n (+ j 1)))
                  (t (mat-set R j j vn)
                     (gs-normalize Q v m j vn)
                     (gs-main A-rows Q R m n (+ j 1)))))))))

(define (copy-column A m j)
  (let ((v (make-matrix m 1)) (i 0))
    (copy-col-iter A v m j i)))

(define (copy-col-iter A v m j i)
  (cond ((>= i m) v)
        (t (mat-set v i 0 (row-ref A i j))
           (copy-col-iter A v m j (+ i 1)))))

(define (row-ref A i j)
  (let ((row (nth i A)))
    (nth j row)))

(define (gs-subtract R Q v A m n j k)
  (cond ((>= k j) nil)
        (t (let ((rkj (col-dot v Q m k)))
          (mat-set R k j rkj)
          (col-axpy v Q m k (- 0.0 rkj))
          (gs-subtract R Q v A m n j (+ k 1))))))

(define (col-dot v Q m k)
  (let ((acc 0.0) (i 0))
    (col-dot-iter v Q m k i acc)))

(define (col-dot-iter v Q m k i acc)
  (cond ((>= i m) acc)
        (t (col-dot-iter v Q m k (+ i 1)
                         (+ acc (* (mat-ref v i 0) (mat-ref Q i k)))))))

(define (col-axpy v Q m k alpha)
  (col-axpy-iter v Q m k alpha 0))

(define (col-axpy-iter v Q m k alpha i)
  (cond ((>= i m) nil)
        (t (mat-set v i 0 (+ (mat-ref v i 0) (* alpha (mat-ref Q i k))))
           (col-axpy-iter v Q m k alpha (+ i 1)))))

(define (vec-norm v m)
  (sqrt (col-dot-iter v v m 0 0 0.0)))

(define (gs-normalize Q v m j vn)
  (gs-normalize-iter Q v m j vn 0))

(define (gs-normalize-iter Q v m j vn i)
  (cond ((>= i m) nil)
        (t (mat-set Q i j (/ (mat-ref v i 0) vn))
           (gs-normalize-iter Q v m j vn (+ i 1)))))

; --- Test: 3x2 matrix ---
(define a-qr (list (list 12 -51) (list 6 167) (list -4 24)))
(define qr-decomp (gram-schmidt a-qr))
(define qr-q (car qr-decomp))
(define qr-r (cadr qr-decomp))
(print "QR Q(0,0)=" (mat-ref qr-q 0 0) " R(0,0)=" (mat-ref qr-r 0 0))

; --- what QR actually has to satisfy (floats: compare with a tolerance) ---
(define TOL 0.00000001)
(define (close-p x y) (< (abs (- x y)) TOL))

; inner product of columns j and k of the 3x2 factor Q
(define (qcol-dot j k)
  (+ (* (mat-ref qr-q 0 j) (mat-ref qr-q 0 k))
     (* (mat-ref qr-q 1 j) (mat-ref qr-q 1 k))
     (* (mat-ref qr-q 2 j) (mat-ref qr-q 2 k))))

; (Q R)(i,j), reassembled from the two factors
(define (qr-entry i j)
  (+ (* (mat-ref qr-q i 0) (mat-ref qr-r 0 j))
     (* (mat-ref qr-q i 1) (mat-ref qr-r 1 j))))

(check "Q-has-orthonormal-columns"
  (and (close-p (qcol-dot 0 0) 1)
       (close-p (qcol-dot 1 1) 1)
       (close-p (qcol-dot 0 1) 0)))

(check "R-is-upper-triangular" (close-p (mat-ref qr-r 1 0) 0))

; QR = A, entry by entry, against the ORIGINAL matrix a-qr
(check "QR-reconstructs-A"
  (and (close-p (qr-entry 0 0) 12)  (close-p (qr-entry 0 1) -51)
       (close-p (qr-entry 1 0) 6)   (close-p (qr-entry 1 1) 167)
       (close-p (qr-entry 2 0) -4)  (close-p (qr-entry 2 1) 24)))

; The classical answer for this matrix: first column of A has norm
; sqrt(144+36+16) = 14, so R(0,0) = 14 and q0 = (6/7, 3/7, -2/7).
; Both sides derived without running Gram-Schmidt.
(check "R00-is-norm-of-first-column"
  (and (close-p (mat-ref qr-r 0 0) (sqrt (+ (* 12 12) (* 6 6) (* 4 4))))
       (close-p (mat-ref qr-q 0 0) (/ 6 7))
       (close-p (mat-ref qr-q 1 0) (/ 3 7))
       (close-p (mat-ref qr-q 2 0) (/ -2 7))))

; negative control: Q's columns are NOT equal to each other
(check "Q-columns-are-distinct" (not (close-p (qcol-dot 0 1) 1)))

; --- Least squares: fit y = a + b*x to data ---
; Data: (x,y) = (0,1), (1,3), (2,2), (3,4), (4,3)
; Design matrix A: [[1 x1] [1 x2] ...], we want A*x ≈ y

; Simple normal equations: A^T * A * x = A^T * y
(define (normal-eqn A-rows y-vec)
  (let* ((m (length A-rows)) (n (length (car A-rows))))
    (let* ((AtA (make-matrix n n))
           (Aty (make-matrix n 1)))
      (build-ata A-rows AtA m n)
      (build-aty A-rows y-vec Aty m n)
      (list AtA Aty))))

(define (build-ata A AtA m n)
  (build-ata-ij A AtA m n 0 0))

(define (build-ata-ij A AtA m n i j)
  (cond ((>= i n) nil)
        ((>= j n) (build-ata-ij A AtA m n (+ i 1) 0))
        (t (let ((sum 0.0) (k 0))
          (mat-set AtA i j (ata-sum A m i j 0 0.0))
          (build-ata-ij A AtA m n i (+ j 1))))))

(define (ata-sum A m i j k acc)
  (cond ((>= k m) acc)
        (t (ata-sum A m i j (+ k 1)
                    (+ acc (* (row-ref A k i) (row-ref A k j)))))))

(define (build-aty A y Aty m n)
  (build-aty-i A y Aty m n 0))

(define (build-aty-i A y Aty m n i)
  (cond ((>= i n) nil)
        (t (let ((sum 0.0) (k 0))
          (mat-set Aty i 0 (aty-sum A y m i 0 0.0))
          (build-aty-i A y Aty m n (+ i 1))))))

(define (aty-sum A y m i k acc)
  (cond ((>= k m) acc)
        (t (aty-sum A y m i (+ k 1)
                    (+ acc (* (row-ref A k i) (nth k y)))))))

; Simple 2x2 solve for normal equations (just Cramer's)
(define (solve2 AtA Aty)
  (let* ((a (mat-ref AtA 0 0)) (b (mat-ref AtA 0 1))
         (c (mat-ref AtA 1 0)) (d (mat-ref AtA 1 1))
         (e (mat-ref Aty 0 0)) (f (mat-ref Aty 1 0))
         (det (- (* a d) (* b c))))
    (list (/ (- (* e d) (* b f)) (+ det EPS))
          (/ (- (* a f) (* c e)) (+ det EPS)))))

(define design (list (list 1 0) (list 1 1) (list 1 2) (list 1 3) (list 1 4)))
(define ys (list 1.0 3.0 2.0 4.0 3.0))
(define ne (normal-eqn design ys))
(define coef (solve2 (car ne) (cadr ne)))
(print "LS fit: y =" (car coef) "+" (cadr coef) "*x")

; Independently-derived answer: for a straight-line fit the slope is
; Sxy/Sxx and the intercept is ybar - slope*xbar.  Computed here straight
; from the raw data, with no reference to the normal-equation solver.
(define xs (list 0 1 2 3 4))
(define (mean-of l) (/ (foldl (lambda (acc x) (+ acc x)) 0 l) (length l)))
(define xbar (mean-of xs))
(define ybar (mean-of ys))
(define pairs (map (lambda (i) (cons (nth i xs) (nth i ys))) (list 0 1 2 3 4)))
(define sxy (foldl (lambda (acc pr) (+ acc (* (- (car pr) xbar) (- (cdr pr) ybar)))) 0 pairs))
(define sxx (foldl (lambda (acc x) (+ acc (* (- x xbar) (- x xbar)))) 0 xs))
(define slope-ref (/ sxy sxx))
(define intercept-ref (- ybar (* slope-ref xbar)))

(check "ls-coefficients-match-closed-form"
  (and (close-p (cadr coef) slope-ref)
       (close-p (car coef) intercept-ref)))

; the residual vector must be orthogonal to both design columns
; (the defining property of a least-squares solution)
(define (resid-at x y) (- y (+ (car coef) (* (cadr coef) x))))
(check "ls-residual-orthogonal-to-design"
  (and (close-p (foldl (lambda (acc pr) (+ acc (resid-at (car pr) (cdr pr)))) 0 pairs) 0)
       (close-p (foldl (lambda (acc pr)
                         (+ acc (* (car pr) (resid-at (car pr) (cdr pr))))) 0 pairs) 0)))

(print "=== QR Decomposition Done ===")
