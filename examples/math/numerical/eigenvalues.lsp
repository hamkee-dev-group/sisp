; eigenvalues.lsp — Power iteration, Rayleigh quotient, spectral shift and a
; Gram-Schmidt QR factorisation, on a 3x3 symmetric matrix whose
; characteristic polynomial is checked independently.
(print "=== Eigenvalues ===")


(define EPS 0.000000000000001)

; matrix-vector product (A is list-of-lists)
(define (mvec A v)
  (let ((n (length v)) (res nil) (i 0))
    (mvec-iter A v n i res)))

(define (mvec-iter A v n i acc)
  (cond ((>= i n) (reverse acc))
        (t (let ((row (nth i A)) (sum 0.0) (j 0))
          (mvec-row row v n j sum A i acc)))))

(define (mvec-row row v n j sum A i acc)
  (cond ((>= j n) (mvec-iter A v n (+ i 1) (cons sum acc)))
        (t (mvec-row row v n (+ j 1) (+ sum (* (nth j row) (nth j v))) A i acc))))

; vector dot product
(define (vdot a b n)
  (vdot-iter a b n 0 0.0))

(define (vdot-iter a b n i acc)
  (cond ((>= i n) acc)
        (t (vdot-iter a b n (+ i 1) (+ acc (* (nth i a) (nth i b)))))))

; vector norm
(define (vnorm v n) (sqrt (vdot v v n)))

; normalize vector
(define (vnormalize v n)
  (let ((vn (vnorm v n)))
    (vnormalize-iter v vn n 0 nil)))

(define (vnormalize-iter v vn n i acc)
  (cond ((>= i n) (reverse acc))
        (t (vnormalize-iter v vn n (+ i 1) (cons (/ (nth i v) vn) acc)))))

; power iteration: A * v_k = lambda_k * v_{k+1}, lambda ~ Rayleigh quotient
(define (power-iteration A v0 n iters)
  (power-iter A v0 n 0 iters))

(define (power-iter A v n k maxit)
  (cond ((>= k maxit) (let ((av (mvec A v)))
                       (list (/ (vdot v av n) (vdot v v n)) v)))
        (t (let ((av (mvec A v)))
          (let ((vnew (vnormalize av n)))
            (power-iter A vnew n (+ k 1) maxit))))))

; --- tolerance helpers (never compare floats exactly) ---
(define TOL 0.000001)
(define (close-p x y) (< (abs (- x y)) TOL))

; --- characteristic polynomial, computed independently of any iteration ---
; det(M - x*I) for a 3x3 M, by cofactor expansion along the first row.
(define (det3-shift mtx x)
  (let ((a11 (- (nth 0 (nth 0 mtx)) x)) (a12 (nth 1 (nth 0 mtx))) (a13 (nth 2 (nth 0 mtx)))
        (a21 (nth 0 (nth 1 mtx))) (a22 (- (nth 1 (nth 1 mtx)) x)) (a23 (nth 2 (nth 1 mtx)))
        (a31 (nth 0 (nth 2 mtx))) (a32 (nth 1 (nth 2 mtx))) (a33 (- (nth 2 (nth 2 mtx)) x)))
    (+ (* a11 (- (* a22 a33) (* a23 a32)))
       (- 0 (* a12 (- (* a21 a33) (* a23 a31))))
       (* a13 (- (* a21 a32) (* a22 a31))))))

; test on 3x3 symmetric matrix.  char poly = -(x^3 - 10x^2 + 27x - 18)
;                                          = -(x-1)(x-3)(x-6), so spec = {1,3,6}.
(define A3 (list (list 4 -1 1) (list -1 3 -2) (list 1 -2 3)))

(check "charpoly-roots-are-1-3-6"
  (and (= (det3-shift A3 1) 0)
       (= (det3-shift A3 3) 0)
       (= (det3-shift A3 6) 0)))

; negative control: 2, 4 and 5 are NOT eigenvalues
(check "charpoly-nonroots"
  (and (not (= (det3-shift A3 2) 0))
       (not (= (det3-shift A3 4) 0))
       (not (= (det3-shift A3 5) 0))))

(define v0 (list 1.0 0.0 0.0))
(define ev (power-iteration A3 v0 3 40))
(define lam1 (car ev))
(define vec1 (cadr ev))
(print "Dominant eigenvalue ~" lam1)

; power iteration must converge to the LARGEST characteristic root, 6 --
; and that target came from det3-shift above, not from the iteration.
(check "power-iter-finds-dominant-eigenvalue" (close-p lam1 6))

; ... and the value it returns must actually annihilate the characteristic
; polynomial (independent confirmation that it is an eigenvalue at all).
(check "power-iter-value-is-a-charpoly-root"
  (< (abs (det3-shift A3 lam1)) 0.00001))

; residual: A v = lambda v for the converged eigenvector
(define (max-resid mtx lam vv)
  (let ((av (mvec mtx vv)))
    (foldl (lambda (acc pr) (let ((d (abs (- (car pr) (* lam (cdr pr))))))
                              (cond ((> d acc) d) (t acc))))
           0.0
           (list (cons (nth 0 av) (nth 0 vv))
                 (cons (nth 1 av) (nth 1 vv))
                 (cons (nth 2 av) (nth 2 vv))))))

(check "eigenpair-residual-is-small" (< (max-resid A3 lam1 vec1) 0.00001))

; the converged eigenvector must be parallel to (1,-1,1), the kernel of A-6I
(check "dominant-eigenvector-direction"
  (let ((s (nth 0 vec1)))
    (and (close-p (nth 1 vec1) (- 0 s))
         (close-p (nth 2 vec1) s)
         (> (abs s) 0.1))))

; Rayleigh quotient
(define (rayleigh A v)
  (let ((av (mvec A v)))
    (/ (vdot v av (length v)) (+ (vdot v v (length v)) EPS))))

; R(e1) = A[0][0] = 4 exactly; R(v1) = the dominant eigenvalue 6.
(define rq (rayleigh A3 v0))
(print "Rayleigh quotient =" rq)
(check "rayleigh-at-e1-is-a11" (close-p rq 4))
(check "rayleigh-at-eigenvector-is-eigenvalue" (close-p (rayleigh A3 vec1) 6))

; Shifted matrix A - sigma*I.  Inverse iteration would apply (A - sigma*I)^{-1}
; repeatedly to find the eigenvalue nearest sigma; here only plain power
; iteration is run on the shifted matrix.
(define (shifted A sigma n)
  (let ((As nil) (i 0))
    (shifted-rows A sigma n i As)))

(define (shifted-rows A sigma n i acc)
  (cond ((>= i n) (reverse acc))
        (t (let ((row (nth i A)) (new-row nil) (j 0))
          (shifted-cols row sigma n i j new-row A acc)))))

(define (shifted-cols row sigma n i j new-row A acc)
  (cond ((>= j n) (shifted-rows A sigma n (+ i 1)
                    (cons (reverse new-row) acc)))
        (t (shifted-cols row sigma n i (+ j 1)
             (cons (if (= i j) (- (nth j row) sigma) (nth j row)) new-row) A acc))))

; NOTE: a literal like 2.0 reads as the INTEGER 2 in SISP, so this shift is 2.
(define sig 2)
(define As3 (shifted A3 sig 3))
(print "Shifted A constructed")

; `shifted` must subtract sigma from the diagonal and leave everything else alone.
(check "shift-matrix-is-A-minus-sigma-I"
  (and (equal (nth 0 As3) (list (- 4 sig) -1 1))
       (equal (nth 1 As3) (list -1 (- 3 sig) -2))
       (equal (nth 2 As3) (list 1 -2 (- 3 sig)))))

; Spectral consequence, derived independently: spec(A - sigma I) = spec(A) - sigma.
; Each of 1,3,6 must shift down by sigma and still kill the characteristic poly
; of the SHIFTED matrix; the unshifted values must no longer be roots.
(check "shift-translates-the-spectrum"
  (and (= (det3-shift As3 (- 1 sig)) 0)
       (= (det3-shift As3 (- 3 sig)) 0)
       (= (det3-shift As3 (- 6 sig)) 0)
       (not (= (det3-shift As3 6) 0))))

; and power iteration on the shifted matrix converges to 6 - sigma = 4
(check "power-iter-on-shifted-matrix"
  (close-p (car (power-iteration As3 v0 3 40)) (- 6 sig)))

; --- one QR step by Gram-Schmidt, on a symmetric matrix ---
; A = QR with Q orthonormal; the QR *algorithm* forms A' = RQ, which is
; similar to A (A' = Q^T A Q) and therefore has the SAME spectrum.
; Here only the factorisation A = QR of the full 3x3 matrix is built and
; checked, by hand-rolled Gram-Schmidt.
(define (col-of mtx j nn)
  (cond ((>= nn 3) nil)
        (t (cons (nth j (nth nn mtx)) (col-of mtx j (+ nn 1))))))

(define (vscale v s) (map (lambda (x) (* x s)) v))
(define (vsub a b) (list (- (nth 0 a) (nth 0 b))
                         (- (nth 1 a) (nth 1 b))
                         (- (nth 2 a) (nth 2 b))))

; Gram-Schmidt on the three columns of A3
(define c0 (col-of A3 0 0))
(define c1 (col-of A3 1 0))
(define c2 (col-of A3 2 0))
(define q0 (vnormalize c0 3))
(define q1 (vnormalize (vsub c1 (vscale q0 (vdot q0 c1 3))) 3))
(define q2 (vnormalize (vsub (vsub c2 (vscale q0 (vdot q0 c2 3)))
                             (vscale q1 (vdot q1 c2 3))) 3))

(check "gram-schmidt-Q-is-orthonormal"
  (and (close-p (vdot q0 q0 3) 1) (close-p (vdot q1 q1 3) 1) (close-p (vdot q2 q2 3) 1)
       (close-p (vdot q0 q1 3) 0) (close-p (vdot q0 q2 3) 0) (close-p (vdot q1 q2 3) 0)))

; QR reconstruction: column j of A equals sum_i q_i * R(i,j), R(i,j) = q_i . a_j
(define (qr-recon-col aj)
  (let ((r0 (vdot q0 aj 3)) (r1 (vdot q1 aj 3)) (r2 (vdot q2 aj 3)))
    (list (+ (* (nth 0 q0) r0) (* (nth 0 q1) r1) (* (nth 0 q2) r2))
          (+ (* (nth 1 q0) r0) (* (nth 1 q1) r1) (* (nth 1 q2) r2))
          (+ (* (nth 2 q0) r0) (* (nth 2 q1) r1) (* (nth 2 q2) r2)))))

(check "QR-reconstructs-A"
  (and (close-p (nth 0 (qr-recon-col c0)) (nth 0 c0))
       (close-p (nth 1 (qr-recon-col c0)) (nth 1 c0))
       (close-p (nth 2 (qr-recon-col c0)) (nth 2 c0))
       (close-p (nth 0 (qr-recon-col c1)) (nth 0 c1))
       (close-p (nth 1 (qr-recon-col c1)) (nth 1 c1))
       (close-p (nth 2 (qr-recon-col c1)) (nth 2 c1))
       (close-p (nth 0 (qr-recon-col c2)) (nth 0 c2))
       (close-p (nth 1 (qr-recon-col c2)) (nth 1 c2))
       (close-p (nth 2 (qr-recon-col c2)) (nth 2 c2))))

(print "=== Eigenvalues Done ===")
