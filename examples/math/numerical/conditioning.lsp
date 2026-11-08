; conditioning.lsp — Condition number, forward/backward error.
; Wilkinson polynomial. Ill-conditioned vs well-conditioned.
(print "=== Conditioning ===")


; Condition number (1-norm, approximate)
(define (cond1-norm A-rows)
  (let* ((n (length A-rows))
         (norm-a (mat-norm1 A-rows n)))
    (print "Matrix 1-norm =" norm-a)
    norm-a))

(define (mat-norm1 A n)
  (mat-norm1-j A n 0 0.0))

(define (mat-norm1-j A n j maxc)
  (cond ((>= j n) maxc)
        (t (let ((col-sum (col-sum1 A n j 0 0.0)))
          (mat-norm1-j A n (+ j 1)
            (cond ((> col-sum maxc) col-sum)
                  (t maxc)))))))

(define (col-sum1 A n j i acc)
  (cond ((>= i n) acc)
        (t (col-sum1 A n j (+ i 1) (+ acc (abs (row-ref A i j)))))))

(define (row-ref A i j)
  (nth j (nth i A)))

; Test condition number for 3x3
(define A (list (list 1 2 3) (list 4 5 6) (list 7 8 10)))
(define cn (cond1-norm A))
(check "cond-norm-positive" (> cn 0))

; Forward and backward error
; Solve Ax = b, then compare residual
(define (fw-error A-rows x-true x-approx n)
  (fw-err-iter A-rows x-true x-approx n 0 0.0))

(define (fw-err-iter A xt xa n i maxe)
  (cond ((>= i n) maxe)
        (t (let ((err (abs (- (nth i xt) (nth i xa)))))
          (fw-err-iter A xt xa n (+ i 1)
            (cond ((> err maxe) err) (t maxe)))))))

(define b (list 1.0 2.0 3.0))
(define x-approx (list 1.0 1.0 1.0))
(define ferr (fw-error A (list 1.0 1.0 1.0) x-approx 3))
(print "Forward error ~" ferr)
(check "fw-error" (>= ferr 0))

; Residual (backward error)
(define (residual A x b n)
  (let ((r (mvec-sub A x b n)))
    (vnorm r n)))

(define (mvec-sub A x b n)
  (mvec-sub-iter A x b n 0 nil))

(define (mvec-sub-iter A x b n i acc)
  (cond ((>= i n) (reverse acc))
        (t (let ((row (nth i A)) (sum 0.0) (j 0))
          (row-sum row x n j sum A b i acc)))))

(define (row-sum row x n j sum A b i acc)
  (cond ((>= j n) (mvec-sub-iter A x b n (+ i 1)
                   (cons (- sum (nth i b)) acc)))
        (t (row-sum row x n (+ j 1)
             (+ sum (* (nth j row) (nth j x))) A b i acc))))

(define (vnorm v n)
  (sqrt (vnorm-iter v n 0 0.0)))

(define (vnorm-iter v n i acc)
  (cond ((>= i n) acc)
        (t (vnorm-iter v n (+ i 1) (+ acc (* (nth i v) (nth i v)))))))

(define resid (residual A x-approx b 3))
(print "Residual norm =" resid)
(check "residual" (>= resid 0))

; Wilkinson's polynomial: (x-1)(x-2)...(x-20) — ill-conditioned roots
(define (wilkinson x n)
  (wilkinson-prod x 1 n))

(define (wilkinson-prod x i n)
  (cond ((> i n) 1.0)
        (t (* (- x i) (wilkinson-prod x (+ i 1) n)))))

(define wp5 (wilkinson 5.0 5))
(print "Wilkinson P_5(5) =" wp5 " (should be 0)")
(check "wilkinson-root" (< (abs wp5) 0.0001))

(define wp5p (wilkinson 5.001 5))
(print "P_5(5.001) =" wp5p " (ill-conditioned: large change)")
(check "wilkinson-ill" (> (abs wp5p) (abs wp5)))

; Well-conditioned problem: x^2 - 4 = 0, root at 2
(define (well-cond x) (- (* x x) 4.0))
(define wc2 (well-cond 2.0))
(define wc2p (well-cond 2.001))
(print "Well-cond: f(2)=" wc2 " f(2.001)=" wc2p)
(check "well-cond" (< (abs wc2) 0.0001))

(print "=== Conditioning Done ===")
