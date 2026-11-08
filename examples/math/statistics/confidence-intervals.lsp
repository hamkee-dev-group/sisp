(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== Confidence Intervals ===")

; half-open integer range [lo, hi)
(define (range lo hi) (cond ((>= lo hi) nil) (t (cons lo (range (+ lo 1) hi)))))
(seed 42)

(define pi 3.141592653589793)

; NOTE: a literal like 1.0 reads as the INTEGER 1 here, so (random 1.0) is
; (random 1) == 0 and (log 0) overflows.  Use the zero-argument (random),
; which returns a float in [0,1), kept strictly positive for the logarithm.
(define (uni) (let ((u (random))) (cond ((> u 0) u) (t 0.0000001))))

(define (mean lst) (/ (foldl (lambda (acc x) (+ acc x)) 0.0 lst) (length lst)))
(define (std-dev lst)
  (let* ((n (length lst)) (mu (mean lst)))
    (sqrt (/ (foldl (lambda (acc x) (+ acc (^ (- x mu) 2))) 0.0 lst) (- n 1)))))

; Lanczos log-gamma (Numerical Recipes coefficients), ~1e-10 relative accuracy
(define (gamma-ln xx)
  (let* ((tmp0 (+ xx 5.5))
         (tmp (- tmp0 (* (+ xx 0.5) (log tmp0))))
         (ser (+ 1.000000000190015
                 (/ 76.18009172947146 (+ xx 1))
                 (/ -86.50532032941677 (+ xx 2))
                 (/ 24.01409824083091 (+ xx 3))
                 (/ -1.231739572450155 (+ xx 4))
                 (/ 0.001208650973866179 (+ xx 5))
                 (/ -0.000005395239384953 (+ xx 6)))))
    (+ (- tmp) (log (/ (* 2.5066282746310005 ser) xx)))))

(define (tiny-guard v)
  (cond ((< (abs v) 0.0000000000000000000000000000001) 0.0000000000000000000000000000001)
        (t v)))

; --- regularized incomplete gamma P(a,x) ----------------------------------
(define (gser-loop a x ap del sum n)
  (cond ((> n 300) sum)
        (t (let* ((ap1 (+ ap 1)) (del1 (* del (/ x ap1))) (sum1 (+ sum del1)))
             (cond ((< (abs del1) (* (abs sum1) 0.0000000000001)) sum1)
                   (t (gser-loop a x ap1 del1 sum1 (+ n 1))))))))

(define (gser a x)
  (let ((d0 (/ 1 a)))
    (* (gser-loop a x a d0 d0 0) (exp (+ (- (gamma-ln a)) (* a (log x)) (- x))))))

(define (gcf-loop a x i b c d h)
  (cond ((> i 300) h)
        (t (let* ((an (* (- 0 i) (- i a)))
                  (b1 (+ b 2))
                  (d1 (/ 1 (tiny-guard (+ (* an d) b1))))
                  (c1 (tiny-guard (+ b1 (/ an c))))
                  (del (* d1 c1))
                  (h1 (* h del)))
             (cond ((< (abs (- del 1)) 0.0000000000001) h1)
                   (t (gcf-loop a x (+ i 1) b1 c1 d1 h1)))))))

(define (gcf a x)
  (let* ((b0 (+ x 1 (- a)))
         (c0 (/ 1 0.000000000000000000000000000001))
         (d0 (/ 1 (tiny-guard b0))))
    (* (exp (+ (- x) (* a (log x)) (- (gamma-ln a))))
       (gcf-loop a x 1 b0 c0 d0 d0))))

(define (gammp a x)
  (cond ((<= x 0) 0)
        ((< x (+ a 1)) (gser a x))
        (t (- 1 (gcf a x)))))

(define (erf-approx x)
  (cond ((< x 0) (- (gammp 0.5 (* x x)))) (t (gammp 0.5 (* x x)))))

(define (norm-cdf x mu sigma)
  (let ((z (/ (- x mu) sigma)))
    (* 0.5 (+ 1 (erf-approx (/ z (sqrt 2)))))))

; --- regularized incomplete beta I_x(a,b) ---------------------------------
(define (betacf-loop a b x m h c d)
  (cond ((> m 200) h)
        (t (let* ((m2 (* 2 m))
                  (aa (/ (* m (- b m) x) (* (+ a m2 -1) (+ a m2))))
                  (d1 (/ 1 (tiny-guard (+ 1 (* aa d)))))
                  (c1 (tiny-guard (+ 1 (/ aa c))))
                  (h1 (* h d1 c1))
                  (ab (/ (* (- 0 (+ a m)) (+ a b m) x) (* (+ a m2) (+ a m2 1))))
                  (d2 (/ 1 (tiny-guard (+ 1 (* ab d1)))))
                  (c2 (tiny-guard (+ 1 (/ ab c1))))
                  (del (* d2 c2))
                  (h2 (* h1 del)))
             (cond ((< (abs (- del 1)) 0.0000000000001) h2)
                   (t (betacf-loop a b x (+ m 1) h2 c2 d2)))))))

(define (betacf a b x)
  (let ((d0 (/ 1 (tiny-guard (- 1 (/ (* (+ a b) x) (+ a 1)))))))
    (betacf-loop a b x 1 d0 1 d0)))

(define (betai a b x)
  (cond ((<= x 0) 0)
        ((>= x 1) 1)
        (t (let ((bt (exp (+ (gamma-ln (+ a b)) (- (gamma-ln a)) (- (gamma-ln b))
                             (* a (log x)) (* b (log (- 1 x)))))))
             (cond ((< x (/ (+ a 1) (+ a b 2))) (/ (* bt (betacf a b x)) a))
                   (t (- 1 (/ (* bt (betacf b a (- 1 x))) b))))))))

; Student-t CDF: P(T <= x) = 1 - I_{df/(df+x^2)}(df/2, 1/2) / 2 for x > 0
(define (t-cdf x df)
  (let ((half (* 0.5 (betai (* df 0.5) 0.5 (/ df (+ df (* x x)))))))
    (cond ((> x 0) (- 1 half)) (t half))))

(define (chisq-cdf x df) (gammp (* df 0.5) (* x 0.5)))

; --- quantiles by bisection on the (increasing) CDFs ----------------------
(define (bisect f target lo hi n)
  (cond ((<= n 0) (* 0.5 (+ lo hi)))
        (t (let ((mid (* 0.5 (+ lo hi))))
             (cond ((< (f mid) target) (bisect f target mid hi (- n 1)))
                   (t (bisect f target lo mid (- n 1))))))))

(define (norm-inv p) (bisect (lambda (z) (norm-cdf z 0 1)) p -10.0 10.0 60))
(define (norm-quantile p mu sigma) (+ mu (* sigma (norm-inv p))))
(define (t-quantile p df) (bisect (lambda (x) (t-cdf x df)) p -100.0 100.0 70))
(define (chisq-quantile p df) (bisect (lambda (x) (chisq-cdf x df)) p 0.0 1000.0 70))

(define (z-crit alpha) (norm-quantile (- 1 (/ alpha 2)) 0 1))
; the t critical value genuinely depends on df; it is not the normal one
(define (t-crit alpha df) (t-quantile (- 1 (/ alpha 2)) df))

(define (rand-norm-na)
  (* (sqrt (* -2 (log (uni)))) (cos (* 2 pi (uni)))))

; --- confidence intervals for a mean --------------------------------------
(define (ci-mean-z data sigma alpha)
  (let* ((n (length data))
         (xbar (mean data))
         (z (z-crit alpha))
         (margin (* z (/ sigma (sqrt n)))))
    (list (- xbar margin) (+ xbar margin))))

(define (ci-mean-t data alpha)
  (let* ((n (length data))
         (xbar (mean data))
         (s (std-dev data))
         (t-c (t-crit alpha (- n 1)))
         (margin (* t-c (/ s (sqrt n)))))
    (list (- xbar margin) (+ xbar margin))))

(define sample-z (map (lambda (i) (rand-norm-na)) (range 0 100)))

(define ci1 (ci-mean-z sample-z 1.0 0.05))
(check "ci-mean-covers-zero" (and (< (car ci1) 0) (> (cadr ci1) 0)))

(define ci2 (ci-mean-t sample-z 0.05))
(check "ci-mean-t-finite" (< (abs (- (cadr ci2) (car ci2))) 10))

; --- Wald interval for a proportion ---------------------------------------
(define (ci-prop p-hat n alpha)
  (let* ((z (z-crit alpha))
         (margin (* z (sqrt (/ (* p-hat (- 1 p-hat)) n)))))
    (list (max 0 (- p-hat margin)) (min 1 (+ p-hat margin)))))

(define pc (ci-prop 0.5 100 0.05))
(check "ci-prop-covers-half" (and (< (car pc) 0.5) (> (cadr pc) 0.5)))

; --- chi-square interval for a variance -----------------------------------
; (n-1)s^2 / chi2_{1-a/2}  <=  sigma^2  <=  (n-1)s^2 / chi2_{a/2}
(define (chisq-quant-upper p df) (chisq-quantile p df))
(define (chisq-quant-lower p df) (chisq-quantile p df))

(define (ci-var data alpha)
  (let* ((n (length data))
         (s2 (^ (std-dev data) 2)))
    (list (/ (* (- n 1) s2) (chisq-quant-upper (- 1 (/ alpha 2)) (- n 1)))
          (/ (* (- n 1) s2) (chisq-quant-lower (/ alpha 2) (- n 1))))))

(define cv (ci-var sample-z 0.05))
(check "ci-var-positive" (> (car cv) 0))

; --- bootstrap ------------------------------------------------------------
(define (bootstrap-sample data)
  (let ((n (length data)))
    (map (lambda (i) (nth (random n) data)) (range 0 n))))

(define (bootstrap-stat data stat-fn boot-reps)
  (map (lambda (i) (stat-fn (bootstrap-sample data))) (range 0 boot-reps)))

(define (sort-na lst)
  (cond ((eq lst nil) nil) (t (insert-na (car lst) (sort-na (cdr lst))))))

(define (insert-na x l)
  (cond ((eq l nil) (list x)) ((< x (car l)) (cons x l)) (t (cons (car l) (insert-na x (cdr l))))))

; clamp a fractional position to a valid index of an n-element list
(define (quantile-index q n)
  (let ((idx (truncate (* q n))))
    (cond ((< idx 0) 0) ((>= idx n) (- n 1)) (t idx))))

(define (percentile-ci boots alpha)
  (let* ((sorted (sort-na boots))
         (n (length sorted))
         (lo (nth (quantile-index (/ alpha 2) n) sorted))
         (hi (nth (quantile-index (- 1 (/ alpha 2)) n) sorted)))
    (list lo hi)))

(define boots (bootstrap-stat sample-z mean 200))
(define bci (percentile-ci boots 0.05))
(check "bootstrap-ci-covers-zero" (and (< (car bci) 0) (> (cadr bci) 0)))

; --- empirical coverage of the z interval ---------------------------------
(define (ci-coverage samples true-param ci-fn alpha)
  (let ((total (length samples)))
    (/ (foldl (lambda (c data)
                (let ((ci (ci-fn data alpha)))
                  (cond ((and (<= (car ci) true-param) (<= true-param (cadr ci))) (+ c 1)) (t c))))
              0 samples)
       total)))

(define many-samples (map (lambda (i) (map (lambda (j) (rand-norm-na)) (range 0 30))) (range 0 50)))
(define coverage (ci-coverage many-samples 0.0 (lambda (d a) (ci-mean-z d 1.0 a)) 0.05))
(check "coverage-simulation" (> coverage 0))

; --- BCa bootstrap interval -----------------------------------------------
; bias correction z0 from the share of bootstrap replicates below theta-hat,
; acceleration a from the jackknife skewness of the statistic.
(define (drop-nth i lst)
  (append (take lst i) (drop lst (+ i 1))))

(define (jackknife-accel data stat-fn)
  (let* ((n (length data))
         (vals (map (lambda (i) (stat-fn (drop-nth i data))) (range 0 n)))
         (vbar (mean vals))
         (d2 (foldl (lambda (acc v) (+ acc (^ (- vbar v) 2))) 0.0 vals))
         (d3 (foldl (lambda (acc v) (+ acc (^ (- vbar v) 3))) 0.0 vals)))
    (cond ((<= d2 0) 0)
          (t (/ d3 (* 6 (exp (* 1.5 (log d2)))))))))

(define (bca-endpoint z0 accel z-alpha)
  (let ((adj (+ z0 (/ (+ z0 z-alpha) (- 1 (* accel (+ z0 z-alpha)))))))
    (norm-cdf adj 0 1)))

(define (bca-ci data stat-fn boot-reps alpha)
  (let* ((theta-hat (stat-fn data))
         (boots (bootstrap-stat data stat-fn boot-reps))
         (sorted (sort-na boots))
         (below (length (filter (lambda (b) (< b theta-hat)) boots)))
         (frac (cond ((<= below 0) (/ 1 (* 2 boot-reps)))
                     ((>= below boot-reps) (- 1 (/ 1 (* 2 boot-reps))))
                     (t (/ below boot-reps))))
         (z0 (norm-inv frac))
         (accel (jackknife-accel data stat-fn))
         (q-lo (bca-endpoint z0 accel (norm-inv (/ alpha 2))))
         (q-hi (bca-endpoint z0 accel (norm-inv (- 1 (/ alpha 2))))))
    (list (nth (quantile-index q-lo boot-reps) sorted)
          (nth (quantile-index q-hi boot-reps) sorted))))

(define bca-c (bca-ci sample-z mean 200 0.05))
(check "bca-ci-finite" (< (abs (- (cadr bca-c) (car bca-c))) 10))

(print "=== Confidence Intervals Done ===")
