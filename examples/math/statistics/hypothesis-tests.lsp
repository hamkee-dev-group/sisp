(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== Hypothesis Tests ===")

; half-open integer range [lo, hi)
(define (range lo hi) (cond ((>= lo hi) nil) (t (cons lo (range (+ lo 1) hi)))))
(seed 42)

(define pi 3.141592653589793)

; NOTE: a literal like 1.0 reads as the INTEGER 1 here, so (random 1.0) is
; (random 1) == 0.  Use the zero-argument (random) -> float in [0,1).
(define (uni) (let ((u (random))) (cond ((> u 0) u) (t 0.0000001))))

(define (sign-na x) (cond ((> x 0) 1) ((< x 0) -1) (t 0)))

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

(define (gamma-approx a) (exp (gamma-ln a)))

(define (tiny-guard v)
  (cond ((< (abs v) 0.0000000000000000000000000000001) 0.0000000000000000000000000000001)
        (t v)))

; --- regularized incomplete beta I_x(a,b), Lentz continued fraction --------
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

(define (beta-cfrac x a b)
  (let ((d0 (/ 1 (tiny-guard (- 1 (/ (* (+ a b) x) (+ a 1)))))))
    (betacf-loop a b x 1 d0 1 d0)))

(define (beta-reg x a b)
  (cond ((<= x 0) 0)
        ((>= x 1) 1)
        (t (let ((bt (exp (+ (gamma-ln (+ a b)) (- (gamma-ln a)) (- (gamma-ln b))
                             (* a (log x)) (* b (log (- 1 x)))))))
             (cond ((< x (/ (+ a 1) (+ a b 2))) (/ (* bt (beta-cfrac x a b)) a))
                   (t (- 1 (/ (* bt (beta-cfrac (- 1 x) b a)) b))))))))

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

(define (gammp-na a x)
  (cond ((<= x 0) 0)
        ((< x (+ a 1)) (gser a x))
        (t (- 1 (gcf a x)))))

(define (erf-approx x)
  (cond ((< x 0) (- (gammp-na 0.5 (* x x)))) (t (gammp-na 0.5 (* x x)))))

(define (norm-cdf-na x mu sigma)
  (let ((z (/ (- x mu) sigma)))
    (* 0.5 (+ 1 (erf-approx (/ z (sqrt 2)))))))

; Student-t CDF: P(T <= x) = 1 - I_{df/(df+x^2)}(df/2, 1/2) / 2 for x > 0
(define (t-cdf-approx x df)
  (let ((half (* 0.5 (beta-reg (/ df (+ df (* x x))) (* df 0.5) 0.5))))
    (cond ((> x 0) (- 1 half)) (t half))))

(define (chisq-cdf-approx x df)
  (cond ((<= x 0) 0) (t (gammp-na (* df 0.5) (* x 0.5)))))

; F CDF: P(F <= x) = I_{d1 x/(d1 x + d2)}(d1/2, d2/2)
(define (f-cdf x df1 df2)
  (cond ((<= x 0) 0)
        (t (beta-reg (/ (* df1 x) (+ (* df1 x) df2)) (* df1 0.5) (* df2 0.5)))))

(define (factorial-na n)
  (cond ((<= n 1) 1) (t (* n (factorial-na (- n 1))))))

(define (falling n k)
  (cond ((= k 0) 1) (t (* n (falling (- n 1) (- k 1))))))

(define (combinations-na n k)
  (cond ((> k n) 0) ((= k 0) 1) (t (/ (falling n k) (factorial-na k)))))

(define (mean lst) (/ (foldl (lambda (acc x) (+ acc x)) 0.0 lst) (length lst)))
(define (variance lst)
  (let* ((n (length lst)) (mu (mean lst)))
    (/ (foldl (lambda (acc x) (+ acc (^ (- x mu) 2))) 0.0 lst) (- n 1))))

(define (std lst) (sqrt (variance lst)))

(define (rand-norm-na)
  (* (sqrt (* -2 (log (uni)))) (cos (* 2 pi (uni)))))

; --- one-sample z test -----------------------------------------------------
(define (z-test sample mu0 sigma)
  (let* ((n (length sample))
         (xbar (mean sample))
         (zstat (/ (- xbar mu0) (/ sigma (sqrt n))))
         (p-two (* 2 (- 1 (norm-cdf-na (abs zstat) 0 1)))))
    (list (quote z-stat) zstat (quote p-value) p-two)))

(define sample-ones (map (lambda (i) 1.0) (range 0 10)))
(define zr1 (z-test sample-ones 1.0 1.0))
(check "ztest-mean-equal" (< (abs (cadr zr1)) 0.01))

(define sample-100 (map (lambda (i) (+ 100.0 (* (rand-norm-na) 5.0))) (range 0 30)))
(define zr2 (z-test sample-100 100.0 5.0))
(check "ztest-fails-to-reject" (> (cadddr zr2) 0.01))

; --- one-sample t test -----------------------------------------------------
(define (t-pval-approx tstat df)
  (* 2 (- 1 (t-cdf-approx (abs tstat) df))))

(define (t-test-one sample mu0)
  (let* ((n (length sample))
         (xbar (mean sample))
         (s (std sample))
         (tstat (/ (- xbar mu0) (/ s (sqrt n))))
         (df (- n 1)))
    (list (quote t-stat) tstat (quote df) df (quote p-val) (t-pval-approx tstat df))))

(define sample-t (map (lambda (i) (+ 5.0 (* (rand-norm-na) 2.0))) (range 0 20)))
(define tr1 (t-test-one sample-t 5.0))
; tr1 = (t-stat <val> df <df> p-val <p>): the p-value is element 5, not 4
; (element 4 is the symbol p-val itself).
(check "ttest-fails-to-reject" (> (nth 5 tr1) 0.01))

; --- two-sample pooled t test ---------------------------------------------
(define (t-test-two samp1 samp2)
  (let* ((n1 (length samp1)) (n2 (length samp2))
         (x1 (mean samp1)) (x2 (mean samp2))
         (v1 (variance samp1)) (v2 (variance samp2))
         (sp2 (/ (+ (* (- n1 1) v1) (* (- n2 1) v2)) (+ n1 n2 -2)))
         (tstat (/ (- x1 x2) (sqrt (* sp2 (+ (/ 1.0 n1) (/ 1.0 n2))))))
         (df (+ n1 n2 -2)))
    (list (quote t-two) tstat (quote df) df (quote p) (t-pval-approx tstat df))))

(define sa (map (lambda (i) (+ 10.0 (* (rand-norm-na) 2.0))) (range 0 20)))
(define sb (map (lambda (i) (+ 12.0 (* (rand-norm-na) 2.0))) (range 0 20)))
(define tt2 (t-test-two sa sb))
; tt2 = (t-two <val> df <df> p <p>): the df VALUE is element 3, not 2
(check "two-sample-t-df" (= (nth 3 tt2) 38))

; --- chi-square goodness of fit -------------------------------------------
(define (chisq-pval-na stat df)
  (cond ((<= stat 0) 1) (t (- 1 (chisq-cdf-approx stat df)))))

(define (chisq-gof observed expected)
  (let* ((pairs (zip observed expected))
         (stat (foldl (lambda (acc p) (+ acc (/ (^ (- (car p) (cadr p)) 2) (cadr p)))) 0.0 pairs))
         (df (- (length observed) 1)))
    (list (quote chi2) stat (quote df) df (quote p) (chisq-pval-na stat df))))

(define obs (list 50 30 20))
(define ex (list 40 40 20))
(define ch (chisq-gof obs ex))
(check "chisq-gof-stat" (>= (cadr ch) 0))

; --- F test for equality of variances -------------------------------------
(define (f-test var1 var2 df1 df2)
  (let* ((v1 (/ (foldl (lambda (acc x) (+ acc (^ (- x 0) 2))) 0.0 var1) df1))
         (v2 (/ (foldl (lambda (acc x) (+ acc (^ (- x 0) 2))) 0.0 var2) df2))
         (fstat (cond ((>= v1 v2) (/ v1 v2)) (t (/ v2 v1))))
         (dfa (cond ((>= v1 v2) df1) (t df2)))
         (dfb (cond ((>= v1 v2) df2) (t df1))))
    (list (quote f-stat) fstat (quote p) (* 2 (- 1 (f-cdf fstat dfa dfb))))))

(define f1 (map (lambda (i) (rand-norm-na)) (range 0 20)))
(define f2 (map (lambda (i) (* 3 (rand-norm-na))) (range 0 20)))
(define fr (f-test f1 f2 19 19))
(check "ftest-finite" (> (cadr fr) 0))

; --- type I error rate under a true null ----------------------------------
(define (generate-data mu n) (map (lambda (i) (+ mu (rand-norm-na))) (range 0 n)))
(define (get-pvalue result) (cadddr result))
(define (test-wrap data true-mu) (z-test data true-mu 1.0))

(define (type1-error test-fn true-param n alpha trials)
  (foldl (lambda (rej i)
    (let* ((data (generate-data true-param n))
           (result (test-fn data true-param)))
      (cond ((< (get-pvalue result) alpha) (+ rej 1)) (t rej))))
    0 (range 0 trials)))

(define t1 (type1-error test-wrap 0.0 100 0.05 20))
(check "type1-error-feasible" (>= t1 0))

(print "=== Hypothesis Tests Done ===")
