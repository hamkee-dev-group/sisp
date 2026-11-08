(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== Statistical Distributions ===")

; half-open integer range [lo, hi)
(define (range lo hi) (cond ((>= lo hi) nil) (t (cons lo (range (+ lo 1) hi)))))
(seed 42)

(define pi 3.141592653589793)

; NOTE: a literal like 1.0 reads as the INTEGER 1 here, so (random 1.0) is
; (random 1) == 0.  Use the zero-argument (random), which returns a float in
; [0,1), and keep it strictly positive so (log u) is finite.
(define (uni) (let ((u (random))) (cond ((> u 0) u) (t 0.0000001))))

; real exponent power, for b > 0
(define (powf b e) (exp (* e (log b))))

(define (factorial n)
  (cond ((<= n 1) 1) (t (* n (factorial (- n 1))))))

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
(define (gamma-val x) (exp (gamma-ln x)))

; --- regularized incomplete gamma P(a,x): series + continued fraction -------
(define (tiny-guard v)
  (cond ((< (abs v) 0.0000000000000000000000000000001) 0.0000000000000000000000000000001)
        (t v)))

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

; erf(x) = sign(x) * P(1/2, x^2)
(define (sign x) (cond ((> x 0) 1) ((< x 0) -1) (t 0)))
(define (erf-approx x)
  (cond ((< x 0) (- (gammp 0.5 (* x x))))
        (t (gammp 0.5 (* x x)))))

; --- normal ----------------------------------------------------------------
(define (norm-pdf x mu sigma)
  (let ((z (/ (- x mu) sigma)))
    (/ (exp (* -0.5 z z)) (* sigma (sqrt (* 2 pi))))))

(define (norm-cdf x mu sigma)
  (let ((z (/ (- x mu) sigma)))
    (* 0.5 (+ 1 (erf-approx (/ z (sqrt 2)))))))

; solve a monotone-increasing f(x) = target by bisection
(define (bisect f target lo hi n)
  (cond ((<= n 0) (* 0.5 (+ lo hi)))
        (t (let ((mid (* 0.5 (+ lo hi))))
             (cond ((< (f mid) target) (bisect f target mid hi (- n 1)))
                   (t (bisect f target lo mid (- n 1))))))))

(define (norm-inv p) (bisect (lambda (z) (norm-cdf z 0 1)) p -10.0 10.0 60))

(define (norm-quantile p mu sigma)
  (+ mu (* sigma (norm-inv p))))

(define (norm-sample mu sigma)
  (+ mu (* sigma (sqrt (* -2 (log (uni)))) (cos (* 2 pi (uni))))))

(check "norm-pdf-mean" (< (abs (- (norm-pdf 0 0 1) 0.398942)) 0.01))
(check "norm-cdf-median" (< (abs (- (norm-cdf 0 0 1) 0.5)) 0.01))
(check "norm-quantile-median" (< (abs (- (norm-quantile 0.5 0 1) 0.0)) 0.1))

; --- binomial --------------------------------------------------------------
(define (falling n k)
  (cond ((= k 0) 1) (t (* n (falling (- n 1) (- k 1))))))

(define (combinations n k)
  (cond ((> k n) 0) ((= k 0) 1) (t (/ (falling n k) (factorial k)))))

(define (binom-pmf k n p)
  (* (combinations n k) (^ p k) (^ (- 1 p) (- n k))))

(define (binom-cdf k n p)
  (foldl (lambda (acc i) (+ acc (binom-pmf i n p))) 0.0 (range 0 (+ k 1))))

(define (binom-sample n p)
  (foldl (lambda (acc i) (cond ((< (uni) p) (+ acc 1)) (t acc))) 0 (range 0 n)))

(define (fact-exact n)
  (cond ((<= n 1) 1) (t (* n (fact-exact (- n 1))))))

(check "binom-pmf-sum-to-1" (< (abs (- (binom-cdf 10 10 0.5) 1.0)) 0.001))
(check "binom-pmf-known" (< (abs (- (binom-pmf 5 10 0.5) 0.24609)) 0.01))

; --- Poisson ---------------------------------------------------------------
; computed in log space: k*ln(lam) - lam - ln(k!), so k = 50 does not overflow
(define (poisson-pmf k lam)
  (exp (- (* k (log lam)) lam (gamma-ln (+ k 1)))))

(define (poisson-cdf k lam)
  (foldl (lambda (acc i) (+ acc (poisson-pmf i lam))) 0.0 (range 0 (+ k 1))))

; Knuth: multiply uniforms until the product drops below exp(-lam)
(define (poisson-knuth lim p k)
  (let ((p1 (* p (uni))))
    (cond ((<= p1 lim) k) (t (poisson-knuth lim p1 (+ k 1))))))

(define (poisson-sample lam) (poisson-knuth (exp (- lam)) 1 0))

; The mean and variance of a Poisson are both lam.  Compute them from the pmf
; itself -- sum k*p(k) and sum k^2*p(k) -- rather than just returning lam.
(define (poisson-pdf-mean lam)
  (foldl (lambda (acc k) (+ acc (* k (poisson-pmf k lam)))) 0.0 (range 0 60)))

(define (poisson-pdf-var lam)
  (let ((mu (poisson-pdf-mean lam)))
    (- (foldl (lambda (acc k) (+ acc (* k k (poisson-pmf k lam)))) 0.0 (range 0 60))
       (* mu mu))))

(check "poisson-pmf-sum" (< (abs (- (poisson-cdf 50 3.0) 1.0)) 0.001))
(check "poisson-mean-from-pmf-is-lambda"
  (< (abs (- (poisson-pdf-mean 3.0) 3.0)) 0.001))
(check "poisson-variance-from-pmf-is-lambda"
  (< (abs (- (poisson-pdf-var 3.0) 3.0)) 0.001))
; negative control: the lam=3 mean must not match a different lam
(check "poisson-mean-distinguishes-lambda"
  (> (abs (- (poisson-pdf-mean 3.0) 5.0)) 1.0))

; --- exponential -----------------------------------------------------------
(define (exp-pdf x lam)
  (cond ((< x 0) 0) (t (* lam (exp (* (- lam) x))))))

(define (exp-cdf x lam)
  (cond ((< x 0) 0) (t (- 1 (exp (* (- lam) x))))))

(define (exp-quantile p lam)
  (/ (- (log (- 1 p))) lam))

(define (exp-sample lam)
  (/ (- (log (uni))) lam))

(check "exp-cdf-mean" (< (abs (- (exp-cdf (/ 1 2.0) 2.0) 0.632120)) 0.01))
(check "exp-quantile-median" (< (abs (- (exp-quantile 0.5 1.0) 0.693147)) 0.01))

; --- chi-square ------------------------------------------------------------
; log pdf = (k/2 - 1)ln(x) - x/2 - (k/2)ln(2) - lnGamma(k/2)
(define (chisq-pdf x df)
  (cond ((<= x 0) 0)
        (t (let ((a (* df 0.5)))
             (exp (- (* (- a 1) (log x)) (* x 0.5) (* a (log 2)) (gamma-ln a)))))))

(define (chisq-cdf x df) (gammp (* df 0.5) (* x 0.5)))

(define (chisq-sample df)
  (foldl (lambda (acc i) (+ acc (^ (norm-sample 0 1) 2))) 0.0 (range 0 (truncate df))))

(check "chisq-pdf-positive" (> (chisq-pdf 1.0 1) 0))
(check "chisq-sample-positive" (> (chisq-sample 5) 0))

; --- Student t -------------------------------------------------------------
; log pdf = lnG((v+1)/2) - lnG(v/2) - (1/2)ln(pi*v) - ((v+1)/2)ln(1 + x^2/v)
(define (t-pdf x df)
  (let ((h (* 0.5 (+ df 1))))
    (exp (- (gamma-ln h)
            (gamma-ln (* df 0.5))
            (* 0.5 (log (* pi df)))
            (* h (log (+ 1 (/ (* x x) df))))))))

(define (t-sample df)
  (/ (norm-sample 0 1) (sqrt (/ (chisq-sample df) df))))

(check "t-pdf-at-zero" (> (t-pdf 0 10) 0))
(check "t-sample-finite" (not (eq (t-sample 10) nil)))

; --- F ---------------------------------------------------------------------
; log pdf = (d1/2)ln(d1/d2) + (d1/2 - 1)ln(x)
;           - ((d1+d2)/2)ln(1 + d1*x/d2) - lnB(d1/2, d2/2)
(define (f-pdf x df1 df2)
  (cond ((<= x 0) 0)
        (t (let* ((a (* df1 0.5)) (b (* df2 0.5)) (r (/ df1 df2)))
             (exp (- (+ (* a (log r)) (* (- a 1) (log x)))
                     (* (+ a b) (log (+ 1 (* r x))))
                     (- (+ (gamma-ln a) (gamma-ln b)) (gamma-ln (+ a b)))))))))

(define (f-sample df1 df2)
  (/ (/ (chisq-sample df1) df1) (/ (chisq-sample df2) df2)))

(check "f-pdf-positive" (> (f-pdf 1.0 5 10) 0))

; --- maximum-likelihood fit ------------------------------------------------
(define (fit-normal data)
  (let* ((n (length data))
         (total (foldl (lambda (acc x) (+ acc x)) 0.0 data))
         (mu (/ total n))
         (var (/ (foldl (lambda (acc x) (+ acc (^ (- x mu) 2))) 0.0 data) n)))
    (list mu (sqrt var))))

(define sample-data (map (lambda (i) (norm-sample 5.0 2.0)) (range 0 1000)))
(define fit (fit-normal sample-data))
(check "fit-normal-mean" (< (abs (- (car fit) 5.0)) 0.5))
(check "fit-normal-sigma" (< (abs (- (cadr fit) 2.0)) 0.5))

; --- density normalisation and the t -> normal limit ----------------------
; A pdf must integrate to 1.  Midpoint rule on a fine grid; and the Student t
; must converge to the standard normal as df grows -- two densities computed
; by completely separate formulas (gamma-ln ratios vs. exp(-x^2/2)/sqrt(2pi)).
(define (integrate-mid pdff lo hi steps)
  (let ((h (/ (- hi lo) steps)))
    (* h (foldl (lambda (acc i) (+ acc (pdff (+ lo (* h (+ i 0.5))))))
                0.0 (range 0 steps)))))

(check "t-pdf-integrates-to-1"
  (< (abs (- (integrate-mid (lambda (x) (t-pdf x 10)) -40 40 4000) 1.0)) 0.001))

(check "t-pdf-is-symmetric"
  (and (< (abs (- (t-pdf 1.7 10) (t-pdf -1.7 10))) 0.000000001)
       (< (abs (- (t-pdf 0.3 4) (t-pdf -0.3 4))) 0.000000001)))

(check "t-pdf-tends-to-normal-as-df-grows"
  (and (< (abs (- (t-pdf 0 100000) (norm-pdf 0 0 1))) 0.0001)
       (< (abs (- (t-pdf 1 100000) (norm-pdf 1 0 1))) 0.0001)
       (< (abs (- (t-pdf 2 100000) (norm-pdf 2 0 1))) 0.0001)))

; negative control: at df = 2 the t is visibly heavier-tailed than the normal
(check "t-pdf-df2-differs-from-normal"
  (> (- (t-pdf 3 2) (norm-pdf 3 0 1)) 0.01))

; The F density is peaked near 0 and has a long polynomial tail, so integrate
; it in two panels (a single fine grid over the whole range exhausts the stack).
(define (f510 x) (f-pdf x 5 10))
(check "f-pdf-integrates-to-1"
  (< (abs (- (+ (integrate-mid f510 0 20 4000)
                (integrate-mid f510 20 500 4000))
             1.0))
     0.005))

; Mode of F(d1,d2) is ((d1-2)/d1)*(d2/(d2+2)) = (3/5)*(10/12) = 0.5 for (5,10).
; Confirm by scanning the density rather than by quoting the formula.
(define (argmax-scan g lo h steps)
  (foldl (lambda (best i)
           (let ((x (+ lo (* h i))))
             (cond ((> (g x) (g best)) x) (t best))))
         lo (range 1 steps)))

(check "f-pdf-mode-matches-closed-form"
  (< (abs (- (argmax-scan f510 0.001 0.001 4000)
             (* (/ (- 5 2) 5) (/ 10 (+ 10 2)))))
     0.01))
(print "=== Distributions Done ===")
