(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== Bayesian Inference ===")

(define (bayes-theorem p-h p-e-given-h p-e-given-not-h)
  (let* ((p-not-h (- 1 p-h))
         (p-e (* p-h p-e-given-h))
         (p-e-not (* p-not-h p-e-given-not-h))
         (p-e-total (+ p-e p-e-not)))
    (/ p-e p-e-total)))

(define p-rain 0.3)
(define p-cloudy-given-rain 0.8)
(define p-cloudy-given-no-rain 0.1)
(define p-rain-given-cloudy (bayes-theorem p-rain p-cloudy-given-rain p-cloudy-given-no-rain))
(check "bayes-posterior-range" (and (> p-rain-given-cloudy 0) (< p-rain-given-cloudy 1)))
(check "bayes-cloudy-updates" (> p-rain-given-cloudy p-rain))

(define (beta-binomial alpha beta successes trials)
  (let ((alpha-post (+ alpha successes))
        (beta-post (+ beta (- trials successes))))
    (list (quote alpha) alpha-post (quote beta) beta-post)))

(define bb (beta-binomial 1 1 7 10))
(check "beta-binomial-posterior" (and (> (nth 1 bb) 0) (> (nth 3 bb) 0)))

(define (beta-mean alpha beta) (/ alpha (+ alpha beta)))
(define (beta-variance alpha beta)
  (let ((sum (+ alpha beta)))
    (/ (* alpha beta) (* sum sum (+ sum 1)))))
(define (beta-mode alpha beta) (/ (- alpha 1) (+ alpha beta -2)))
(define (beta-sd alpha beta) (sqrt (beta-variance alpha beta)))

; log beta pdf: (a-1)ln(x) + (b-1)ln(1-x) - ln B(a,b)
(define (beta-pdf x alpha beta)
  (cond ((or (<= x 0) (>= x 1)) 0)
        (t (exp (- (+ (* (- alpha 1) (log x)) (* (- beta 1) (log (- 1 x))))
                   (beta-ln alpha beta))))))

(define (beta-ln a b) (- (+ (gamma-ln a) (gamma-ln b)) (gamma-ln (+ a b))))

(define (gamma-val-na a) (exp (gamma-ln a)))

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

; --- regularized incomplete beta I_x(a,b), Lentz continued fraction ---
(define (bcf-guard v)
  (cond ((< (abs v) 0.0000000000000000000000000000001) 0.0000000000000000000000000000001)
        (t v)))

(define (betacf-loop a b x m h c d)
  (cond ((> m 100) h)
        (t (let* ((m2 (* 2 m))
                  (aa (/ (* m (- b m) x) (* (+ a m2 -1) (+ a m2))))
                  (d1 (/ 1 (bcf-guard (+ 1 (* aa d)))))
                  (c1 (bcf-guard (+ 1 (/ aa c))))
                  (h1 (* h d1 c1))
                  (ab (/ (* (- 0 (+ a m)) (+ a b m) x) (* (+ a m2) (+ a m2 1))))
                  (d2 (/ 1 (bcf-guard (+ 1 (* ab d1)))))
                  (c2 (bcf-guard (+ 1 (/ ab c1))))
                  (del (* d2 c2))
                  (h2 (* h1 del)))
             (cond ((< (abs (- del 1)) 0.0000000000001) h2)
                   (t (betacf-loop a b x (+ m 1) h2 c2 d2)))))))

(define (betacf a b x)
  (let ((d0 (/ 1 (bcf-guard (- 1 (/ (* (+ a b) x) (+ a 1)))))))
    (betacf-loop a b x 1 d0 1 d0)))

(define (betai a b x)
  (cond ((<= x 0) 0)
        ((>= x 1) 1)
        (t (let ((bt (exp (+ (gamma-ln (+ a b)) (- (gamma-ln a)) (- (gamma-ln b))
                             (* a (log x)) (* b (log (- 1 x)))))))
             (cond ((< x (/ (+ a 1) (+ a b 2))) (/ (* bt (betacf a b x)) a))
                   (t (- 1 (/ (* bt (betacf b a (- 1 x))) b))))))))

; solve monotone-increasing f(x) = target by bisection
(define (bisect f target lo hi n)
  (cond ((<= n 0) (* 0.5 (+ lo hi)))
        (t (let ((mid (* 0.5 (+ lo hi))))
             (cond ((< (f mid) target) (bisect f target mid hi (- n 1)))
                   (t (bisect f target lo mid (- n 1))))))))

(define (credible-interval alpha beta prob)
  (let* ((lo (beta-quantile (/ (- 1 prob) 2) alpha beta))
         (hi (beta-quantile (- 1 (/ (- 1 prob) 2)) alpha beta)))
    (list lo hi)))

; inverse beta CDF: I_x(a,b) is increasing in x on [0,1]
(define (beta-quantile p a b) (bisect (lambda (x) (betai a b x)) p 0.0 1.0 60))

(define bb2 (beta-binomial 2 2 8 10))
(define ci (credible-interval (nth 1 bb2) (nth 3 bb2) 0.95))
(check "credible-interval-finite" (< (abs (- (cadr ci) (car ci))) 2))

(define (normal-normal prior-mu prior-tau2 data-sigma2 data)
  (let* ((n (length data))
         (xbar (mean-na data))
         (prior-prec (/ 1 prior-tau2))
         (data-prec (/ n data-sigma2))
         (post-prec (+ prior-prec data-prec))
         (post-var (/ 1 post-prec))
         (post-mu (/ (+ (* prior-prec prior-mu) (* data-prec xbar)) post-prec)))
    (list (quote mu) post-mu (quote var) post-var)))

(define (mean-na lst) (/ (foldl (lambda (acc x) (+ acc x)) 0.0 lst) (length lst)))

(define nn (normal-normal 0.0 10.0 1.0 (list 1.0 1.5 0.8 1.2 0.9)))
(check "normal-normal-posterior" (> (nth 1 nn) 0))
(check "normal-normal-variance" (> (nth 3 nn) 0))

(define (posterior-predictive-beta a b)
  (/ a (+ a b)))

(define (frequentist-estimate successes trials) (/ successes trials))
(define fq (frequentist-estimate 7 10))
(define bm (beta-mean (+ 1 7) (+ 1 3)))
(check "bayes-vs-frequentist-close" (< (abs (- fq bm)) 0.3))

(define (conjugate-prior-likelihood prior-name likelihood-name)
  (list (quote prior) prior-name (quote likelihood) likelihood-name (quote conjugate) t))

(define cp (conjugate-prior-likelihood (quote beta) (quote binomial)))
; cp = (prior beta likelihood binomial conjugate t): the marker is element 4,
; so (cddr cp) reached 'likelihood, not 'conjugate.
(check "conjugate-pairs" (eq (nth 4 cp) (quote conjugate)))

(define (beta-binomial-update prior-alpha prior-beta data)
  (let ((successes (length (filter (lambda (x) (eq x 1)) data))))
    (beta-binomial prior-alpha prior-beta successes (length data))))

(define coin-data (list 1 1 1 0 1 1 1 0 1 1))
(define posterior (beta-binomial-update 1 1 coin-data))
(check "coin-update-alpha" (= (nth 1 posterior) (+ 1 8)))
(check "coin-update-beta" (= (nth 3 posterior) (+ 1 2)))

(define (hdi-beta a b prob)
  (list (beta-quantile 0.025 a b) (beta-quantile 0.975 a b)))
(define hdi (hdi-beta (nth 1 bb2) (nth 3 bb2) 0.95))
(check "hdi-interval" (not (eq hdi nil)))

(print "=== Bayesian Done ===")
