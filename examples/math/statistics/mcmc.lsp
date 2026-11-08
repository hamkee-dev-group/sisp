(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== MCMC: Metropolis-Hastings & Gibbs Sampling ===")

; half-open integer range [lo, hi)
(define (range lo hi) (cond ((>= lo hi) nil) (t (cons lo (range (+ lo 1) hi)))))
(seed 42)

(define pi 3.141592653589793)

; NOTE: a literal like 1.0 reads as the INTEGER 1 here, so (random 1.0) is
; (random 1) == 0 and (log 0) overflows.  Use the zero-argument (random),
; which returns a float in [0,1), kept strictly positive for the logarithm.
(define (uni) (let ((u (random))) (cond ((> u 0) u) (t 0.0000001))))

; log N(x; mu, sigma) = -(x-mu)^2 / (2 sigma^2) - log(sigma sqrt(2 pi))
(define (norm-log-pdf x mu sigma)
  (- (- 0 (/ (^ (- x mu) 2) (* 2 sigma sigma)))
     (log (* sigma (sqrt (* 2 pi))))))

(define (gaussian-sample mu sigma)
  (+ mu (* sigma (sqrt (* -2 (log (uni)))) (cos (* 2 pi (uni))))))

(define (metropolis-hastings target-log-pdf proposal-std initial iterations)
  (reverse (mh-step target-log-pdf proposal-std initial iterations nil)))

; accept a proposal with probability min(1, exp(log-alpha)),
; i.e. accept exactly when log(u) < log-alpha for u ~ Uniform(0,1)
(define (mh-step target proposal-std current remaining acc)
  (cond ((<= remaining 0) (cons current acc))
        (t (let* ((proposal (+ current (gaussian-sample 0 proposal-std)))
                  (log-alpha (- (target proposal) (target current)))
                  (accept (< (log (uni)) log-alpha)))
             (cond (accept (mh-step target proposal-std proposal (- remaining 1) (cons current acc)))
                   (t (mh-step target proposal-std current (- remaining 1) (cons current acc))))))))

(define (accept-mh target current proposal)
  (let ((log-ratio (- (target proposal) (target current))))
    (cond ((< (log (uni)) log-ratio) proposal) (t nil))))

(define (norm-target x)
  (norm-log-pdf x 0.0 1.0))

(define chain (metropolis-hastings norm-target 1.0 0.0 1000))

(define (avg lst) (/ (foldl (lambda (acc x) (+ acc x)) 0.0 lst) (length lst)))

; discard the first half of a chain as burn-in before summarising it
(define (post-burn chain) (drop chain (truncate (/ (length chain) 2))))

(define (chain-mean chain) (avg (post-burn chain)))
(define (chain-var chain)
  (let* ((post (post-burn chain)) (mu (avg post)))
    (/ (foldl (lambda (acc x) (+ acc (^ (- x mu) 2))) 0.0 post) (length post))))

(check "mh-chain-length" (> (length chain) 900))
(check "mh-converges-to-zero" (< (abs (chain-mean chain)) 0.3))

; count states that differ from their predecessor: one linear pass, not O(n^2)
(define (count-moves lst)
  (cond ((eq (cdr lst) nil) 0)
        (t (+ (cond ((= (car lst) (cadr lst)) 0) (t 1)) (count-moves (cdr lst))))))

(define (mh-accept-rate chain)
  (let ((n (- (length chain) 1)))
    (cond ((<= n 0) 0) (t (/ (count-moves chain) n)))))

(define ar (mh-accept-rate chain))
(check "mh-accept-nonzero" (> ar 0))
(check "mh-accept-below-1" (< ar 1))

(define (trace-plot chain filename)
  (foldl (lambda (acc x)
    (print (list (quote step) x))
    (+ acc 1))
    0 (take chain 20)))

(define (gelman-rubin chains)
  (let* ((m (length chains))
         (n (length (post-burn (car chains))))
         (chain-means (map (lambda (c) (chain-mean c)) chains))
         (grand-mean (avg chain-means))
         (b (* (/ n (- m 1)) (foldl (lambda (acc mu) (+ acc (^ (- mu grand-mean) 2))) 0.0 chain-means)))
         (w (/ (foldl (lambda (acc c) (+ acc (chain-var c))) 0.0 chains) m))
         (r-hat (sqrt (/ (+ (* (/ (- n 1) n) w) (/ b n)) w))))
    r-hat))

(define chain2 (metropolis-hastings norm-target 0.5 0.5 1000))
(define chain3 (metropolis-hastings norm-target 0.3 -0.2 1000))
(define gr (gelman-rubin (list chain chain2 chain3)))
(check "gelman-rubin-finite" (> gr 0))

; Gibbs sampler for the standard bivariate normal with correlation rho:
; x | y ~ N(rho y, 1 - rho^2) and y | x ~ N(rho x, 1 - rho^2)
(define (gibbs-bivariate-normal rho iterations)
  (let ((cond-sd (sqrt (- 1 (* rho rho)))))
    (gibbs-step rho cond-sd 0.0 0.0 iterations nil)))

(define (gibbs-step rho cond-sd x y remaining acc)
  (cond ((<= remaining 0) (reverse acc))
        (t (let* ((new-x (+ (* rho y) (* cond-sd (gaussian-sample 0 1))))
                  (new-y (+ (* rho new-x) (* cond-sd (gaussian-sample 0 1)))))
             (gibbs-step rho cond-sd new-x new-y (- remaining 1) (cons (list new-x new-y) acc))))))

(define gibbs-chain (gibbs-bivariate-normal 0.7 500))

(define (gibbs-mean-x chain)
  (/ (foldl (lambda (acc p) (+ acc (car p))) 0.0 (take chain 200)) 200))
(define (gibbs-var-x chain)
  (let ((mx (gibbs-mean-x chain)))
    (/ (foldl (lambda (acc p) (+ acc (^ (- (car p) mx) 2))) 0.0 (take chain 200)) 200)))

(check "gibbs-chain-length" (> (length gibbs-chain) 400))
(check "gibbs-mean-near-zero" (< (abs (gibbs-mean-x gibbs-chain)) 0.5))
(check "gibbs-variance-near-one" (< (abs (- (gibbs-var-x gibbs-chain) 1.0)) 1.0))

; lag-k autocorrelation, computed in one pass against the chain's own mean
(define (autocorrelation chain lag)
  (let* ((mu (avg chain))
         (dev (map (lambda (x) (- x mu)) chain))
         (shifted (drop dev lag))
         (num (foldl (lambda (acc p) (+ acc (* (car p) (cadr p)))) 0.0 (zip dev shifted)))
         (den (foldl (lambda (acc d) (+ acc (* d d))) 0.0 dev)))
    (/ num den)))

; ESS = n / tau with the integrated autocorrelation time tau = 1 + 2 sum_k rho_k
(define (effective-sample-size chain max-lag)
  (let* ((n (length chain))
         (rho-sum (foldl (lambda (acc k) (+ acc (autocorrelation chain k))) 0.0 (range 1 max-lag)))
         (tau-int (+ 1 (* 2 rho-sum))))
    (cond ((<= tau-int 0) n) (t (/ n tau-int)))))

(define ess (effective-sample-size chain 10))
(check "ess-positive" (> ess 0))

(define (compare-samples target-fn samples true-mean true-sd)
  (let* ((n (length samples))
         (sample-mean (avg samples))
         (sample-sd (sqrt (/ (foldl (lambda (acc x) (+ acc (^ (- x sample-mean) 2))) 0.0 samples) n))))
    (list (quote mean-err) (abs (- sample-mean true-mean)) (quote sd-err) (abs (- sample-sd true-sd)))))

(define cmp (compare-samples norm-target chain 0.0 1.0))
(check "mcmc-mean-close" (< (cadr cmp) 0.5))

(print "=== MCMC Done ===")
