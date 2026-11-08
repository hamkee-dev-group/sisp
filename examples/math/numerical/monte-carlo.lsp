; monte-carlo.lsp — Monte Carlo integration. Importance sampling.
; MCMC Metropolis algorithm. Compute π via Monte Carlo.
; Compare with deterministic quadrature.
(print "=== Monte Carlo ===")


(define PI 3.141592653589793)

; π via Monte Carlo: random points in unit square, count in circle
(define (pi-mc n)
  (let ((inside 0))
    (pi-mc-iter 0 n inside)))

(define (pi-mc-iter i n inside)
  (cond ((>= i n) (* 4.0 (/ inside n)))
        (t (let* ((x (random)) (y (random))
                  (dist (+ (* x x) (* y y))))
          (cond ((< dist 1.0) (pi-mc-iter (+ i 1) n (+ inside 1)))
                (t (pi-mc-iter (+ i 1) n inside)))))))

(define pi-est (pi-mc 1000))
(print "MC π with 1000 points =" pi-est " error =" (abs (- pi-est PI)))
(check "pi-mc-approx" (< (abs (- pi-est PI)) 0.3))

; Monte Carlo integration: ∫_0^1 e^x dx = e-1
(define (mc-integral f a b n)
  (let ((mc-sum 0.0) (width (- b a)))
    (mc-integral-iter f a width 0 n mc-sum)))

(define (mc-integral-iter f a width i n sum)
  (cond ((>= i n) (* width (/ sum n)))
        (t (let ((x (+ a (* (random) width))))
          (mc-integral-iter f a width (+ i 1) n (+ sum (f x)))))))

(define (expf x) (exp x))
(define exact-e (- (exp 1.0) 1.0))
(define mc-e (mc-integral expf 0.0 1.0 1000))
(print "MC ∫e^x =" mc-e " exact =" exact-e)
(check "mc-integral" (< (abs (- mc-e exact-e)) 0.1))

; Importance sampling: better sampling near peak of e^x at x=1
; Use biased proposal g(x) = 2x (pdf on [0,1]) with CDF F(x) = x^2
(define (importance-sampling f n)
  (let ((sum 0.0))
    (importance-sampler f n 0 sum)))

(define (importance-sampler f n i sum)
  (cond ((>= i n) (/ sum n))
        (t (let* ((u (random))
                   (x (sqrt u))
                   (weight (/ 1.0 (* 2.0 x))))
          (importance-sampler f n (+ i 1)
                             (+ sum (* (f x) weight)))))))

(define is-e (importance-sampling expf 1000))
(print "Importance sampling ∫e^x =" is-e)
(check "importance-sampling" (< (abs (- is-e exact-e)) 0.1))

; MCMC: Metropolis algorithm for sampling from ∝ e^{-x^2/2}
(define (metropolis n)
  (let ((samples nil) (x 0.0))
    (metropolis-iter n 0 x samples)))

(define (metropolis-iter n i x acc)
  (cond ((>= i n) (reverse acc))
        ; proposal width ~2 sigma: a 0.5-wide step barely moves for a
        ; unit-variance target, so the chain never mixes in n steps.
        (t (let* ((prop (+ x (* 2.0 (- (* 2.0 (random)) 1.0))))
                   (accept-ratio (/ (target-fn prop) (+ (target-fn x) 0.000000000000001)))
                   (xnew (cond ((> (random) accept-ratio) x)
                               (t prop))))
          (metropolis-iter n (+ i 1) xnew (cons xnew acc))))))

(define (target-fn x)
  (exp (* -0.5 x x)))

; mc-mean must be defined before it is used below
(define (mc-mean samples i acc)
  (cond ((eq samples nil) (/ acc (+ i 0.000000000000001)))
        (t (mc-mean (cdr samples) (+ i 1) (+ acc (car samples))))))

; start at the mode (x=0) so no burn-in is needed; 5000 draws keeps the
; standard error of the chain mean well under the 0.5 tolerance below
(define chain (metropolis 5000))
(define chain-mean (mc-mean chain 0 0.0))
(print "MCMC chain mean (should be ~0) =" chain-mean)
(check "mcmc-mean" (< (abs chain-mean) 0.5))

; Compare MC with deterministic quadrature (Gauss5)
(define (gauss5 f a b)
  (let* ((mid (/ (+ a b) 2)) (half (/ (- b a) 2)))
    (* half (+
      (* 0.236926885056189 (f (- mid (* half 0.906179845938664))))
      (* 0.478628670499366 (f (- mid (* half 0.538469310105683))))
      (* 0.568888888888889 (f mid))
      (* 0.478628670499366 (f (+ mid (* half 0.538469310105683))))
      (* 0.236926885056189 (f (+ mid (* half 0.906179845938664))))))))

(define gs-e (gauss5 expf 0.0 1.0))
(print "Gauss5 ∫e^x =" gs-e)
(define mc-err (abs (- mc-e exact-e)))
(define gs-err (abs (- gs-e exact-e)))
(print "MC error =" mc-err " Gauss5 error =" gs-err)
(check "gauss5-more-accurate" (< gs-err mc-err))

(print "=== Monte Carlo Done ===")
