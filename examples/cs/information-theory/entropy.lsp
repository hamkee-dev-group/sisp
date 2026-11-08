; Shannon entropy and channel capacity.
; H(p1...pn) = -Σ pi log2(pi) — exact rational for dyadic, float otherwise.
; Joint entropy, conditional entropy, mutual information, BSC capacity.
; Verify: I(X;Y) ≥ 0, H(X|Y) ≤ H(X).

(define (entropy dist)
  (let ((terms (map (lambda (p) (cond ((= p 0) 0)
                                      (t (* p (log2 p))))) dist)))
    (- 0 (foldl + 0 terms))))

(define (joint-entropy xy-dist)
  (entropy xy-dist))

(define (cond-entropy x-dist xy-dist)
  (- (joint-entropy xy-dist) (entropy x-dist)))

(define (mutual-info x-dist y-dist xy-dist)
  (+ (entropy x-dist) (entropy y-dist) (- 0 (joint-entropy xy-dist))))

(define (bsc-capacity p)
  (- 1 (entropy (list p (- 1 p)))))

; Verify mutual information is non-negative
(define (verify-mi-nonneg x-dist y-dist xy-dist)
  (>= (mutual-info x-dist y-dist xy-dist) 0))

; Verify H(X|Y) <= H(X)
(define (verify-cond-entropy x-dist xy-dist)
  (<= (cond-entropy x-dist xy-dist) (entropy x-dist)))

; -- Tests on known distributions --

; Fair coin: H = 1
(define fair-coin (list 1/2 1/2))
(print (list 'fair-coin-H (entropy fair-coin))) ; => 1

; Deterministic: H = 0
(define det-dist (list 1 0))
(print (list 'deterministic-H (entropy det-dist))) ; => 0

; Fair die: H = log2(6) ≈ 2.585
(define fair-die (list 1/6 1/6 1/6 1/6 1/6 1/6))
(print (list 'fair-die-H (entropy fair-die)))

; Biased coin: H(0.25, 0.75) = -(0.25 log2 0.25 + 0.75 log2 0.75) ≈ 0.811
(define biased-coin (list 1/4 3/4))
(print (list 'biased-coin-H (entropy biased-coin)))

; BSC capacity: C = 1 - H(p)
(print (list 'bsc-capacity-p01 (bsc-capacity 1/10)))
(print (list 'bsc-capacity-p05 (bsc-capacity 1/2)))

; Mutual information for independent X,Y
(define indep-x (list 1/2 1/2))
(define indep-y (list 1/2 1/2))
(define indep-xy (list 1/4 1/4 1/4 1/4))
(print (list 'mi-indep (mutual-info indep-x indep-y indep-xy)))
(print (list 'mi-nonneg (verify-mi-nonneg indep-x indep-y indep-xy)))

; Mutual information for perfectly correlated X=Y
(define corr-x (list 1/2 1/2))
(define corr-y (list 1/2 1/2))
(define corr-xy (list 1/2 0 0 1/2))
(print (list 'mi-corr (mutual-info corr-x corr-y corr-xy)))

; Conditional entropy bound
(print (list 'cond-bound (verify-cond-entropy corr-x corr-xy)))

; Joint entropy same as H(X) when X=Y deterministic
(print (list 'joint-eq-x (joint-entropy corr-xy)))
