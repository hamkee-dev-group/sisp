; ============================================================
; 54 — Markov Chains with Exact Rational Probabilities
; IMPOSSIBLE in float languages: transition probs stay exact
; ============================================================

(print "=== Markov Chain (Exact Rationals) ===")

; Weather model: {sunny, cloudy, rainy}
; Transition matrix (as association list):
;   sunny  → sunny 1/2, cloudy 1/3, rainy 1/6
;   cloudy → sunny 1/4, cloudy 1/2, rainy 1/4
;   rainy  → sunny 1/5, cloudy 2/5, rainy 2/5

(print "States: {sunny, cloudy, rainy}")
(print "--- Transition Probabilities ---")
(print "sunny  → sunny=1/2  cloudy=1/3  rainy=1/6")
(print "cloudy → sunny=1/4  cloudy=1/2  rainy=1/4")
(print "rainy  → sunny=1/5  cloudy=2/5  rainy=2/5")

; State vector: (p_sunny p_cloudy p_rainy)
; Multiply by transition matrix

(define (step-markov ps pc pr)
  ; new_sunny  = ps*1/2 + pc*1/4 + pr*1/5
  ; new_cloudy = ps*1/3 + pc*1/2 + pr*2/5
  ; new_rainy  = ps*1/6 + pc*1/4 + pr*2/5
  (let* ((ns (+ (* ps 1/2) (* pc 1/4) (* pr 1/5)))
         (nc (+ (* ps 1/3) (* pc 1/2) (* pr 2/5)))
         (nr (+ (* ps 1/6) (* pc 1/4) (* pr 2/5))))
    (list ns nc nr)))

; Start: definitely sunny
(print "--- Starting: P(sunny)=1 ---")
(define s0 (list 1 0 0))
(print "t=0:") (print s0)

(define s1 (step-markov 1 0 0))
(print "t=1:") (print s1)

(define s2 (step-markov (nth 0 s1) (nth 1 s1) (nth 2 s1)))
(print "t=2:") (print s2)

(define s3 (step-markov (nth 0 s2) (nth 1 s2) (nth 2 s2)))
(print "t=3:") (print s3)

(define s4 (step-markov (nth 0 s3) (nth 1 s3) (nth 2 s3)))
(print "t=4:") (print s4)

; Verify probabilities sum to 1 at each step (exact!)
(print "--- Probability sum = 1 (exact) ---")
(print "t=1 sum:") (print (+ (nth 0 s1) (nth 1 s1) (nth 2 s1)))
(print "t=2 sum:") (print (+ (nth 0 s2) (nth 1 s2) (nth 2 s2)))
(print "t=3 sum:") (print (+ (nth 0 s3) (nth 1 s3) (nth 2 s3)))
(print "t=4 sum:") (print (+ (nth 0 s4) (nth 1 s4) (nth 2 s4)))

; All sums are EXACTLY 1, not 0.9999999... ← impossible with floats

; --- Starting from rainy ---
(print "--- Starting: P(rainy)=1 ---")
(define r1 (step-markov 0 0 1))
(print "t=1 from rainy:") (print r1)
(define r2 (step-markov (nth 0 r1) (nth 1 r1) (nth 2 r1)))
(print "t=2 from rainy:") (print r2)

; Both starting points converge to same stationary distribution
(print "--- Convergence ---")
(print "From sunny t=4:") (print s4)
(print "Probabilities converge to stationary distribution")
(print "No floating point drift — exact rationals forever!")

(print "=== Markov Chain Done ===")
