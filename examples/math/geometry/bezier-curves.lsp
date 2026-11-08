; ============================================================
; bezier-curves.lsp — Bézier curves (quadratic, cubic).
; de Casteljau, subdivision, derivatives, B-splines, length.
; ============================================================

(print "=== Bézier Curves ===")

(define (px0 p) (nth 0 p))
(define (py0 p) (nth 1 p))
; range-r must be ASCENDING (0 1 ... n-1): de Casteljau relies on the index
; order to keep the control polygon in sequence.  The old descending version
; reversed the polygon at every level, so B(0) came out as P1, not P0.
(define (range-from i n) (if (>= i n) nil (cons i (range-from (+ i 1) n))))
(define (range-r n) (range-from 0 n))

(define (lerp-bez a b t0) (+ a (* t0 (- b a))))

(define (lerp-pt pt0 pt1 t0)
  (list (lerp-bez (px0 pt0) (px0 pt1) t0)
        (lerp-bez (py0 pt0) (py0 pt1) t0)))

(define (distance-pt pt0 pt1)
  (sqrt (+ (* (- (px0 pt0) (px0 pt1)) (- (px0 pt0) (px0 pt1)))
           (* (- (py0 pt0) (py0 pt1)) (- (py0 pt0) (py0 pt1))))))

(define (my-expt x n) (if (= n 0) 1 (* x (my-expt x (- n 1)))))

(define (binomial n k)
  (if (or (= k 0) (= k n)) 1
    (+ (binomial (- n 1) (- k 1)) (binomial (- n 1) k))))

(define (de-casteljau ctrl t0)
  (let* ((n (length ctrl)))
    (define (iter pts0)
      (if (equal (cdr pts0) nil) (car pts0)
        (iter (map (lambda (i) (lerp-pt (nth i pts0) (nth (+ i 1) pts0) t0))
                   (range-r (- (length pts0) 1))))))
    (iter ctrl)))

(define (bezier-bernstein ctrl t0)
  (let* ((n (- (length ctrl) 1)))
    (foldl (lambda (acc i)
      (let* ((bi (* (binomial n i) (* (my-expt t0 i) (my-expt (- 1 t0) (- n i)))))
             (pti (nth i ctrl)))
        (list (+ (px0 acc) (* (px0 pti) bi))
              (+ (py0 acc) (* (py0 pti) bi)))))
      (list 0 0) (range-r (+ n 1)))))

(define (bezier-derivative ctrl)
  (let* ((n (- (length ctrl) 1)))
    ; NB: the (range-r n) argument belongs to MAP, not to the lambda body.
    (map (lambda (i)
      (let* ((a (nth i ctrl)) (b (nth (+ i 1) ctrl)))
        (list (* n (- (px0 b) (px0 a)))
              (* n (- (py0 b) (py0 a))))))
      (range-r n))))

(define (bezier-length-0 ctrl depth)
  (if (or (= depth 0) (< (length ctrl) 2)) 0
    ; (/ i depth) on two integers is an exact rational; force float sampling.
    (let* ((seg-points (map (lambda (i) (de-casteljau ctrl (/ i (* 1.0 depth))))
                           (range-r (+ depth 1))))
           (n0 (- (length seg-points) 1)))
      (foldl (lambda (acc i) (+ acc (distance-pt (nth i seg-points) (nth (+ i 1) seg-points))))
             0 (range-r n0)))))

(define (bspline-basis-bez i k0 t0 knots)
  (if (= k0 1)
    (if (and (>= t0 (nth i knots)) (< t0 (nth (+ i 1) knots))) 1 0)
    (let* ((den1 (- (nth (+ i k0 -1) knots) (nth i knots)))
           (den2 (- (nth (+ i k0) knots) (nth (+ i 1) knots)))
           (term1 (if (= den1 0) 0
                    (/ (* (- t0 (nth i knots)) (bspline-basis-bez i (- k0 1) t0 knots)) den1)))
           (term2 (if (= den2 0) 0
                    (/ (* (- (nth (+ i k0) knots) t0) (bspline-basis-bez (+ i 1) (- k0 1) t0 knots)) den2))))
      (+ term1 term2))))

(define quad-ctrl '((0 0) (2 3) (4 0)))
(define cubic-ctrl '((0 0) (1 2) (3 2) (4 0)))

(print "--- Quadratic de Casteljau ---")
(print "at t=0.0:") (print (de-casteljau quad-ctrl 0.0))
(print "at t=0.5:") (print (de-casteljau quad-ctrl 0.5))
(print "at t=1.0:") (print (de-casteljau quad-ctrl 1.0))
(check "quad-start" (equal (de-casteljau quad-ctrl 0.0) (nth 0 quad-ctrl)))
(check "quad-end" (equal (de-casteljau quad-ctrl 1.0) (nth 2 quad-ctrl)))

(print "--- Bernstein vs de Casteljau ---")
(define bb05 (bezier-bernstein quad-ctrl 0.5))
(define dc05 (de-casteljau quad-ctrl 0.5))
(print (list "Bernstein:" bb05 "deCasteljau:" dc05))
(check "bernstein-match" (and (< (abs (- (px0 bb05) (px0 dc05))) 0.001)
                              (< (abs (- (py0 bb05) (py0 dc05))) 0.001)))

(print "--- Cubic de Casteljau ---")
(print (de-casteljau cubic-ctrl 0.5))
(check "cubic-valid" (consp (de-casteljau cubic-ctrl 0.5)))

(print "--- Derivative ---")
(define quad-deriv (bezier-derivative quad-ctrl))
(print quad-deriv)
(check "deriv-count" (= (length quad-deriv) 2))

(print "--- Arc Length ---")
(define qlen (bezier-length-0 quad-ctrl 10))
(define clen (bezier-length-0 cubic-ctrl 10))
(print (list "quad len:" qlen "cubic len:" clen))
(check "quad-length-pos" (> qlen 0))
(check "cubic-length-pos" (> clen 0))

(print "--- B-spline Basis ---")
(define knots-bez '(0 0 0 1 2 3 3 3))
(print (list "B0,3(0.5):" (bspline-basis-bez 0 3 0.5 knots-bez)))
(print (list "B1,3(0.5):" (bspline-basis-bez 1 3 0.5 knots-bez)))
(print (list "B2,3(0.5):" (bspline-basis-bez 2 3 0.5 knots-bez)))
(check "bspline-sum-1" (< (abs (- (+ (bspline-basis-bez 0 3 0.5 knots-bez)
                                      (bspline-basis-bez 1 3 0.5 knots-bez)
                                      (bspline-basis-bez 2 3 0.5 knots-bez)
                                      (bspline-basis-bez 3 3 0.5 knots-bez)) 1)) 0.01))

(print "=== Done ===")
