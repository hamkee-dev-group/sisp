; ============================================================
; 79 — Lagrange Interpolation
;
; Given n points (xi, yi), constructs the unique polynomial of
; degree n-1 passing through all points.  Verifies that the
; interpolant evaluates correctly at each given point, and
; that it reproduces known polynomials exactly.
; ============================================================

; Lagrange basis polynomial L_i(x) = prod_{j!=i} (x - xj) / (xi - xj)
(define (lagrange-basis i points x)
  (lag-basis i points x 0 1.0 1.0))

(define (lag-basis i pts x j num den)
  (cond ((>= j (length pts))
         (cond ((= den 0.0) 0.0)
               (t (/ num den))))
        ((= i j) (lag-basis i pts x (+ j 1) num den))
        (t (let ((xi (car (nth i pts)))
                 (xj (car (nth j pts))))
             (lag-basis i pts x (+ j 1)
               (* num (- x xj))
               (* den (- xi xj)))))))

; Full Lagrange interpolant: sum_i yi * L_i(x)
(define (lagrange-eval points x)
  (lag-sum points x 0 0.0))

(define (lag-sum pts x i acc)
  (cond ((>= i (length pts)) acc)
        (t (let ((yi (cadr (nth i pts))))
             (lag-sum pts x (+ i 1)
               (+ acc (* yi (lagrange-basis i pts x))))))))

; Test: interpolate points (0,1), (1,3), (2,7)
; Known polynomial: y = x^2 + x + 1
(define pts3 (list (list 0.0 1.0) (list 1.0 3.0) (list 2.0 7.0)))

; Verify at original points
(< (abs (- (lagrange-eval pts3 0.0) 1.0)) 0.0001)
; => t

(< (abs (- (lagrange-eval pts3 1.0) 3.0)) 0.0001)
; => t

(< (abs (- (lagrange-eval pts3 2.0) 7.0)) 0.0001)
; => t

; Predict at x=3: should be 3^2+3+1 = 13
(< (abs (- (lagrange-eval pts3 3.0) 13.0)) 0.0001)
; => t

; Test: interpolate sin(0)=0, sin(pi/2)=1, sin(pi)=0
; Interpolant at pi/4 should be close to sin(pi/4) = 0.7071
(define sin-pts (list (list 0.0 0.0) (list 1.5708 1.0) (list 3.14159 0.0)))
(< (abs (- (lagrange-eval sin-pts 0.7854) 0.7071)) 0.1)
; => t

; Linear interpolation: two points (0,0), (1,2). y = 2x
(define line-pts (list (list 0.0 0.0) (list 1.0 2.0)))
(< (abs (- (lagrange-eval line-pts 0.5) 1.0)) 0.0001)
; => t
