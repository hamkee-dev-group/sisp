; ============================================================
; quadrature.lsp — Simpson, Gaussian, Romberg, Adaptive.
; ============================================================

(print "=== Quadrature ===")


(define PI 3.141592653589793)
(define EPS 0.000000000000001)

; --- Composite Simpson (n even) ---
(define (simpson f a b n)
  (let* ((gn a) ; guard: non-integral to force float 
         (h (/ (- b a) n))
         (h3 (/ h 3.0)))
    (simpson-sum f a h h3 0 n 0.0 0.0)))

(define (simpson-sum f a h h3 i n sum-e sum-o)
  (cond ((> i n) (* h3 (+ (f (+ a EPS)) (* 2.0 sum-e) (* 4.0 sum-o) (f (+ a (* n h))))))
        (t (let ((vi (f (+ a (* i h)))))
          (cond ((= i 0) (simpson-sum f a h h3 (+ i 1) n sum-e sum-o))
                ((= i n) (simpson-sum f a h h3 (+ i 1) n sum-e sum-o))
                ((= (mod i 2) 0) (simpson-sum f a h h3 (+ i 1) n (+ sum-e vi) sum-o))
                (t (simpson-sum f a h h3 (+ i 1) n sum-e (+ sum-o vi))))))))

; --- Gaussian quadrature ---
(define (gauss2 f a b)
  (let* ((mid (/ (+ a b) 2)) (half (/ (- b a) 2))
         (p 0.577350269189626))
    (* half (+ (f (- mid (* half p))) (f (+ mid (* half p)))))))

(define (gauss3 f a b)
  (let* ((mid (/ (+ a b) 2)) (half (/ (- b a) 2))
         (p 0.774596669241483))
    (* half (+ (* 0.555555555555556 (f (- mid (* half p))))
               (* 0.888888888888889 (f mid))
               (* 0.555555555555556 (f (+ mid (* half p))))))))

(define (gauss4 f a b)
  (let* ((mid (/ (+ a b) 2)) (half (/ (- b a) 2))
         (x1 (- mid (* half 0.861136311594053)))
         (x2 (- mid (* half 0.339981043584856)))
         (x3 (+ mid (* half 0.339981043584856)))
         (x4 (+ mid (* half 0.861136311594053))))
    (* half (+ (* 0.347854845137454 (f x1)) (* 0.652145154862546 (f x2))
               (* 0.652145154862546 (f x3)) (* 0.347854845137454 (f x4))))))

(define (gauss5 f a b)
  (let* ((mid (/ (+ a b) 2)) (half (/ (- b a) 2)))
    (* half (+
      (* 0.236926885056189 (f (- mid (* half 0.906179845938664))))
      (* 0.478628670499366 (f (- mid (* half 0.538469310105683))))
      (* 0.568888888888889 (f mid))
      (* 0.478628670499366 (f (+ mid (* half 0.538469310105683))))
      (* 0.236926885056189 (f (+ mid (* half 0.906179845938664))))))))

; --- Romberg (trapezoid + Richardson) ---
(define (trap f a b n)
  (let* ((h (/ (- b a) n))
         (aa (+ a EPS))
         (sum 0.0)
         (i 1))
    (trap-loop f aa h i n sum)))

(define (trap-loop f a h k n sum)
  (cond ((>= k n) (* h (+ (/ (+ (f a) (f (+ a (* n h)))) 2.0) sum)))
        (t (trap-loop f a h (+ k 1) n (+ sum (f (+ a (* k h))))))))

; NOTE: the tableau cannot be named T -- identifiers are lowercased at
; read time, so T collides with the reserved name t.
; R[m][0] = trapezoid with 2^m subintervals
; R[m][k] = (4^k * R[m][k-1] - R[m-1][k-1]) / (4^k - 1)
(define (romberg f a b order)
  (let ((tab (make-matrix (+ order 1) (+ order 1))))
    (mat-set tab 0 0 (trap f a b 1))
    (romberg-fill f a b tab 1 order)
    tab))

(define (romberg-fill f a b tab m n)
  (cond ((> m n) nil)
        (t (mat-set tab m 0 (trap f a b (^ 2 m)))
           (romberg-eff tab m 1)
           (romberg-fill f a b tab (+ m 1) n))))

(define (romberg-eff tab m k)
  (cond ((> k m) nil)
        (t (let* ((p4 (^ 4 k))
                  (nr (/ (- (* p4 (mat-ref tab m (- k 1)))
                            (mat-ref tab (- m 1) (- k 1)))
                         (- p4 1))))
          (mat-set tab m k nr)
          (romberg-eff tab m (+ k 1))))))

; --- Test ---
(define (expf x) (exp x))
(define (sinef x) (sin x))
(define exact-exp (- (exp 1.0) 1.0))

(print "∫_0^1 e^x =" exact-exp)
(print "∫_0^π sin(x) = 2")

(define s10 (simpson expf 0.0 1.0 10))
(define s100 (simpson expf 0.0 1.0 100))
(define ge2 (gauss2 expf 0.0 1.0))
(define ge3 (gauss3 expf 0.0 1.0))
(define ge5 (gauss5 expf 0.0 1.0))

(print "Simpson(10) =" s10)
(print "Gauss3 =" ge3)
(print "Gauss5 =" ge5)

(check "simpson-exp" (< (abs (- s10 exact-exp)) 0.1))
(check "gauss3-exp" (< (abs (- ge3 exact-exp)) 0.00001))
(check "gauss5-exp" (< (abs (- ge5 exact-exp)) 0.000001))

(define ss100 (simpson sinef 0.0 PI 100))
(define gs5 (gauss5 sinef 0.0 PI))
(print "Simpson sin =" ss100)
(print "Gauss5 sin =" gs5)
(check "simpson-sin" (< (abs (- ss100 2.0)) 0.01))
(check "gauss5-sin" (< (abs (- gs5 2.0)) 0.0001))

(define rmat (romberg expf 0.0 1.0 4))
(define rom (mat-ref rmat 4 4))
(print "Romberg R44 =" rom)
(check "romberg-ok" (< (abs (- rom exact-exp)) 0.01))

; --- Adaptive quadrature ---
(define (adaptive f a b tol depth)
  (let* ((mid (/ (+ a b) 2))
         (fa (f a)) (fb (f b)) (fm (f mid))
         (h (- b a))
         (s1 (* h 0.5 (+ fa fb)))
         (s2 (* h 0.25 (+ fa (* 2.0 fm) fb))))
    (cond ((or (< (abs (- s2 s1)) tol) (>= depth 15)) s2)
          (t (+ (adaptive f a mid (/ tol 2.0) (+ depth 1))
                (adaptive f mid b (/ tol 2.0) (+ depth 1)))))))

(define ad (adaptive expf 0.0 1.0 0.00001 0))
(print "Adaptive =" ad)
(check "adaptive-ok" (< (abs (- ad exact-exp)) 0.01))

(print "=== Quadrature Done ===")
