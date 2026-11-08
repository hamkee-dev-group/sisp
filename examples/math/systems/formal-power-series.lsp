; ============================================================
; 91 — Formal Power Series (TAOCP Vol 2, 4.7)
; Truncated series as coefficient lists (constant term first).
; Add, multiply (convolution), derivative, integral, reciprocal
; (to degree n). Verify identities:
;   (1/(1-x)) * (1-x) = 1
;   derivative of sin series = cos series (to degree 5)
; ============================================================

(print "=== Formal Power Series ===")


; ---------- series operations ----------
(define (s-add a b)
  (cond ((and (eq a nil) (eq b nil)) nil)
        ((eq a nil) (cons (car b) (s-add nil (cdr b))))
        ((eq b nil) (cons (car a) (s-add (cdr a) nil)))
        (t (cons (+ (car a) (car b)) (s-add (cdr a) (cdr b))))))

; Convolution: c_n = sum_{k=0..n} a_k * b_{n-k}.
; The result has length (length a + length b - 1): the full product.
(define (s-mul a b)
  (cond ((or (eq a nil) (eq b nil)) nil)
        (t (cons (* (car a) (car b))
             (s-add (map (lambda (x) (* (car a) x)) (cdr b))
                    (s-mul (cdr a) b))))))

; Derivative: shift left, multiply by index+1.
(define (s-deriv a)
  (cond ((or (eq a nil) (eq (cdr a) nil)) nil)
        (t (labels
            (((walk coeffs k)
              (cond ((eq coeffs nil) nil)
                    (t (cons (* k (car coeffs))
                             (walk (cdr coeffs) (+ k 1)))))))
            (walk (cdr a) 1)))))

; Integral: shift right, divide by index.
(define (s-integ a)
  (cons 0
    (labels
      (((walk coeffs k)
        (cond ((eq coeffs nil) nil)
              (t (cons (/ (car coeffs) k)
                       (walk (cdr coeffs) (+ k 1)))))))
      (walk a 1))))

; Reciprocal: b_0 = 1/a_0, b_k = -(sum_{i=1..k} a_i * b_{k-i}) / a_0.
; This first definition returns only b_0; it is replaced by the full version
; below.
(define (s-recip a deg)
  (cond ((= deg 0) nil)
        (t (let ((a0 (car a)))
             (list (/ 1 a0))))))

; Iterative construction: b-prev holds the coefficients found so far and
; each step appends the next one.  (s-recip a n) returns b_0..b_{n-1}.
(define (s-recip-iter a deg k b-prev)
  (cond ((> k deg) b-prev)
        (t (let ((a0 (car a)))
             (let ((next-b (s-recip-coeff a b-prev k a0)))
               (s-recip-iter a deg (+ k 1)
                 (append b-prev (list next-b))))))))

(define (s-recip-coeff a b-prev k a0)
  (cond ((= k 1) (/ 1 a0))
        (t (let ((sumb (s-recip-sum a b-prev (- k 1) 0 1)))
             (/ (- sumb) a0)))))

(define (s-recip-sum a b-prev i acc idx)
  (cond ((> idx i) acc)
        (t
         (let ((ai (nth idx a))
               (bi (nth (- i idx) b-prev)))
           (s-recip-sum a b-prev i
             (+ acc (* (cond ((eq ai nil) 0) (t ai))
                       (cond ((eq bi nil) 0) (t bi))))
             (+ idx 1))))))

(define (s-recip a deg)
  (s-recip-iter a deg 1 nil))

; ---------- test series ----------
(define one-minus-x (list 1 -1))
(define inv-one-minus-x (s-recip one-minus-x 8))
(check "recip-1-x-is-geometric"
  (equal inv-one-minus-x (list 1 1 1 1 1 1 1 1)))

(define prod (s-mul inv-one-minus-x one-minus-x))
(print (list 'prod prod))
(check "recip-times-original-is-one"
  (equal (take prod (length inv-one-minus-x))
         (list 1 0 0 0 0 0 0 0)))

; sin: x - x^3/3! + x^5/5! = [0, 1, 0, -1/6, 0, 1/120]
(define sin-series (list 0 1 0 (/ -1 6) 0 (/ 1 120)))
; cos: 1 - x^2/2! + x^4/4! = [1, 0, -1/2, 0, 1/24]
(define cos-series (list 1 0 (/ -1 2) 0 (/ 1 24)))

(check "d-sin-is-cos"
  (equal (s-deriv sin-series) cos-series))

; d(cos) = -sin (compare the 4 terms of d(cos))
(define neg-sin (map (lambda (x) (* -1 x)) sin-series))
(define d-cos (s-deriv cos-series))
(check "d-cos-is-neg-sin"
  (equal (take d-cos (length d-cos))
         (take neg-sin (length d-cos))))

; integral of cos = sin (first 6 terms, through x^5)
(check "integ-cos-is-sin"
  (equal (take (s-integ cos-series) (length sin-series))
         sin-series))

; Add and multiply tests.
(define a-series (list 1 2 3))
(define b-series (list 4 5 6))
(check "series-add" (equal (s-add a-series b-series) (list 5 7 9)))
(check "series-mul" (equal (s-mul a-series b-series) (list 4 13 28 27 18)))

(print "=== Formal Power Series Done ===")
