; ============================================================
; Continued Fraction Expansions of Rationals
; Compute [a0; a1, a2, ...] from p/q, convert back, and show
; that convergents give best rational approximations.
; ============================================================

(print "=== Continued Fractions ===")

(define (sub a b) (+ a (* -1 b)))
(define (floor-div a b) (/ (sub a (mod a b)) b))

; Compute the continued fraction expansion of p/q as a list [a0, a1, a2, ...]
; Works on positive integers p, q.
(define (cf-expand p q)
  (if (= q 0) nil
    (let* ((a (floor-div p q))
           (r (mod p q)))
      (cons a (cf-expand q r)))))

; Convert a continued fraction [a0; a1, a2, ...] back to a fraction p/q
; Uses the recurrence: h_k = a_k * h_{k-1} + h_{k-2}
;                      k_k = a_k * k_{k-1} + k_{k-2}
(define (cf-to-frac cfs)
  (labels (((convert lst h1 h2 k1 k2)
    (if (equal lst nil) (/ h1 k1)
      (let* ((a (car lst))
             (h (+ (* a h1) h2))
             (k (+ (* a k1) k2)))
        (convert (cdr lst) h h1 k k1)))))
  (convert cfs 1 0 0 1)))

; Compute all convergents of a continued fraction
(define (cf-convergents cfs)
  (labels (((conv lst h1 h2 k1 k2 acc)
    (if (equal lst nil) acc
      (let* ((a (car lst))
             (h (+ (* a h1) h2))
             (k (+ (* a k1) k2)))
        (conv (cdr lst) h h1 k k1 (cons (/ h k) acc))))))
  (labels (((rev lst acc) (if (equal lst nil) acc (rev (cdr lst) (cons (car lst) acc)))))
    (rev (conv cfs 1 0 0 1 nil) nil))))

; --- Examples ---
(print "--- 355/113 (famous pi approximation) ---")
(print "Continued fraction expansion:")
(print (cf-expand 355 113))
(print "Reconstructed fraction:")
(print (cf-to-frac (cf-expand 355 113)))
(print "Convergents:")
(print (cf-convergents (cf-expand 355 113)))

(print "--- 5/7 ---")
(print "CF expansion:")
(print (cf-expand 5 7))
(print "Reconstructed:")
(print (cf-to-frac (cf-expand 5 7)))
(print "Convergents:")
(print (cf-convergents (cf-expand 5 7)))

(print "--- 22/7 ---")
(print "CF expansion:")
(print (cf-expand 22 7))
(print "Reconstructed:")
(print (cf-to-frac (cf-expand 22 7)))

(print "--- 43/19 ---")
(print "CF expansion:")
(print (cf-expand 43 19))
(print "Reconstructed:")
(print (cf-to-frac (cf-expand 43 19)))
(print "Convergents (best approximations):")
(print (cf-convergents (cf-expand 43 19)))

(print "--- 1393/985 (Fibonacci ratio, approximates golden ratio) ---")
(print "CF expansion (all 1s for Fibonacci ratios):")
(print (cf-expand 1393 985))
(print "Convergents:")
(print (cf-convergents (cf-expand 1393 985)))

; Verify round-trip for several fractions
(print "--- Round-trip verification ---")
(define (verify-cf p q)
  (let ((result (cf-to-frac (cf-expand p q))))
    (if (= result (/ p q))
      (progn (print (/ p q)) (print "round-trip OK"))
      (progn (print (/ p q)) (print "round-trip FAILED")))))
(verify-cf 7 11)
(verify-cf 17 53)
(verify-cf 99 70)

(print "=== Continued Fractions Done ===")
