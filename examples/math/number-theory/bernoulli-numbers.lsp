; ============================================================
; Bernoulli Numbers B_0 through B_8
; Computed using the recursive formula:
;   sum_{k=0}^{n} C(n+1,k) * B_k = 0
; These are exact rationals.
; ============================================================

(print "=== Bernoulli Numbers ===")

(define (sub a b) (+ a (* -1 b)))

; Factorial
(define (fact n)
  (if (<= n 1) 1 (* n (fact (sub n 1)))))

; Binomial coefficient C(n, k) = n! / (k! * (n-k)!)
(define (binom n k)
  (if (> k n) 0
    (/ (fact n) (* (fact k) (fact (sub n k))))))

; Compute B_n using the recurrence:
;   B_n = -1/(n+1) * sum_{k=0}^{n-1} C(n+1, k) * B_k
; We build up a list of known Bernoulli numbers.
(define (bernoulli-list n)
  (labels (
    ; Get kth element from list (0-indexed)
    ((get-at k lst)
      (if (= k 0) (car lst) (get-at (sub k 1) (cdr lst))))
    ; Sum C(m+1, k) * B_k for k=0..m-1 using the already-computed list
    ((bern-sum m k bs)
      (if (= k m) 0
        (+ (* (binom (+ m 1) k) (get-at k bs))
           (bern-sum m (+ k 1) bs))))
    ; Build up Bernoulli numbers B_0..B_n in a list
    ((build m bs)
      (if (> m n) bs
        (let* ((s (bern-sum m 0 bs))
               (bm (* (/ -1 (+ m 1)) s)))
          (build (+ m 1) (append bs (list bm)))))))
  ; B_0 = 1
  (build 1 (list 1))))

; Compute Bernoulli numbers B_0 through B_8
(define bernoullis (bernoulli-list 8))

; Display each one
(define (show-bernoulli k lst)
  (if (equal lst nil) nil
    (progn
      (print "---")
      (print "B_") (print k)
      (print (car lst))
      (show-bernoulli (+ k 1) (cdr lst)))))

(show-bernoulli 0 bernoullis)

; --- Verify known values ---
(print "--- Verification ---")
(define (get-at k lst) (if (= k 0) (car lst) (get-at (sub k 1) (cdr lst))))

; B_0 = 1
(if (= (get-at 0 bernoullis) 1)
  (print "B_0 = 1 VERIFIED")
  (print "B_0 FAILED"))

; B_1 = -1/2
(if (= (get-at 1 bernoullis) -1/2)
  (print "B_1 = -1/2 VERIFIED")
  (print "B_1 FAILED"))

; B_2 = 1/6
(if (= (get-at 2 bernoullis) 1/6)
  (print "B_2 = 1/6 VERIFIED")
  (print "B_2 FAILED"))

; B_3 = 0 (odd Bernoulli numbers beyond B_1 are zero)
(if (= (get-at 3 bernoullis) 0)
  (print "B_3 = 0 VERIFIED")
  (print "B_3 FAILED"))

; B_4 = -1/30
(if (= (get-at 4 bernoullis) -1/30)
  (print "B_4 = -1/30 VERIFIED")
  (print "B_4 FAILED"))

; B_5 = 0
(if (= (get-at 5 bernoullis) 0)
  (print "B_5 = 0 VERIFIED")
  (print "B_5 FAILED"))

; B_6 = 1/42
(if (= (get-at 6 bernoullis) 1/42)
  (print "B_6 = 1/42 VERIFIED")
  (print "B_6 FAILED"))

; B_8 = -1/30
(if (= (get-at 8 bernoullis) -1/30)
  (print "B_8 = -1/30 VERIFIED")
  (print "B_8 FAILED"))

(print "=== Bernoulli Numbers Done ===")
