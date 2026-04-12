; ============================================================
; Farey Sequence F_n
; All reduced fractions p/q with 0 <= p/q <= 1, q <= n,
; in ascending order. Verifies the mediant property:
; consecutive a/b, c/d satisfy |b*c - a*d| = 1.
; ============================================================

(print "=== Farey Sequence ===")

(define (sub a b) (+ a (* -1 b)))
(define (my-abs x) (if (< x 0) (* -1 x) x))
(define (gcd-fn a b) (if (= b 0) a (gcd-fn b (mod a b))))

; Generate Farey sequence F_n using the next-term formula:
; Given consecutive terms a/b, c/d in F_n, the next term e/f is:
;   k = floor((n + b) / d), e = k*c - a, f = k*d - b
(define (farey-next n a b c d)
  (let* ((k (/ (sub (+ n b) (mod (+ n b) d)) d))
         (e (sub (* k c) a))
         (f (sub (* k d) b)))
    (list e f)))

; Build the full Farey sequence starting from 0/1, 1/n
(define (build-farey n)
  (labels (((build a b c d acc)
    (if (and (= c 1) (= d 1))
      (cons (list c d) acc)
      (let* ((nxt (farey-next n a b c d))
             (e (car nxt))
             (f (car (cdr nxt))))
        (build c d e f (cons (list c d) acc))))))
  (let ((result (build 0 1 1 n (list (list 0 1)))))
    ; reverse the list
    (labels (((rev lst acc) (if (equal lst nil) acc (rev (cdr lst) (cons (car lst) acc)))))
      (rev result nil)))))

; Print a Farey sequence as fractions
(define (print-farey-fracs seq)
  (if (equal seq nil) nil
    (let* ((pair (car seq))
           (p (car pair))
           (q (car (cdr pair))))
      (progn
        (print (/ p q))
        (print-farey-fracs (cdr seq))))))

; Verify mediant property: for consecutive a/b, c/d: |bc - ad| = 1
(define (verify-mediant seq)
  (if (equal (cdr seq) nil) t
    (let* ((first (car seq))
           (second (car (cdr seq)))
           (a (car first))
           (b (car (cdr first)))
           (c (car second))
           (d (car (cdr second)))
           (cross (my-abs (sub (* b c) (* a d)))))
      (if (= cross 1)
        (verify-mediant (cdr seq))
        (progn (print "FAIL: mediant property violated") nil)))))

; --- F_1 through F_5 ---
(print "--- F_3 ---")
(define f3 (build-farey 3))
(print-farey-fracs f3)

(print "--- F_4 ---")
(define f4 (build-farey 4))
(print-farey-fracs f4)

(print "--- F_5 ---")
(define f5 (build-farey 5))
(print-farey-fracs f5)

; Verify mediant property
(print "--- Mediant property verification ---")
(if (equal (verify-mediant f3) t)
  (print "F_3: mediant property holds")
  (print "F_3: mediant property FAILED"))
(if (equal (verify-mediant f4) t)
  (print "F_4: mediant property holds")
  (print "F_4: mediant property FAILED"))
(if (equal (verify-mediant f5) t)
  (print "F_5: mediant property holds")
  (print "F_5: mediant property FAILED"))

; Count terms in F_n
(print "--- Farey sequence sizes ---")
(print "F_3 size:") (print (ord f3))
(print "F_4 size:") (print (ord f4))
(print "F_5 size:") (print (ord f5))
(print "F_6 size:") (print (ord (build-farey 6)))

(print "=== Farey Sequence Done ===")
