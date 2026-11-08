; ============================================================
; 122 — Branch and Bound (0/1 Knapsack)
; Capacity=10, items: (w=4,v=8), (w=3,v=5), (w=5,v=12), (w=2,v=3)
; B&B with LP relaxation (fractional knapsack) bound.
; Optimal: items {5+3+2} = weight 10, value 20.
; Verify against brute force enumeration of all 16 subsets.
; ============================================================

(print "=== Branch and Bound (Knapsack) ===")


(define items '((5 12) (4 8) (3 5) (2 3)))
(define kncap 10)
(define n (length items))

; Items already sorted by value/weight: 12/5=2.4, 8/4=2, 5/3≈1.67, 3/2=1.5
(define sorted-items items) ; already sorted

(print "Items (weight value):")
(print sorted-items)

; Best solution
(setq *best-val* 0)
(setq *best-set* nil)

; Fractional knapsack upper bound
(define (frac-bound start-idx rem-w rem-v)
  (cond ((>= start-idx n) rem-v)
        ((<= rem-w 0) rem-v)
        (t (let ((item (nth start-idx sorted-items)))
          (let ((w (car item)) (v (cadr item)))
            (cond ((<= w rem-w)
                   (frac-bound (+ start-idx 1) (- rem-w w) (+ rem-v v)))
                  (t (+ rem-v (* (/ rem-w w) v)))))))))

; Branch and bound
(define (bb idx rem-w rem-v chosen)
  (cond ((>= idx n)
         (cond ((> rem-v *best-val*)
                (setq *best-val* rem-v)
                (setq *best-set* chosen)
                t)
               (t nil)))
        (t (let* ((item (nth idx sorted-items))
                  (w (car item)) (v (cadr item))
                  (skip-bound (frac-bound (+ idx 1) rem-w rem-v))
                  (take-bound (cond ((>= rem-w w)
                                     (frac-bound (+ idx 1) (- rem-w w) (+ rem-v v)))
                                    (t 0))))
             ; Branch: take item if it fits and bound promising
             (cond ((and (>= rem-w w) (> take-bound *best-val*))
                    (bb (+ idx 1) (- rem-w w) (+ rem-v v) (cons idx chosen))
                    nil)
                   (t nil))
             ; Branch: skip item if bound promising
             (cond ((> skip-bound *best-val*)
                    (bb (+ idx 1) rem-w rem-v chosen)
                    nil)
                   (t nil))))))

(bb 0 kncap 0 nil)

(print "B&B best value:")
(print *best-val*)
(print "B&B chosen indices:")
(print *best-set*)

(check "bb-best-20" (= *best-val* 20))

; --- Brute force ---
(define (all-subsets lst)
  (cond ((equal lst nil) (list nil))
        (t (let ((r (all-subsets (cdr lst))))
          (append r (map (lambda (s) (cons (car lst) s)) r))))))

(define idx-list (seq 0 (- n 1)))
(define all-subs (all-subsets idx-list))

(define (subset-value indices)
  (foldl (lambda (a i) (+ a (cadr (nth i items)))) 0 indices))

(define (subset-weight indices)
  (foldl (lambda (a i) (+ a (car (nth i items)))) 0 indices))

(define (valid-subset indices)
  (<= (subset-weight indices) kncap))

(define valid-subs (filter valid-subset all-subs))
(define best-brute (foldl (lambda (a s) (let ((v (subset-value s))) (cond ((> v a) v) (t a)))) 0 valid-subs))
(print "Brute force best value:")
(print best-brute)
(check "brute-force-is-20" (= best-brute 20))
(check "bb-matches-brute" (= *best-val* best-brute))

(print "=== Branch and Bound Done ===")
