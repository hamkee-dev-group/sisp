; ============================================================
; 0/1 Knapsack — Dynamic Programming
; Capacity = 50, items: ((10,60) (20,100) (30,120))
; Optimal: items 2 and 3, value = 220 (100+120, weight 20+30=50)
; Verify against brute force enumeration of 2^n subsets.
; ============================================================

(print "=== 0/1 Knapsack (DP) ===")


; ---------- DP solution ----------
; dp[i][w] = max value using first i items (1-indexed) with capacity w
; Items are 1-indexed: item i has weight wi and value vi
(define (knapsack-dp items capacity)
  (let ((n (length items)))
    (let ((dp (make-dp-table n capacity)))
      (let ((dp-filled (fill-dp dp items n capacity)))
        (backtrack dp-filled items n capacity)))))

; Create flat DP table: size (n+1) * (capacity+1), all zeros
(define (make-dp-table n cap)
  (map (lambda (ignored) 0) (seq 0 (- (* (+ n 1) (+ cap 1)) 1))))

(define (dp-get dp n cap i w)
  (nth (+ (* i (+ cap 1)) w) dp))

(define (dp-set dp n cap i w val)
  (let ((idx (+ (* i (+ cap 1)) w)))
    (list-with dp idx val)))

(define (list-with xs i v)
  (cond ((= i 0) (cons v (cdr xs)))
        (t (cons (car xs) (list-with (cdr xs) (- i 1) v)))))

; Fill DP table
(define (fill-dp dp items n cap)
  (fill-dp-i dp items n cap 1))

(define (fill-dp-i dp items n cap i)
  (cond ((> i n) dp)
        (t (fill-dp-i (fill-dp-w dp items n cap i 0) items n cap (+ i 1)))))

(define (fill-dp-w dp items n cap i w)
  (cond ((> w cap) dp)
        (t (let* ((item (nth (- i 1) items))
                  (wi (car item)) (vi (cadr item))
                  (skip-val (dp-get dp n cap (- i 1) w))
                  (take-val (cond ((>= w wi)
                                   (+ vi (dp-get dp n cap (- i 1) (- w wi))))
                                  (t 0)))
                  (best (cond ((> take-val skip-val) take-val) (t skip-val))))
             (fill-dp-w (dp-set dp n cap i w best) items n cap i (+ w 1))))))

; Backtrack to find selected items
(define (backtrack dp items n cap)
  (bt-iter dp items n cap n cap 0 nil))

(define (bt-iter dp items n cap i w total items-chosen)
  (cond ((= i 0) (list total items-chosen))
        (t (let* ((item (nth (- i 1) items))
                  (wi (car item)) (vi (cadr item))
                  (current-val (dp-get dp n cap i w))
                  (prev-val (dp-get dp n cap (- i 1) w)))
             (cond ((and (>= w wi)
                         (= current-val (+ vi (dp-get dp n cap (- i 1) (- w wi)))))
                    (bt-iter dp items n cap (- i 1) (- w wi) (+ total vi)
                             (cons i items-chosen)))
                   (t (bt-iter dp items n cap (- i 1) w total items-chosen)))))))

; ---------- Brute force enumeration ----------
; Enumerate all 2^n subsets, find max value within capacity
(define (knapsack-brute items capacity)
  (let ((n (length items)))
    (knap-brute items capacity n 0 0 nil)))

(define (knap-brute items cap n i best-val best-set)
  (cond ((= i n) (list best-val best-set))
        (t (let* ((item (nth i items))
                  (wi (car item)) (vi (cadr item))
                  (skip-res (knap-brute items cap n (+ i 1) best-val best-set))
                  (take-val (car skip-res))
                  (take-set (cadr skip-res)))
             ; Try including item i
             (cond ((<= wi cap)
                    (let ((inc-res (knap-brute items (- cap wi) n (+ i 1)
                                    (+ best-val vi)
                                    (cons (+ i 1) best-set))))
                      (cond ((> (car inc-res) take-val) inc-res)
                            (t skip-res))))
                   (t skip-res))))))

; ---------- Tests ----------
(define items (list (list 10 60) (list 20 100) (list 30 120)))
(define capacity 50)

(define dp-result (knapsack-dp items capacity))
(print "DP result (value items):" dp-result)
(check "dp-value-is-220" (= (car dp-result) 220))
(check "dp-items-are-2-3" (equal (cadr dp-result) (list 2 3)))

; Brute force verification
(define bf-result (knapsack-brute items capacity))
(print "Brute force result (value items):" bf-result)
(check "bf-value-is-220" (= (car bf-result) 220))
(check "bf-dp-agree-value" (= (car dp-result) (car bf-result)))

; Test 2: smaller capacity
(define r2 (knapsack-dp items 10))
(print "Capacity 10 result:" r2)
(check "cap10-value-60" (= (car r2) 60))
(check "cap10-item-1" (equal (cadr r2) (list 1)))

; Test 3: more items, verify against brute force
(define items2 (list (list 5 10) (list 4 8) (list 6 15) (list 3 6)))
(define cap2 10)
(define dp2 (knapsack-dp items2 cap2))
(define bf2 (knapsack-brute items2 cap2))
(print "DP more items:" dp2)
(print "BF more items:" bf2)
(check "more-items-dp-bf-agree" (= (car dp2) (car bf2)))

; Test 4: exact fit
(define items3 (list (list 25 100) (list 25 50)))
(define r3 (knapsack-dp items3 50))
(check "exact-fit-value-150" (= (car r3) 150))

(print "=== Knapsack DP Done ===")
