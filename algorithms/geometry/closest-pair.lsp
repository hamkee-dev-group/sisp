; ============================================================
; name: closest-pair
; file: algorithms/geometry/closest-pair.lsp
; subject: geometry
; capability: geometry search
; aliases: "closest pair" "closest pair of points" "divide and conquer geometry"
; signature: (closest-pair points) -> (p q dist2)
; complexity: O(n log n) divide and conquer
; see-also: convex-hull
; derived-from: Shamos & Hoey 1975; CLRS 33.4
; status: example (tested)
; checks: 8 - vs brute-force O(n^2) on three fixed point sets
; ============================================================
; Closest Pair of Points — Divide and Conquer O(n log n)
; Points: ((2,3) (12,30) (40,50) (5,1) (12,10) (3,4))
; Closest: (2,3) and (3,4) distance^2 = 2
; Verify against brute force O(n^2).
; ============================================================

(print "=== Closest Pair (Divide & Conquer) ===")


(define (px p) (car p))
(define (py p) (cadr p))
(define (myabs x) (cond ((< x 0) (- x)) (t x)))

(define (dist2 p1 p2)
  (let ((dx (- (px p1) (px p2)))
        (dy (- (py p1) (py p2))))
    (+ (* dx dx) (* dy dy))))

; ---------- Brute force O(n^2) ----------
(define (brute-force-closest points)
  (let ((n (length points)))
    (bfc points n 0 -1 nil nil)))

(define (bfc points n i best-d best-a best-b)
  (cond ((>= i n) (list best-a best-b best-d))
        (t (bfc-inner points n i (+ i 1) best-d best-a best-b))))

(define (bfc-inner points n i j best-d best-a best-b)
  (cond ((>= j n)
         (bfc points n (+ i 1) best-d best-a best-b))
        (t (let ((d (dist2 (nth i points) (nth j points))))
             (cond ((or (< best-d 0) (< d best-d))
                    (bfc-inner points n i (+ j 1) d (nth i points) (nth j points)))
                   (t (bfc-inner points n i (+ j 1) best-d best-a best-b)))))))

; ---------- Divide and Conquer ----------
(define (sort-by-x points)
  (sort (lambda (a b) (< (px a) (px b))) points))

(define (sort-by-y points)
  (sort (lambda (a b) (< (py a) (py b))) points))

; Returns (p1 p2 dist2)
(define (closest-pair points)
  (let* ((px (sort-by-x points))
         (py (sort-by-y points))
         (n (length points)))
    (cp-recur px py n)))

; Base case: n <= 3, brute force
(define (cp-recur px py n)
  (cond ((<= n 3) (brute-force-small px n))
        (t (let* ((mid (truncate (/ n 2)))
                  (mid-x (px (nth (- mid 1) px)))
                  (left-px (take px mid))
                  (right-px (drop px mid))
                  (left-py (filter-y py mid-x nil))
                  (right-py (filter-y py mid-x t))
                  (lres (cp-recur left-px left-py (length left-px)))
                  (rres (cp-recur right-px right-py (length right-px)))
                  (best (better-result lres rres))
                  (strip (make-strip py mid-x (caddr best)))
                  (sres (check-strip strip (caddr best) (car best) (cadr best))))
             (better-result best sres)))))

(define (brute-force-small px n)
  (bfc px n 0 -1 nil nil))

(define (filter-y py mid-x ge)
  (cond ((eq py nil) nil)
        ((cond (ge (>= (px (car py)) mid-x))
               (t (< (px (car py)) mid-x)))
         (cons (car py) (filter-y (cdr py) mid-x ge)))
        (t (filter-y (cdr py) mid-x ge))))

(define (better-result a b)
  (cond ((< (caddr a) (caddr b)) a)
        (t b)))

(define (make-strip py mid-x d)
  (cond ((eq py nil) nil)
        ((< (myabs (- (px (car py)) mid-x)) d)
         (cons (car py) (make-strip (cdr py) mid-x d)))
        (t (make-strip (cdr py) mid-x d))))

(define (check-strip strip d best-a best-b)
  (let ((n (length strip)))
    (strip-iter strip n 0 d best-a best-b)))

(define (strip-iter strip n i d best-a best-b)
  (cond ((>= i n) (list best-a best-b d))
        (t (strip-j strip n i (+ i 1) d best-a best-b))))

(define (strip-j strip n i j d best-a best-b)
  (cond ((>= j n) (strip-iter strip n (+ i 1) d best-a best-b))
        ((> (- (py (nth j strip)) (py (nth i strip))) d)
         (strip-iter strip n (+ i 1) d best-a best-b))
        (t (let ((nd (dist2 (nth i strip) (nth j strip))))
             (cond ((< nd d)
                    (strip-j strip n i (+ j 1) nd (nth i strip) (nth j strip)))
                   (t (strip-j strip n i (+ j 1) d best-a best-b)))))))

; ---------- Tests ----------
(define points
  (list (list 2 3) (list 12 30) (list 40 50)
        (list 5 1) (list 12 10) (list 3 4)))

(define dc-result (closest-pair points))
(print "Divide & Conquer result (p1 p2 dist2):" dc-result)

(define bf-result (brute-force-closest points))
(print "Brute force result (p1 p2 dist2):" bf-result)

(check "dc-dist2-is-2" (= (caddr dc-result) 2))
(check "bf-dist2-is-2" (= (caddr bf-result) 2))
(check "dc-bf-dist2-agree" (= (caddr dc-result) (caddr bf-result)))

(check "dc-has-2-3"
  (or (equal (car dc-result) (list 2 3))
      (equal (cadr dc-result) (list 2 3))))
(check "dc-has-3-4"
  (or (equal (car dc-result) (list 3 4))
      (equal (cadr dc-result) (list 3 4))))

; Test on ordered points
(define s2 (list (list 0 0) (list 1 1) (list 2 2)))
(check "small-dc-matches-bf"
  (= (caddr (closest-pair s2)) (caddr (brute-force-closest s2))))

; Test where closest is between different halves
(define s3 (list (list 0 0) (list 5 0) (list 10 0) (list 6 1)))
(define s3-dc (closest-pair s3))
(define s3-bf (brute-force-closest s3))
(check "cross-half-dist2-2" (= (caddr s3-bf) 2))
(check "cross-half-matches-bf" (= (caddr s3-dc) (caddr s3-bf)))

(print "=== Closest Pair Done ===")
