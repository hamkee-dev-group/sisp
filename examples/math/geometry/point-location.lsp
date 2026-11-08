; ============================================================
; point-location.lsp — Point in polygon (ray casting, winding
; number). Trapezoidal map. Kirkpatrick's hierarchy.
; ============================================================

(print "=== Point Location ===")

(define (px0 p) (nth 0 p))
(define (py0 p) (nth 1 p))
(define (range-r n)
  (if (= n 0) nil (cons (- n 1) (range-r (- n 1)))))

(define (point-in-poly-ray pt0 poly)
  (let* ((px_ (px0 pt0)) (py_ (py0 pt0))
         (n (length poly))
         (cnt (foldl (lambda (acc i)
                (let* ((si (nth i poly)) (sj (nth (mod (+ i 1) n) poly))
                       (six (px0 si)) (siy (py0 si))
                       (sjx (px0 sj)) (sjy (py0 sj)))
                  (if (or (and (<= siy py_) (< py_ sjy)
                               (< (* (- sjx six) (- py_ siy))
                                  (* (- sjy siy) (- px_ six))))
                          (and (<= sjy py_) (< py_ siy)
                               (< (* (- six sjx) (- py_ sjy))
                                  (* (- siy sjy) (- px_ sjx)))))
                    (+ acc 1) acc)))
                0 (range-r n))))
    (= (mod cnt 2) 1)))

(define (winding-number-0 pt0 poly)
  (let* ((px_ (px0 pt0)) (py_ (py0 pt0))
         (n (length poly))
         (wn (foldl (lambda (acc i)
                (let* ((si (nth i poly)) (sj (nth (mod (+ i 1) n) poly))
                       (six (px0 si)) (siy (py0 si))
                       (sjx (px0 sj)) (sjy (py0 sj)))
                  (if (<= siy py_)
                    (if (and (> sjy py_)
                             (> (- (* (- sjx six) (- py_ siy))
                                   (* (- sjy siy) (- px_ six))) 0))
                      (+ acc 1) acc)
                    (if (and (<= sjy py_)
                             (< (- (* (- sjx six) (- py_ siy))
                                   (* (- sjy siy) (- px_ six))) 0))
                      (- acc 1) acc))))
              0 (range-r n))))
    (/= wn 0)))

(define test-square '((0 0) (4 0) (4 4) (0 4)))
(define test-star '((3 0) (4 2) (6 2) (4.5 3.5) (5 6) (3 4.5) (1 6) (1.5 3.5) (0 2) (2 2)))

(print "--- Ray Casting ---")
(check "ray-in-square" (point-in-poly-ray (list 2 2) test-square))
(check "ray-out-square" (not (point-in-poly-ray (list 5 5) test-square)))
(check "ray-in-star" (point-in-poly-ray (list 3 3) test-star))
(check "ray-out-star" (not (point-in-poly-ray (list 0 0) test-star)))

(print "--- Winding Number ---")
(check "wind-in-square" (winding-number-0 (list 2 2) test-square))
(check "wind-out-square" (not (winding-number-0 (list 5 5) test-square)))
(check "wind-in-star" (winding-number-0 (list 3 3) test-star))
(check "wind-out-star" (not (winding-number-0 (list 0 0) test-star)))

(print "--- Ray vs Winding Consistency ---")
; The two tests provably agree only for points that are NOT on the boundary.
; The old test set led with (0 0), which is a VERTEX of the square: ray casting
; reports it outside and the winding number reports it inside.  Neither is
; wrong — membership on the boundary is a tie-breaking convention, and the two
; algorithms do not share one.  So state the theorem where it actually holds,
; over points strictly inside or strictly outside.
(define inside-points '((2 2) (1 3) (3 1) (3.5 3.5) (0.5 0.5)))
(define outside-points '((5 5) (-1 2) (2 -1) (2 5) (-1 -1)))
(define test-points (append inside-points outside-points))
(define all-consistent
  (every (lambda (pt0)
    (let* ((rr (point-in-poly-ray pt0 test-square))
           (wr (winding-number-0 pt0 test-square)))
      (or (and rr wr) (and (not rr) (not wr)))))
    test-points))
(check "ray-winding-consistent" all-consistent)

; Agreeing is not enough — both must also give the right answer.
(check "ray-classifies-inside"
       (every (lambda (q) (point-in-poly-ray q test-square)) inside-points))
(check "ray-classifies-outside"
       (every (lambda (q) (not (point-in-poly-ray q test-square))) outside-points))
(check "wind-classifies-inside"
       (every (lambda (q) (winding-number-0 q test-square)) inside-points))
(check "wind-classifies-outside"
       (every (lambda (q) (not (winding-number-0 q test-square))) outside-points))

(print "--- Trapezoidal Map ---")
(define (build-trapezoids segs)
  (let* ((yvals (map (lambda (s)
              (list (nth 1 s) (nth 3 s)))
              segs))
         (all-y (append (map car yvals) (map cadr yvals)))
         (sorted-y (sort (lambda (a b) (< a b)) (dedup all-y))))
    (if (< (length sorted-y) 2)
      (list (list 0 10 (car sorted-y) (car sorted-y)))
      (foldl (lambda (acc i)
        (if (< i (- (length sorted-y) 1))
          (cons (list 0 10 (nth (+ i 1) sorted-y) (nth i sorted-y)) acc)
          acc))
        nil (range-r (- (length sorted-y) 1))))))

(define subdiv-segs
  '((0 0 4 4)
    (4 0 0 4)
    (2 0 2 4)
    (0 2 4 2)))
(define trap-map (build-trapezoids subdiv-segs))
(print (list "Trapezoids:" (length trap-map)))
(check "trap-has-regions" (> (length trap-map) 0))
; The four segments have endpoint y-coordinates {0, 2, 4}: three distinct
; sweep lines bound exactly two horizontal slabs.
(check "trap-slab-count" (= (length trap-map) 2))

(print "--- Kirkpatrick's Hierarchy ---")
(define (strictly-decreasing l)
  (cond ((eq l nil) t)
        ((eq (cdr l) nil) t)
        ((>= (cadr l) (car l)) nil)
        (t (strictly-decreasing (cdr l)))))
(define (kirkpatrick-build pts nlevels)
  (define (remove-low pts-left level)
    (if (>= level nlevels) (list pts-left)
      (let* ((kept (foldl (lambda (acc i)
                 (if (= (mod i 2) 0) (cons (nth i pts-left) acc) acc))
               nil (reverse (range-r (length pts-left))))))
        (if (< (length kept) 3) (list pts-left)
          (cons pts-left (remove-low kept (+ level 1)))))))
  (remove-low pts 0))

(define kh-pts
  '((0 0) (1 0) (2 0) (3 0) (4 0)
    (0 1) (2 1) (4 1)
    (0 2) (1 2) (2 2) (3 2) (4 2)))
(define kh-result (kirkpatrick-build kh-pts 3))
(print (list "Hierarchy levels:" (length kh-result)))
(print (list "Level sizes:" (map length kh-result)))
(check "kirkpatrick-hierarchy" (> (length kh-result) 1))
; Each level keeps the even-indexed points, so 13 -> 7 -> 4 over 3 levels.
(check "kirkpatrick-level-count" (= (length kh-result) 3))
(check "kirkpatrick-sizes" (equal (map length kh-result) '(13 7 4)))
; A point-location hierarchy is only useful if every level is strictly smaller.
(check "kirkpatrick-strictly-shrinks" (strictly-decreasing (map length kh-result)))

(print "=== Done ===")
