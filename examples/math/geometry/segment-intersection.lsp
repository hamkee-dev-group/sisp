; ============================================================
; segment-intersection.lsp — Bentley-Ottmann sweep line
; Detect all intersections among n line segments.
; For n<=20: brute force verify against sweep line.
; ============================================================

(print "=== Segment Intersection ===")

(define (px0 p) (nth 0 p))
(define (py0 p) (nth 1 p))

(define (cross-sub ax ay bx by cx cy)
  (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax))))

(define (orient-fn ax ay bx by cx cy)
  (let* ((val (cross-sub ax ay bx by cx cy)))
    (cond ((> val 0) 1) ((< val 0) -1) (t 0))))

(define (on-segment ax ay bx by cx0 cy0)
  (and (<= (min ax bx) cx0) (>= (max ax bx) cx0)
       (<= (min ay by) cy0) (>= (max ay by) cy0)))

(define (segments-intersect ax ay bx by cx cy dx dy)
  (let* ((o1 (orient-fn ax ay bx by cx cy))
         (o2 (orient-fn ax ay bx by dx dy))
         (o3 (orient-fn cx cy dx dy ax ay))
         (o4 (orient-fn cx cy dx dy bx by)))
    (or (and (/= o1 0) (/= o2 0) (/= o3 0) (/= o4 0)
             (/= o1 o2) (/= o3 o4))
        (and (= o1 0) (on-segment ax ay bx by cx cy))
        (and (= o2 0) (on-segment ax ay bx by dx dy))
        (and (= o3 0) (on-segment cx cy dx dy ax ay))
        (and (= o4 0) (on-segment cx cy dx dy bx by)))))

(define (intersection-point ax ay bx by cx cy dx dy)
  (let* ((a1 (- by ay))
         (b1 (- ax bx))
         (c1 (+ (* a1 ax) (* b1 ay)))
         (a2 (- dy cy))
         (b2 (- cx dx))
         (c2 (+ (* a2 cx) (* b2 cy)))
         (det (- (* a1 b2) (* a2 b1))))
    (if (= det 0) nil
      (list (/ (- (* b2 c1) (* b1 c2)) det)
            (/ (- (* a1 c2) (* a2 c1)) det)))))

(define (range-r n)
  (if (= n 0) nil (cons (- n 1) (range-r (- n 1)))))

; All unordered index pairs (i j) with i < j, each exactly once.
; flatmap is unusable here: it DEEP-flattens, so a list of (i j) pairs collapses
; into a flat run of integers and (nth 0 pr) then indexes into a number.
; (foldr append nil (map ...)) concatenates exactly one level.
; The old index expressions (- n-segs (+ i 1) i) were arithmetic nonsense.
(define (index-pairs n)
  (foldr append nil
    (map (lambda (i)
           (map (lambda (j) (list i j))
                (filter (lambda (j) (> j i)) (range-r n))))
         (range-r n))))

(define (sweep-intersections segs)
  (let* ((n-segs (length segs))
         (pairs (index-pairs n-segs)))
    (foldl (lambda (acc pr)
      (let* ((i (nth 0 pr)) (j (nth 1 pr))
             (si0 (nth i segs)) (sj0 (nth j segs))
             (six (px0 si0)) (siy (py0 si0))
             (sjx (nth 2 si0)) (sjy (nth 3 si0))
             (s2x (px0 sj0)) (s2y (py0 sj0))
             (s4x (nth 2 sj0)) (s4y (nth 3 sj0)))
        (if (segments-intersect six siy sjx sjy s2x s2y s4x s4y)
          (let* ((ip (intersection-point six siy sjx sjy s2x s2y s4x s4y)))
            (if ip (cons (list i j ip) acc) acc))
          acc)))
      nil pairs)))

(define (brute-intersections segs)
  (let* ((n-segs (length segs)))
    (define (iter idx acc)
      (if (= idx n-segs) acc
        (let* ((s (nth idx segs))
               ; only j > idx: scanning every j counted each crossing pair
               ; twice (once as (i j), once as (j i)) and so could never match
               ; the sweep, which enumerates each pair once.
               (inner (foldl (lambda (a j)
                  (let* ((t0 (nth j segs))
                         (ax (px0 s)) (ay (py0 s))
                         (bx (nth 2 s)) (by (nth 3 s))
                         (cx (px0 t0)) (cy (py0 t0))
                         (dx (nth 2 t0)) (dy (nth 3 t0)))
                    (if (segments-intersect ax ay bx by cx cy dx dy)
                      (let* ((ip (intersection-point ax ay bx by cx cy dx dy)))
                        (if ip (cons (list idx j ip) a) a))
                      a)))
                  acc (filter (lambda (j) (> j idx)) (range-r n-segs)))))
          (iter (+ idx 1) inner))))
    (iter 0 nil)))

(define test-segments
  '((0 0 4 4)
    (0 4 4 0)
    (1 0 1 4)
    (3 0 3 4)
    (0 1 4 1)))

(print "--- Sweep Line ---")
(define sweep-result (sweep-intersections test-segments))
(print (list "count:" (length sweep-result)))
(check "sweep-detected-intersections" (> (length sweep-result) 0))

(print "--- Brute Force ---")
(define brute-result (brute-intersections test-segments))
(print (list "count:" (length brute-result)))
(check "brute-detected-intersections" (> (length brute-result) 0))

(check "sweep-matches-brute" (= (length sweep-result) (length brute-result)))

; The five test segments give 10 index pairs; only (2 3) — the two verticals
; x=1 and x=3 — are parallel, so exactly 9 pairs cross:
;   (0 1)@(2,2) (0 2)@(1,1) (0 3)@(3,3) (0 4)@(1,1) (1 2)@(1,3)
;   (1 3)@(3,1) (1 4)@(3,1) (2 4)@(1,1) (3 4)@(3,1)
(check "sweep-count-exact" (= (length sweep-result) 9))

; Matching counts is weak — the two methods must report the SAME pairs.
(define (pair-key e) (+ (* 100 (nth 0 e)) (nth 1 e)))
(define (pair-keys r) (sort (lambda (a b) (< a b)) (map pair-key r)))
(check "sweep-matches-brute-pairs"
       (equal (pair-keys sweep-result) (pair-keys brute-result)))
; Every reported crossing must lie on both segments.
(check "sweep-points-on-both-segments"
       (every (lambda (e)
                (let* ((i (nth 0 e)) (j (nth 1 e)) (ip (nth 2 e))
                       (si0 (nth i test-segments)) (sj0 (nth j test-segments)))
                  (and (on-segment (px0 si0) (py0 si0) (nth 2 si0) (nth 3 si0)
                                   (px0 ip) (py0 ip))
                       (on-segment (px0 sj0) (py0 sj0) (nth 2 sj0) (nth 3 sj0)
                                   (px0 ip) (py0 ip)))))
              sweep-result))

(seed 42)
(define small-segs (map (lambda (j)
  (list (mod (random 50) 50) (mod (random 50) 50)
        (mod (random 50) 50) (mod (random 50) 50)))
  (range-r 8)))

(print "--- Random 8 Segments ---")
(define sw2 (sweep-intersections small-segs))
(define br2 (brute-intersections small-segs))
(print (list "sweep:" (length sw2) "brute:" (length br2)))
(check "random-sweep-matches-brute" (= (length sw2) (length br2)))
(check "random-sweep-matches-brute-pairs" (equal (pair-keys sw2) (pair-keys br2)))
; Guard against the agreement being the vacuous 0 = 0.
(check "random-has-intersections" (> (length sw2) 0))

(print "=== Done ===")
