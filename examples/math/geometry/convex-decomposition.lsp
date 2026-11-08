; ============================================================
; convex-decomposition.lsp — Convex decomposition of a simple
; polygon by cutting along a diagonal from a reflex vertex and
; recursing.  (Hertel-Mehlhorn proper starts from a triangulation
; and removes inessential diagonals, giving at most 4x the minimum
; number of pieces; the function named after it below only counts
; reflex vertices.)
; ============================================================

(print "=== Convex Decomposition ===")

(define (px0 p) (nth 0 p))
(define (py0 p) (nth 1 p))
(define (range-r n) (if (= n 0) nil (cons (- n 1) (range-r (- n 1)))))

(define (cross-xy ax ay bx by cx cy)
  (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax))))

(define (convex-polygon-p poly)
  (let* ((n0 (length poly)) (pmin (nth 0 poly)))
    (if (< n0 3) t
      (let* ((p0 (nth 0 poly)) (p1 (nth 1 poly)) (p2 (nth 2 poly))
             (first-sign (let* ((v (cross-xy (px0 p0) (py0 p0) (px0 p1) (py0 p1) (px0 p2) (py0 p2))))
                           (cond ((> v 0) 1) ((< v 0) -1) (t 0)))))
        (if (= first-sign 0) nil
          (every (lambda (i)
            (let* ((a (nth i poly)) (b (nth (mod (+ i 1) n0) poly)) (c (nth (mod (+ i 2) n0) poly))
                   (v (cross-xy (px0 a) (py0 a) (px0 b) (py0 b) (px0 c) (py0 c))))
              (let* ((s (cond ((> v 0) 1) ((< v 0) -1) (t 0))))
                (= s first-sign))))
            (range-r n0)))))))

(define (point-in-poly-ray pt0 poly)
  (let* ((px_ (px0 pt0)) (py_ (py0 pt0)) (n0 (length poly))
         (cnt (foldl (lambda (acc i)
                (let* ((si (nth i poly)) (sj (nth (mod (+ i 1) n0) poly))
                       (six (px0 si)) (siy (py0 si)) (sjx (px0 sj)) (sjy (py0 sj)))
                  (if (or (and (<= siy py_) (< py_ sjy)
                               (< (* (- sjx six) (- py_ siy)) (* (- sjy siy) (- px_ six))))
                          (and (<= sjy py_) (< py_ siy)
                               (< (* (- six sjx) (- py_ sjy)) (* (- siy sjy) (- px_ sjx)))))
                    (+ acc 1) acc)))
                0 (range-r n0))))
    (= (mod cnt 2) 1)))

(define (segments-intersect ax ay bx by cx cy dx dy)
  (define (on-seg xa ya xb yb px_ py_)
    (and (<= (min xa xb) px_) (>= (max xa xb) px_)
         (<= (min ya yb) py_) (>= (max ya yb) py_)))
  (define (orient3 ax ay bx by cx cy)
    (let* ((v (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax)))))
      (cond ((> v 0) 1) ((< v 0) -1) (t 0))))
  (let* ((o1 (orient3 ax ay bx by cx cy))
         (o2 (orient3 ax ay bx by dx dy))
         (o3 (orient3 cx cy dx dy ax ay))
         (o4 (orient3 cx cy dx dy bx by)))
    (or (and (/= o1 0) (/= o2 0) (/= o3 0) (/= o4 0) (/= o1 o2) (/= o3 o4))
        (and (= o1 0) (on-seg ax ay bx by cx cy))
        (and (= o2 0) (on-seg ax ay bx by dx dy))
        (and (= o3 0) (on-seg cx cy dx dy ax ay))
        (and (= o4 0) (on-seg cx cy dx dy bx by)))))

(define (is-diagonal poly idx-i idx-j)
  (let* ((n0 (length poly)) (pi (nth idx-i poly)) (pj (nth idx-j poly)))
    (if (or (= idx-i idx-j) (= (mod (- idx-j idx-i) n0) 1) (= (mod (- idx-i idx-j) n0) 1)) nil
      (let* ((diag-mid (list (/ (+ (px0 pi) (px0 pj)) 2) (/ (+ (py0 pi) (py0 pj)) 2)))
             (ray-ok (point-in-poly-ray diag-mid poly)))
        (and ray-ok
          (every (lambda (k)
            (let* ((ek (nth k poly)) (enk (nth (mod (+ k 1) n0) poly)))
              (if (or (= k idx-i) (= k idx-j)
                      (= (mod (+ k 1) n0) idx-i) (= (mod (+ k 1) n0) idx-j)) t
                (not (segments-intersect (px0 pi) (py0 pi) (px0 pj) (py0 pj)
                                         (px0 ek) (py0 ek) (px0 enk) (py0 enk))))))
            (range-r n0)))))))

; Cutting a simple polygon along a diagonal (lo,hi) produces TWO sub-polygons:
;   vertices lo..hi          and    vertices hi..end followed by 0..lo.
(define (poly-slice poly lo hi)
  (take (drop poly lo) (+ (- hi lo) 1)))

(define (poly-coslice poly lo hi)
  (append (drop poly hi) (take poly (+ lo 1))))

(define (reflex-indices poly n0)
  (filter (lambda (i)
            (let* ((prev (nth (mod (- i 1) n0) poly))
                   (cur (nth i poly))
                   (nxt (nth (mod (+ i 1) n0) poly))
                   (v (cross-xy (px0 prev) (py0 prev) (px0 cur) (py0 cur) (px0 nxt) (py0 nxt))))
              (< v 0)))
          (range-r n0)))

; A decomposition has to recurse into BOTH halves of the cut and concatenate
; their pieces; keeping only one half would lose area.
(define (convex-decompose-0 poly)
  (let* ((n0 (length poly)))
    (if (< n0 4) (list poly)
      (let* ((reflexes (reflex-indices poly n0)))
        (if (eq reflexes nil) (list poly)
          (let* ((reflex-idx (car reflexes))
                 (diag-target (find (lambda (k)
                             (and (/= k reflex-idx)
                                  (/= k (mod (- reflex-idx 1) n0))
                                  (/= k (mod (+ reflex-idx 1) n0))
                                  (is-diagonal poly reflex-idx k)))
                           (range-r n0))))
            (if (eq diag-target nil) (list poly)
              (let* ((lo (min reflex-idx diag-target))
                     (hi (max reflex-idx diag-target)))
                (append (convex-decompose-0 (poly-slice poly lo hi))
                        (convex-decompose-0 (poly-coslice poly lo hi)))))))))))

; Shoelace area (absolute, so orientation does not matter).
(define (poly-area poly)
  (let* ((n0 (length poly)))
    (/ (abs (foldl (lambda (acc i)
                     (let* ((a (nth i poly)) (b (nth (mod (+ i 1) n0) poly)))
                       (+ acc (- (* (px0 a) (py0 b)) (* (px0 b) (py0 a))))))
                   0 (range-r n0)))
       2)))

; Number of reflex (clockwise-turning) vertices; 0 means already convex.
(define hertel-mehlhorn
  (lambda (poly)
    (let* ((n0 (length poly))
           (reflex-count (count (lambda (i)
                         (let* ((a (nth (mod (- i 1) n0) poly))
                                (b (nth i poly))
                                (c (nth (mod (+ i 1) n0) poly))
                                (v (cross-xy (px0 a) (py0 a) (px0 b) (py0 b) (px0 c) (py0 c))))
                           (< v 0)))
                       (range-r n0))))
      reflex-count)))

(define concave-poly '((0 0) (4 0) (4 2) (2 1) (4 4) (0 4)))
(define convex-square '((0 0) (5 0) (5 5) (0 5)))

(print "--- Convexity Check ---")
(check "square-convex" (convex-polygon-p convex-square))
(check "concave-not-convex" (not (convex-polygon-p concave-poly)))

(print "--- Reflex Vertices ---")
(define rc (hertel-mehlhorn concave-poly))
(print (list "Reflex count:" rc))
(check "reflex-count-ok" (> rc 0))
; (2 1) is the only vertex of concave-poly that turns clockwise.
(check "reflex-count-exact" (= rc 1))
(check "convex-has-no-reflex" (= (hertel-mehlhorn convex-square) 0))

(print "--- Convex Decomposition ---")
(define decomp-result (convex-decompose-0 concave-poly))
(print (list "Pieces:" (length decomp-result)))
(print (list "Piece areas:" (map poly-area decomp-result)))
(check "decomp-has-pieces" (consp decomp-result))
; A polygon with a reflex vertex cannot be a single convex piece.
(check "decomp-splits-concave" (> (length decomp-result) 1))
; Every piece the decomposition returns must actually be convex...
(check "decomp-all-convex"
       (every (lambda (pc) (convex-polygon-p pc)) decomp-result))
; ...and the pieces must tile the original exactly: areas sum to 14, computed
; by the shoelace formula on '((0 0) (4 0) (4 2) (2 1) (4 4) (0 4)).
; Arithmetic here is exact rational, so this is an exact equality.
(check "decomp-area-preserved"
       (= (foldl (lambda (acc pc) (+ acc (poly-area pc))) 0 decomp-result)
          (poly-area concave-poly)))
(check "concave-area-is-14" (= (poly-area concave-poly) 14))

; A convex polygon must come back as a single undivided piece.
(define square-decomp (convex-decompose-0 convex-square))
(check "convex-decomp-single-piece" (= (length square-decomp) 1))
(check "convex-decomp-area" (= (poly-area (car square-decomp)) 25))

(print "=== Done ===")
