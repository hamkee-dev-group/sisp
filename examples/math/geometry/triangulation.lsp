; ============================================================
; triangulation.lsp — Ear clipping, Delaunay flip algorithm.
; Constrained Delaunay. Minimum angle quality.
; ============================================================

(print "=== Triangulation ===")

(define (px0 p) (nth 0 p))
(define (py0 p) (nth 1 p))
(define (range-r n) (if (= n 0) nil (cons (- n 1) (range-r (- n 1)))))

(define (cross-xy ax ay bx by cx cy)
  (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax))))

(define (point-in-triangle pti a b c)
  (let* ((px_ (px0 pti)) (py_ (py0 pti))
         (d1 (cross-xy (px0 a) (py0 a) (px0 b) (py0 b) px_ py_))
         (d2 (cross-xy (px0 b) (py0 b) (px0 c) (py0 c) px_ py_))
         (d3 (cross-xy (px0 c) (py0 c) (px0 a) (py0 a) px_ py_))
         (neg (or (< d1 0) (< d2 0) (< d3 0)))
         (pos (or (> d1 0) (> d2 0) (> d3 0))))
    (not (and pos neg))))

(define (is-ear-0 poly idx)
  (let* ((n0 (length poly)))
    (if (< n0 3) nil
      (let* ((prev (nth (mod (- idx 1) n0) poly))
             (cur (nth idx poly))
             (nxt (nth (mod (+ idx 1) n0) poly))
             (cr (cross-xy (px0 prev) (py0 prev) (px0 cur) (py0 cur) (px0 nxt) (py0 nxt))))
        (if (< cr 0) nil
          (let* ((inside (any (lambda (i)
                   (if (or (= i idx) (= i (mod (- idx 1) n0)) (= i (mod (+ idx 1) n0)))
                     nil
                     (point-in-triangle (nth i poly) prev cur nxt)))
                   (range-r n0))))
            (not inside)))))))

(define (find-ear-0 poly)
  (find (lambda (i) (is-ear-0 poly i)) (range-r (length poly))))

(define (ear-clip-0 poly)
  (define (clip-loop remaining tris)
    (let* ((n0 (length remaining)))
      (if (< n0 3) tris
        (let* ((ear-idx (find-ear-0 remaining)))
          (if (eq ear-idx nil) tris
            (let* ((prev (nth (mod (- ear-idx 1) n0) remaining))
                   (cur (nth ear-idx remaining))
                   (nxt (nth (mod (+ ear-idx 1) n0) remaining))
                   (new-tris (cons (list prev cur nxt) tris))
                   (new-poly (foldl (lambda (acc i)
                              (if (/= i ear-idx) (cons (nth i remaining) acc) acc))
                            nil (reverse (range-r n0)))))
              (clip-loop (reverse new-poly) new-tris)))))))
  (clip-loop poly nil))

(define (distance-sq a b)
  (let* ((dx (- (px0 a) (px0 b))) (dy (- (py0 a) (py0 b))))
    (+ (* dx dx) (* dy dy))))

(define (circumcircle-contains a b c pt0)
  (let* ((ax (px0 a)) (ay (py0 a)) (bx (px0 b)) (by (py0 b)) (cx (px0 c)) (cy (py0 c)) (dx (px0 pt0)) (dy (py0 pt0))
         (a11 (- ax dx)) (a12 (- ay dy))
         (a21 (- bx dx)) (a22 (- by dy))
         (a31 (- cx dx)) (a32 (- cy dy))
         (b1 (+ (* a11 a11) (* a12 a12)))
         (b2 (+ (* a21 a21) (* a22 a22)))
         (b3 (+ (* a31 a31) (* a32 a32)))
         (det (+ (* a11 (- (* a22 b3) (* a32 b2)))
                 (* a12 (- (* a31 b2) (* a21 b3)))
                 (* b1 (- (* a21 a32) (* a31 a22))))))
    (> det 0)))

(define (init-delaunay-fan points)
  (let* ((sorted (sort (lambda (a b) (< (px0 a) (px0 b))) points))
         (hub (nth 0 sorted))
         (n0 (length sorted)))
    (filter (lambda (tr) (consp tr))
      (foldl (lambda (acc i)
        (if (< i (- n0 1))
          (cons (list hub (nth i sorted) (nth (+ i 1) sorted)) acc)
          acc))
        nil (range-r (- n0 1))))))

(define (shared-edge t1 t2)
  (let* ((v1 (list (nth 0 t1) (nth 1 t1) (nth 2 t1)))
         (v2 (list (nth 0 t2) (nth 1 t2) (nth 2 t2)))
         (sh0 (filter (lambda (v) (contains v v2)) v1)))
    (if (/= (length sh0) 2) nil sh0)))

(define (other-vertex tri sh0)
  (find (lambda (v) (not (contains v sh0)))
        (list (nth 0 tri) (nth 1 tri) (nth 2 tri))))

(define (delaunay-flip-0 points tris)
  (let* ((n0 (length tris)))
    (if (< n0 2) tris
      (let* ((result (foldl (lambda (acc i)
                  (if (consp acc) acc
                    ; shared-edge returns the shared pair or nil, and SISP's
                    ; and takes strict booleans, so test it with consp.
                    (let* ((found-j (find (lambda (j)
                                 (and (> j i)
                                      (consp (shared-edge (nth i tris)
                                                          (nth j tris)))))
                               (range-r n0))))
                      (if (eq found-j nil) acc
                        (let* ((j found-j) (ti (nth i tris)) (tj (nth j tris))
                               (sh0 (shared-edge ti tj)))
                          (if (eq sh0 nil) acc
                            (let* ((oi (other-vertex ti sh0))
                                   (oj (other-vertex tj sh0))
                                   (s1 (nth 0 sh0)) (s2 (nth 1 sh0)))
                              (if (circumcircle-contains s1 s2 oi oj)
                                (let* ((remaining (foldl (lambda (a k)
                                               (if (or (= k i) (= k j)) a
                                                 (cons (nth k tris) a)))
                                             nil (reverse (range-r n0)))))
                                  (delaunay-flip-0 points (reverse (cons (list s1 oj oi)
                                                                      (cons (list s2 oi oj) remaining)))))
                                  acc))))))))
                  nil (range-r n0))))
        (if (consp result) result tris)))))

(define (triangle-min-angle tri)
  (let* ((a (nth 0 tri)) (b (nth 1 tri)) (c (nth 2 tri))
         (ab (sqrt (distance-sq a b))) (bc (sqrt (distance-sq b c))) (ca (sqrt (distance-sq c a))))
    (if (or (= ab 0) (= bc 0) (= ca 0)) 0
      (let* ((a1 (acos (/ (+ (* ab ab) (* ca ca) (- (* bc bc))) (* 2 ab ca))))
             (a2 (acos (/ (+ (* ab ab) (* bc bc) (- (* ca ca))) (* 2 ab bc))))
             (a3 (acos (/ (+ (* bc bc) (* ca ca) (- (* ab ab))) (* 2 bc ca)))))
        (min a1 (min a2 a3))))))

(define (min-angle-0 tris)
  (foldl (lambda (acc tri) (min acc (triangle-min-angle tri))) 100 tris))

(print "--- Test Polygon ---")
(define simple-poly '((0 0) (5 0) (5 3) (3 5) (0 3)))
(print simple-poly)

(print "--- Ear Clipping ---")
(define ec-tris (ear-clip-0 simple-poly))
(print (list "Triangles:" (length ec-tris)))
(check "ear-clip-tris" (> (length ec-tris) 0))

(print "--- Min Angle ---")
(print (list "min angle:" (min-angle-0 ec-tris)))
(check "min-angle-pos" (> (min-angle-0 ec-tris) 0))

(print "--- Delaunay Fan ---")
(define random-pts (list (list 0 0) (list 5 0) (list 5 5) (list 0 5) (list 2 2) (list 3 1)))
(define df-tris (init-delaunay-fan random-pts))
(print (list "Fan triangles:" (length df-tris)))
(check "fan-tris" (> (length df-tris) 0))

(print "--- Delaunay Edge Flip ---")
; delaunay-flip-0 was previously unreachable: an unclosed lambda swallowed
; foldl's accumulator and list, so the whole call raised as soon as it ran.
; Nothing here exercised it, so the file still reported three passes.
; These checks call it directly.
(define fl-a (list 0 0))
(define fl-b (list 10 0))
(define fl-c (list 5 1))
(define fl-d (list 5 -1))
; Two thin triangles sharing the vertical diagonal c-d.  The circumcircle of
; (c d a) contains b, so the diagonal is wrong and must be flipped to a-b.
(define fl-before (list (list fl-a fl-c fl-d) (list fl-b fl-c fl-d)))
(define fl-after (delaunay-flip-0 nil fl-before))
(print (list "flipped:" fl-after))
(check "flip-detects-bad-diagonal" (circumcircle-contains fl-c fl-d fl-a fl-b))
(check "flip-keeps-two-triangles" (= (length fl-after) 2))
(check "flip-replaces-the-diagonal"
       (equal (shared-edge (nth 0 fl-after) (nth 1 fl-after))
              (list fl-a fl-b)))
(check "flip-drops-the-old-diagonal"
       (not (equal (shared-edge (nth 0 fl-after) (nth 1 fl-after))
                   (list fl-c fl-d))))
; An already-Delaunay pair must be left alone, and degenerate inputs pass through.
(define sq-tris (list (list (list 0 0) (list 4 0) (list 0 4))
                      (list (list 4 0) (list 0 4) (list 4 4))))
(check "flip-leaves-good-triangulation" (equal (delaunay-flip-0 nil sq-tris) sq-tris))
(check "flip-single-triangle-unchanged"
       (= (length (delaunay-flip-0 nil (list (list fl-a fl-b fl-c)))) 1))
(check "flip-empty-is-empty" (eq (delaunay-flip-0 nil nil) nil))

(print "=== Done ===")
