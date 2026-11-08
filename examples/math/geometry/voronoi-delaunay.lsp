; ============================================================
; voronoi-delaunay.lsp — Voronoi from Delaunay dual.
; Fortune's sweep line (simplified for small sets).
; Compute for 10 random points.
; ============================================================

(print "=== Voronoi-Delaunay ===")

(define (px0 p) (nth 0 p))
(define (py0 p) (nth 1 p))
; Ascending 0..n-1.  voronoi-from-delaunay rebuilds the cell list by mapping
; over these indices, so a descending range silently reversed the list and
; every subsequent (nth k cells) addressed the wrong site.
(define (range-from i n) (if (>= i n) nil (cons i (range-from (+ i 1) n))))
(define (range-r n) (range-from 0 n))

(define (distance-sq-voronoi pt1 pt2)
  (let* ((dx (- (px0 pt1) (px0 pt2))) (dy (- (py0 pt1) (py0 pt2))))
    (+ (* dx dx) (* dy dy))))

(define (midpoint-v a b)
  (list (/ (+ (px0 a) (px0 b)) 2) (/ (+ (py0 a) (py0 b)) 2)))

(define (circumcenter-v a b c)
  (let* ((ax (px0 a)) (ay (py0 a)) (bx (px0 b)) (by (py0 b)) (cx (px0 c)) (cy (py0 c))
         ; Standard circumcenter determinant.  The old d0 used (ax-bx) where
         ; the identity needs (bx-cx), so the denominator was simply wrong.
         (d0 (* 2.0 (+ (* ax (- by cy)) (* bx (- cy ay)) (* cx (- ay by))))))
    (if (= d0 0) nil
      (let* ((na (+ (* ax ax) (* ay ay)))
             (nb (+ (* bx bx) (* by by)))
             (ncc (+ (* cx cx) (* cy cy)))
             (cx0 (/ (+ (* na (- by cy)) (* nb (- cy ay)) (* ncc (- ay by))) d0))
             (cy0 (/ (+ (* na (- cx bx)) (* nb (- ax cx)) (* ncc (- bx ax))) d0)))
        (list cx0 cy0)))))

(define (orient-v a b c)
  (let* ((ax (px0 a)) (ay (py0 a)) (bx (px0 b)) (by (py0 b)) (cx (px0 c)) (cy (py0 c)))
    (< 0 (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax))))))

(define (delaunay-fan points)
  ; `t` is reserved and cannot be a parameter name, so the old
  ; (lambda (t) (consp t)) filter aborted this whole define.
  ; Also: the old fan started at sorted[0] = hub itself, emitting the
  ; degenerate triangle (hub hub s1).  Sorting the remaining sites by angle
  ; about the leftmost site (which is on the hull, so they all lie in one
  ; half-plane) makes consecutive pairs a proper, non-overlapping fan.
  (let* ((hub (foldl (lambda (acc pt) (if (< (px0 pt) (px0 acc)) pt acc))
                     (nth 0 points) points))
         (rest (filter (lambda (pt) (not (equal pt hub))) points))
         (fan (sort (lambda (a b) (orient-v hub a b)) rest))
         (m0 (length fan)))
    (filter (lambda (tri) (consp tri))
      (map (lambda (i) (list hub (nth i fan) (nth (+ i 1) fan)))
           (range-r (- m0 1))))))

(define (voronoi-from-delaunay points tris)
  (let* ((n0 (length points))
         (init-cells (map (lambda (pt0) nil) points)))
    (foldl (lambda (acc tri)
      (let* ((cc (circumcenter-v (nth 0 tri) (nth 1 tri) (nth 2 tri))))
        (if (eq cc nil) acc
          (let* ((v0 (nth 0 tri)) (v1 (nth 1 tri)) (v2 (nth 2 tri))
                 (i0 (index-of v0 points))
                 (i1 (index-of v1 points))
                 (i2 (index-of v2 points)))
            (if (or (eq i0 nil) (eq i1 nil) (eq i2 nil)) acc
              (let* ((add0 (lambda (cells idx)
                      (map (lambda (k)
                        (if (= k idx)
                          (let* ((old (nth k cells)))
                            (if old (cons cc old) (list cc)))
                          (nth k cells)))
                        (range-r n0)))))
                (let* ((a1 (add0 acc i0))
                       (a2 (add0 a1 i1))
                       (a3 (add0 a2 i2)))
                  a3)))))))
      ; This close paren for the foldl lambda was MISSING: the reader then
      ; swallowed the whole following (define (fortune-sweep ...)) as an extra
      ; argument to foldl, which is why fortune-sweep was never defined.
      init-cells tris)))

(define (fortune-sweep points)
  (let* ((n0 (length points))
         (sorted (sort (lambda (a b) (< (py0 a) (py0 b))) points))
         ; flatmap DEEP-flattens, which would shred every 4-number edge into
         ; loose numbers (and then `filter consp` would return nil).
         ; Concatenate exactly one level instead.
         (candidates (foldr append nil (map (lambda (i)
            (filter (lambda (e) (consp e))
              (map (lambda (j)
                (if (> j i)
                  (let* ((pti (nth i sorted)) (ptj (nth j sorted))
                         (mid (midpoint-v pti ptj))
                         (mx (px0 mid)) (my0 (py0 mid))
                         (perp-dx (- (py0 pti) (py0 ptj)))
                         (perp-dy (- (px0 ptj) (px0 pti))))
                    (list mx my0 (+ mx perp-dx) (+ my0 perp-dy)))
                  nil))
                (range-r n0))))
            (range-r n0)))))
    candidates))

(print "--- 10 Random Points ---")
(seed 12345)
(define random-points (map (lambda (j)
  (list (mod (random 1000) 100) (mod (random 1000) 100)))
  (range-r 10)))
(foreach (lambda (pt0) (print pt0)) random-points)
(check "10-points" (= (length random-points) 10))

(print "--- Delaunay Fan ---")
(define dt-fan (delaunay-fan random-points))
(print (list "Fan triangles:" (length dt-fan)))
(check "fan-nonzero" (> (length dt-fan) 0))

(print "--- Voronoi from Delaunay ---")
(define vor-cells (voronoi-from-delaunay random-points dt-fan))
(define nc (count (lambda (c) (and (consp c) (> (length c) 0))) vor-cells))
(print (list "Nonempty cells:" nc))
(check "voronoi-has-cells" (> nc 0))

(print "--- Fortune's Sweep Line ---")
(define f-edges (fortune-sweep random-points))
(print (list "Fortune edges:" (length f-edges)))
(check "fortune-edges" (> (length f-edges) 0))

(print "=== Done ===")
