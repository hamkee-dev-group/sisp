; ============================================================
; name: johnson-apsp
; subject: graph
; capability: all-pairs shortest paths
; status: healthy
; canonical-home: lib/graph/shortest-path.lsp
; notes: Purely functional; 99999 stands for infinity.  The test graph
;        is also a fixture for lib/graph/shortest-path.lsp.
; ============================================================
; ============================================================
; Johnson's Algorithm — All-Pairs Shortest Paths
; Graph with negative edges but no negative cycles.
; Reweight using Bellman-Ford, then Dijkstra from each vertex.
; Compare with Floyd-Warshall on same graph.
; ============================================================

(print "=== Johnson All-Pairs Shortest Paths ===")


; ---------- graph data: 4 vertices, 2 negative edges, no negative cycles ----------
(define test-edges (list
  (list 1 2 4) (list 1 3 2) (list 2 3 -1)
  (list 2 4 3) (list 3 4 2) (list 4 2 -1)))

(define test-verts (list 1 2 3 4))

(print "Graph edges (u v weight):")
(print test-edges)

; ---------- alist helpers ----------
(define (alist-get al key)
  (cond ((eq al nil) nil)
        ((= (caar al) key) (cdar al))
        (t (alist-get (cdr al) key))))

(define (alist-set al key val)
  (cons (cons key val) al))

(define (make-dist-map verts default)
  (foldl (lambda (acc v) (cons (cons v default) acc)) nil verts))

; ---------- extract-min for Dijkstra ----------
(define (extract-min-d unvisited dist)
  (cond ((eq unvisited nil) nil)
        ((eq (cdr unvisited) nil) (car unvisited))
        (t (foldl (lambda (best v)
             (let ((db (alist-get dist best))
                   (dv (alist-get dist v)))
               (cond ((< dv db) v) (t best))))
             (car unvisited) (cdr unvisited)))))

; ---------- relax edges for Dijkstra ----------
(define (relax-edges edges u dist)
  (let ((du (alist-get dist u)))
    (cond ((= du 99999) dist)
          (t (foldl (lambda (acc e)
             (cond ((= (car e) u)
               (let ((v (cadr e)) (w (caddr e)))
                 (let ((dv (alist-get acc v)))
                   (cond ((< (+ du w) dv) (alist-set acc v (+ du w)))
                         (t acc)))))
               (t acc)))
             dist edges)))))

; ---------- Dijkstra ----------
(define (dijkstra edges source verts)
  (let ((dist (alist-set (make-dist-map verts 99999) source 0))
        (unvisited (map (lambda (v) v) verts)))
    (labels (((dijk-loop unv d)
      (cond ((eq unv nil) d)
            (t (let ((u (extract-min-d unv d)))
              (cond ((eq u nil) d)
                    (t (let ((new-d (relax-edges edges u d))
                             (new-unv (remove (lambda (x) (= x u)) unv)))
                      (dijk-loop new-unv new-d)))))))))
      (dijk-loop unvisited dist))))

; ---------- Bellman-Ford ----------
(define (bellman-ford edges source verts)
  (let ((dist (alist-set (make-dist-map verts 99999) source 0)))
    (labels (((relax-all k d)
      (cond ((= k 0) d)
            (t (relax-all (- k 1)
                 (foldl (lambda (acc e)
                   (let ((u (car e)) (v (cadr e)) (w (caddr e)))
                     (let ((du (alist-get acc u)) (dv (alist-get acc v)))
                       (cond ((and (not (= du 99999)) (< (+ du w) dv))
                              (alist-set acc v (+ du w)))
                             (t acc)))))
                   d edges))))))
      (relax-all (- (length verts) 1) dist))))

; ---------- negative cycle check ----------
(define (has-negative-cycle edges verts dist)
  (foldl (lambda (acc e)
    (cond (acc acc)
          (t (let ((u (car e)) (v (cadr e)) (w (caddr e)))
            (let ((du (alist-get dist u)) (dv (alist-get dist v)))
              (cond ((and (not (= du 99999)) (< (+ du w) dv)) t)
                    (t nil)))))))
    nil edges))

; ---------- Johnson's algorithm ----------
(define (johnson edges verts)
  (let ((q 0))
    (let ((aug-edges (append edges
                     (map (lambda (v) (list q v 0)) verts)))
          (aug-verts (cons q verts)))
      (let ((bf-dist (bellman-ford aug-edges q aug-verts)))
        (cond ((has-negative-cycle aug-edges aug-verts bf-dist)
               (print "Negative cycle detected!")
               nil)
              (t (let ((h-fn (lambda (v) (alist-get bf-dist v))))
                (let ((rew-edges (map (lambda (e)
                        (let ((u (car e)) (v (cadr e)) (w (caddr e)))
                          (list u v (+ w (h-fn u) (- (h-fn v))))))
                        edges)))
                  (map (lambda (s)
                    (let ((dijk-dist (dijkstra rew-edges s verts)))
                      (cons s (map (lambda (v)
                        (let ((d (alist-get dijk-dist v)))
                          (cond ((= d 99999) 99999)
                                (t (+ d (h-fn v) (- (h-fn s)))))))
                        verts))))
                    verts)))))))))

; ---------- Floyd-Warshall using list-of-lists ----------
; All indices are 0-based internally

(define (fw-init n edges)
  ; create n×n matrix with 99999, 0 on diagonal
  (map (lambda (r)
    (map (lambda (c) (cond ((= r c) 0) (t 99999))) (seq 0 (- n 1))))
    (seq 0 (- n 1))))

(define (list-set-row row idx val)
  (cond ((= idx 0) (cons val (cdr row)))
        (t (cons (car row) (list-set-row (cdr row) (- idx 1) val)))))

(define (fw-set mat r c val)
  (cond ((= r 0) (cons (list-set-row (car mat) c val) (cdr mat)))
        (t (cons (car mat) (fw-set (cdr mat) (- r 1) c val)))))

(define (fw-floyd mat edges n)
  ; set edge weights
  (let ((d (foldl (lambda (acc e)
            (fw-set acc (- (car e) 1) (- (cadr e) 1) (caddr e)))
          mat edges)))
    ; triple loop
    (labels (((k-loop k dk)
      (cond ((>= k n) dk)
            (t (k-loop (+ k 1) (i-loop 0 k dk))))))
    (k-loop 0 d))))

(define (i-loop i k d)
  (cond ((>= i (length d)) d)
        (t (i-loop (+ i 1) k (j-loop 0 i k d)))))

(define (j-loop j i k d)
  (cond ((>= j (length d)) d)
        (t (let ((dik (nth k (nth i d)))
                 (dkj (nth j (nth k d))))
          (let ((via (cond ((or (= dik 99999) (= dkj 99999)) 99999)
                           (t (+ dik dkj)))))
          (cond ((< via (nth j (nth i d)))
                 (j-loop (+ j 1) i k (fw-set d i j via)))
                (t (j-loop (+ j 1) i k d))))))))

(define (floyd-warshall edges verts)
  (let ((n (length verts)))
    (fw-floyd (fw-init n edges) edges n)))

(define (fw-get d i j n)
  (nth (- j 1) (nth (- i 1) d)))

; ---------- run both algorithms ----------
(define j-res (johnson test-edges test-verts))

(print "Johnson result:")
(print j-res)

(define fw-d (floyd-warshall test-edges test-verts))
(define n (length test-verts))

(print "Floyd-Warshall flat:")
(print fw-d)

; ---------- verification ----------
(check "johnson-returns-results" (not (eq j-res nil)))

(define (jt-dist j-res s dest)
  (let ((entry (assoc s j-res)))
    (cond ((eq entry nil) 99999)
          (t (nth (- dest 1) (cdr entry))))))

(check "all-pairs-agree"
  (every (lambda (s)
    (every (lambda (d)
      (= (jt-dist j-res s d) (fw-get fw-d s d n)))
      test-verts))
    test-verts))

(check "self-dist-zero"
  (every (lambda (v) (= (jt-dist j-res v v) 0)) test-verts))

; Triangle inequality
(check "triangle-inequality"
  (every (lambda (ii)
    (every (lambda (jj)
      (every (lambda (kk)
        (let ((dij (jt-dist j-res ii jj))
              (dik (jt-dist j-res ii kk))
              (dkj (jt-dist j-res kk jj)))
          (or (= dij 99999) (<= dij (+ dik dkj)))))
        test-verts))
      test-verts))
    test-verts))

(print "=== Johnson All-Pairs Shortest Paths Done ===")
