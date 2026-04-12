; ============================================================
; 38-graph-theory.lsp
; Represent graphs as sets of edges (pairs).
; Compute degree of vertices. Find neighbors.
; Check if a path exists between two vertices using BFS.
; ============================================================

(print "=== Graph Theory with Sets ===")

; --- Helpers ---
(define (my-append a b)
  (if (equal a nil) b
    (cons (car a) (my-append (cdr a) b))))

(define (my-memberp x lst)
  (if (equal lst nil) nil
    (if (equal (car lst) x) t
        (my-memberp x (cdr lst)))))

; ============================================================
; Graph representation: undirected graph as list of edge pairs
; Each edge (u v) means u--v.
; ============================================================
; Graph:
;   1 --- 2 --- 3
;   |           |
;   4     5 --- 6     7 (isolated)
;
(define edges '((1 2) (2 3) (1 4) (3 6) (5 6)))
(define vertices '(1 2 3 4 5 6 7))

(print "--- Graph ---")
(print "Vertices:")
(print vertices)
(print "Edges:")
(print edges)

; ============================================================
; Find neighbors of a vertex
; ============================================================
(define (neighbors v edge-list)
  (if (equal edge-list nil) nil
    (let ((e (car edge-list))
          (rest (neighbors v (cdr edge-list))))
      (cond
        ((equal (car e) v) (cons (car (cdr e)) rest))
        ((equal (car (cdr e)) v) (cons (car e) rest))
        (t rest)))))

(print "--- Neighbors ---")
(print "Neighbors of 1:")
(print (neighbors 1 edges))
(print "Neighbors of 2:")
(print (neighbors 2 edges))
(print "Neighbors of 3:")
(print (neighbors 3 edges))
(print "Neighbors of 7 (isolated):")
(print (neighbors 7 edges))

; ============================================================
; Compute degree of a vertex
; ============================================================
(define (degree v edge-list)
  (ord (neighbors v edge-list)))

(print "--- Degrees ---")
(print "deg(1) =") (print (degree 1 edges))
(print "deg(2) =") (print (degree 2 edges))
(print "deg(3) =") (print (degree 3 edges))
(print "deg(7) =") (print (degree 7 edges))

; Verify handshaking lemma: sum of degrees = 2 * |E|
(define (sum-degrees verts edge-list)
  (if (equal verts nil) 0
    (+ (degree (car verts) edge-list)
       (sum-degrees (cdr verts) edge-list))))

(define total-deg (sum-degrees vertices edges))
(print "Sum of all degrees:")
(print total-deg)
(print "2 * |E| =")
(print (* 2 (ord edges)))
(if (= total-deg (* 2 (ord edges)))
    (print "VERIFIED: Handshaking lemma holds")
    (print "FAILED"))

; ============================================================
; BFS: check if path exists between two vertices
; Uses a visited list (acting as a set) and a queue
; ============================================================
(define (bfs-path start goal edge-list)
  (labels (((bfs-loop queue visited)
    (if (equal queue nil) nil
      (let ((current (car queue))
            (rest-q (cdr queue)))
        (if (equal current goal) t
          (if (my-memberp current visited)
              (bfs-loop rest-q visited)
            (let ((nbrs (neighbors current edge-list))
                  (new-visited (cons current visited)))
              (labels (((add-unvisited ns)
                (if (equal ns nil) nil
                  (if (my-memberp (car ns) new-visited)
                      (add-unvisited (cdr ns))
                      (cons (car ns) (add-unvisited (cdr ns)))))))
                (bfs-loop (my-append rest-q (add-unvisited nbrs))
                          new-visited)))))))))
    (bfs-loop (list start) nil)))

(print "--- Path existence (BFS) ---")

(print "Path 1 -> 6?")
(define p16 (bfs-path 1 6 edges))
(print p16)
(if p16 (print "VERIFIED: Path exists 1->6") (print "FAILED"))

(print "Path 1 -> 5?")
(define p15 (bfs-path 1 5 edges))
(print p15)
(if p15 (print "VERIFIED: Path exists 1->5") (print "FAILED"))

(print "Path 1 -> 7?")
(define p17 (bfs-path 1 7 edges))
(print p17)
(if (not p17) (print "VERIFIED: No path 1->7 (isolated vertex)") (print "FAILED"))

(print "Path 4 -> 6?")
(define p46 (bfs-path 4 6 edges))
(print p46)
(if p46 (print "VERIFIED: Path exists 4->6") (print "FAILED"))

(print "=== Graph Theory Complete ===")
