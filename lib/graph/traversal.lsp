; ============================================================
; lib/graph/traversal.lsp -- breadth-first and depth-first traversal
;
; A graph is an adjacency alist mapping each vertex to its list of
; out-neighbors: ((v . (n1 n2 ...)) ...).  A vertex that appears only as
; a neighbor has no out-edges.  Undirected graphs are represented by
; listing each edge in both directions (see graph-from-undirected-edges).
;
; Public:   (graph-from-edges edges) -> graph
;           (graph-from-undirected-edges edges) -> graph
;           (graph-vertices graph) -> list of vertices
;           (bfs-order graph start) -> vertices in breadth-first visit order
;           (dfs-order graph start) -> vertices in depth-first pre-order
;           (connected-components graph) -> list of vertex lists
; Private:  graph-member-p, graph-add-missing, graph-verts-acc,
;           graph-bfs-loop, graph-fresh, graph-dfs-visit, graph-dfs-fold,
;           graph-cc-loop
; Depends:  lib/collections/alist.lsp
; Derived-from: CLRS 22.2/22.3
; Assumes:  vertices compared with equal; adjacency order determines
;           visit order among same-level neighbors; bfs/dfs visit only
;           the component containing start
; ============================================================
(load "lib/collections/alist.lsp")

; equal-based membership (in/assoc default to eql and miss structural keys)
(define (graph-member-p x l)
  (cond ((null l) nil)
        ((equal x (car l)) t)
        (t (graph-member-p x (cdr l)))))

; ---- constructors ----

(define (graph-add-missing vs acc)
  (cond ((null vs) acc)
        ((graph-member-p (car vs) acc) (graph-add-missing (cdr vs) acc))
        (t (graph-add-missing (cdr vs) (cons (car vs) acc)))))

(define (graph-from-edges edges)
  "adjacency alist for a directed edge list ((u v) ...), preserving edge order"
  (reverse (graph-from-edges-acc edges nil)))

(define (graph-from-edges-acc edges acc)
  (cond ((null edges) acc)
        (t (let* ((e (car edges))
                  (u (car e))
                  (v (cadr e))
                  (entry (alist-get acc u)))
             (cond ((eq entry nil)
                    (graph-from-edges-acc (cdr edges)
                                          (cons (cons u (list v)) acc)))
                   (t (graph-from-edges-acc (cdr edges)
                        (cons (cons u (append entry (list v)))
                              (remove (lambda (p) (equal (car p) u))
                                      acc)))))))))

(define (graph-undirected-expand edges)
  (cond ((null edges) nil)
        (t (cons (car edges)
                 (cons (list (cadr (car edges)) (car (car edges)))
                       (graph-undirected-expand (cdr edges)))))))

(define (graph-from-undirected-edges edges)
  "adjacency alist with every edge listed in both directions;
   each vertex's neighbor list follows the order edges were given"
  (graph-from-edges (graph-undirected-expand edges)))

; ---- vertex enumeration ----

(define (graph-vertices graph)
  "every vertex of graph (keys and neighbor-only vertices), first-appearance order"
  (graph-verts-acc graph nil))

(define (graph-verts-acc entries acc)
  (cond ((null entries) (reverse acc))
        (t (graph-verts-acc (cdr entries)
                            (graph-add-missing (cdar entries)
                                               (graph-add-missing (list (caar entries))
                                                                  acc))))))

; ---- breadth-first ----

(define (graph-fresh ns visited)
  (cond ((null ns) nil)
        ((graph-member-p (car ns) visited) (graph-fresh (cdr ns) visited))
        (t (cons (car ns) (graph-fresh (cdr ns) visited)))))

(define (graph-bfs-loop graph queue visited out)
  (cond ((null queue) (reverse out))
        (t (let* ((v (car queue))
                  (ns (alist-get graph v))
                  (fresh (graph-fresh (cond ((eq ns nil) nil) (t ns)) visited)))
             (graph-bfs-loop graph
                             (append (cdr queue) fresh)
                             (append visited fresh)
                             (cons v out))))))

(define (bfs-order graph start)
  "vertices reachable from start, in breadth-first visit order"
  (graph-bfs-loop graph (list start) (list start) nil))

; ---- depth-first ----

; Traversal state is a dotted pair (reversed-output . visited).
(define (graph-dfs-visit graph v state)
  (let ((out (cons v (car state)))
        (visited (cons v (cdr state))))
    (graph-dfs-fold graph
                    (let ((ns (alist-get graph v)))
                      (cond ((eq ns nil) nil) (t ns)))
                    (cons out visited))))

(define (graph-dfs-fold graph ns state)
  (cond ((null ns) state)
        (t (graph-dfs-fold graph (cdr ns)
                           (cond ((graph-member-p (car ns) (cdr state)) state)
                                 (t (graph-dfs-visit graph (car ns) state)))))))

(define (dfs-order graph start)
  "vertices reachable from start, in depth-first pre-order"
  (reverse (car (graph-dfs-visit graph start (cons nil nil)))))

; ---- components ----

(define (graph-cc-loop verts graph visited comps)
  (cond ((null verts) (reverse comps))
        ((graph-member-p (car verts) visited)
         (graph-cc-loop (cdr verts) graph visited comps))
        (t (let ((r (graph-dfs-visit graph (car verts) (cons nil visited))))
             (graph-cc-loop (cdr verts) graph (cdr r)
                            (cons (reverse (car r)) comps))))))

(define (connected-components graph)
  "partition of all vertices into connected components (treats edges as undirected
   only if the adjacency lists are symmetric; use graph-from-undirected-edges)"
  (graph-cc-loop (graph-vertices graph) graph nil nil))
