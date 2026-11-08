; ============================================================
; lib/graph/mst.lsp -- minimum spanning tree (Kruskal)
;
; Public:   (kruskal-mst vertices edges) -> list of edges | nil
;           (mst-weight edges) -> number
; Private:  mst-by-weight, mst-edge-less, mst-loop
; Depends:  lib/collections/disjoint-set.lsp, lib/math/sorting.lsp
; Derived-from: CLRS 23.1 (Kruskal's algorithm)
; Assumes:  undirected graph as vertices list + edge list ((u v w) ...);
;           each undirected edge appears once (either orientation);
;           weights comparable with < ; vertices comparable with =
;
; Contract: returns the MST as a list of edges in selection order.
;           The MST of a single vertex is the empty list.  Returns nil
;           when the graph is disconnected (no spanning tree exists),
;           rather than returning a spanning forest.
; ============================================================
(load "lib/collections/disjoint-set.lsp")
(load "lib/math/sorting.lsp")

(define (mst-edge-less a b) (< (caddr a) (caddr b)))

(define (mst-by-weight edges)
  (merge-sort mst-edge-less edges))

; state = (dsu selected); walk sorted edges, union distinct components
(define (mst-loop edges state)
  (cond ((null edges) state)
        (t (let* ((e (car edges))
                  (dsu (car state))
                  (picked (cdr state))
                  (merged (dsu-union dsu (car e) (cadr e))))
             (cond ((car merged)
                    (mst-loop (cdr edges)
                              (cons (cdr merged) (cons e picked))))
                   (t (mst-loop (cdr edges) state)))))))

(define (kruskal-mst vertices edges)
  "minimum spanning tree of an undirected weighted graph, or nil if disconnected"
  (let ((final (mst-loop (mst-by-weight edges)
                         (cons (dsu-init vertices) nil))))
    (cond ((= (length (cdr final)) (- (length vertices) 1))
           (reverse (cdr final)))
          (t nil))))

(define (mst-weight edges)
  "total weight of an edge list"
  (foldl (lambda (acc e) (+ acc (caddr e))) 0 edges))
