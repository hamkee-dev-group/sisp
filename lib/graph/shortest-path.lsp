; ============================================================
; lib/graph/shortest-path.lsp -- single-source shortest paths
;
; Public:   (dijkstra vertices edges source) -> dist alist
;           (bellman-ford vertices edges source) -> dist alist | negative-cycle
;           (has-negative-cycle vertices edges source) -> t | nil
; Private:  sp-build-adj, sp-add-edge, sp-remove-vertex, sp-drop,
;           sp-caddar, sp-scan-min, sp-relax-out, sp-dijkstra-loop,
;           sp-collect, sp-has-negative, sp-first-negative,
;           sp-bf-passes, sp-bf-pass, sp-check-negative
; Depends:  lib/collections/alist.lsp
; Derived-from: none (written fresh from CLRS 24.3/24.1; fixtures taken
;               from examples/math/graphs/shortest-paths.lsp and
;               examples/math/johnson-asp.lsp)
; Assumes:  edges = ((u v w) ...) directed weighted edge list; vertices
;           comparable with = ; the dist alist maps each reachable vertex
;           to its distance -- UNREACHABLE VERTICES HAVE NO ENTRY.
;
; Contract: dijkstra requires non-negative weights and aborts with
;           error on a negative edge.  bellman-ford tolerates negative
;           weights and returns the symbol negative-cycle when a
;           negative-weight cycle is reachable from source.
;           Complexity: O(V^2 + VE) (list-based extract-min).
; ============================================================
(load "lib/collections/alist.lsp")

; ---- adjacency build: alist vertex -> list of (v w) ----

(define (sp-add-edge adj u v w)
  (let ((entry (alist-get adj u)))
    (cond ((eq entry nil)
           (cons (cons u (list (list v w))) adj))
          (t (cons (cons u (append entry (list (list v w))))
                   (sp-remove-vertex adj u))))))

(define (sp-remove-vertex adj u)
  (cond ((null adj) nil)
        ((equal (caar adj) u) (cdr adj))
        (t (cons (car adj) (sp-remove-vertex (cdr adj) u)))))

; remove first occurrence of u from a flat list
(define (sp-drop xs u)
  (cond ((null xs) nil)
        ((equal (car xs) u) (cdr xs))
        (t (cons (car xs) (sp-drop (cdr xs) u)))))

(define (sp-build-adj edges adj)
  (cond ((null edges) (reverse adj))
        (t (sp-build-adj (cdr edges)
                         (sp-add-edge adj (caar edges) (cadar edges)
                                      (sp-caddar edges))))))

; third element of the first edge
(define (sp-caddar x) (caddr (car x)))

; ---- dijkstra ----

(define (sp-scan-min unvisited dist best)
  (cond ((null unvisited) best)
        (t (let ((d (alist-get dist (car unvisited))))
             (cond ((eq d nil) (sp-scan-min (cdr unvisited) dist best))
                   ((eq best nil) (sp-scan-min (cdr unvisited) dist
                                               (car unvisited)))
                   ((< d (alist-get dist best))
                    (sp-scan-min (cdr unvisited) dist (car unvisited)))
                   (t (sp-scan-min (cdr unvisited) dist best)))))))

(define (sp-relax-out dist u du out)
  (cond ((null out) dist)
        (t (let* ((v (caar out))
                  (w (cadar out))
                  (dv (alist-get dist v))
                  (nd (+ du w)))
             (cond ((or (eq dv nil) (< nd dv))
                    (sp-relax-out (alist-set dist v nd) u du (cdr out)))
                   (t (sp-relax-out dist u du (cdr out))))))))

(define (sp-dijkstra-loop unvisited dist adj)
  (let ((u (sp-scan-min unvisited dist nil)))
    (cond ((eq u nil) dist)
          (t (let* ((du (alist-get dist u))
                    (out (alist-get adj u))
                    (dist2 (sp-relax-out dist u du
                                         (cond ((eq out nil) nil) (t out)))))
               (sp-dijkstra-loop (sp-drop unvisited u) dist2 adj))))))

; collect a clean alist: one entry per reachable vertex, in vertex order
(define (sp-collect dist vertices acc)
  (cond ((null vertices) (reverse acc))
        (t (let ((d (alist-get dist (car vertices))))
             (cond ((eq d nil) (sp-collect dist (cdr vertices) acc))
                   (t (sp-collect dist (cdr vertices)
                                  (cons (cons (car vertices) d) acc))))))))

(define (dijkstra vertices edges source)
  "shortest distances from source; aborts on negative edge weight"
  (cond ((sp-has-negative edges)
         (error "DIJKSTRA: NEGATIVE EDGE WEIGHT" (sp-first-negative edges)))
        (t (sp-collect (sp-dijkstra-loop vertices
                                         (list (cons source 0))
                                         (sp-build-adj edges nil))
                       vertices nil))))

(define (sp-has-negative edges)
  (cond ((null edges) nil)
        ((< (caddr (car edges)) 0) t)
        (t (sp-has-negative (cdr edges)))))

(define (sp-first-negative edges)
  (cond ((null edges) nil)
        ((< (caddr (car edges)) 0) (car edges))
        (t (sp-first-negative (cdr edges)))))

; ---- bellman-ford ----

(define (sp-bf-pass dist edges)
  (cond ((null edges) dist)
        (t (let* ((u (caar edges))
                  (v (cadar edges))
                  (w (caddr (car edges)))
                  (du (alist-get dist u))
                  (dv (alist-get dist v)))
             (cond ((eq du nil) (sp-bf-pass dist (cdr edges)))
                    ((or (eq dv nil) (< (+ du w) dv))
                     (sp-bf-pass (alist-set dist v (+ du w)) (cdr edges)))
                    (t (sp-bf-pass dist (cdr edges))))))))

(define (sp-bf-passes dist edges n)
  (cond ((= n 0) dist)
        (t (sp-bf-passes (sp-bf-pass dist edges) edges (- n 1)))))

(define (sp-check-negative dist edges)
  (cond ((null edges) nil)
        (t (let* ((u (caar edges))
                  (v (cadar edges))
                  (w (caddr (car edges)))
                  (du (alist-get dist u))
                  (dv (alist-get dist v)))
             (cond ((eq du nil) (sp-check-negative dist (cdr edges)))
                    ((or (eq dv nil) (< (+ du w) dv)) t)
                    (t (sp-check-negative dist (cdr edges))))))))

(define (bellman-ford vertices edges source)
  "shortest distances from source, or the symbol negative-cycle"
  (let ((dist (sp-bf-passes (list (cons source 0)) edges
                            (- (length vertices) 1))))
    (cond ((sp-check-negative dist edges) (quote negative-cycle))
          (t (sp-collect dist vertices nil)))))

(define (has-negative-cycle vertices edges source)
  "t when a negative-weight cycle is reachable from source"
  (eq (bellman-ford vertices edges source) (quote negative-cycle)))
