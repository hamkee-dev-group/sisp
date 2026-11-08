; ============================================================
; name: maximum-flow
; file: algorithms/graphs/maximum-flow.lsp
; subject: graph
; capability: flow optimize
; aliases: "edmonds-karp" "max flow" "ford-fulkerson"
; signature: (edmonds-karp vertices edges source sink)
;              -> (maxflow-value . residual-alist)
; complexity: O(V E^2) worst case
; see-also: bipartite-matching shortest-path
; derived-from: Edmonds & Karp 1972; CLRS 26.2
; status: example (tested)
; checks: 12 checks incl. a 4-vertex fixture (15) and CLRS 26.1 (23);
;         run: src/sisp <this file> from repo root
; ============================================================
; Edmonds-Karp max flow: BFS augmenting paths in the residual network.
; edges = ((u v cap) ...) directed.  Residual capacity is kept in an
; alist keyed (u . v); the reverse arc (v . u) starts at 0.  Returns a
; dotted pair (value . residuals); mf-flow-of recovers the flow on an
; original arc as capacity - residual.

(load "lib/collections/alist.lsp")

; residual lookup with default 0 for absent arcs
(define (mf-residual res u v)
  (let ((entry (alist-get res (cons u v))))
    (cond ((eq entry nil) 0) (t entry))))

(define (mf-set-residual res u v c)
  (alist-set res (cons u v) c))

; outgoing arcs of u with positive residual, as (v . residual)
(define (mf-neighbors res vertices u)
  (mf-neighbors-walk res vertices u nil))

(define (mf-neighbors-walk res vertices u acc)
  (cond ((null vertices) (reverse acc))
        ((equal (car vertices) u) (mf-neighbors-walk res (cdr vertices) u acc))
        (t (let ((c (mf-residual res u (car vertices))))
             (cond ((> c 0)
                    (mf-neighbors-walk res (cdr vertices) u
                                       (cons (cons (car vertices) c) acc)))
                   (t (mf-neighbors-walk res (cdr vertices) u acc)))))))

(define (mf-fresh-neighbors res vertices u visited)
  (mf-fresh-walk (mf-neighbors res vertices u) visited nil))

(define (mf-flat-member x l)
  (cond ((null l) nil) ((equal x (car l)) t) (t (mf-flat-member x (cdr l)))))

(define (mf-fresh-walk ns visited acc)
  (cond ((null ns) (reverse acc))
        ((not (mf-flat-member (caar ns) visited))
         (mf-fresh-walk (cdr ns) visited (cons (car ns) acc)))
        (t (mf-fresh-walk (cdr ns) visited acc))))

(define (mf-member v ns)
  (cond ((null ns) nil)
        ((equal (caar ns) v) t)
        (t (mf-member v (cdr ns)))))

; BFS that records parents (needed to reconstruct the path)
(define (mf-bfs-parents res vertices source sink)
  (mf-bfs-p-walk res vertices (list source)
                 (list (cons source source)) sink))

(define (mf-bfs-p-walk res vertices queue parents sink)
  (cond ((null queue) nil)
        (t (let* ((u (car queue))
                  (fresh (mf-fresh-neighbors res vertices u
                                             (mf-parent-keys parents))))
             (cond ((mf-member sink fresh)
                    (alist-set parents sink u))
                   (t (mf-bfs-p-walk res vertices
                                     (append (cdr queue) (map car fresh))
                                     (mf-record-parents fresh u parents)
                                     sink)))))))

(define (mf-parent-keys parents)
  (map car parents))

(define (mf-record-parents fresh u parents)
  (cond ((null fresh) parents)
        (t (mf-record-parents (cdr fresh) u
                              (alist-set parents (caar fresh) u)))))

; bottleneck along the path sink -> source following parents
(define (mf-bottleneck res parents source sink)
  (cond ((equal source sink) 9223372036854775807)
        (t (let ((p (alist-get parents sink)))
             (min (mf-residual res p sink)
                  (mf-bottleneck res parents source p))))))

; augment along the path: forward -= b, reverse += b
(define (mf-augment res parents source sink b)
  (cond ((equal source sink) res)
        (t (let* ((p (alist-get parents sink))
                  (res2 (mf-set-residual res p sink
                                         (- (mf-residual res p sink) b)))
                  (res3 (mf-set-residual res2 sink p
                                         (+ (mf-residual res2 sink p) b))))
             (mf-augment res3 parents source p b)))))

(define (mf-loop res vertices source sink total)
  (let ((parents (mf-bfs-parents res vertices source sink)))
    (cond ((eq (alist-get parents sink) nil)
           (cons total res))
          (t (let ((b (mf-bottleneck res parents source sink)))
               (mf-loop (mf-augment res parents source sink b)
                        vertices source sink (+ total b)))))))

; build initial residuals: forward = capacity, reverse = 0
(define (mf-init-residual edges res)
  (cond ((null edges) res)
        (t (mf-init-residual (cdr edges)
                             (mf-set-residual res (caar edges) (cadar edges)
                                              (caddar edges))))))

(define (edmonds-karp vertices edges source sink)
  "max flow from source to sink; returns (value . residual-alist)"
  (mf-loop (mf-init-residual edges nil) vertices source sink 0))

; the actual flow on an arc = capacity - residual, counting only
; original edges (the residual of a reverse arc is not negative flow)
(define (mf-flow-of res edges u v)
  (cond ((= (mf-capacity edges u v) 0) 0)
        (t (- (mf-capacity edges u v) (mf-residual res u v)))))

(define (mf-capacity edges u v)
  (cond ((null edges) 0)
        ((and (equal (caar edges) u) (equal (cadar edges) v))
         (caddar edges))
        (t (mf-capacity (cdr edges) u v))))

; total flow out of source
(define (mf-value res edges vertices source)
  (foldl (lambda (acc v) (+ acc (mf-flow-of res edges source v))) 0 vertices))

; ---- check helpers (defined before use) ----

; flow conservation: for every vertex except s/t, inflow = outflow
; (walk-vertices is the remaining checklist; all-vertices never shrinks)
(define (mf-conserved-p res edges vertices source sink)
  (mf-conserved-walk res edges vertices vertices source sink))

(define (mf-conserved-walk res edges remaining all-vertices source sink)
  (cond ((null remaining) t)
        ((or (equal (car remaining) source) (equal (car remaining) sink))
         (mf-conserved-walk res edges (cdr remaining) all-vertices source sink))
        (t (let ((inflow (foldl (lambda (acc u)
                                  (+ acc (mf-flow-of res edges u (car remaining))))
                                0 all-vertices))
                 (outflow (foldl (lambda (acc v)
                                   (+ acc (mf-flow-of res edges (car remaining) v)))
                                 0 all-vertices)))
             (and (= inflow outflow)
                  (mf-conserved-walk res edges (cdr remaining)
                                     all-vertices source sink))))))

; capacity constraints: 0 <= flow <= capacity on every arc
(define (mf-capacity-ok-p res edges)
  (cond ((null edges) t)
        (t (let ((f (mf-flow-of res edges (caar edges) (cadar edges))))
             (and (>= f 0)
                  (<= f (caddar edges))
                  (mf-capacity-ok-p res (cdr edges)))))))

; cut capacity: sum of capacities from S to T
(define (mf-cut-capacity edges s-side vertices)
  (mf-cut-walk edges s-side vertices 0))

(define (mf-cut-walk edges s-side vertices acc)
  (cond ((null edges) acc)
        (t (let* ((u (caar edges))
                  (v (cadar edges))
                  (c (caddar edges)))
             (cond ((and (mf-in u s-side) (not (mf-in v s-side)))
                    (mf-cut-walk (cdr edges) s-side vertices (+ acc c)))
                   (t (mf-cut-walk (cdr edges) s-side vertices acc)))))))

(define (mf-in x l)
  (cond ((null l) nil) ((equal x (car l)) t) (t (mf-in x (cdr l)))))

; ---- checks ----

; 4-vertex fixture: s=0, a=1, b=2, t=3; s->a 10, s->b 5, a->b 3,
; a->t 7, b->t 8
(define mf-fixture-edges
  (list (list 0 1 10) (list 0 2 5) (list 1 2 3) (list 1 3 7) (list 2 3 8)))
(define mf-fixture-result
  (edmonds-karp (list 0 1 2 3) mf-fixture-edges 0 3))

(check "max-flow: corpus fixture value = 15"
       (= (mf-value (cdr mf-fixture-result) mf-fixture-edges (list 0 1 2 3) 0)
          15))

(check "max-flow: corpus fixture min cut = max flow (S = {0}, cut 10+5)"
       (= (mf-cut-capacity mf-fixture-edges (list 0) (list 0 1 2 3))
          (mf-value (cdr mf-fixture-result) mf-fixture-edges (list 0 1 2 3) 0)))

(check "max-flow: corpus fixture flow conservation holds"
       (mf-conserved-p (cdr mf-fixture-result) mf-fixture-edges
                       (list 0 1 2 3) 0 3))

(check "max-flow: corpus fixture capacity constraints hold"
       (mf-capacity-ok-p (cdr mf-fixture-result) mf-fixture-edges))

; CLRS Fig 26.1: s=0, v1..v4 = 1..4, t=5; max flow 23
(define mf-clrs-edges
  (list (list 0 1 16) (list 0 2 13) (list 1 2 10) (list 2 1 4)
        (list 1 3 12) (list 2 4 14) (list 3 2 9) (list 4 3 7)
        (list 3 5 20) (list 4 5 4)))
(define mf-clrs-result
  (edmonds-karp (list 0 1 2 3 4 5) mf-clrs-edges 0 5))

(check "max-flow: CLRS 26.1 value = 23"
       (= (mf-value (cdr mf-clrs-result) mf-clrs-edges (list 0 1 2 3 4 5) 0)
          23))

(check "max-flow: CLRS conservation and capacity"
       (and (mf-conserved-p (cdr mf-clrs-result) mf-clrs-edges
                            (list 0 1 2 3 4 5) 0 5)
          (mf-capacity-ok-p (cdr mf-clrs-result) mf-clrs-edges)))

(check "max-flow: CLRS min cut = 23 (S = {0 1 2 4}, out: 12+7+4)"
       (= (mf-cut-capacity mf-clrs-edges (list 0 1 2 4) (list 0 1 2 3 4 5))
          23))

(check "max-flow: single arc"
       (= (mf-value (cdr (edmonds-karp (list 1 2) (list (list 1 2 7)) 1 2))
                    (list (list 1 2 7)) (list 1 2) 1)
          7))

(check "max-flow: no path -> 0"
       (= (mf-value (cdr (edmonds-karp (list 1 2 3)
                                       (list (list 1 2 5) (list 3 2 5)) 1 2))
                    (list (list 1 2 5) (list 3 2 5)) (list 1 2 3) 1)
          5))

(check "max-flow: parallel paths split the flow"
       (= (mf-value (cdr (edmonds-karp (list 1 2 3 4)
                                       (list (list 1 2 3) (list 1 3 2)
                                             (list 2 4 2) (list 3 4 4)) 1 4))
                    (list (list 1 2 3) (list 1 3 2) (list 2 4 2) (list 3 4 4))
                    (list 1 2 3 4) 1)
          4))

(check "max-flow: bottleneck respected"
       (= (mf-value (cdr (edmonds-karp (list 1 2 3)
                                       (list (list 1 2 10) (list 2 3 3)) 1 3))
                    (list (list 1 2 10) (list 2 3 3)) (list 1 2 3) 1)
          3))

(check "max-flow: residual network allows re-routing (classic back-arc case)"
       (= (mf-value (cdr (edmonds-karp (list 1 2 3 4)
                                       (list (list 1 2 3) (list 1 3 2)
                                             (list 2 3 2) (list 2 4 2)
                                             (list 3 4 3)) 1 4))
                    (list (list 1 2 3) (list 1 3 2) (list 2 3 2)
                          (list 2 4 2) (list 3 4 3))
                    (list 1 2 3 4) 1)
          5))
