; ============================================================
; name: eulerian-circuit
; file: algorithms/graphs/eulerian-circuit.lsp
; subject: graph
; capability: traversal circuit
; aliases: "hierholzer" "euler tour" "eulerian path"
; signature: (eulerian-circuit edges) -> vertex list | nil
; complexity: O(E^2) in this edge-list representation
; see-also: strongly-connected-components dfs-order
; derived-from: Hierholzer 1873
; status: example (tested)
; checks: 11 checks incl. every-edge-exactly-once validity;
;         run: src/sisp <this file> from repo root
; ============================================================
; Hierholzer's algorithm for an Eulerian circuit: an undirected graph
; has one iff every vertex has even degree and all edges are in one
; component.  edges = ((u v) ...).  Returns the circuit as a vertex
; list (start = end), or nil when no circuit exists.

(load "lib/collections/alist.lsp")

(define (ec-member x l)
  (cond ((null l) nil) ((equal x (car l)) t) (t (ec-member x (cdr l)))))

; degree of v over the edge list
(define (ec-degree edges v)
  (cond ((null edges) 0)
        ((ec-edge-touches (car edges) v)
         (+ 1 (ec-degree (cdr edges) v)))
        (t (ec-degree (cdr edges) v))))

(define (ec-edge-touches e v)
  (or (equal (car e) v) (equal (cadr e) v)))

(define (ec-all-even-p edges vertices)
  (cond ((null vertices) t)
        ((= (mod (ec-degree edges (car vertices)) 2) 0)
         (ec-all-even-p edges (cdr vertices)))
        (t nil)))

(define (ec-vertices edges acc)
  (cond ((null edges) acc)
        (t (ec-vertices (cdr edges)
                        (cond ((ec-member (caar edges) acc)
                               (cond ((ec-member (cadar edges) acc) acc)
                                     (t (cons (cadar edges) acc))))
                              (t (cons (caar edges)
                                       (cond ((ec-member (cadar edges) acc) acc)
                                             (t (cons (cadar edges) acc))))))))))

; neighbors of v over remaining edges
(define (ec-neighbors edges v)
  (cond ((null edges) nil)
        ((equal (caar edges) v) (cons (cadar edges) (ec-neighbors (cdr edges) v)))
        ((equal (cadar edges) v) (cons (caar edges) (ec-neighbors (cdr edges) v)))
        (t (ec-neighbors (cdr edges) v))))

; remove the first edge between u and v (either orientation)
(define (ec-remove-edge edges u v)
  (cond ((null edges) nil)
        ((or (and (equal (caar edges) u) (equal (cadar edges) v))
             (and (equal (caar edges) v) (equal (cadar edges) u)))
         (cdr edges))
        (t (cons (car edges) (ec-remove-edge (cdr edges) u v)))))

; Hierholzer state = (stack circuit remaining-edges)
(define (ec-step state)
  (let* ((stack (car state))
         (circuit (cadr state))
         (remaining (caddr state))
         (v (car stack))
         (ns (ec-neighbors remaining v)))
    (cond ((null ns)
           ; dead end: move stack top to circuit
           (list (cdr stack) (cons v circuit) remaining))
          (t ; walk to the first neighbor, consuming the edge
             (list (cons (car ns) stack) circuit
                   (ec-remove-edge remaining v (car ns)))))))

(define (ec-finished-p state)
  (and (null (car state)) (null (caddr state))))

(define (ec-run state)
  (cond ((ec-finished-p state) (cadr state))
        (t (ec-run (ec-step state)))))

(define (eulerian-circuit edges)
  "eulerian circuit as a vertex list (start = end), or nil"
  (let ((vertices (ec-vertices edges nil)))
    (cond ((null edges) nil)
          ((not (ec-all-even-p edges vertices)) nil)
          ((not (ec-connected-p edges vertices)) nil)
          (t (ec-run (list (list (car vertices)) nil edges))))))

; connectivity over vertices that appear in edges
(define (ec-connected-p edges vertices)
  (let ((reached (ec-reach edges (list (car vertices)) nil)))
    (ec-all-reached vertices reached)))

(define (ec-reach edges frontier seen)
  (cond ((null frontier) seen)
        (t (let ((fresh (ec-fresh-neighbors edges (car frontier) seen)))
             (ec-reach edges (append (cdr frontier) fresh)
                       (cons (car frontier) seen))))))

(define (ec-fresh-neighbors edges v seen)
  (ec-fresh-walk (ec-neighbors edges v) seen nil))

(define (ec-fresh-walk ns seen acc)
  (cond ((null ns) acc)
        ((ec-member (car ns) seen) (ec-fresh-walk (cdr ns) seen acc))
        (t (ec-fresh-walk (cdr ns) seen (cons (car ns) acc)))))

(define (ec-all-reached vertices reached)
  (cond ((null vertices) t)
        ((ec-member (car vertices) reached)
         (ec-all-reached (cdr vertices) reached))
        (t nil)))

; ---- check helpers (defined before use) ----

; does the circuit use every edge exactly once (either orientation)?
(define (ec-circuit-valid-p circuit edges)
  (and (ec-every-edge-once circuit edges)
       (ec-walk-valid-p circuit edges)))

(define (ec-every-edge-once circuit edges)
  (cond ((null edges) t)
        (t (and (= (ec-count-edge circuit (car edges)) 1)
                (ec-every-edge-once circuit (cdr edges))))))

; count occurrences of edge (u v) in the circuit's consecutive pairs
(define (ec-count-edge circuit e)
  (ec-count-walk circuit e 0))

(define (ec-count-walk circuit e acc)
  (cond ((or (null circuit) (null (cdr circuit))) acc)
        ((or (and (equal (car circuit) (car e))
                  (equal (cadr circuit) (cadr e)))
             (and (equal (car circuit) (cadr e))
                  (equal (cadr circuit) (car e))))
         (ec-count-walk (cdr circuit) e (+ acc 1)))
        (t (ec-count-walk (cdr circuit) e acc))))

; every consecutive pair in the circuit is an edge
(define (ec-walk-valid-p circuit edges)
  (cond ((or (null circuit) (null (cdr circuit))) t)
        ((ec-is-edge edges (car circuit) (cadr circuit))
         (ec-walk-valid-p (cdr circuit) edges))
        (t nil)))

(define (ec-is-edge edges u v)
  (cond ((null edges) nil)
        ((or (and (equal (caar edges) u) (equal (cadar edges) v))
             (and (equal (caar edges) v) (equal (cadar edges) u))) t)
        (t (ec-is-edge (cdr edges) u v))))

; ---- checks ----

; fixture: two triangles sharing vertex 3
(define ec-fixture (list (list 1 2) (list 2 3) (list 3 1)
                         (list 3 4) (list 4 5) (list 5 3)))

(check "eulerian: two-triangle fixture produces a circuit"
       (let ((c (eulerian-circuit ec-fixture)))
         (and (not (eq c nil))
              (equal (car c) (car (reverse c))))))

(check "eulerian: fixture circuit uses every edge exactly once"
       (ec-circuit-valid-p (eulerian-circuit ec-fixture) ec-fixture))

(check "eulerian: fixture circuit length = edges + 1"
       (= (length (eulerian-circuit ec-fixture)) 7))

(check "eulerian: single triangle"
       (let ((c (eulerian-circuit (list (list 1 2) (list 2 3) (list 3 1)))))
         (and (not (eq c nil))
              (= (length c) 4)
              (ec-circuit-valid-p c (list (list 1 2) (list 2 3) (list 3 1))))))

(check "eulerian: square with diagonals is not eulerian (odd degrees)"
       (eq (eulerian-circuit (list (list 1 2) (list 2 3) (list 3 4)
                                   (list 4 1) (list 1 3)))
           nil))

(check "eulerian: disconnected even-degree graph -> nil"
       (eq (eulerian-circuit (list (list 1 2) (list 2 1)
                                   (list 3 4) (list 4 3)))
           nil))

(check "eulerian: single edge is not eulerian (odd degrees)"
       (eq (eulerian-circuit (list (list 1 2))) nil))

(check "eulerian: empty -> nil"
       (eq (eulerian-circuit nil) nil))

(check "eulerian: square (all degree 2) has a circuit"
       (let ((c (eulerian-circuit (list (list 1 2) (list 2 3)
                                        (list 3 4) (list 4 1)))))
         (and (not (eq c nil))
              (= (length c) 5)
              (ec-circuit-valid-p c (list (list 1 2) (list 2 3)
                                          (list 3 4) (list 4 1))))))

(check "eulerian: figure-eight (bowtie) uses every edge once"
       (let ((c (eulerian-circuit (list (list 1 2) (list 2 3) (list 3 1)
                                        (list 1 4) (list 4 5) (list 5 1)))))
         (and (not (eq c nil))
              (ec-circuit-valid-p c (list (list 1 2) (list 2 3) (list 3 1)
                                          (list 1 4) (list 4 5) (list 5 1))))))

(check "eulerian: kite with a tail edge is not eulerian"
       (eq (eulerian-circuit (list (list 1 2) (list 2 3) (list 3 1)
                                   (list 3 4)))
           nil))
