; ============================================================
; name: topological-sort
; file: algorithms/graphs/topological-sort.lsp
; subject: graph
; capability: dag ordering scheduling
; aliases: "toposort" "kahn" "dependency order"
; signature: (topological-sort vertices edges) -> order | cycle
; complexity: O(V + E) with a queue and adjacency lists; O(V (V + E))
;             in this list-scanning form
; see-also: dfs-order strongly-connected-components
; derived-from: Kahn 1962; CLRS 22.4
; status: example (tested)
; checks: 11 checks; run: src/sisp <this file> from repo root
; ============================================================
; Kahn's algorithm: repeatedly emit a vertex of in-degree zero.
; edges = ((u v) ...) meaning u must come before v.  Returns the symbol
; cycle when the graph has a directed cycle.  Equal-priority vertices
; come out in input order (deterministic).

(load "lib/collections/alist.lsp")

(define (ts-member x l)
  (cond ((null l) nil) ((equal x (car l)) t) (t (ts-member x (cdr l)))))

; in-degree alist: every vertex starts at 0
(define (ts-init-degrees vertices edges)
  (ts-count-degrees edges (map (lambda (v) (cons v 0)) vertices)))

(define (ts-count-degrees edges degrees)
  (cond ((null edges) degrees)
        (t (ts-count-degrees (cdr edges)
                             (ts-bump-degree degrees (cadar edges))))))

(define (ts-bump-degree degrees v)
  (cond ((null degrees) nil)
        ((equal (caar degrees) v)
         (cons (cons v (+ (cdar degrees) 1)) (cdr degrees)))
        (t (cons (car degrees) (ts-bump-degree (cdr degrees) v)))))

(define (ts-outgoing edges v)
  (cond ((null edges) nil)
        ((equal (caar edges) v) (cons (cadar edges) (ts-outgoing (cdr edges) v)))
        (t (ts-outgoing (cdr edges) v))))

; first vertex in `remaining` whose degree is 0, or nil
(define (ts-pick-zero remaining degrees)
  (cond ((null remaining) nil)
        ((= (alist-get degrees (car remaining)) 0) (car remaining))
        (t (ts-pick-zero (cdr remaining) degrees))))

(define (ts-remove x l)
  (cond ((null l) nil)
        ((equal (car l) x) (cdr l))
        (t (cons (car l) (ts-remove x (cdr l))))))

(define (ts-loop remaining degrees edges out)
  (let ((v (ts-pick-zero remaining degrees)))
    (cond ((eq v nil)
           (cond ((null remaining) (reverse out))
                 (t (quote cycle))))
          (t (ts-loop (ts-remove v remaining)
                      (ts-decrement-neighbors (ts-outgoing edges v) degrees)
                      edges
                      (cons v out))))))

(define (ts-lower-degree degrees v)
  (cond ((null degrees) nil)
        ((equal (caar degrees) v)
         (cons (cons v (- (cdar degrees) 1)) (cdr degrees)))
        (t (cons (car degrees) (ts-lower-degree (cdr degrees) v)))))

(define (ts-decrement-neighbors ns degrees)
  (cond ((null ns) degrees)
        (t (ts-decrement-neighbors (cdr ns) (ts-lower-degree degrees (car ns))))))

(define (topological-sort vertices edges)
  "topological order of a DAG, or the symbol cycle"
  (ts-loop vertices (ts-init-degrees vertices edges) edges nil))

; ---- check helpers (defined before use) ----

; verify an order respects every edge
(define (ts-valid-order-p order edges)
  (cond ((null edges) t)
        (t (and (ts-before-p (caar edges) (cadar edges) order)
                (ts-valid-order-p order (cdr edges))))))

(define (ts-before-p a b order)
  (cond ((null order) nil)
        ((equal (car order) a) (ts-member b (cdr order)))
        (t (ts-before-p a b (cdr order)))))

; ---- checks ----

(check "topological-sort: single chain"
       (equal (topological-sort (list 1 2 3) (list (list 1 2) (list 2 3)))
              (list 1 2 3)))

(check "topological-sort: diamond"
       (let ((order (topological-sort (list 1 2 3 4)
                                      (list (list 1 2) (list 1 3)
                                            (list 2 4) (list 3 4)))))
         (and (equal (car order) 1)
              (equal (cadddr order) 4)
              (ts-valid-order-p order (list (list 1 2) (list 1 3)
                                            (list 2 4) (list 3 4))))))

(check "topological-sort: CLRS dressing example is a valid order"
       (let ((order (topological-sort
                      (list "undershorts" "pants" "belt" "shirt" "tie"
                            "jacket" "socks" "shoes" "watch")
                      (list (list "undershorts" "pants") (list "pants" "belt")
                            (list "pants" "shoes") (list "undershorts" "shoes")
                            (list "socks" "shoes") (list "shirt" "belt")
                            (list "shirt" "tie") (list "tie" "jacket")
                            (list "belt" "jacket")))))
         (ts-valid-order-p order
                           (list (list "undershorts" "pants") (list "pants" "belt")
                                 (list "pants" "shoes") (list "undershorts" "shoes")
                                 (list "socks" "shoes") (list "shirt" "belt")
                                 (list "shirt" "tie") (list "tie" "jacket")
                                 (list "belt" "jacket")))))

(check "topological-sort: cycle detected"
       (eq (topological-sort (list 1 2 3)
                             (list (list 1 2) (list 2 3) (list 3 1)))
           (quote cycle)))

(check "topological-sort: self-loop is a cycle"
       (eq (topological-sort (list 1) (list (list 1 1))) (quote cycle)))

(check "topological-sort: no edges -> input order"
       (equal (topological-sort (list 3 1 2) nil) (list 3 1 2)))

(check "topological-sort: disconnected components both emitted"
       (let ((order (topological-sort (list 1 2 3 4)
                                      (list (list 1 2) (list 3 4)))))
         (and (= (length order) 4)
              (ts-valid-order-p order (list (list 1 2) (list 3 4))))))

(check "topological-sort: deterministic for equal priorities"
       (equal (topological-sort (list 1 2 3 4)
                                (list (list 1 3) (list 2 3)))
              (list 1 2 3 4)))

(check "topological-sort: order includes every vertex exactly once"
       (let ((order (topological-sort (list 1 2 3 4 5)
                                      (list (list 1 2) (list 2 3)
                                            (list 4 5) (list 1 4)))))
         (and (= (length order) 5)
              (= (length (dedup order)) 5))))

(check "topological-sort: valid order on a bigger random-ish DAG"
       (ts-valid-order-p
         (topological-sort (list 1 2 3 4 5 6)
                           (list (list 1 2) (list 1 3) (list 2 4)
                                 (list 3 4) (list 4 5) (list 3 6)))
         (list (list 1 2) (list 1 3) (list 2 4)
               (list 3 4) (list 4 5) (list 3 6))))

(check "topological-sort: 4-cycle detected"
       (eq (topological-sort (list 1 2 3 4)
                             (list (list 1 2) (list 2 3)
                                   (list 3 4) (list 4 1)))
           (quote cycle)))
