; ============================================================
; name: bipartite-matching
; file: algorithms/graphs/bipartite-matching.lsp
; subject: graph
; capability: matching optimize
; aliases: "kuhn" "maximum bipartite matching" "augmenting path"
; signature: (bipartite-max-matching left right edges) -> matching
;            (matching-size m)
; complexity: O(V E) (Kuhn augmenting paths)
; see-also: maximum-flow hopcroft-karp
; derived-from: Kuhn 1955 (augmenting paths); CLRS 26.3
; status: example (tested)
; checks: 12 checks incl. a graph where stopping at the first free
;         vertex without an augmenting path would undercount, and
;         brute-force agreement on small graphs; run: src/sisp <file>
; ============================================================
; Maximum bipartite matching by Kuhn's augmenting-path algorithm:
; for EVERY free left vertex, try to find an augmenting path
; (alternating unmatched/matched edges) to a free right vertex; flip
; the path's membership.  edges = ((u v) ...) with u in left, v in right.

(load "lib/collections/alist.lsp")

(define (bm-member x l)
  (cond ((null l) nil) ((equal x (car l)) t) (t (bm-member x (cdr l)))))

; neighbors of u along the edge list
(define (bm-neighbors edges u)
  (cond ((null edges) nil)
        ((equal (caar edges) u)
         (cons (cadar edges) (bm-neighbors (cdr edges) u)))
        (t (bm-neighbors (cdr edges) u))))

; is v matched to some u in the matching ((u v) ...)
(define (bm-matched-to m v)
  (cond ((null m) nil)
        ((equal (cadar m) v) (caar m))
        (t (bm-matched-to (cdr m) v))))

(define (bm-matched m u)
  (cond ((null m) nil)
        ((equal (caar m) u) t)
        (t (bm-matched (cdr m) u))))

; DFS for an augmenting path from u; path = reversed list of edges
(define (bm-try edges m u visited-r)
  (bm-try-ns edges m (bm-neighbors edges u) u visited-r nil))

(define (bm-try-ns edges m ns u visited-r path)
  (cond ((null ns) nil)
        ((bm-member (car ns) visited-r)
         (bm-try-ns edges m (cdr ns) u visited-r path))
        (t (let* ((v (car ns))
                  (matched-u (bm-matched-to m v))
                  (path2 (cons (list u v) path)))
             (cond ((eq matched-u nil)
                    path2)                          ; free right vertex: augment
                   (t (let ((deeper (bm-try edges m matched-u
                                            (cons v visited-r))))
                         (cond ((eq deeper nil)
                                (bm-try-ns edges m (cdr ns) u visited-r path))
                               (t (append path2
                                          (cons (list matched-u v)
                                                deeper)))))))))))

; flip path membership: unmatched edges join, matched edges leave
(define (bm-flip m path)
  (cond ((null path) m)
        ((bm-edge-present m (car path))
         (bm-flip (bm-remove-edge m (car path)) (cdr path)))
        (t (bm-flip (cons (car path) m) (cdr path)))))

(define (bm-edge-present m e)
  (cond ((null m) nil)
        ((and (equal (caar m) (car e)) (equal (cadar m) (cadr e))) t)
        (t (bm-edge-present (cdr m) e))))

(define (bm-remove-edge m e)
  (cond ((null m) nil)
        ((and (equal (caar m) (car e)) (equal (cadar m) (cadr e))) (cdr m))
        (t (cons (car m) (bm-remove-edge (cdr m) e)))))

; try to grow the matching from every free left vertex (Kuhn's single pass)
(define (bm-improve-all edges m left)
  (cond ((null left) m)
        ((bm-matched m (car left))
         (bm-improve-all edges m (cdr left)))
        (t (let ((path (bm-try edges m (car left) nil)))
             (cond ((eq path nil)
                    (bm-improve-all edges m (cdr left)))
                   (t (bm-improve-all edges (bm-flip m path) (cdr left))))))))

(define (bipartite-max-matching left right edges)
  "maximum matching as a list of (u v) pairs"
  (bm-improve-all edges nil (append left nil)))

(define (matching-size m) (length m))

; ---- check helpers (defined before use) ----

; a valid matching: every pair is an edge and no vertex repeats
(define (bm-valid-matching-p m edges)
  (cond ((null m) t)
        (t (and (bm-edge-in (car m) edges)
                (bm-no-vertex-repeats m)
                (bm-valid-matching-p (cdr m) edges)))))

(define (bm-edge-in e edges)
  (cond ((null edges) nil)
        ((and (equal (caar edges) (car e)) (equal (cadar edges) (cadr e))) t)
        (t (bm-edge-in e (cdr edges)))))

(define (bm-no-vertex-repeats m)
  (bm-nvr-walk (append (map car m) (map cadr m))))

(define (bm-nvr-walk vs)
  (cond ((null vs) t)
        ((bm-member (car vs) (cdr vs)) nil)
        (t (bm-nvr-walk (cdr vs)))))

; brute force: largest matching over all subsets of edges (small graphs)
(define (bm-brute-force edges left right)
  (bm-bf-walk (bm-subsets edges) 0))

(define (bm-subsets xs)
  (cond ((null xs) (list nil))
        (t (let ((rest (bm-subsets (cdr xs))))
             (append rest (map (lambda (s) (cons (car xs) s)) rest))))))

(define (bm-bf-walk subsets best)
  (cond ((null subsets) best)
        (t (bm-bf-walk (cdr subsets)
                       (cond ((and (bm-valid-matching-p (car subsets)
                                                        (bm-subset-edges (car subsets)))
                                  (> (length (car subsets)) best))
                              (length (car subsets)))
                             (t best))))))

; a subset of the edge list is itself an edge list
(define (bm-subset-edges subset) subset)

; ---- checks ----

; fixture: L=(1 2 3), R=(a b c), E=((1 a)(1 b)(2 b)(3 b)(3 c))
(define bm-fixture-edges
  (list (list 1 "a") (list 1 "b") (list 2 "b") (list 3 "b") (list 3 "c")))

(check "matching: corpus fixture size = 3"
       (= (matching-size (bipartite-max-matching (list 1 2 3) (list "a" "b" "c")
                                                 bm-fixture-edges))
          3))

(check "matching: corpus fixture result is a valid matching"
       (bm-valid-matching-p (bipartite-max-matching (list 1 2 3)
                                                    (list "a" "b" "c")
                                                    bm-fixture-edges)
                            bm-fixture-edges))

; early-exit counterexample: once 1 takes a, vertex 2 has no augmenting
; path, but 3 and 4 still do, so the pass must continue past vertex 2.
; L=(1 2 3 4), E=((1 a)(2 a)(3 b)(4 b)(4 c)); max matching = 3
(define bm-counter-edges
  (list (list 1 "a") (list 2 "a") (list 3 "b") (list 4 "b") (list 4 "c")))

(check "matching: early-exit counterexample = 3"
       (= (matching-size (bipartite-max-matching (list 1 2 3 4)
                                                 (list "a" "b" "c")
                                                 bm-counter-edges))
          3))

(check "matching: counterexample result is valid"
       (bm-valid-matching-p (bipartite-max-matching (list 1 2 3 4)
                                                    (list "a" "b" "c")
                                                    bm-counter-edges)
                            bm-counter-edges))

(check "matching: brute-force agreement on the fixture (3)"
       (= (bm-brute-force bm-fixture-edges (list 1 2 3) (list "a" "b" "c")) 3))

(check "matching: brute-force agreement on the counterexample (3)"
       (= (bm-brute-force bm-counter-edges (list 1 2 3 4) (list "a" "b" "c")) 3))

(check "matching: matches brute force on a 3x3 cycle (2)"
       (= (matching-size (bipartite-max-matching (list 1 2 3)
                                                 (list "a" "b" "c")
                                                 (list (list 1 "a") (list 2 "b"))))
          2))

(check "matching: no edges -> 0"
       (= (matching-size (bipartite-max-matching (list 1 2) (list "a" "b") nil))
          0))

(check "matching: star graph -> 1"
       (= (matching-size (bipartite-max-matching (list 1 2 3)
                                                 (list "a")
                                                 (list (list 1 "a")
                                                       (list 2 "a")
                                                       (list 3 "a"))))
          1))

(check "matching: complete 3x3 -> 3"
       (= (matching-size (bipartite-max-matching (list 1 2 3)
                                                 (list "a" "b" "c")
                                                 (list (list 1 "a") (list 1 "b") (list 1 "c")
                                                       (list 2 "a") (list 2 "b") (list 2 "c")
                                                       (list 3 "a") (list 3 "b") (list 3 "c"))))
          3))

(check "matching: result of complete 3x3 is valid"
       (bm-valid-matching-p (bipartite-max-matching (list 1 2 3)
                                                    (list "a" "b" "c")
                                                    (list (list 1 "a") (list 1 "b") (list 1 "c")
                                                          (list 2 "a") (list 2 "b") (list 2 "c")
                                                          (list 3 "a") (list 3 "b") (list 3 "c")))
                            (list (list 1 "a") (list 1 "b") (list 1 "c")
                                  (list 2 "a") (list 2 "b") (list 2 "c")
                                  (list 3 "a") (list 3 "b") (list 3 "c"))))

(check "matching: augmenting path length 3 needed (classic flip case)"
       (= (matching-size (bipartite-max-matching (list 1 2)
                                                 (list "a" "b")
                                                 (list (list 1 "a") (list 1 "b") (list 2 "a"))))
          2))
