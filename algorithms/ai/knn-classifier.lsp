; ============================================================
; name: knn-classifier
; file: algorithms/ai/knn-classifier.lsp
; subject: ai
; capability: classify supervised
; aliases: "knn" "k-nearest-neighbors" "nearest neighbor classification"
; signature: (knn-classify examples query k) -> label
; complexity: O(n d) per query
; see-also: knn-regression kmeans
; derived-from: Cover & Hart 1967
; status: example (tested)
; checks: 2 - known-label queries
; ============================================================
; k-nearest-neighbour classification votes among the closest labeled points.
(define (knnc-distance2 left right)
  (cond ((eq left nil) 0)
        (t (+ (^ (- (car left) (car right)) 2) (knnc-distance2 (cdr left) (cdr right))))))
(define (knnc-count votes label)
  (length (filter (lambda (vote) (equal vote label)) votes)))
(define (knnc-majority votes)
  (car (foldl (lambda (best label)
    (let ((count (knnc-count votes label)))
      (cond ((or (eq best nil) (> count (cadr best))) (list label count)) (t best))))
    nil votes)))
(define (knn-classify examples query neighbors)
  (knnc-majority (map (lambda (entry) (cadr entry))
    (knnc-prefix (sort (lambda (a b) (< (car a) (car b)))
      (map (lambda (example) (list (knnc-distance2 (car example) query) (cadr example))) examples))
      neighbors))))
(define (knnc-prefix values count)
  (cond ((= count 0) nil) (t (cons (car values) (knnc-prefix (cdr values) (- count 1))))))
(knn-classify '(((0 0) red) ((1 0) red) ((5 5) blue) ((6 5) blue)) '(1 1) 3) ; => red
(knn-classify '(((0 0) red) ((1 0) red) ((5 5) blue) ((6 5) blue)) '(5 4) 3) ; => blue


; ---- checks ----
(check "knn-classifier: (1 1) is red"
       (eq (knn-classify '(((0 0) red) ((1 0) red) ((5 5) blue) ((6 5) blue)) '(1 1) 3)
           'red))

(check "knn-classifier: (5 4) is blue"
       (eq (knn-classify '(((0 0) red) ((1 0) red) ((5 5) blue) ((6 5) blue)) '(5 4) 3)
           'blue))
