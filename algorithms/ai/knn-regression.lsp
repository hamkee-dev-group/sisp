; ============================================================
; name: knn-regression
; file: algorithms/ai/knn-regression.lsp
; subject: ai
; capability: regress supervised
; aliases: "knn regression" "k-nearest-neighbors regression"
; signature: (knn-regress examples query k) -> value
; complexity: O(n d) per query
; see-also: knn-classifier
; derived-from: none (standard k-NN averaging)
; status: example (tested)
; checks: 2 - averaged-neighbor prediction
; ============================================================
; k-nearest-neighbour regression averages the targets of nearby observations.
(define (knnr-distance2 left right)
  (cond ((eq left nil) 0)
        (t (+ (^ (- (car left) (car right)) 2) (knnr-distance2 (cdr left) (cdr right))))))
(define (knnr-prefix values count)
  (cond ((= count 0) nil) (t (cons (car values) (knnr-prefix (cdr values) (- count 1))))))
(define (knn-regress examples query neighbors)
  (let ((near (knnr-prefix (sort (lambda (a b) (< (car a) (car b)))
                (map (lambda (example) (list (knnr-distance2 (car example) query)
                                               (cadr example))) examples)) neighbors)))
    (/ (foldl (lambda (sum entry) (+ sum (cadr entry))) 0 near) (length near))))
(knn-regress '(((0) 0) ((1) 2) ((2) 4) ((10) 20)) '(1) 3) ; => 2
(knn-regress '(((0 0) 1) ((0 1) 3) ((9 9) 100)) '(0 0) 2) ; => 2


; ---- checks ----
(check "knn-regression: 1d neighbors average to 2"
       (= (knn-regress '(((0) 0) ((1) 2) ((2) 4) ((10) 20)) '(1) 3) 2))

(check "knn-regression: 2d near neighbors average to 2"
       (= (knn-regress '(((0 0) 1) ((0 1) 3) ((9 9) 100)) '(0 0) 2) 2))
