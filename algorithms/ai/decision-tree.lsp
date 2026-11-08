; ============================================================
; name: decision-tree
; file: algorithms/ai/decision-tree.lsp
; subject: ai
; capability: classify supervised
; aliases: "decision tree" "id3" "information gain"
; signature: (dt-build rows features) -> tree ; (dt-classify tree row) -> label
; complexity: O(features * rows * log rows)
; see-also: naive-bayes
; derived-from: Quinlan 1986 (ID3)
; status: example (tested)
; checks: 3 - learned tree classifies its training rows; root splits on feature 0
; ============================================================
; ID3 builds a categorical decision tree by maximizing information gain.
(define (dt-unique values)
  (foldl (lambda (result value) (cond ((in value result) result)
    (t (append result (list value))))) nil values))
(define (dt-labels rows) (map (lambda (row) (cadr row)) rows))
(define (dt-count values target)
  (length (filter (lambda (value) (equal value target)) values)))
(define (dt-entropy rows)
  (let ((labels (dt-labels rows)))
    (foldl (lambda (total label)
      (let ((probability (/ (dt-count labels label) (length labels))))
        (- total (* probability (log2 probability)))))
      0.0 (dt-unique labels))))
; Information gain is parent entropy minus the weighted child entropies.
(define (dt-values rows feature)
  (dt-unique (map (lambda (row) (nth feature (car row))) rows)))
(define (dt-subset rows feature value)
  (filter (lambda (row) (equal (nth feature (car row)) value)) rows))
(define (dt-gain rows feature)
  (- (dt-entropy rows)
     (foldl (lambda (remainder value)
       (let ((part (dt-subset rows feature value)))
         (+ remainder (* (/ (length part) (length rows)) (dt-entropy part)))))
       0.0 (dt-values rows feature))))
(define (dt-best-feature rows features)
  (car (foldl (lambda (best feature)
    (let ((candidate (list feature (dt-gain rows feature))))
      (cond ((or (eq best nil) (> (cadr candidate) (cadr best))) candidate) (t best))))
    nil features)))
; Exhausted features fall back to the majority class at that node.
(define (dt-majority rows)
  (car (foldl (lambda (best label)
    (let ((candidate (list label (dt-count (dt-labels rows) label))))
      (cond ((or (eq best nil) (> (cadr candidate) (cadr best))) candidate) (t best))))
    nil (dt-unique (dt-labels rows)))))
; Branches retain original feature indexes, making the learned tree directly executable.
(define (dt-build rows features)
  (let ((labels (dt-unique (dt-labels rows))))
    (cond ((= (length labels) 1) (list 'leaf (car labels)))
          ((eq features nil) (list 'leaf (dt-majority rows)))
          (t (let ((feature (dt-best-feature rows features)))
            (list 'node feature
              (map (lambda (value) (list value
                (dt-build (dt-subset rows feature value)
                  (remove (lambda (item) (= item feature)) features))))
                (dt-values rows feature))))))))
(define (dt-classify tree features)
  (cond ((eq (car tree) 'leaf) (cadr tree))
        (t (let ((branch (find (lambda (entry)
          (equal (car entry) (nth (cadr tree) features))) (caddr tree))))
          (dt-classify (cadr branch) features)))))
(define dt-weather (dt-build '(((sunny hot) no) ((sunny cool) no) ((rainy hot) yes) ((rainy cool) yes)) '(0 1)))
(dt-classify dt-weather '(rainy cool)) ; => yes
(dt-classify dt-weather '(sunny hot)) ; => no

; ---- checks ----
(check "decision-tree: rainy cool -> yes"
       (eq (dt-classify dt-weather '(rainy cool)) 'yes))

(check "decision-tree: sunny hot -> no"
       (eq (dt-classify dt-weather '(sunny hot)) 'no))

(check "decision-tree: root splits on feature 0"
       (= (cadr dt-weather) 0))
