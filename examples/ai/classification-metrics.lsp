; Confusion counts derive accuracy, precision, recall, and F1 for binary classifiers.
(define (cm-count actual predicted wanted-actual wanted-predicted)
  (foldl (lambda (count index)
    (+ count (cond ((and (= (nth index actual) wanted-actual)
                         (= (nth index predicted) wanted-predicted)) 1) (t 0))))
    0 (cm-indexes (length actual) 0)))
(define (cm-indexes count index)
  (cond ((= index count) nil) (t (cons index (cm-indexes count (+ index 1))))))
(define (confusion-metrics actual predicted)
  (let ((tp (cm-count actual predicted 1 1)) (tn (cm-count actual predicted 0 0))
        (fp (cm-count actual predicted 0 1)) (fn (cm-count actual predicted 1 0)))
    (let ((precision (/ tp (+ tp fp))) (recall (/ tp (+ tp fn))))
      (list (list 'tp tp) (list 'tn tn) (list 'fp fp) (list 'fn fn)
        (list 'accuracy (/ (+ tp tn) (+ tp tn fp fn)))
        (list 'precision precision) (list 'recall recall)
        (list 'f1 (/ (* 2 precision recall) (+ precision recall)))))))
(confusion-metrics '(1 1 0 0) '(1 0 1 0)) ; => ((tp 1) (tn 1) (fp 1) (fn 1) (accuracy 1/2) (precision 1/2) (recall 1/2) (f1 1/2))
(cadr (find (lambda (entry) (eq (car entry) 'accuracy)) (confusion-metrics '(1 0 1) '(1 0 1)))) ; => 1

; ---- checks ----
(check "classification-metrics: 1-1-0-0 vs 1-0-1-0 exact values"
       (equal (confusion-metrics '(1 1 0 0) '(1 0 1 0))
              '((tp 1) (tn 1) (fp 1) (fn 1) (accuracy 1/2)
                (precision 1/2) (recall 1/2) (f1 1/2))))

(check "classification-metrics: perfect prediction accuracy = 1"
       (= (cadr (find (lambda (entry) (eq (car entry) 'accuracy))
                      (confusion-metrics '(1 0 1) '(1 0 1))))
          1))
