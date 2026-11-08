; ============================================================
; name: logistic-regression
; file: algorithms/ai/logistic-regression.lsp
; subject: ai
; capability: classify supervised
; aliases: "logistic regression" "sigmoid classifier" "gradient descent"
; signature: (logistic-train examples rate steps model) -> model
;            (lrn-classify model x) -> 0/1
; complexity: O(iters * n * d)
; see-also: perceptron naive-bayes
; derived-from: none (standard batch gradient descent on log-loss)
; status: example (tested)
; checks: 2 - learns a linear boundary
; ============================================================
; Batch gradient descent fits a binary logistic-regression classifier.
(define (lrn-dot weights features)
  (cond ((eq weights nil) 0)
        (t (+ (* (car weights) (car features)) (lrn-dot (cdr weights) (cdr features))))))
(define (lrn-sigmoid value) (/ 1.0 (+ 1.0 (exp (- 0 value)))))
(define (lrn-probability model features)
  (lrn-sigmoid (+ (lrn-dot (car model) features) (cadr model))))
(define (lrn-indexes count index)
  (cond ((= index count) nil) (t (cons index (lrn-indexes count (+ index 1))))))
(define (lrn-gradient examples model index)
  (/ (foldl (lambda (sum example)
    (+ sum (* (- (lrn-probability model (car example)) (cadr example))
              (nth index (car example))))) 0.0 examples) (length examples)))
(define (lrn-bias-gradient examples model)
  (/ (foldl (lambda (sum example)
    (+ sum (- (lrn-probability model (car example)) (cadr example)))) 0.0 examples)
    (length examples)))
; One batch step updates every weight and the independent bias.
(define (lrn-step examples model rate)
  (list (map (lambda (index) (- (nth index (car model))
                 (* rate (lrn-gradient examples model index))))
          (lrn-indexes (length (car model)) 0))
        (- (cadr model) (* rate (lrn-bias-gradient examples model)))))
(define (logistic-train examples rate steps model)
  (cond ((= steps 0) model)
        (t (logistic-train examples rate (- steps 1) (lrn-step examples model rate)))))
(define (lrn-classify model features)
  (cond ((>= (lrn-probability model features) 0.5) 1) (t 0)))
(define lrn-threshold (logistic-train '(((0) 0) ((1) 0) ((2) 1) ((3) 1)) 0.5 80 '((0) 0)))
(map (lambda (x) (lrn-classify lrn-threshold (list x))) '(0 1 2 3)) ; => (0 0 1 1)
(> (lrn-probability lrn-threshold '(3)) (lrn-probability lrn-threshold '(0))) ; => t

; ---- checks ----
(check "logistic-regression: learns the threshold (0 0 1 1)"
       (equal (map (lambda (x) (lrn-classify lrn-threshold (list x))) '(0 1 2 3))
              '(0 0 1 1)))

(check "logistic-regression: P(3) > P(0)"
       (> (lrn-probability lrn-threshold '(3)) (lrn-probability lrn-threshold '(0))))
