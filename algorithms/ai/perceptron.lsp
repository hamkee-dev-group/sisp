; ============================================================
; name: perceptron
; file: algorithms/ai/perceptron.lsp
; subject: ai
; capability: classify supervised
; aliases: "perceptron" "linear classifier" "rosenblatt"
; signature: (perceptron-train examples rate epochs model) -> model
;            (pc-predict model features) -> 1/-1
; complexity: O(iters * n * d)
; see-also: logistic-regression
; derived-from: Rosenblatt 1958
; status: example (tested)
; checks: 2 - learns AND; separable convergence
; ============================================================
; Binary perceptron training learns a linear separator from labeled vectors.
(define (pc-dot left right)
  (cond ((eq left nil) 0)
        (t (+ (* (car left) (car right)) (pc-dot (cdr left) (cdr right))))))
(define (pc-predict model features)
  (cond ((>= (+ (pc-dot (car model) features) (cadr model)) 0) 1) (t -1)))
(define (pc-update weights features amount)
  (map (lambda (pair) (+ (car pair) (* amount (cadr pair))))
    (map (lambda (index) (list (nth index weights) (nth index features)))
      (pc-indexes (length weights) 0))))
(define (pc-indexes count index)
  (cond ((= index count) nil) (t (cons index (pc-indexes count (+ index 1))))))
(define (pc-example model example rate)
  (let ((error (- (cadr example) (pc-predict model (car example)))))
    (cond ((= error 0) model)
          (t (list (pc-update (car model) (car example) (* rate (cadr example)))
               (+ (cadr model) (* rate (cadr example))))))))
(define (pc-epoch model examples rate)
  (foldl (lambda (current example) (pc-example current example rate)) model examples))
(define (perceptron-train examples rate epochs model)
  (cond ((= epochs 0) model)
        (t (perceptron-train examples rate (- epochs 1)
             (pc-epoch model examples rate)))))
(define pc-and (perceptron-train '(((0 0) -1) ((0 1) -1) ((1 0) -1) ((1 1) 1)) 1 8 '((0 0) 0)))
(map (lambda (x) (pc-predict pc-and x)) '((0 0) (0 1) (1 0) (1 1))) ; => (-1 -1 -1 1)
(pc-predict pc-and '(1 1)) ; => 1


; ---- checks ----
(check "perceptron: learns AND"
       (equal (map (lambda (x) (pc-predict pc-and x)) '((0 0) (0 1) (1 0) (1 1)))
              '(-1 -1 -1 1)))

(check "perceptron: predicts (1 1) as 1"
       (= (pc-predict pc-and '(1 1)) 1))
