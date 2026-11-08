; optim.lsp — parameter and SGD helpers.

(define (llm-zero-gradients parameters)
  (foreach (lambda (parameter) (set-grad parameter 0.0)) parameters))

(define (llm-gradient-squared-sum parameters)
  (foldl (lambda (acc parameter)
           (+ acc (* (vgrad parameter) (vgrad parameter))))
         0.0 parameters))

(define (llm-gradient-norm parameters)
  (sqrt (llm-gradient-squared-sum parameters)))

(define (llm-sgd-step parameters learning-rate max-gradient-norm)
  (let ((norm (llm-gradient-norm parameters)))
    (let ((clip-scale
      (cond ((> norm max-gradient-norm)
              (/ max-gradient-norm (+ norm 0.000000000001)))
            (t 1.0))))
      (foreach (lambda (parameter)
        (set-data parameter
          (- (vdata parameter)
             (* learning-rate clip-scale (vgrad parameter))))) parameters)
      norm)))

(define (llm-parameter-count parameters)
  (foldl (lambda (count parameter) (+ count 1)) 0 parameters))
