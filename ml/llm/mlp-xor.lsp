; mlp-xor.lsp — setup for a shared-library 2-4-1 XOR MLP.
; Use ml/llm/run-xor.sh.

(seed 1)
(define *xor-layer1* (llm-make-linear 2 4 1.0))
(define *xor-layer2* (llm-make-linear 4 1 1.0))
(define *xor-parameters*
  (append (llm-linear-parameters *xor-layer1*)
          (llm-linear-parameters *xor-layer2*)))
(define *xor-data*
  (list (list (list 0.0 0.0) 0.0)
        (list (list 0.0 1.0) 1.0)
        (list (list 1.0 0.0) 1.0)
        (list (list 1.0 1.0) 0.0)))

(define (xor-forward numbers)
  (car (llm-linear-forward *xor-layer2*
    (llm-vector-tanh
      (llm-linear-forward *xor-layer1* (map val numbers))))))

(define (xor-example-loss example)
  (gsquare (g- (xor-forward (car example)) (val (cadr example)))))
(define (xor-loss examples)
  (sum-vals (map xor-example-loss examples)))

(define *xor-step* 0)
(define *xor-initial-loss* (vdata (xor-loss *xor-data*)))
(define *xor-last-loss* *xor-initial-loss*)
(define (xor-training-step)
  (llm-zero-gradients *xor-parameters*)
  (let ((loss (xor-loss *xor-data*)))
    (setq *xor-last-loss* (vdata loss))
    (backward loss)
    (llm-sgd-step *xor-parameters* 0.1 10.0)
    (setq *xor-step* (+ *xor-step* 1))
    (cond ((= (mod *xor-step* 50) 0)
            (print (list (quote step) *xor-step* (quote loss) *xor-last-loss*)))
          (t nil))
    *xor-last-loss*))

(print "=== training a shared-library 2-4-1 MLP on XOR ===")
(print (list (quote parameters) (length *xor-parameters*)
             (quote initial-loss) *xor-initial-loss*))

