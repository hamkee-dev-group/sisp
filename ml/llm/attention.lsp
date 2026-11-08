; attention.lsp — stable scaled dot-product self-attention, forward-only.
; Run: src/sisp ml/llm/attention.lsp

(load 'ml/llm/engine)
(load 'ml/llm/math)
(load 'ml/llm/tensor)
(load 'ml/llm/metrics)
(llm-reset-tests)
(seed 7)

(define (plain-scale-matrix scalar matrix)
  (map (lambda (row) (map (lambda (x) (* x scalar)) row)) matrix))
(define (plain-softmax-rows matrix) (map llm-softmax-numbers matrix))
(define (plain-random-matrix rows cols)
  (map (lambda (i)
    (map (lambda (j) (- (* 2.0 (random)) 1.0)) (seq 1 cols)))
    (seq 1 rows)))

(define X (list (list 1.0 0.0 1.0 0.0)
                (list 0.0 2.0 0.0 1.0)
                (list 1.0 1.0 0.0 0.0)))
(define attention-dimension 4)
(define Wq (plain-random-matrix attention-dimension attention-dimension))
(define Wk (plain-random-matrix attention-dimension attention-dimension))
(define Wv (plain-random-matrix attention-dimension attention-dimension))

(define Q (llm-matmul-numbers X Wq))
(define K (llm-matmul-numbers X Wk))
(define V (llm-matmul-numbers X Wv))
(define scores
  (plain-scale-matrix (/ 1.0 (sqrt attention-dimension))
    (llm-matmul-numbers Q (llm-transpose K))))
(define A (plain-softmax-rows scores))
(define OUT (llm-matmul-numbers A V))

(print "=== stable self-attention forward (3 tokens, dimension 4) ===")
(print (list (quote attention-weights) A))
(print (list (quote output) OUT))

(foreach (lambda (row)
  (llm-check-close "attention-row-sums-to-one"
    (llm-sum-numbers row) 1.0 0.000000000001 0.000000000001)) A)
(llm-check "attention-shape"
  (and (= (length OUT) 3) (= (length (car OUT)) 4)))

; This would overflow a naive exp(logit) implementation.
(define extreme-softmax (llm-softmax-numbers (list 1000.0 1001.0 999.0)))
(llm-check-close "extreme-softmax-stable"
  (llm-sum-numbers extreme-softmax) 1.0 0.000000000001 0.000000000001)
(llm-check "extreme-softmax-argmax" (= (llm-argmax extreme-softmax) 1))

(llm-test-summary)
(print "=== done ===")
