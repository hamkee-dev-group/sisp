; transformer-report.lsp — metrics and generation after transformer training.

(define *tiny-final-loss* (llm-evaluate *tiny-model* *tiny-windows*))
(print (list (quote final-mean-nll) *tiny-final-loss*
             (quote final-perplexity) (exp *tiny-final-loss*)))
(print (list (quote runtime-seconds) (runtime)))

; These contexts have the same suffix "ab" but different correct next tokens.
; A bigram cannot distinguish them; the causal transformer can.
(define *tiny-cab-probabilities*
  (llm-softmax-numbers (llm-vector-data
    (llm-model-logits *tiny-model*
      (llm-encode "cab" *tiny-vocabulary*)))))
(define *tiny-dab-probabilities*
  (llm-softmax-numbers (llm-vector-data
    (llm-model-logits *tiny-model*
      (llm-encode "dab" *tiny-vocabulary*)))))
(print (list (quote context-cab-next-d) *tiny-cab-probabilities*))
(print (list (quote context-dab-next-c) *tiny-dab-probabilities*))

(seed 23)
(define *tiny-seed* (llm-encode "abc" *tiny-vocabulary*))
(define *tiny-generated-ids*
  (llm-model-generate *tiny-model* *tiny-seed* 30 0.35))
(define *tiny-generated-text*
  (llm-decode *tiny-generated-ids* *tiny-vocabulary*))
(print (list (quote generated) *tiny-generated-text*))

(llm-reset-tests)
(llm-check "training-lowered-loss" (< *tiny-final-loss* *tiny-initial-loss*))
(llm-check "training-substantially-lowered-loss"
  (< *tiny-final-loss* (* 0.75 *tiny-initial-loss*)))
(llm-check "parameter-count" (= (llm-parameter-count *tiny-parameters*) 130))
(llm-check "cab-predicts-d" (= (llm-argmax *tiny-cab-probabilities*) 3))
(llm-check "dab-predicts-c" (= (llm-argmax *tiny-dab-probabilities*) 2))
(llm-check "generated-length" (= (length *tiny-generated-text*) 33))
(llm-test-summary)
(print "=== done ===")

