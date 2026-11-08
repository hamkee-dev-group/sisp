(define *bigram-final-loss* (bigram-evaluate))
(define *bigram-after-b*
  (llm-softmax-numbers (llm-vector-data (bigram-logits 1))))
(print (list (quote final-mean-nll) *bigram-final-loss*
             (quote perplexity) (exp *bigram-final-loss*)))
(print (list (quote after-b) *bigram-after-b*
             (quote note) "c and d remain ambiguous without context"))
(seed 31)
(define *bigram-generated-ids* (bigram-generate (list 0) 0 32 0.5))
(define *bigram-generated-text*
  (llm-decode *bigram-generated-ids* *bigram-vocabulary*))
(print (list (quote generated) *bigram-generated-text*))

(llm-reset-tests)
(llm-check "bigram-loss-decreased" (< *bigram-final-loss* *bigram-initial-loss*))
(llm-check "bigram-learns-a-to-b" (= (llm-argmax
  (llm-softmax-numbers (llm-vector-data (bigram-logits 0)))) 1))
(llm-check "bigram-b-is-context-ambiguous"
  (< (abs (- (nth 2 *bigram-after-b*) (nth 3 *bigram-after-b*))) 0.2))
(llm-check "bigram-generated-length" (= (length *bigram-generated-text*) 33))
(llm-test-summary)
(print "=== done ===")

