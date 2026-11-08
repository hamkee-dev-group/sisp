; corpus-transformer-report.lsp — report the top-level-scheduled sample.

(define *corpus-generated-text*
  (llm-decode *corpus-generated-ids* *corpus-vocabulary*))
(print (list (quote generated) *corpus-generated-text*))

(llm-reset-tests)
(llm-check "training-ran" (> *corpus-step* 0))
(llm-check "training-lowered-loss"
  (< *corpus-final-loss* *corpus-initial-loss*))
(llm-check "generated-length"
  (= (length *corpus-generated-text*) (+ 24 *corpus-generation-count*)))
(llm-test-summary)
(print "=== done ===")
