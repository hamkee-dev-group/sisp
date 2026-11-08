(print (list (quote final-test-nll) *matrix-final-loss*
  (quote final-test-bits-per-byte) (/ *matrix-final-loss* (log 2.0))
  (quote final-test-perplexity) (exp *matrix-final-loss*)
  (quote sequences) *matrix-evaluation-count*))
