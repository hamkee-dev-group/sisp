; Final metrics after top-level-scheduled, corpus-wide distributed evaluation.

(matrix-checkpoint-require
  (and (matrix-finite-number *matrix-final-training-loss*)
       (matrix-finite-number *matrix-final-loss*))
  "refusing to finalize non-finite evaluation metrics")
(print (list (quote final-training-nll) *matrix-final-training-loss*
  (quote final-validation-nll) *matrix-final-loss*
  (quote final-validation-bits-per-byte) (/ *matrix-final-loss* (log 2.0))
  (quote final-validation-perplexity) (exp *matrix-final-loss*)
  (quote characters-processed) *matrix-characters-processed*))
(cond ((> (length *matrix-checkpoint-output*) 0)
  (write-file (cat *matrix-checkpoint-output* "/metrics.txt")
    (list-to-str (list
      "step " (num-to-str *matrix-step*)
      " validation-nll " (num-to-str *matrix-final-loss*)
      " training-nll " (num-to-str *matrix-final-training-loss*)
      " characters " (num-to-str *matrix-characters-processed*)))))
  (t nil))
(print (list (quote runtime-seconds) (runtime)))
