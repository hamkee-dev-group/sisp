; Replace validation data only after strict checkpoint restoration has verified
; the frozen training/validation contract.  Test data is never checkpointed.

(setq *matrix-validation-source* (read-file *matrix-test-corpus-path*))
(setq *matrix-validation-spans* *matrix-test-spans*)
(setq *matrix-validation-window-count*
  (matrix-span-prepare *matrix-validation-spans* *matrix-validation-source*))
(matrix-checkpoint-require
  (> (length *matrix-validation-source*) *matrix-context*)
  "test corpus must be longer than context")
(print (list (quote final-test-corpus)
  (quote bytes) (length *matrix-validation-source*)
  (quote checksum) *matrix-test-corpus-checksum*))
