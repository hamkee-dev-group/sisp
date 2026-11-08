; Initial metrics are evaluated after an optional checkpoint has been restored.

(print "=== native matrix causal transformer ===")
(print (list (quote profile) *matrix-profile*
  (quote parameters) (mparams-count *matrix-parameters*)
  (quote context) *matrix-context*
  (quote dimension) *matrix-dimension*
  (quote heads) *matrix-heads*
  (quote layers) *matrix-layers*
  (quote hidden) *matrix-hidden*
  (quote starting-step) *matrix-step*
  (quote vocabulary) (length *matrix-vocabulary*)
  (quote microbatch) *matrix-microbatch*
  (quote sampling) *matrix-sampling-mode*
  (quote evaluation-sequences) *matrix-evaluation-count*
  (quote train-bytes) (length *matrix-source*)
  (quote validation-bytes) (length *matrix-validation-source*)))
(print (list (quote unit-aware-training) (/= *matrix-training-window-count* 0)
  (quote unit-aware-validation) (/= *matrix-validation-window-count* 0)
  (quote train-valid-windows) *matrix-training-window-count*
  (quote validation-valid-windows) *matrix-validation-window-count*))
(define *matrix-run-start-step* *matrix-step*)
