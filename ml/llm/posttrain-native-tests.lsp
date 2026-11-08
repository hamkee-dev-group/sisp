; posttrain-native-tests.lsp — an actual optimizer update through the SISP model.

(llm-reset-tests)
(print "=== native transformer posttraining update ===")

(define *matrix-vocabulary* (str-to-list " abc"))
(define *matrix-context* 4)
(define *matrix-model* (mtransformer-make 4 4 4 2 1 8 0.1))
(define *matrix-parameters* (mtransformer-parameters *matrix-model*))
(define *matrix-step* 0)
(define *matrix-clip-norm* 1.0)
(define *matrix-beta2* 0.95)
(define *matrix-weight-decay* 0.0)
(define *matrix-current-learning-rate* 0.0)
(define *matrix-last-gradient-norm* 0.0)
(define *posttrain-learning-rate* 0.001)
(define *posttrain-mode* (quote sft))
(define *posttrain-mode-name* "sft")
(define *matrix-checkpoint-output* "/tmp")

(write-file "/tmp/sisp-posttrain-native-prompt.txt" "ab")
(write-file "/tmp/sisp-posttrain-native-target.txt" "c")
(define pt-native-before
  (mat-ref (mp-data (mtransformer-token-embedding *matrix-model*)) 3 0))
(posttrain-session-begin)
(posttrain-session-load "/tmp/sisp-posttrain-native-prompt.txt"
  "/tmp/sisp-posttrain-native-target.txt")
(posttrain-session-accumulate-next 1.0)
(define pt-native-objective
  (/ *posttrain-loss-total* *posttrain-loss-windows*))
(posttrain-session-apply pt-native-objective)
(define pt-native-after
  (mat-ref (mp-data (mtransformer-token-embedding *matrix-model*)) 3 0))

(llm-check "posttrain-native-objective-finite"
  (matrix-finite-number pt-native-objective))
(llm-check "posttrain-native-gradient-nonzero"
  (> *posttrain-last-gradient-norm* 0.0))
(llm-check "posttrain-native-parameter-updated"
  (/= pt-native-before pt-native-after))
(llm-check "posttrain-native-step-advanced" (= *matrix-step* 1))
(llm-check "posttrain-native-completion-covered"
  (= *posttrain-offset* (length *posttrain-completion-ids*)))

(llm-test-summary)
