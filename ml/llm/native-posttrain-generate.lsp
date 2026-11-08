; Generate the configured completion using the already loaded native model.

(setq *functional-temperature* *native-posttrain-temperature*)
(setq *functional-top-k* *native-posttrain-top-k*)
(setq *functional-top-p* *native-posttrain-top-p*)
(define *native-posttrain-generation-prompt*
  (llm-encode (read-file *native-posttrain-prompt-path*) *matrix-vocabulary*))
(matrix-checkpoint-require (> (length *native-posttrain-generation-prompt*) 0)
  "native posttraining generation prompt is empty")
(define *native-posttrain-generation-result*
  (functional-generate-walk *native-posttrain-generation-prompt*
    *native-posttrain-generation-count*))
(write-file *native-posttrain-chosen-path*
  (llm-decode
    (drop *native-posttrain-generation-result*
      (length *native-posttrain-generation-prompt*))
    *matrix-vocabulary*))
(print (list (quote native-posttrain-generated)
  *native-posttrain-chosen-path*
  (quote characters) *native-posttrain-generation-count*))
