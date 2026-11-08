; Export only host operations that SISP cannot perform: enumerate a directory,
; create an output directory, and isolate execution of untrusted generated code.

(matrix-checkpoint-require
  (or (eq *native-posttrain-mode* (quote sft))
      (eq *native-posttrain-mode* (quote dpo))
      (eq *native-posttrain-mode* (quote reinforce)))
  "native posttraining mode must be sft, dpo, or reinforce")
(matrix-checkpoint-require
  (and (> *native-posttrain-learning-rate* 0.0)
       (> *native-posttrain-generation-count* 0)
       (> *native-posttrain-temperature* 0.0)
       (> *native-posttrain-top-k* 0)
       (> *native-posttrain-top-p* 0.0)
       (<= *native-posttrain-top-p* 1.0))
  "native posttraining numeric configuration is invalid")

(define *native-posttrain-resolved-mode-name*
  (cond ((eq *native-posttrain-mode* (quote sft)) "sft")
        ((eq *native-posttrain-mode* (quote dpo)) "dpo")
        (t "reinforce")))

(write-file *native-posttrain-plan-path*
  (list-to-str (list
    *native-posttrain-checkpoint* "|"
    *native-posttrain-output* "|"
    *native-posttrain-resolved-mode-name* "|"
    (cond (*native-posttrain-generate-p* "generate") (t "fixed")) "|"
    *native-posttrain-prompt-path* "|"
    *native-posttrain-chosen-path* "|"
    *native-posttrain-rejected-path* "|"
    *native-posttrain-check-path* "|"
    *native-posttrain-reward-path* "|"
    (num-to-str *native-posttrain-pass-reward*) "|"
    (num-to-str *native-posttrain-failure-reward*))))
