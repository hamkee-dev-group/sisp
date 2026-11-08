; native-posttrain-config.lsp — the complete user-facing posttraining setup.
;
; Edit this file, then run ml/llm/run-native-posttrain.sh.  SISP itself
; consumes every model, objective, sampling, reward, and path setting below.

(define *native-posttrain-mode* (quote reinforce)) ; sft, dpo, or reinforce

(define *native-posttrain-checkpoint*
  "ml/llm/checkpoints/knowledge-best")
(define *native-posttrain-output*
  "ml/llm/checkpoints/native-posttrain-run")

(define *native-posttrain-prompt-path*
  "ml/llm/eval/list-sum.prompt")
(define *native-posttrain-chosen-path*
  "/tmp/sisp-native-posttrain-candidate.lsp")
(define *native-posttrain-rejected-path*
  "ml/llm/eval/list-sum.rejected")

; In reinforce mode SISP generates chosen-path, another isolated SISP process
; executes it with check-path, and main.lsp reads the resulting numeric reward.
(define *native-posttrain-generate-p* t)
(define *native-posttrain-generation-count* 32)
(define *native-posttrain-check-path*
  "ml/llm/eval/list-sum.check.lsp")
(define *native-posttrain-reward-path*
  "/tmp/sisp-native-posttrain-reward.txt")
(define *native-posttrain-reward* -0.1)
(define *native-posttrain-pass-reward* 1.0)
(define *native-posttrain-failure-reward* -0.1)

(define *native-posttrain-temperature* 0.8)
(define *native-posttrain-top-k* 20)
(define *native-posttrain-top-p* 0.95)
(define *native-posttrain-learning-rate* 0.00002)
(define *native-posttrain-beta* 0.1)
(define *native-posttrain-baseline* 0.0)
(define *native-posttrain-baseline-rate* 0.1)
