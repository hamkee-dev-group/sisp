; native-posttrain-main.lsp — complete SISP-side SFT, DPO, and REINFORCE run.

(define *posttrain-mode* *native-posttrain-mode*)
(define *posttrain-mode-name*
  (cond ((eq *native-posttrain-mode* (quote sft)) "sft")
        ((eq *native-posttrain-mode* (quote dpo)) "dpo")
        (t "reinforce")))
(define *posttrain-learning-rate* *native-posttrain-learning-rate*)
(setq *matrix-checkpoint-output* *native-posttrain-output*)

(define (native-posttrain-score-all)
  (cond ((posttrain-session-done-p) *posttrain-score*)
        (t (posttrain-session-score-next) (native-posttrain-score-all))))

(define (native-posttrain-accumulate-all scale)
  (cond ((posttrain-session-done-p) nil)
        (t (posttrain-session-accumulate-next scale)
           (native-posttrain-accumulate-all scale))))

(define (native-posttrain-read-reward)
  (cond (*native-posttrain-generate-p*
    (let ((forms (read-from-string
      (read-file *native-posttrain-reward-path*))))
      (matrix-checkpoint-require (= (length forms) 1)
        "native reward file must contain one number")
      (car forms)))
    (t *native-posttrain-reward*)))

(define (native-posttrain-sft)
  (posttrain-session-begin)
  (posttrain-session-load *native-posttrain-prompt-path*
    *native-posttrain-chosen-path*)
  (native-posttrain-accumulate-all 1.0)
  (posttrain-session-apply
    (/ *posttrain-loss-total* *posttrain-loss-windows*)))

(define (native-posttrain-dpo)
  (posttrain-session-load *native-posttrain-prompt-path*
    *native-posttrain-chosen-path*)
  (native-posttrain-score-all)
  (let ((reference-chosen *posttrain-score*)
        (chosen-count *posttrain-score-count*))
    (posttrain-session-load *native-posttrain-prompt-path*
      *native-posttrain-rejected-path*)
    (native-posttrain-score-all)
    (let ((reference-rejected *posttrain-score*)
          (rejected-count *posttrain-score-count*))
      (let ((statistics
        (posttrain-dpo-statistics reference-chosen reference-rejected
          reference-chosen reference-rejected *native-posttrain-beta*
          chosen-count rejected-count)))
        (posttrain-session-begin)
        (posttrain-session-load *native-posttrain-prompt-path*
          *native-posttrain-chosen-path*)
        (native-posttrain-accumulate-all (nth 2 statistics))
        (posttrain-session-load *native-posttrain-prompt-path*
          *native-posttrain-rejected-path*)
        (native-posttrain-accumulate-all (nth 3 statistics))
        (posttrain-session-apply (nth 0 statistics))))))

(define (native-posttrain-reinforce)
  (let ((reward (native-posttrain-read-reward)))
    (let ((count (length (read-file *native-posttrain-chosen-path*))))
      (let ((statistics
        (posttrain-reinforce-statistics reward *native-posttrain-baseline*
          *native-posttrain-baseline-rate* count)))
        (posttrain-session-begin)
        (posttrain-session-load *native-posttrain-prompt-path*
          *native-posttrain-chosen-path*)
        (native-posttrain-accumulate-all (nth 1 statistics))
        (let ((objective (* (nth 0 statistics) *posttrain-loss-total*)))
          (posttrain-session-apply objective)
          (print (list (quote native-reinforce) (quote reward) reward
            (quote advantage) (nth 0 statistics)
            (quote baseline) (nth 2 statistics)))
          objective)))))

(define *native-posttrain-objective*
  (cond ((eq *native-posttrain-mode* (quote sft)) (native-posttrain-sft))
        ((eq *native-posttrain-mode* (quote dpo)) (native-posttrain-dpo))
        (t (native-posttrain-reinforce))))
(posttrain-session-write-metrics)
