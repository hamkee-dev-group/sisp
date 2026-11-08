; posttrain-session.lsp — top-level state machine for native transformer updates.
;
; The shell runner repeats the small `*-next` forms at top level.  Completed
; activation caches can therefore be collected between windows while model
; parameters, optimizer moments, and scalar session state remain resident.

(define *posttrain-prompt-ids* nil)
(define *posttrain-completion-ids* nil)
(define *posttrain-offset* 0)
(define *posttrain-score* 0.0)
(define *posttrain-score-count* 0)
(define *posttrain-loss-total* 0.0)
(define *posttrain-loss-windows* 0)
(define *posttrain-objective-count* 0)
(define *posttrain-last-objective* 0.0)
(define *posttrain-last-gradient-norm* 0.0)
(define *posttrain-padding-token* nil)

(define (posttrain-session-load prompt-path completion-path)
  (setq *posttrain-padding-token*
    (llm-char-id (car (str-to-list " ")) *matrix-vocabulary*))
  (setq *posttrain-prompt-ids*
    (llm-encode (read-file prompt-path) *matrix-vocabulary*))
  (setq *posttrain-completion-ids*
    (llm-encode (read-file completion-path) *matrix-vocabulary*))
  (matrix-checkpoint-require (> (length *posttrain-prompt-ids*) 0)
    "native posttraining prompt is empty")
  (matrix-checkpoint-require (> (length *posttrain-completion-ids*) 0)
    "native posttraining completion is empty")
  (setq *posttrain-offset* 0)
  (setq *posttrain-score* 0.0)
  (setq *posttrain-score-count* 0)
  (setq *posttrain-loss-total* 0.0)
  (setq *posttrain-loss-windows* 0)
  (setq *posttrain-objective-count* (length *posttrain-completion-ids*))
  nil)

(define (posttrain-session-done-p)
  (>= *posttrain-offset* (length *posttrain-completion-ids*)))

(define (posttrain-session-score-next)
  (cond ((posttrain-session-done-p) nil)
        (t (let ((window
          (posttrain-completion-window *posttrain-prompt-ids*
            *posttrain-completion-ids* *posttrain-offset* *matrix-context*
            *posttrain-padding-token*)))
          (let ((count (nth 4 window)))
            (setq *posttrain-score*
              (+ *posttrain-score*
                (posttrain-window-log-probability *matrix-model*
                  (nth 0 window) (nth 1 window) (nth 2 window) (nth 3 window))))
            (setq *posttrain-score-count* (+ *posttrain-score-count* count))
            (setq *posttrain-offset* (nth 5 window))
            nil)))))

; total-scale multiplies the mean NLL over the entire completion.  Each window
; has its own mean gradient, so weight it by its fraction of answer characters.
(define (posttrain-session-accumulate-next total-scale)
  (cond ((posttrain-session-done-p) nil)
        (t (let ((window
          (posttrain-completion-window *posttrain-prompt-ids*
            *posttrain-completion-ids* *posttrain-offset* *matrix-context*
            *posttrain-padding-token*)))
          (let ((count (nth 4 window)))
            (let ((result
              (posttrain-window-accumulate *matrix-model*
                (nth 0 window) (nth 1 window) (nth 2 window) (nth 3 window)
                (* total-scale (/ count *posttrain-objective-count*)))))
              (setq *posttrain-loss-total*
                (+ *posttrain-loss-total* (* (car result) count)))
              (setq *posttrain-loss-windows* (+ *posttrain-loss-windows* count))
              (setq *posttrain-offset* (nth 5 window))
              nil))))))

(define (posttrain-session-begin)
  (mparams-zero *matrix-parameters*)
  (setq *posttrain-last-objective* 0.0)
  nil)

(define (posttrain-session-apply objective)
  (setq *posttrain-last-gradient-norm*
    (mparams-clip *matrix-parameters* *matrix-clip-norm*))
  (mparams-adamw-step *matrix-parameters* (+ *matrix-step* 1)
    *posttrain-learning-rate* 0.9 *matrix-beta2* 0.00000001
    *matrix-weight-decay*)
  (matrix-checkpoint-require (mparams-finite *matrix-parameters*)
    "native posttraining produced non-finite parameters")
  (setq *matrix-step* (+ *matrix-step* 1))
  (setq *matrix-current-learning-rate* *posttrain-learning-rate*)
  (setq *matrix-last-gradient-norm* *posttrain-last-gradient-norm*)
  (setq *posttrain-last-objective* objective)
  (print (list (quote native-posttrain-step) *matrix-step*
    (quote mode) *posttrain-mode*
    (quote objective) objective
    (quote gradient-norm) *posttrain-last-gradient-norm*
    (quote learning-rate) *posttrain-learning-rate*))
  objective)

(define (posttrain-session-write-metrics)
  (write-file (cat *matrix-checkpoint-output* "/metrics.txt")
    (list-to-str (list "step " (num-to-str *matrix-step*)
      " mode " *posttrain-mode-name*
      " objective " (num-to-str *posttrain-last-objective*)
      " gradient-norm " (num-to-str *posttrain-last-gradient-norm*)
      " learning-rate " (num-to-str *posttrain-learning-rate*))))
  nil)
