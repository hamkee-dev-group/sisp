; posttrain.lsp — completion-only SFT, DPO, and policy-gradient primitives.
;
; This file deliberately builds on the same mtransformer parameters and manual
; gradients as matrix-corpus.lsp.  It does not introduce a second toy model.
; Callers keep forward/backward operations as separate top-level forms so SISP
; can collect completed activation caches between examples.

(define (posttrain-zero-row matrix row column)
  (cond ((= column (mat-cols matrix)) matrix)
        (t (mat-set matrix row column 0.0)
           (posttrain-zero-row matrix row (+ column 1)))))

; Cross-entropy for the half-open row interval [start,end).  Rows outside that
; interval have exactly zero gradient.  This is the key distinction from
; ordinary pretraining: prompt and padding characters condition the model but
; do not become targets.
(define (posttrain-cross-entropy-walk logits probabilities targets
          row start end loss count dlogits)
  (cond ((eq targets nil)
    (list (/ loss count) (* dlogits (/ 1.0 count)) probabilities count))
    (t (let ((target (car targets)))
      (cond ((and (>= row start) (< row end))
        (let ((target-probability (mat-ref probabilities row target)))
          (mat-set dlogits row target (- target-probability 1.0))
          (posttrain-cross-entropy-walk logits probabilities (cdr targets)
            (+ row 1) start end
            (+ loss
              (cond ((> target-probability 0.0)
                (- 0.0 (log target-probability)))
                (t (mtransformer-logsumexp-row-loss logits row target))))
            (+ count 1) dlogits)))
        (t (posttrain-zero-row dlogits row 0)
           (posttrain-cross-entropy-walk logits probabilities (cdr targets)
             (+ row 1) start end loss count dlogits)))))))

(define (posttrain-cross-entropy-forward logits targets start end)
  (let ((rows (mat-rows logits)))
    (matrix-checkpoint-require
      (and (>= start 0) (> end start) (<= end rows) (= (length targets) rows))
      "invalid completion-only target interval")
    (let ((probabilities (mat-softmax logits)))
      (posttrain-cross-entropy-walk logits probabilities targets
        0 start end 0.0 0 (matrix-copy probabilities)))))

; Returns (mean-loss mean-dlogits forward-cache probabilities target-count).
(define (posttrain-window-loss-forward model inputs targets start end)
  (let ((forward (mtransformer-forward-all model inputs)))
    (let ((loss-gradient
      (posttrain-cross-entropy-forward (car forward) targets start end)))
      (list (car loss-gradient) (cadr loss-gradient) (cadr forward)
            (caddr loss-gradient) (cadddr loss-gradient)))))

(define (posttrain-window-log-probability model inputs targets start end)
  (let ((result (posttrain-window-loss-forward model inputs targets start end)))
    (- 0.0 (* (car result) (nth 4 result)))))

; Accumulate a scaled completion loss into the model's existing gradients.
; scale=1 is SFT.  DPO and REINFORCE supply signed objective coefficients.
(define (posttrain-window-accumulate model inputs targets start end scale)
  (let ((result (posttrain-window-loss-forward model inputs targets start end)))
    (mtransformer-backward-all model
      (* (cadr result) scale) (caddr result))
    (list (car result) (nth 4 result))))

(define (posttrain-repeat value count)
  (cond ((= count 0) nil)
        (t (cons value (posttrain-repeat value (- count 1))))))

; Construct one exact-context training window.  completion-offset is the first
; not-yet-trained character in completion.  The return value is:
;   (inputs targets active-start active-end trained-count next-offset)
; Successive calls with next-offset cover every completion character once.
(define (posttrain-completion-window prompt completion completion-offset
          context padding-token)
  (let ((prompt-length (length prompt))
        (completion-length (length completion)))
    (matrix-checkpoint-require (> prompt-length 0)
      "posttraining prompt must not be empty")
    (matrix-checkpoint-require
      (and (>= completion-offset 0) (< completion-offset completion-length))
      "completion offset is outside the completion")
    (let ((combined (append prompt completion))
          (first-target (+ prompt-length completion-offset)))
      ; Reserve roughly half the window for preceding context and half for new
      ; answer targets.  Keeping a full context would expose only one new target
      ; per later window; keeping none would make continuation learning myopic.
      (let ((window-start (max 0 (- first-target (floor (/ context 2))))))
        (let ((available (min (+ context 1) (- (length combined) window-start))))
          (let ((window
            (append (take (drop combined window-start) available)
              (posttrain-repeat padding-token (- (+ context 1) available)))))
            (let ((active-start (- first-target window-start 1))
                  (active-end (- available 1)))
              (let ((trained-count (- active-end active-start)))
                (list (take window context) (drop window 1)
                  active-start active-end trained-count
                  (+ completion-offset trained-count))))))))))

; Numerically stable logistic helpers used by DPO.
(define (posttrain-softplus value)
  (cond ((> value 0.0) (+ value (log (+ 1.0 (exp (- 0.0 value))))))
        (t (log (+ 1.0 (exp value))))))
(define (posttrain-sigmoid value)
  (cond ((>= value 0.0) (/ 1.0 (+ 1.0 (exp (- 0.0 value)))))
        (t (let ((e (exp value))) (/ e (+ 1.0 e))))))

; Reference-anchored Direct Preference Optimization statistics.
; Returns (loss delta chosen-gradient-scale rejected-gradient-scale).
; The scales multiply mean completion NLL gradients for the two continuations.
(define (posttrain-dpo-statistics current-chosen-logp current-rejected-logp
          reference-chosen-logp reference-rejected-logp beta
          chosen-count rejected-count)
  (let ((delta (* beta
    (- (- current-chosen-logp current-rejected-logp)
       (- reference-chosen-logp reference-rejected-logp)))))
    (let ((coefficient (* beta (- (posttrain-sigmoid delta) 1.0))))
      (list (posttrain-softplus (- 0.0 delta)) delta
        (* (- 0.0 coefficient) chosen-count)
        (* coefficient rejected-count)))))

; REINFORCE on generated completion characters with an exponential moving
; reward baseline.  The returned scale multiplies mean completion NLL.
; Returns (advantage gradient-scale updated-baseline).
(define (posttrain-reinforce-statistics reward baseline baseline-rate count)
  (let ((advantage (- reward baseline)))
    (list advantage (* advantage count)
      (+ (* (- 1.0 baseline-rate) baseline) (* baseline-rate reward)))))

; Distillation is completion-only cross-entropy against teacher probabilities.
; The teacher matrix and student logits have identical shapes.  Rows outside
; [start,end) are conditioning/padding and receive zero gradient.
(define (posttrain-distill-walk probabilities teacher row start end dlogits)
  (cond ((= row (mat-rows probabilities)) dlogits)
        ((and (>= row start) (< row end))
          (posttrain-distill-walk probabilities teacher (+ row 1) start end
            dlogits))
        (t (posttrain-zero-row dlogits row 0)
           (posttrain-distill-walk probabilities teacher (+ row 1) start end
             dlogits))))
(define (posttrain-distill-forward logits teacher start end temperature)
  (matrix-checkpoint-require
    (and (= (mat-rows logits) (mat-rows teacher))
         (= (mat-cols logits) (mat-cols teacher))
         (> end start) (> temperature 0.0))
    "invalid distillation shapes or interval")
  (let ((student (mat-softmax (* logits (/ 1.0 temperature)))))
    (let ((gradient
      (posttrain-distill-walk student teacher 0 start end
        (* (- student teacher)
          (/ temperature (- end start))))))
      (list gradient student))))
