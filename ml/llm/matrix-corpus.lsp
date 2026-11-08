; matrix-corpus.lsp — streaming full-sequence transformer training.
; The runner defines architecture, corpus, optimizer, and schedule variables.

(seed 211)

(define *matrix-vocabulary* (str-to-list (read-file *matrix-vocabulary-path*)))
(define *matrix-source* (read-file *matrix-corpus-path*))
(define *matrix-validation-source* (read-file *matrix-validation-corpus-path*))

; Corpus preparation can provide native matrices whose rows are
; (unit-start unit-length cumulative-valid-windows).  The third column is
; filled for the active context here, so the same corpus supports every model
; profile.  Sampling is then uniform over valid context+1 windows and can never
; cross an unrelated source-unit boundary.
(define (matrix-span-prepare-row spans source-length row cumulative)
  (cond ((= row (mat-rows spans)) cumulative)
        (t (let ((start (truncate (mat-ref spans row 0)))
                 (length (truncate (mat-ref spans row 1))))
          (matrix-checkpoint-require
            (and (>= start 0) (> length 0) (<= (+ start length) source-length))
            "corpus span is outside its source")
          (let ((next (+ cumulative (max 0 (- length *matrix-context*)))))
            (mat-set spans row 2 next)
            (matrix-span-prepare-row spans source-length (+ row 1) next))))))
(define (matrix-span-prepare spans source)
  (cond ((eq spans nil) 0)
        (t
          (matrix-checkpoint-require (= (mat-cols spans) 3)
            "corpus span matrix must have three columns")
          (let ((total (matrix-span-prepare-row spans (length source) 0 0)))
            (matrix-checkpoint-require (> total 0)
              "corpus spans contain no unit longer than context")
            total))))
(define *matrix-training-window-count*
  (matrix-span-prepare *matrix-training-spans* *matrix-source*))
(define *matrix-validation-window-count*
  (matrix-span-prepare *matrix-validation-spans* *matrix-validation-source*))

(define (matrix-span-find-row spans target low high)
  (cond ((= low high) low)
        (t (let ((middle (floor (/ (+ low high) 2))))
          (cond ((> (truncate (mat-ref spans middle 2)) target)
            (matrix-span-find-row spans target low middle))
            (t (matrix-span-find-row spans target (+ middle 1) high)))))))
(define (matrix-span-position spans target)
  (let ((row (matrix-span-find-row spans target 0 (- (mat-rows spans) 1))))
    (let ((previous
      (cond ((= row 0) 0) (t (truncate (mat-ref spans (- row 1) 2))))))
      (+ (truncate (mat-ref spans row 0)) (- target previous)))))

(matrix-checkpoint-require
  (> (length *matrix-source*) *matrix-context*)
  "training corpus must be longer than context")
(matrix-checkpoint-require
  (> (length *matrix-validation-source*) *matrix-context*)
  "validation corpus must be longer than context")
(matrix-checkpoint-require (= (mod *matrix-dimension* *matrix-heads*) 0)
  "model dimension must be divisible by attention heads")
(matrix-checkpoint-require
  (and (> *matrix-temperature* 0.0)
       (<= *matrix-top-k* (length *matrix-vocabulary*))
       (> *matrix-top-p* 0.0) (<= *matrix-top-p* 1.0))
  "sampling temperature/top-k/top-p are outside valid ranges")

(define *matrix-model*
  (mtransformer-make (length *matrix-vocabulary*) *matrix-context*
    *matrix-dimension* *matrix-heads* *matrix-layers* *matrix-hidden*
    *matrix-init-scale*))
(define *matrix-parameters* (mtransformer-parameters *matrix-model*))
(matrix-checkpoint-require
  (= (length *matrix-parameters*) *matrix-expected-parameter-objects*)
  "runner parameter-object count does not match model architecture")
(matrix-checkpoint-require
  (= (mparams-count *matrix-parameters*) *matrix-expected-parameters*)
  "runner scalar parameter count does not match model architecture")

(define *matrix-window-index* 0)
(define *matrix-epoch* 0)
(define *matrix-step* 0)
(define *matrix-sequences-processed* 0)
(define *matrix-characters-processed* 0)
(define *matrix-last-loss* 0.0)
(define *matrix-last-gradient-norm* 0.0)
(define *matrix-current-learning-rate* 0.0)
(define *matrix-accumulated* 0)
(define *matrix-accumulated-loss* 0.0)
(define *matrix-evaluation-total* 0.0)
(define *matrix-evaluation-sequences* 0)

; Return (input-ids target-ids) from one context+1 substring. Only this short
; substring is boxed as characters/token IDs; corpus memory stays O(file size).
(define (matrix-sequence-at source position)
  (let ((ids
    (llm-encode (substr position (+ *matrix-context* 1) source)
                *matrix-vocabulary*)))
    (list (take ids *matrix-context*) (drop ids 1))))

(define (matrix-current-sequence)
  (matrix-sequence-at *matrix-source* *matrix-window-index*))

; A deterministic modular sampler decorrelates adjacent microsequences and varies
; absolute-position alignment while remaining exactly resumable from the saved
; sequence counter. The sequential mode remains available for comparisons.
(define (matrix-sampler-hash index)
  (let ((first
    (mod (+ (* (+ index *matrix-sampler-seed* 1) 48271) 1) 2147483647)))
    (mod (+ (* (+ first 1) 69621) 1) 2147483647)))
(define (matrix-sampled-position index)
  (cond ((eq *matrix-training-spans* nil)
    (mod (matrix-sampler-hash index)
      (- (length *matrix-source*) *matrix-context*)))
    (t (matrix-span-position *matrix-training-spans*
      (mod (matrix-sampler-hash index) *matrix-training-window-count*)))))
(define (matrix-select-training-position)
  (cond ((eq *matrix-sampling-mode* (quote shuffle))
    (setq *matrix-window-index*
      (matrix-sampled-position *matrix-sequences-processed*)))
    ((/= *matrix-training-window-count* 0)
      (setq *matrix-window-index*
        (matrix-span-position *matrix-training-spans*
          (mod (* *matrix-sequences-processed* *matrix-context*)
               *matrix-training-window-count*))))
    (t nil)))

(define (matrix-advance-training-position)
  (cond ((/= *matrix-training-window-count* 0)
    (setq *matrix-epoch*
      (floor (/ *matrix-characters-processed* (length *matrix-source*)))))
    (t (let ((next (+ *matrix-window-index* *matrix-context*))
        (last-position (- (length *matrix-source*) *matrix-context* 1)))
    (cond ((= *matrix-window-index* last-position)
      (setq *matrix-window-index* 0)
      (setq *matrix-epoch* (+ *matrix-epoch* 1)))
      ((> next last-position) (setq *matrix-window-index* last-position))
      (t (setq *matrix-window-index* next)))))))

(define (matrix-learning-rate-for-step step)
  (let ((minimum (* *matrix-learning-rate* *matrix-min-learning-rate-ratio*)))
    (cond ((and (> *matrix-warmup-steps* 0) (<= step *matrix-warmup-steps*))
      (* *matrix-learning-rate* (/ step *matrix-warmup-steps*)))
      ((>= step *matrix-decay-steps*) minimum)
      (t (let ((progress
        (/ (- step *matrix-warmup-steps*)
           (- *matrix-decay-steps* *matrix-warmup-steps*))))
        (+ minimum
          (* 0.5 (- *matrix-learning-rate* minimum)
            (+ 1.0 (cos (* (acos -1.0) progress))))))))))

; Each function is scheduled as its own top-level form. This lets SISP collect a
; completed forward/backward cache between microsequences while gradient matrices
; remain resident in the parameters.
(define (matrix-training-begin)
  (mparams-zero *matrix-parameters*)
  (setq *matrix-accumulated* 0)
  (setq *matrix-accumulated-loss* 0.0))

(define (matrix-training-accumulate)
  (matrix-select-training-position)
  (let ((sequence (matrix-current-sequence)))
    (let ((loss
      (mtransformer-accumulate-sequence *matrix-model*
        (car sequence) (cadr sequence))))
      (setq *matrix-accumulated-loss* (+ *matrix-accumulated-loss* loss))
      (setq *matrix-accumulated* (+ *matrix-accumulated* 1))
      (setq *matrix-sequences-processed* (+ *matrix-sequences-processed* 1))
      (setq *matrix-characters-processed*
        (+ *matrix-characters-processed* *matrix-context*))
      (matrix-checkpoint-require (matrix-finite-number loss)
        "non-finite training loss")
      (cond ((eq *matrix-sampling-mode* (quote sequential))
        (matrix-advance-training-position))
        (t (setq *matrix-epoch*
          (floor (/ *matrix-characters-processed* (length *matrix-source*))))))
      loss)))

(define (matrix-training-apply)
  (matrix-checkpoint-require (= *matrix-accumulated* *matrix-microbatch*)
    "optimizer step has an incomplete accumulated batch")
  (mparams-scale-gradients *matrix-parameters* (/ 1.0 *matrix-microbatch*))
  (setq *matrix-last-gradient-norm*
    (mparams-clip *matrix-parameters* *matrix-clip-norm*))
  (setq *matrix-current-learning-rate*
    (matrix-learning-rate-for-step (+ *matrix-step* 1)))
  (mparams-adamw-step *matrix-parameters* (+ *matrix-step* 1)
    *matrix-current-learning-rate* 0.9 *matrix-beta2* 0.00000001
    *matrix-weight-decay*)
  (matrix-checkpoint-require (mparams-finite *matrix-parameters*)
    "non-finite parameter or optimizer state")
  (setq *matrix-last-loss* (/ *matrix-accumulated-loss* *matrix-microbatch*))
  (setq *matrix-step* (+ *matrix-step* 1))
  (cond ((= (mod *matrix-step* *matrix-log-every*) 0)
    (print (list (quote step) *matrix-step*
      (quote epoch) *matrix-epoch*
      (quote train-nll) *matrix-last-loss*
      (quote grad-norm) *matrix-last-gradient-norm*
      (quote learning-rate) *matrix-current-learning-rate*
      (quote characters) *matrix-characters-processed*)))
    (t nil))
  *matrix-last-loss*)

; Evaluation positions are evenly spread across a complete source. Each sample
; is invoked as a separate top-level form by the runner, allowing GC between
; forward caches even when a long-run validation uses many samples.
(define (matrix-evaluation-position source spans window-count index)
  (cond ((eq spans nil)
    (let ((last-position (- (length source) *matrix-context* 1)))
      (cond ((= *matrix-evaluation-count* 1) 0)
            (t (floor (/ (* index last-position)
                         (- *matrix-evaluation-count* 1)))))))
    ((= *matrix-evaluation-count* 1) (matrix-span-position spans 0))
    (t (matrix-span-position spans
      (floor (/ (* index (- window-count 1))
                (- *matrix-evaluation-count* 1)))))))
(define (matrix-evaluation-reset)
  (setq *matrix-evaluation-total* 0.0)
  (setq *matrix-evaluation-sequences* 0))
(define (matrix-evaluation-accumulate source spans window-count index)
  (let ((sequence
    (matrix-sequence-at source
      (matrix-evaluation-position source spans window-count index))))
    (let ((loss
      (mtransformer-sequence-loss-number *matrix-model*
        (car sequence) (cadr sequence))))
      (matrix-checkpoint-require (matrix-finite-number loss)
        "non-finite evaluation loss")
      (setq *matrix-evaluation-total* (+ *matrix-evaluation-total* loss))
      (setq *matrix-evaluation-sequences* (+ *matrix-evaluation-sequences* 1))
      loss)))
(define (matrix-evaluation-accumulate-training index)
  (matrix-evaluation-accumulate *matrix-source* *matrix-training-spans*
    *matrix-training-window-count* index))
(define (matrix-evaluation-accumulate-validation index)
  (matrix-evaluation-accumulate *matrix-validation-source* *matrix-validation-spans*
    *matrix-validation-window-count* index))
(define (matrix-evaluation-mean)
  (/ *matrix-evaluation-total* *matrix-evaluation-sequences*))
