; matrix-model.lsp — multi-layer, multi-head causal transformer using native matrices.

(define (matrix-mask-fill-row mask row column size)
  (cond ((= column size) nil)
        (t (cond ((> column row) (mat-set mask row column -1000000000.0))
                 (t nil))
           (matrix-mask-fill-row mask row (+ column 1) size))))
(define (matrix-mask-fill mask row size)
  (cond ((= row size) mask)
        (t (matrix-mask-fill-row mask row 0 size)
           (matrix-mask-fill mask (+ row 1) size))))
(define (matrix-causal-mask size)
  (matrix-mask-fill (make-matrix size size) 0 size))

(define (mblocks-make count head-count dimension hidden-size scale residual-scale)
  (matrix-repeat-call count
    (lambda ()
      (mblock-make head-count dimension hidden-size scale residual-scale))))
(define (mblocks-parameters blocks)
  (cond ((eq blocks nil) nil)
        (t (append (mblock-parameters (car blocks))
                   (mblocks-parameters (cdr blocks))))))

(define (mblocks-forward-walk blocks states mask caches)
  (cond ((eq blocks nil) (list states (reverse caches)))
        (t (let ((result (mblock-forward (car blocks) states mask)))
          (mblocks-forward-walk (cdr blocks) (car result) mask
            (cons (cadr result) caches))))))
(define (mblocks-forward blocks states mask)
  (mblocks-forward-walk blocks states mask nil))

(define (mblocks-backward-walk reversed-blocks reversed-caches upstream)
  (cond ((eq reversed-blocks nil) upstream)
        (t (mblocks-backward-walk (cdr reversed-blocks) (cdr reversed-caches)
             (mblock-backward (car reversed-blocks) upstream
                              (car reversed-caches))))))
(define (mblocks-backward blocks caches upstream)
  (mblocks-backward-walk (reverse blocks) (reverse caches) upstream))

; Model layout:
; (mtransformer token-embedding position-embedding blocks final-norm output-bias
;               context vocab dimension head-count layer-count hidden mask positions)
(define (mtransformer-make vocab-size context dimension head-count layer-count hidden-size scale)
  (let ((residual-scale (/ scale (sqrt (* 2.0 layer-count)))))
    (list (quote mtransformer)
      (mparam-randn vocab-size dimension scale)
      (mparam-randn context dimension scale)
      (mblocks-make layer-count head-count dimension hidden-size
        scale residual-scale)
      (mrmsnorm-make dimension)
      (mparam-zero-no-decay 1 vocab-size)
      context vocab-size dimension head-count layer-count hidden-size
      (matrix-causal-mask context)
      (seq 0 (- context 1)))))

(define (mtransformer-token-embedding model) (nth 1 model))
(define (mtransformer-position-embedding model) (nth 2 model))
(define (mtransformer-blocks model) (nth 3 model))
(define (mtransformer-final-norm model) (nth 4 model))
(define (mtransformer-output-bias model) (nth 5 model))
(define (mtransformer-context model) (nth 6 model))
(define (mtransformer-vocab model) (nth 7 model))
(define (mtransformer-dimension model) (nth 8 model))
(define (mtransformer-head-count model) (nth 9 model))
(define (mtransformer-layer-count model) (nth 10 model))
(define (mtransformer-hidden model) (nth 11 model))
(define (mtransformer-mask model) (nth 12 model))
(define (mtransformer-positions model) (nth 13 model))

(define (mtransformer-parameters model)
  (cons (mtransformer-token-embedding model)
    (cons (mtransformer-position-embedding model)
      (append (mblocks-parameters (mtransformer-blocks model))
        (append (mrmsnorm-parameters (mtransformer-final-norm model))
                (list (mtransformer-output-bias model)))))))

; The token embedding is also the output projection. Tying these weights reduces
; parameters and lets input and prediction gradients update one shared matrix.
; Returns (logits normalized-input-cache).
(define (mtransformer-output-forward model normalized)
  (list
    (mat-add-row
      (mat* normalized
        (mat-transpose (mp-data (mtransformer-token-embedding model))))
      (mp-data (mtransformer-output-bias model)))
    normalized))

(define (mtransformer-output-backward model upstream normalized-cache)
  (mat-axpy (mp-grad (mtransformer-token-embedding model)) 1.0
    (mat* (mat-transpose upstream) normalized-cache))
  (mat-axpy (mp-grad (mtransformer-output-bias model)) 1.0
    (mat-sum-axis upstream 0))
  (mat* upstream (mp-data (mtransformer-token-embedding model))))

; Returns (logits cache). cache=(token-ids block-caches norm-cache head-cache).
(define (mtransformer-forward model token-ids)
  (let ((tokens
    (mat-gather-rows (mp-data (mtransformer-token-embedding model)) token-ids))
        (positions
    (mat-gather-rows (mp-data (mtransformer-position-embedding model))
      (mtransformer-positions model))))
    (let ((blocks
      (mblocks-forward (mtransformer-blocks model) (+ tokens positions)
        (mtransformer-mask model))))
      (let ((last-state
        (mat-gather-rows (car blocks)
          (list (- (mtransformer-context model) 1)))))
        (let ((normalized
          (mrmsnorm-forward (mtransformer-final-norm model) last-state)))
          (let ((output-result
            (mtransformer-output-forward model (car normalized))))
            (list (car output-result)
              (list token-ids (cadr blocks) (cadr normalized)
                    (cadr output-result)))))))))

(define (mtransformer-backward model upstream cache)
  (let ((token-ids (nth 0 cache))
        (block-caches (nth 1 cache))
        (norm-cache (nth 2 cache))
        (output-cache (nth 3 cache)))
    (let ((dnormalized
      (mtransformer-output-backward model upstream output-cache)))
      (let ((dlast
        (mrmsnorm-backward (mtransformer-final-norm model)
          dnormalized norm-cache)))
        (let ((dstates
          (make-matrix (mtransformer-context model)
                       (mtransformer-dimension model))))
          (mat-scatter-add dstates
            (list (- (mtransformer-context model) 1)) dlast)
          (let ((dinput
            (mblocks-backward (mtransformer-blocks model) block-caches dstates)))
            (mat-scatter-add
              (mp-grad (mtransformer-token-embedding model)) token-ids dinput)
            (mat-scatter-add
              (mp-grad (mtransformer-position-embedding model))
              (mtransformer-positions model) dinput)
            nil))))))

; Full causal language-model forward. Unlike the compatibility path above, this
; produces logits for every context row and therefore trains on every target.
; cache=(token-ids block-caches final-norm-cache head-cache).
(define (mtransformer-forward-all model token-ids)
  (let ((tokens
    (mat-gather-rows (mp-data (mtransformer-token-embedding model)) token-ids))
        (positions
    (mat-gather-rows (mp-data (mtransformer-position-embedding model))
      (mtransformer-positions model))))
    (let ((blocks
      (mblocks-forward (mtransformer-blocks model) (+ tokens positions)
        (mtransformer-mask model))))
      (let ((normalized
        (mrmsnorm-forward (mtransformer-final-norm model) (car blocks))))
        (let ((output-result
          (mtransformer-output-forward model (car normalized))))
          (list (car output-result)
            (list token-ids (cadr blocks) (cadr normalized)
                  (cadr output-result))))))))

; Prefix inference shares the cached full-context mask with training.  A shorter
; prefix needs a matching temporary mask, but only until generation fills the
; context window.
(define (mtransformer-prefix-mask model size)
  (cond ((= size (mtransformer-context model)) (mtransformer-mask model))
        (t (matrix-causal-mask size))))

(define (mtransformer-prefix-error message)
  (error message))

; Return the final hidden states for a non-empty prefix no longer than context.
(define (mtransformer-prefix-states model token-ids)
  (let ((size (length token-ids)))
    (cond ((= size 0) (mtransformer-prefix-error "empty inference history"))
          ((> size (mtransformer-context model))
            (mtransformer-prefix-error "inference history exceeds context"))
          (t
            (let ((tokens
              (mat-gather-rows
                (mp-data (mtransformer-token-embedding model)) token-ids))
                  (positions-list (take (mtransformer-positions model) size)))
              (let ((positions
                (mat-gather-rows
                  (mp-data (mtransformer-position-embedding model))
                  positions-list)))
                (car (mblocks-forward (mtransformer-blocks model)
                  (+ tokens positions)
                  (mtransformer-prefix-mask model size)))))))))

; Inference accepts a real prefix shorter than the training context.  This form
; preserves all rows for teacher-forced metrics and observer-style inspection.
(define (mtransformer-forward-prefix model token-ids)
  (let ((states (mtransformer-prefix-states model token-ids)))
    (let ((normalized
      (mrmsnorm-forward (mtransformer-final-norm model) states)))
      (mtransformer-output-forward model (car normalized)))))

; Sampling needs only the last vocabulary row.  Avoid projecting every earlier
; prefix state through the tied embedding on every generated character.
(define (mtransformer-forward-prefix-last model token-ids)
  (let ((states (mtransformer-prefix-states model token-ids)))
    (let ((last-state
      (mat-gather-rows states (list (- (length token-ids) 1)))))
      (let ((normalized
        (mrmsnorm-forward (mtransformer-final-norm model) last-state)))
        (mtransformer-output-forward model (car normalized))))))

(define (mtransformer-backward-all model upstream cache)
  (let ((token-ids (nth 0 cache))
        (block-caches (nth 1 cache))
        (norm-cache (nth 2 cache))
        (output-cache (nth 3 cache)))
    (let ((dnormalized
      (mtransformer-output-backward model upstream output-cache)))
      (let ((dstates
        (mrmsnorm-backward (mtransformer-final-norm model)
          dnormalized norm-cache)))
        (let ((dinput
          (mblocks-backward (mtransformer-blocks model) block-caches dstates)))
          (mat-scatter-add
            (mp-grad (mtransformer-token-embedding model)) token-ids dinput)
          (mat-scatter-add
            (mp-grad (mtransformer-position-embedding model))
            (mtransformer-positions model) dinput)
          nil)))))

(define (mtransformer-logsumexp-row-loss logits row target)
  (let ((row-logits (matrix-row-to-list logits row)))
    (let ((maximum (llm-max-number row-logits)))
      (- (+ maximum
            (log (llm-sum-numbers
              (map (lambda (value) (exp (- value maximum))) row-logits))))
         (nth target row-logits)))))

(define (mtransformer-cross-entropy-walk logits probabilities targets row loss dlogits)
  (cond ((eq targets nil)
    (list (/ loss (mat-rows probabilities))
          (* dlogits (/ 1.0 (mat-rows probabilities)))))
    (t (let ((target (car targets)))
      (let ((target-probability (mat-ref probabilities row target)))
        (mat-set dlogits row target
          (- target-probability 1.0))
        (mtransformer-cross-entropy-walk logits probabilities
          (cdr targets) (+ row 1)
          (+ loss
            (cond ((> target-probability 0.0)
              (- 0.0 (log target-probability)))
              (t (mtransformer-logsumexp-row-loss logits row target))))
          dlogits))))))

; Stable cross-entropy from logits. Native fused softmax handles the common path;
; exact log-sum-exp handles a target probability that numerically underflows.
; Returns (mean-loss mean-dlogits probabilities).
(define (mtransformer-cross-entropy-forward logits targets)
  (let ((probabilities (mat-softmax logits)))
    (let ((loss-gradient
      (mtransformer-cross-entropy-walk logits probabilities targets 0 0.0
        (matrix-copy probabilities))))
      (list (car loss-gradient) (cadr loss-gradient) probabilities))))

; Returns (mean-loss mean-dlogits forward-cache probabilities).
(define (mtransformer-sequence-loss-forward model token-ids targets)
  (let ((forward (mtransformer-forward-all model token-ids)))
    (let ((loss-gradient
      (mtransformer-cross-entropy-forward (car forward) targets)))
      (list (car loss-gradient) (cadr loss-gradient)
            (cadr forward) (caddr loss-gradient)))))

(define (mtransformer-sequence-loss-number model token-ids targets)
  (car (mtransformer-sequence-loss-forward model token-ids targets)))

; Accumulate one sequence into existing parameter gradients. The caller controls
; zeroing, averaging, clipping, and the optimizer step across top-level forms.
(define (mtransformer-accumulate-sequence model token-ids targets)
  (let ((result (mtransformer-sequence-loss-forward model token-ids targets)))
    (mtransformer-backward-all model (cadr result) (caddr result))
    (car result)))

; Returns (loss dlogits forward-cache probabilities).
(define (mtransformer-loss-forward model token-ids target)
  (let ((forward (mtransformer-forward model token-ids)))
    (let ((loss-gradient
      (mtransformer-cross-entropy-forward (car forward) (list target))))
      (list (car loss-gradient) (cadr loss-gradient)
            (cadr forward) (caddr loss-gradient)))))

(define (mtransformer-loss-number model token-ids target)
  (car (mtransformer-loss-forward model token-ids target)))

(define (mtransformer-train-one model parameters token-ids target step
          learning-rate clip-norm weight-decay)
  (mparams-zero parameters)
  (let ((result (mtransformer-loss-forward model token-ids target)))
    (mtransformer-backward model (cadr result) (caddr result))
    (let ((gradient-norm (mparams-clip parameters clip-norm)))
      (mparams-adamw-step parameters step learning-rate
        0.9 0.999 0.00000001 weight-decay)
      (list (car result) gradient-norm))))

(define (mtransformer-next-token model history temperature)
  (mtransformer-next-token-top-k model history temperature
    (mtransformer-vocab model)))

(define (mtransformer-next-token-top-k model history temperature top-k)
  (mtransformer-next-token-top-k-top-p model history temperature top-k 1.0))

(define (mtransformer-next-token-top-k-top-p model history temperature top-k top-p)
  (let ((context
    (drop history
      (max 0 (- (length history) (mtransformer-context model))))))
    (let ((last-logits (car (mtransformer-forward-prefix-last model context))))
      (llm-sample-top-k-top-p-logits
        (matrix-row-to-list last-logits 0)
        temperature top-k top-p))))
