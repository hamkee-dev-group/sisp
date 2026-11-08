; observer.lsp — pure-SISP, read-only human microscope for the matrix LLM.
;
; It performs forward inference and numeric inspection only. It never calls a
; backward function, optimizer, parameter mutation, or generated-code evaluator.

(load "lib/math/matrix.lsp")

(define *observer-newline*
  (nth (- (length *matrix-vocabulary*) 1) *matrix-vocabulary*))

(define (observer-tail text maximum)
  (cond ((<= (length text) maximum) text)
        (t (substr (- (length text) maximum) maximum text))))

(define (observer-spaces count)
  (cond ((= count 0) "")
        (t (cat " " (observer-spaces (- count 1))))))

; Return (exact-context padding-count visible-prompt).
(define (observer-context-info prompt)
  (let ((visible (observer-tail prompt *matrix-context*)))
    (let ((padding (- *matrix-context* (length visible))))
      (list (cat (observer-spaces padding) visible) padding visible))))

(define (observer-character character)
  (cond ((equal character *observer-newline*) "<newline>")
        ((equal character " ") "<space>")
        (t character)))

(define (observer-matrix-elements matrix)
  (* (mat-rows matrix) (mat-cols matrix)))

(define (observer-matrix-rms matrix)
  (/ (mat-norm matrix) (sqrt (observer-matrix-elements matrix))))

(define (observer-probability-entries probabilities token-id)
  (cond ((eq probabilities nil) nil)
        (t (cons (list (car probabilities) token-id)
          (observer-probability-entries (cdr probabilities)
            (+ token-id 1))))))

(define (observer-sort-probabilities entries)
  (sort (lambda (left right) (> (car left) (car right))) entries))

(define (observer-entry-display entry)
  (list (observer-character (nth (cadr entry) *matrix-vocabulary*))
    (car entry)))

(define (observer-top-display entries count)
  (map (lambda (entry) (observer-entry-display entry))
    (take entries count)))

(define (observer-probability-entropy probabilities)
  (foldl (lambda (total probability)
    (cond ((> probability 0.0)
      (- total (* probability (log probability))))
      (t total))) 0.0 probabilities))

(define (observer-probability-rank probabilities expected)
  (+ 1 (count (lambda (probability) (> probability expected)) probabilities)))

; Return (ids forward probabilities sorted-entries padding visible-prompt).
(define (observer-forward prompt)
  (let ((info (observer-context-info prompt)))
    (let ((ids (llm-encode (car info) *matrix-vocabulary*)))
      (let ((forward (mtransformer-forward-all *matrix-model* ids)))
        (let ((probability-matrix (mat-softmax (car forward))))
          (let ((probabilities
            (matrix-row-to-list probability-matrix (- *matrix-context* 1))))
            (list ids forward probabilities
              (observer-sort-probabilities
                (observer-probability-entries probabilities 0))
              (cadr info) (caddr info))))))))

(define (observer-probe-report label prompt expected top-count)
  (let ((result (observer-forward prompt)))
    (let ((probabilities (nth 2 result))
          (entries (nth 3 result))
          (expected-id (llm-char-id expected *matrix-vocabulary*)))
      (let ((expected-probability (nth expected-id probabilities)))
        (print (list (quote observer-probe)
          (quote name) label
          (quote expected) (observer-character expected)
          (quote probability) expected-probability
          (quote rank)
            (observer-probability-rank probabilities expected-probability)
          (quote nll)
            (cond ((> expected-probability 0.0)
              (- 0.0 (log expected-probability)))
              (t 1000000000.0))
          (quote entropy) (observer-probability-entropy probabilities)
          (quote top) (observer-top-display entries top-count))))))
  nil)

(define (observer-custom-report prompt top-count)
  (let ((result (observer-forward prompt)))
    (print (list (quote observer-custom-prompt)
      (quote visible) (nth 5 result)
      (quote entropy) (observer-probability-entropy (nth 2 result))
      (quote top) (observer-top-display (nth 3 result) top-count))))
  nil)

; Flat parameter names follow mtransformer-parameters exactly.
(define (observer-head-parameter-names layer head)
  (cond ((= head *matrix-heads*) nil)
        (t (let ((prefix (list-to-str (list "layer-" (num-to-str layer)
          "-head-" (num-to-str head) "-"))))
          (append (list
            (cat prefix "wq") (cat prefix "wk")
            (cat prefix "wv") (cat prefix "wo"))
            (observer-head-parameter-names layer (+ head 1)))))))

(define (observer-layer-parameter-names layer)
  (let ((prefix (list-to-str (list "layer-" (num-to-str layer) "-"))))
    (append (observer-head-parameter-names layer 0)
      (list
        (cat prefix "norm1-gain") (cat prefix "norm2-gain")
        (cat prefix "ff-up-weight") (cat prefix "ff-up-bias")
        (cat prefix "ff-gate-weight") (cat prefix "ff-gate-bias")
        (cat prefix "ff-down-weight") (cat prefix "ff-down-bias")))))

(define (observer-layer-names-walk layer)
  (cond ((= layer *matrix-layers*) nil)
        (t (append (observer-layer-parameter-names layer)
          (observer-layer-names-walk (+ layer 1))))))

(define (observer-parameter-names)
  (append (list "token-embedding" "position-embedding")
    (append (observer-layer-names-walk 0)
      (list "final-norm-gain" "output-bias"))))

(define (observer-parameter-component parameter kind)
  (cond ((eq kind (quote data)) (mp-data parameter))
        ((eq kind (quote gradient)) (mp-grad parameter))
        ((eq kind (quote first-moment)) (mp-m parameter))
        (t (mp-v parameter))))

(define (observer-component-squared parameters kind)
  (foldl (lambda (total parameter)
    (let ((value (mat-norm (observer-parameter-component parameter kind))))
      (+ total (* value value)))) 0.0 parameters))

(define (observer-component-norm parameters kind)
  (sqrt (observer-component-squared parameters kind)))

(define (observer-parameter-summary)
  (print (list (quote observer-parameters)
    (quote objects) (length *matrix-parameters*)
    (quote scalars) (mparams-count *matrix-parameters*)
    (quote weight-norm)
      (observer-component-norm *matrix-parameters* (quote data))
    (quote gradient-norm)
      (observer-component-norm *matrix-parameters* (quote gradient))
    (quote adam-first-moment-norm)
      (observer-component-norm *matrix-parameters* (quote first-moment))
    (quote adam-second-moment-norm)
      (observer-component-norm *matrix-parameters* (quote second-moment))))
  nil)

(define (observer-parameter-report-walk parameters names index)
  (cond ((eq parameters nil) nil)
        (t (let ((parameter (car parameters))
                 (matrix (mp-data (car parameters))))
          (print (list (quote observer-parameter)
            (quote index) index (quote name) (car names)
            (quote shape) (list (mat-rows matrix) (mat-cols matrix))
            (quote weight-rms) (observer-matrix-rms matrix)
            (quote gradient-rms) (observer-matrix-rms (mp-grad parameter))
            (quote first-moment-rms) (observer-matrix-rms (mp-m parameter))
            (quote second-moment-rms) (observer-matrix-rms (mp-v parameter))
            (quote decay) (mp-decay-p parameter)))
          (observer-parameter-report-walk
            (cdr parameters) (cdr names) (+ index 1))))))

(define (observer-parameter-report)
  (observer-parameter-report-walk
    *matrix-parameters* (observer-parameter-names) 0))

; Character embedding similarity.
(define (observer-row-dot left left-row right right-row column)
  (cond ((= column (mat-cols left)) 0.0)
        (t (+ (* (mat-ref left left-row column)
                 (mat-ref right right-row column))
              (observer-row-dot left left-row right right-row
                (+ column 1))))))

(define (observer-row-norm matrix row)
  (sqrt (observer-row-dot matrix row matrix row 0)))

(define (observer-row-cosine matrix left-row right-row)
  (let ((denominator (* (observer-row-norm matrix left-row)
                        (observer-row-norm matrix right-row))))
    (cond ((= denominator 0.0) 0.0)
          (t (/ (observer-row-dot matrix left-row matrix right-row 0)
                denominator)))))

(define (observer-embedding-entries matrix target row)
  (cond ((= row (mat-rows matrix)) nil)
        ((= row target) (observer-embedding-entries matrix target (+ row 1)))
        (t (cons (list (observer-row-cosine matrix target row) row)
          (observer-embedding-entries matrix target (+ row 1))))))

(define (observer-embedding-neighbors character count)
  (let ((matrix (mp-data (mtransformer-token-embedding *matrix-model*)))
        (target (llm-char-id character *matrix-vocabulary*)))
    (let ((entries (observer-sort-probabilities
      (observer-embedding-entries matrix target 0))))
      (print (list (quote observer-embedding)
        (quote character) (observer-character character)
        (quote nearest) (observer-top-display entries count)))))
  nil)

; Attention report for the final prompt position. Entries are
; (probability absolute-position token-id relative-prompt-position).
(define (observer-attention-entries probabilities ids position padding)
  (cond ((= position *matrix-context*) nil)
        (t (cons (list
          (mat-ref probabilities (- *matrix-context* 1) position)
          position (nth position ids) (- position padding))
          (observer-attention-entries probabilities ids
            (+ position 1) padding)))))

(define (observer-attention-visible-mass entries)
  (foldl (lambda (total entry) (+ total (car entry))) 0.0 entries))

(define (observer-attention-display entry)
  (list (quote position) (cadddr entry)
    (quote character)
      (observer-character (nth (caddr entry) *matrix-vocabulary*))
    (quote probability) (car entry)))

(define (observer-attention-top entries count)
  (map (lambda (entry) (observer-attention-display entry))
    (take (observer-sort-probabilities entries) count)))

(define (observer-attention-head-report head-cache ids padding layer head)
  (let ((probabilities (nth 4 head-cache)))
    (let ((entries
      (observer-attention-entries probabilities ids padding padding)))
      (print (list (quote observer-attention)
        (quote layer) layer (quote head) head
        (quote prompt-mass) (observer-attention-visible-mass entries)
        (quote entropy)
          (observer-probability-entropy
            (matrix-row-to-list probabilities (- *matrix-context* 1)))
        (quote top) (observer-attention-top entries 3)))))
  nil)

(define (observer-attention-heads-walk head-caches ids padding layer head)
  (cond ((eq head-caches nil) nil)
        (t (observer-attention-head-report
          (car head-caches) ids padding layer head)
          (observer-attention-heads-walk
            (cdr head-caches) ids padding layer (+ head 1)))))

(define (observer-attention-layers-walk block-caches ids padding layer)
  (cond ((eq block-caches nil) nil)
        (t (observer-attention-heads-walk
          (nth 1 (car block-caches)) ids padding layer 0)
          (observer-attention-layers-walk
            (cdr block-caches) ids padding (+ layer 1)))))

(define (observer-attention-report result)
  (observer-attention-layers-walk
    (nth 1 (cadr (nth 1 result))) (car result) (nth 4 result) 0))

; Per-layer activation scale. The block cache exposes the normalized input,
; attention Q/K/V, SwiGLU up/gate, and hidden matrices used by backward.
(define (observer-head-average-rms head-caches cache-index)
  (/ (foldl (lambda (total cache)
      (+ total (observer-matrix-rms (nth cache-index cache))))
      0.0 head-caches)
    (length head-caches)))

(define (observer-activation-entries matrix row column)
  (cond ((= column (mat-cols matrix)) nil)
        (t (let ((value (mat-ref matrix row column)))
          (cons (list (abs value) column value)
            (observer-activation-entries matrix row (+ column 1)))))))

(define (observer-activation-display entry)
  (list (quote feature) (cadr entry) (quote value) (caddr entry)))

(define (observer-dominant-activations matrix count)
  (map (lambda (entry) (observer-activation-display entry))
    (take (observer-sort-probabilities
      (observer-activation-entries matrix (- *matrix-context* 1) 0)) count)))

(define (observer-activation-layer-report cache layer)
  (let ((norm1-cache (nth 0 cache))
        (head-caches (nth 1 cache))
        (up (nth 3 cache))
        (gate (nth 4 cache))
        (hidden (nth 8 cache)))
    (let ((input (car (car norm1-cache))))
      (print (list (quote observer-activation)
        (quote layer) layer
        (quote input-rms) (observer-matrix-rms input)
        (quote q-rms) (observer-head-average-rms head-caches 1)
        (quote k-rms) (observer-head-average-rms head-caches 2)
        (quote v-rms) (observer-head-average-rms head-caches 3)
        (quote up-rms) (observer-matrix-rms up)
        (quote gate-rms) (observer-matrix-rms gate)
        (quote hidden-rms) (observer-matrix-rms hidden)
        (quote dominant-hidden)
          (observer-dominant-activations hidden 5)))))
  nil)

(define (observer-activation-walk caches layer)
  (cond ((eq caches nil) nil)
        (t (observer-activation-layer-report (car caches) layer)
          (observer-activation-walk (cdr caches) (+ layer 1)))))

(define (observer-activation-report result)
  (observer-activation-walk (nth 1 (cadr (nth 1 result))) 0))

(define (observer-deep-report prompt)
  (let ((result (observer-forward prompt)))
    (print (list (quote observer-deep-prompt) (quote visible) (nth 5 result)))
    (print (list (quote observer-deep-prediction)
      (quote entropy) (observer-probability-entropy (nth 2 result))
      (quote top) (observer-top-display (nth 3 result) 8)))
    (observer-attention-report result)
    (observer-activation-report result))
  nil)

(define (observer-model-report)
  (print (list (quote observer-model)
    (quote profile) *matrix-profile*
    (quote parameters) (mparams-count *matrix-parameters*)
    (quote context) *matrix-context*
    (quote vocabulary) (length *matrix-vocabulary*)
    (quote dimension) *matrix-dimension*
    (quote heads) *matrix-heads*
    (quote layers) *matrix-layers*
    (quote hidden) *matrix-hidden*
    (quote causal) t
    (quote pre-normalization) (quote RMSNorm)
    (quote feed-forward) (quote SwiGLU)
    (quote residual-connections) t
    (quote learned-positions) t
    (quote tied-token-output-weights) t))
  nil)

(define (observer-training-report)
  (print (list (quote observer-training)
    (quote step) *matrix-step*
    (quote epoch) *matrix-epoch*
    (quote characters) *matrix-characters-processed*
    (quote learning-rate) (matrix-learning-rate-for-step *matrix-step*)
    (quote train-bytes) (length *matrix-source*)
    (quote validation-bytes) (length *matrix-validation-source*)
    (quote sampling) *matrix-sampling-mode*))
  nil)

(define (observer-contract-report)
  (print (list (quote observer-contract)
    (quote parameter-names-match)
      (= (length (observer-parameter-names))
         (length *matrix-parameters*))
    (quote parameters-finite) (mparams-finite *matrix-parameters*)
    (quote context-matches-model)
      (= *matrix-context* (mtransformer-context *matrix-model*))
    (quote vocabulary-matches-model)
      (= (length *matrix-vocabulary*)
         (mtransformer-vocab *matrix-model*))))
  nil)

(define (observer-integrity-report starting-weight-norm)
  (let ((ending-weight-norm
    (observer-component-norm *matrix-parameters* (quote data))))
    (print (list (quote observer-integrity)
      (quote starting-weight-norm) starting-weight-norm
      (quote ending-weight-norm) ending-weight-norm
      (quote weights-unchanged) (= starting-weight-norm ending-weight-norm))))
  nil)
