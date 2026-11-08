; matrix-inference.lsp — construct a trained transformer without a corpus.
; The runner defines the architecture, vocabulary, and sampling variables.

(seed 211)

(define *matrix-inference-mode* t)
(define *matrix-vocabulary* (str-to-list (read-file *matrix-vocabulary-path*)))

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

; Checkpoint state is restored into these fields.  They remain useful for
; reporting, but inference never reads training or validation examples.
(define *matrix-window-index* 0)
(define *matrix-epoch* 0)
(define *matrix-step* 0)
(define *matrix-sequences-processed* 0)
(define *matrix-characters-processed* 0)
(define *matrix-last-loss* 0.0)
(define *matrix-last-gradient-norm* 0.0)
(define *matrix-current-learning-rate* 0.0)
