; Top-level-scheduled generation setup. Final evaluation and its finite-value
; gate run before checkpoint publication in matrix-corpus-final.lsp.

(seed 307)
(define *matrix-raw-prompt*
  (cond ((> (length *matrix-prompt-path*) 0) (read-file *matrix-prompt-path*))
        (t (substr 0 *matrix-context* *matrix-validation-source*))))
(matrix-checkpoint-require (> (length *matrix-raw-prompt*) 0)
  "generation prompt must not be empty")
(define *matrix-generation-prompt*
  (cond ((> (length *matrix-raw-prompt*) *matrix-context*)
    (substr (- (length *matrix-raw-prompt*) *matrix-context*)
      *matrix-context* *matrix-raw-prompt*))
    (t *matrix-raw-prompt*)))
(define *matrix-generated-ids*
  (llm-encode *matrix-generation-prompt* *matrix-vocabulary*))
(define (matrix-generation-step)
  (let ((next (mtransformer-next-token-top-k-top-p *matrix-model*
                *matrix-generated-ids* *matrix-temperature* *matrix-top-k*
                *matrix-top-p*)))
    (setq *matrix-generated-ids*
      (append *matrix-generated-ids* (list next)))
    next))
