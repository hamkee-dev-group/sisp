; interactive.lsp -- one human-entered completion using the native SISP LLM.
;
; run-interactive.sh supplies the prompt and output paths, then schedules each
; transformer step as a separate top-level form.  That is the same arrangement
; used by training and by ml/agent: SISP can collect a completed forward
; pass before the next one without modifying the interpreter.

(define *llm-interactive-newline*
  (nth (- (length *matrix-vocabulary*) 1) *matrix-vocabulary*))

(define (llm-interactive-normalize-prompt prompt)
  (cond ((or (equal (str-find prompt "; help:") 0)
             (equal (str-find prompt "; algorithm:") 0))
          (cat prompt *llm-interactive-newline*))
        (t prompt)))

(define *llm-interactive-prompt*
  (llm-interactive-normalize-prompt
    (read-file *llm-interactive-prompt-path*)))
(define *llm-interactive-prompt-token-count* 0)
(define *llm-interactive-ids*
  (llm-encode *llm-interactive-prompt* *matrix-vocabulary*))
(setq *llm-interactive-prompt-token-count*
  (length *llm-interactive-ids*))
(define *llm-interactive-completion* "")

(define (llm-interactive-step)
  (let ((next (mtransformer-next-token-top-k-top-p
                *matrix-model* *llm-interactive-ids*
                *llm-interactive-temperature* *llm-interactive-top-k*
                *llm-interactive-top-p*)))
    (setq *llm-interactive-ids*
      (append *llm-interactive-ids* (list next)))
    next))

(define (llm-interactive-finish)
  (setq *llm-interactive-completion*
    (llm-decode
      (drop *llm-interactive-ids* *llm-interactive-prompt-token-count*)
      *matrix-vocabulary*)))

