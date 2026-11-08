; Final training metrics and generation setup. Generation itself is scheduled as
; separate top-level forms by the runner, just like training.

(define *corpus-final-loss* (llm-evaluate *corpus-model* *corpus-windows*))
(print (list (quote final-mean-nll) *corpus-final-loss*
  (quote final-perplexity) (exp *corpus-final-loss*)))
(print (list (quote runtime-seconds) (runtime)))

(seed 73)
(define *corpus-generated-ids*
  (llm-encode "the cat sat on the mat. " *corpus-vocabulary*))

(define (corpus-generation-step)
  (let ((next
    (llm-model-next-token *corpus-model* *corpus-generated-ids* 0.55)))
    (setq *corpus-generated-ids*
      (append *corpus-generated-ids* (list next)))
    next))
