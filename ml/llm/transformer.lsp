; transformer.lsp — end-to-end causal character transformer training.
; This is the setup stage. Use ml/llm/run-transformer.sh, which evaluates
; the libraries, this setup, every training step, and the report as separate file
; arguments in one persistent SISP process.
;
; Training is loaded as many separate top-level forms. This is intentional:
; SISP collects between forms, bounding graph memory without unsafe mid-eval GC.

(seed 17)

(define *tiny-vocabulary* (list "a" "b" "c" "d"))
(define *tiny-corpus*
  "abcabdabcabdabcabdabcabdabcabdabcabdabcabdabcabdabcabdabcabd")
(define *tiny-context* 3)
(define *tiny-token-ids* (llm-encode *tiny-corpus* *tiny-vocabulary*))
(define *tiny-windows* (llm-make-windows *tiny-token-ids* *tiny-context*))

; dimension=3, feed-forward width=6: 130 trainable scalar parameters.
(define *tiny-model* (llm-make-transformer 4 *tiny-context* 3 6 0.25))
(define *tiny-parameters* (llm-model-parameters *tiny-model*))
(define *tiny-window-index* 0)
(define *tiny-step* 0)
(define *tiny-last-loss* 0.0)
(define *tiny-last-gradient-norm* 0.0)
(define *tiny-last-graph-nodes* 0)

(define (tiny-current-window)
  (nth *tiny-window-index* *tiny-windows*))

(define (tiny-training-step)
  (let ((result
    (llm-train-one *tiny-model* *tiny-parameters* (tiny-current-window)
                   0.025 5.0)))
    (setq *tiny-last-loss* (car result))
    (setq *tiny-last-gradient-norm* (cadr result))
    (setq *tiny-last-graph-nodes* (caddr result))
    (setq *tiny-step* (+ *tiny-step* 1))
    (setq *tiny-window-index*
      (mod (+ *tiny-window-index* 1) (length *tiny-windows*)))
    (cond ((= (mod *tiny-step* 25) 0)
            (print (list (quote step) *tiny-step*
              (quote loss) *tiny-last-loss*
              (quote grad-norm) *tiny-last-gradient-norm*
              (quote graph-nodes) *tiny-last-graph-nodes*)))
          (t nil))
    *tiny-last-loss*))

(print "=== tiny causal character transformer ===")
(print (list (quote parameters) (llm-parameter-count *tiny-parameters*)
             (quote context) *tiny-context*
             (quote windows) (length *tiny-windows*)))
(define *tiny-initial-loss* (llm-evaluate *tiny-model* *tiny-windows*))
(print (list (quote initial-mean-nll) *tiny-initial-loss*
             (quote initial-perplexity) (exp *tiny-initial-loss*)))
