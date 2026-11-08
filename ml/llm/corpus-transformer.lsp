; corpus-transformer.lsp — a configurable character model trained on real text.
; The runner defines the profile variables before loading this setup.

(seed 41)

(define *corpus-vocabulary*
  (list " " "." "a" "c" "d" "e" "g" "h" "l" "m" "n" "o" "r" "s" "t"))
(define *corpus-source-with-newline* (read-file "ml/llm/corpus.txt"))
(define *corpus-source*
  (substr 0 (- (length *corpus-source-with-newline*) 1)
    *corpus-source-with-newline*))

(define (llm-repeat-text text count)
  (cond ((= count 0) "")
        (t (cat text (llm-repeat-text text (- count 1))))))

; Repetition provides many windows without pretending this is a large dataset.
(define *corpus-text* (llm-repeat-text (cat *corpus-source* " ") 12))
(define *corpus-token-ids* (llm-encode *corpus-text* *corpus-vocabulary*))
(define *corpus-windows*
  (llm-make-windows *corpus-token-ids* *corpus-context*))
(define *corpus-model*
  (llm-make-transformer (length *corpus-vocabulary*) *corpus-context*
    *corpus-dimension* *corpus-hidden* 0.1))
(define *corpus-parameters* (llm-model-parameters *corpus-model*))
(define *corpus-window-index* 0)
(define *corpus-step* 0)
(define *corpus-last-loss* 0.0)
(define *corpus-last-gradient-norm* 0.0)
(define *corpus-last-graph-nodes* 0)

(define (corpus-current-window)
  (nth *corpus-window-index* *corpus-windows*))

(define (corpus-training-step)
  (let ((result
    (llm-train-one *corpus-model* *corpus-parameters*
      (corpus-current-window) *corpus-learning-rate* 5.0)))
    (setq *corpus-last-loss* (car result))
    (setq *corpus-last-gradient-norm* (cadr result))
    (setq *corpus-last-graph-nodes* (caddr result))
    (setq *corpus-step* (+ *corpus-step* 1))
    (setq *corpus-window-index*
      (mod (+ *corpus-window-index* 1) (length *corpus-windows*)))
    (cond ((= (mod *corpus-step* 50) 0)
      (print (list (quote step) *corpus-step*
        (quote loss) *corpus-last-loss*
        (quote grad-norm) *corpus-last-gradient-norm*
        (quote graph-nodes) *corpus-last-graph-nodes*)))
      (t nil))
    *corpus-last-loss*))

(print "=== corpus character transformer ===")
(print (list (quote profile) *corpus-profile*
  (quote parameters) (llm-parameter-count *corpus-parameters*)
  (quote context) *corpus-context*
  (quote dimension) *corpus-dimension*
  (quote windows) (length *corpus-windows*)))
(define *corpus-initial-loss* (llm-evaluate *corpus-model* *corpus-windows*))
(print (list (quote initial-mean-nll) *corpus-initial-loss*
  (quote initial-perplexity) (exp *corpus-initial-loss*)))
