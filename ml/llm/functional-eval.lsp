; Functional completion benchmark: teacher-forced probability metrics plus
; greedy candidate files that the shell harness executes with hidden checks.

(define *functional-tasks* 0)
(define *functional-characters* 0)
(define *functional-nll-total* 0.0)
(define *functional-top1-total* 0)
(define *functional-top5-total* 0)
(define *functional-exact-total* 0)
(define *functional-temperature* 1.0)
(define *functional-top-k* 1)
(define *functional-top-p* 1.0)

(define (functional-rank-walk logits target-logit rank)
  (cond ((eq logits nil) rank)
        ((> (car logits) target-logit)
          (functional-rank-walk (cdr logits) target-logit (+ rank 1)))
        (t (functional-rank-walk (cdr logits) target-logit rank))))

(define (functional-target-walk history targets nll top1 top5)
  (cond ((eq targets nil) (list nll top1 top5))
        (t (let ((context
          (drop history (- (length history) *matrix-context*))))
          (let ((logits (car (mtransformer-forward-prefix *matrix-model* context))))
            (let ((row (- (mat-rows logits) 1)) (target (car targets)))
              (let ((row-logits (matrix-row-to-list logits row)))
                (let ((target-logit (nth target row-logits)))
                  (let ((rank (functional-rank-walk row-logits target-logit 1))
                        (loss (mtransformer-logsumexp-row-loss logits row target)))
                    (functional-target-walk (append history (list target))
                      (cdr targets) (+ nll loss)
                      (+ top1 (cond ((= rank 1) 1) (t 0)))
                      (+ top5 (cond ((<= rank 5) 1) (t 0)))))))))))))

(define (functional-generate-walk history remaining)
  (cond ((= remaining 0) history)
        (t (functional-generate-walk
          (append history
            (list (mtransformer-next-token-top-k-top-p *matrix-model*
              history *functional-temperature* *functional-top-k*
              *functional-top-p*)))
          (- remaining 1)))))

(define (functional-eval-one-as-count output-name name prompt-path target-path
          generation-count)
  (let ((prompt (read-file prompt-path)) (target-text (read-file target-path)))
    (let ((prompt-ids (llm-encode prompt *matrix-vocabulary*))
          (target-ids (llm-encode target-text *matrix-vocabulary*)))
      (matrix-checkpoint-require (> (length prompt-ids) 0)
        "functional prompt must not be empty")
      (let ((metrics (functional-target-walk prompt-ids target-ids 0.0 0 0))
            (generated-ids
              (functional-generate-walk prompt-ids
                (min generation-count (length target-ids)))))
        (let ((generated
          (llm-decode (drop generated-ids (length prompt-ids)) *matrix-vocabulary*)))
          (write-file
            (cat *matrix-functional-output* (cat "/" (cat output-name ".lsp")))
            (cat prompt generated))
          (setq *functional-tasks* (+ *functional-tasks* 1))
          (setq *functional-characters* (+ *functional-characters* (length target-ids)))
          (setq *functional-nll-total* (+ *functional-nll-total* (car metrics)))
          (setq *functional-top1-total* (+ *functional-top1-total* (cadr metrics)))
          (setq *functional-top5-total* (+ *functional-top5-total* (caddr metrics)))
          (setq *functional-exact-total*
            (+ *functional-exact-total* (cond ((equal generated target-text) 1) (t 0))))
          (print (list (quote functional-task) name
            (quote output) output-name
            (quote characters) (length target-ids)
            (quote nll) (/ (car metrics) (length target-ids))
            (quote top1) (/ (cadr metrics) (length target-ids))
            (quote top5) (/ (caddr metrics) (length target-ids))
            (quote greedy-exact) (equal generated target-text))))))))

(define (functional-eval-one-as output-name name prompt-path target-path)
  (functional-eval-one-as-count output-name name prompt-path target-path
    (length (read-file target-path))))

(define (functional-eval-one name prompt-path target-path)
  (functional-eval-one-as name name prompt-path target-path))

; Collection skips teacher-forced benchmark metrics.  It only needs a sampled
; completion file for the external hidden-check execution step.
(define (functional-collect-one output-name name prompt-path target-path
          generation-count)
  (let ((prompt (read-file prompt-path)) (target-text (read-file target-path)))
    (let ((prompt-ids (llm-encode prompt *matrix-vocabulary*)))
      (matrix-checkpoint-require (> (length prompt-ids) 0)
        "functional prompt must not be empty")
      (let ((count (min generation-count (length target-text))))
        (let ((generated-ids (functional-generate-walk prompt-ids count)))
          (let ((generated
            (llm-decode (drop generated-ids (length prompt-ids))
              *matrix-vocabulary*)))
            (write-file
              (cat *matrix-functional-output* (cat "/" (cat output-name ".lsp")))
              (cat prompt generated))
            (print (list (quote functional-candidate) name
              (quote output) output-name (quote characters) count))))))))

(define (functional-eval-summary)
  (matrix-checkpoint-require (> *functional-characters* 0)
    "functional benchmark has no target characters")
  (print (list (quote functional-summary)
    (quote tasks) *functional-tasks*
    (quote characters) *functional-characters*
    (quote nll) (/ *functional-nll-total* *functional-characters*)
    (quote bits-per-character)
      (/ *functional-nll-total* (* *functional-characters* (log 2.0)))
    (quote top1) (/ *functional-top1-total* *functional-characters*)
    (quote top5) (/ *functional-top5-total* *functional-characters*)
    (quote greedy-exact-tasks) *functional-exact-total*)))
