; charlm.lsp — neural bigram baseline for the contextual transformer corpus.
; Use ml/llm/run-charlm.sh.

(seed 3)
(define *bigram-vocabulary* (list "a" "b" "c" "d"))
(define *bigram-corpus*
  "abcabdabcabdabcabdabcabdabcabdabcabdabcabdabcabdabcabdabcabd")
(define *bigram-ids* (llm-encode *bigram-corpus* *bigram-vocabulary*))
(define *bigram-weights* (llm-make-param-matrix 4 4 0.0))
(define *bigram-parameters* (llm-flatten *bigram-weights*))

(define (bigram-logits current) (nth current *bigram-weights*))
(define (bigram-pair-loss current next)
  (llm-cross-entropy-logits (bigram-logits current) next))
(define (bigram-pairs ids)
  (cond ((eq (cdr ids) nil) nil)
        (t (cons (list (car ids) (cadr ids))
                 (bigram-pairs (cdr ids))))))
(define *bigram-training-pairs* (bigram-pairs *bigram-ids*))
(define *bigram-pair-index* 0)
(define *bigram-step* 0)

(define (bigram-mean-loss pairs total count)
  (cond ((eq pairs nil) (/ total count))
        (t (bigram-mean-loss (cdr pairs)
             (+ total (vdata (bigram-pair-loss
               (caar pairs) (cadar pairs)))) (+ count 1)))))
(define (bigram-evaluate)
  (bigram-mean-loss *bigram-training-pairs* 0.0 0))

(define *bigram-initial-loss* (bigram-evaluate))
(define *bigram-last-loss* *bigram-initial-loss*)
(define (bigram-training-step)
  (let ((pair (nth *bigram-pair-index* *bigram-training-pairs*)))
    (llm-zero-gradients *bigram-parameters*)
    (let ((loss (bigram-pair-loss (car pair) (cadr pair))))
      (setq *bigram-last-loss* (vdata loss))
      (backward loss)
      (llm-sgd-step *bigram-parameters* 0.12 5.0))
    (setq *bigram-step* (+ *bigram-step* 1))
    (setq *bigram-pair-index*
      (mod (+ *bigram-pair-index* 1) (length *bigram-training-pairs*)))
    (cond ((= (mod *bigram-step* 50) 0)
            (print (list (quote step) *bigram-step*
                         (quote pair-loss) *bigram-last-loss*)))
          (t nil))
    *bigram-last-loss*))

(define (bigram-next current temperature)
  (llm-sample-logits (llm-vector-data (bigram-logits current)) temperature))
(define (bigram-generate history current count temperature)
  (cond ((= count 0) history)
        (t (let ((next (bigram-next current temperature)))
             (bigram-generate (append history (list next)) next
                              (- count 1) temperature)))))

(print "=== neural bigram baseline on the contextual corpus ===")
(print (list (quote parameters) (length *bigram-parameters*)
             (quote initial-mean-nll) *bigram-initial-loss*))

