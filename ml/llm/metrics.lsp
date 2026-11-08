; metrics.lsp — tiny assertion and reporting helpers for runnable examples.

(define *llm-tests-passed* 0)
(define *llm-tests-failed* 0)

(define (llm-reset-tests)
  (setq *llm-tests-passed* 0)
  (setq *llm-tests-failed* 0))

(define (llm-check label condition)
  (cond (condition
          (setq *llm-tests-passed* (+ *llm-tests-passed* 1))
          (print (list (quote pass) label))
          t)
        (t
          (setq *llm-tests-failed* (+ *llm-tests-failed* 1))
          (print (list (quote fail) label))
          nil)))

(define (llm-close-p actual expected atol rtol)
  (let ((difference (abs (- actual expected))))
    (<= difference (+ atol (* rtol (abs expected))))))

(define (llm-check-close label actual expected atol rtol)
  (llm-check label (llm-close-p actual expected atol rtol)))

(define (llm-all-close-p actual expected atol rtol)
  (cond ((and (eq actual nil) (eq expected nil)) t)
        ((or (eq actual nil) (eq expected nil)) nil)
        ((llm-close-p (car actual) (car expected) atol rtol)
          (llm-all-close-p (cdr actual) (cdr expected) atol rtol))
        (t nil)))

(define (llm-check-list-close label actual expected atol rtol)
  (llm-check label (llm-all-close-p actual expected atol rtol)))

(define (llm-test-summary)
  (print (list (quote tests) (quote passed) *llm-tests-passed*
               (quote failed) *llm-tests-failed*))
  (= *llm-tests-failed* 0))
