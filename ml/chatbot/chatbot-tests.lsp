; Lightweight API tests use a tiny fake next-token function, so they test the
; chatbot control layer without loading a 50 MB checkpoint.

(define *chatbot-test-failures* 0)
(define (chatbot-check name condition)
  (cond (condition (print (list (quote pass) name)))
        (t (setq *chatbot-test-failures* (+ *chatbot-test-failures* 1))
           (print (list (quote fail) name)))))

(chatbot-register-current "one")
(setq *matrix-context* 8)
(setq *matrix-model* (list 3))
(chatbot-register-current "two")
(chatbot-check "model-names" (= (length (chatbot-model-names *chatbot-models*)) 2))
(chatbot-check "model-switch" (equal (chatbot-use-model "one") "one"))
(chatbot-check "model-state-restored" (= *matrix-context* 4))
(chatbot-check "missing-model" (eq (chatbot-use-model "missing") nil))
(chatbot-check "sampling-deterministic"
  (equal (chatbot-sampling t) (list 0.3 1 1.0)))
(chatbot-check "sampling-random"
  (equal (chatbot-sampling nil) (list 0.8 20 0.95)))

(setq *agent-action-text* "(car 0)")
(setq *agent-action-recognized* t)
(setq *agent-action-kind* "pending")
(chatbot-agent-eval-action)
(chatbot-check "failing-agent-action-is-caught"
  (and (eq *agent-action-recognized* nil)
       (equal *agent-action-kind* "error")
       (equal (car *agent-last-value*) (quote error))
       (numberp (str-find *agent-observation* "error=>"))))

(print (list (quote chatbot-tests) (quote failed) *chatbot-test-failures*))
(cond ((> *chatbot-test-failures* 0) (quit 2)) (t nil))
