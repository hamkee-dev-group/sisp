(define *reinforce-final-policy*
  (map vdata (llm-softmax-values *reinforce-preferences*)))
(define *reinforce-average-reward*
  (/ *reinforce-total-reward* *reinforce-count*))
(print (list (quote final-policy) *reinforce-final-policy*))
(print (list (quote pulls) *reinforce-pulls*))
(print (list (quote average-reward) *reinforce-average-reward*))
(llm-reset-tests)
(llm-check "reinforce-selects-best-arm"
  (= (llm-argmax *reinforce-final-policy*) 2))
(llm-check "reinforce-best-arm-probable"
  (> (nth 2 *reinforce-final-policy*) 0.85))
(llm-check "reinforce-good-reward" (> *reinforce-average-reward* 0.75))
(llm-test-summary)
(print "=== done ===")

