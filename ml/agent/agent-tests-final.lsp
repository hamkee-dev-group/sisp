; agent-tests-final.lsp — checks run after the generated action input.

(agent-validate-action)
(llm-check "agent-loaded-action-recognized" *agent-action-recognized*)
(llm-check "agent-loaded-action-result" (= *agent-last-value* 13))

(setq *agent-tool-count* 0)
(setq *agent-action-recognized* nil)
(agent-think "first")
(agent-answer "second")
(llm-check "agent-rejects-multiple-tools"
  (and (equal *agent-action-kind* "multiple")
       (eq *agent-action-recognized* nil)
       (eq *agent-done* nil)))

(setq *agent-done* nil)
(setq *agent-tool-count* 0)
(agent-answer "finished")
(llm-check "agent-answer-stops" *agent-done*)
(llm-check "agent-answer-value"
  (equal *agent-final-answer* "finished"))

(llm-test-summary)
(cond ((> *llm-tests-failed* 0) (quit 2)) (t nil))
(print "=== done ===")
