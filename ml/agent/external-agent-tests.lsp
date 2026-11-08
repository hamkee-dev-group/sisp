; Deterministic protocol tests. These test the real SISP transport boundary,
; parser, validator, tool execution, state transition, and persistence. They do
; not claim to test any model's intelligence.

(llm-reset-tests)
(print "=== external model boundary checks ===")

(external-agent-start)
(external-agent-request)
(llm-check "external-request-written"
  (> (length (read-file *external-agent-request-path*)) 0))
(llm-check "external-request-protocol"
  (numberp (str-find (read-file *external-agent-request-path*)
    "Return exactly one action")))

(write-file *external-agent-response-path* "(agent-eval '(+ 20 22))")
(external-agent-accept)
(llm-check "external-direct-tool-executed"
  (equal *agent-observation* "eval=>42"))
(llm-check "external-first-turn-continues"
  (equal (read-file *external-agent-status-path*) "continue"))

(external-agent-request)
(write-file *external-agent-response-path*
  "(progn (agent-think 1) (agent-answer 2))")
(external-agent-accept)
(llm-check "external-rejects-wrapper"
  (numberp (str-find *agent-observation* "invalid=>")))
(llm-check "external-wrapper-had-no-side-effect"
  (eq *agent-final-answer* nil))

(external-agent-request)
(write-file *external-agent-response-path* "(agent-answer 42)")
(external-agent-accept)
(llm-check "external-answer-stops" *agent-done*)
(llm-check "external-answer-persisted"
  (equal (read-file *external-agent-final-path*) "42"))

(llm-test-summary)
(cond ((> *llm-tests-failed* 0) (quit 2)) (t nil))
