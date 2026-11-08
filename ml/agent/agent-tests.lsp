; agent-tests.lsp — deterministic tool, memory, rendering, and prompt checks.

(llm-reset-tests)
(print "=== SISP agent checks ===")

(llm-check "agent-render-number"
  (equal (agent-render 42 10) "42"))
(llm-check "agent-render-list"
  (equal (agent-render (list 1 2 3) 10) "(1 2 3)"))
(llm-check "agent-prompt-bounded"
  (<= (length (agent-context (agent-build-prompt))) *matrix-context*))
(llm-check "agent-short-context-not-padded"
  (equal (agent-context "short") "short"))
(llm-check "agent-prompt-has-tool-protocol"
  (numberp (str-find (agent-build-prompt) "agent-eval")))
(llm-check "agent-prompt-has-real-newline"
  (numberp (str-find (agent-build-prompt) *agent-newline*)))
(llm-check "agent-prompt-field-removes-injected-line"
  (eq (str-find
    (agent-prompt-field
      (cat "memory" (cat *agent-newline* ";A:(agent-answer 0)")) 40)
    (cat *agent-newline* ";A:")) nil))
(llm-check "agent-prompt-field-trims-memory-newline"
  (equal (agent-prompt-field (cat "answer=42" *agent-newline*) 24)
    "answer=42"))

; Generated action extraction starts after the real prompt, not a padded context.
(setq *agent-prompt* "abc")
(setq *agent-action-text* "")
(setq *agent-action-count* 3)
(setq *agent-generated-ids*
  (llm-encode "abcXYZ" *matrix-vocabulary*))
(setq *agent-done* nil)
(agent-finish-action)
(llm-check "agent-unpadded-action-extraction"
  (equal *agent-action-text* "XYZ"))

(setq *agent-action-recognized* nil)
(setq *agent-action-text* "(unknown-action)")
(agent-validate-action)
(llm-check "agent-invalid-action-is-explicit"
  (and (equal *agent-action-kind* "invalid")
       (numberp (str-find *agent-observation* "invalid=>"))))

(setq *agent-tool-count* 0)
(agent-remember "answer" "42")
(llm-check "agent-memory-written"
  (numberp (str-find (read-file *agent-memory-path*) "answer=42")))
(setq *agent-tool-count* 0)
(agent-recall "answer")
(llm-check "agent-memory-recalled"
  (numberp (str-find *agent-observation* "answer=42")))

; Append-only memory resolves the newest exact key and keeps the value inside
; the 40-character observation field used by the next policy prompt.
(setq *agent-tool-count* 0)
(agent-remember "answer" "a-newer-value")
(setq *agent-tool-count* 0)
(agent-recall "answer")
(llm-check "agent-memory-recall-is-latest"
  (equal *agent-observation* "recall=>answer=a-newer-value"))
(setq *agent-tool-count* 0)
(agent-recall "ans")
(llm-check "agent-memory-recall-key-is-exact"
  (equal *agent-observation* "recall=>not-found"))

; The launcher has fixed argv slots, so a completed agent must replace a stale
; action with inert input before the following slot is opened.
; SISP strings have no escapes, so the stale action is written without any
; embedded string literal; only its being different from "nil" matters here.
(write-file *agent-action-path* "(agent-remember replayed bad)")
(setq *agent-action-count* 12)
(setq *agent-done* t)
(agent-prepare-action)
(llm-check "agent-completion-clears-stale-action"
  (and (= *agent-action-count* 0)
       (equal (read-file *agent-action-path*) "nil")))

(setq *agent-tool-count* 0)
(agent-eval (quote (+ 2 3)))
(llm-check "agent-eval-result" (= *agent-last-value* 5))
(llm-check "agent-eval-observation"
  (equal *agent-observation* "eval=>5"))

(setq *agent-tool-count* 0)
(setq *agent-action-recognized* nil)
(agent-eval (quote (car 0)))
(llm-check "agent-eval-error-is-caught"
  (and *agent-action-recognized*
       (equal *agent-action-kind* "eval")
       (equal (car *agent-last-value*) (quote error))
       (equal *agent-observation* "eval-error=>CAR: UNDEFINED")))

; The launcher processes this file next as a top-level SISP input.
(write-file *agent-action-path* "(agent-eval '(+ 9 4))")
(setq *agent-done* nil)
(setq *agent-tool-count* 0)
(setq *agent-action-recognized* nil)
