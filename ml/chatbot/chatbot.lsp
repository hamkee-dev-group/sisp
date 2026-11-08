; chatbot.lsp -- human-facing native SISP transformer and research-agent API.
;
; A local bootstrap loads the transformer libraries, one or more checkpoints,
; ml/agent/agent.lsp, and then this file.  No Python is involved after
; startup.  Generation, memory, tool selection, parsing, and evaluation all
; execute in SISP.

(define *chatbot-models* nil)
(define *chatbot-model-name* "")
(define *chatbot-count* 96)
(define *chatbot-temperature* 0.3)
(define *chatbot-top-k* 1)
(define *chatbot-top-p* 1.0)
(define *chatbot-agent-turns* 3)
(define *chatbot-agent-action-chars* 96)
(define *chatbot-memory-path* "ml/chatbot/local/memory.txt")
(define *chatbot-transcript-path* "ml/chatbot/local/transcript.txt")
(define *chatbot-action-path* "ml/chatbot/local/action.lsp")
(define *chatbot-goal-path* "ml/chatbot/local/goal.txt")

; Preserve the complete inference state.  This lets a bootstrap load several
; checkpoints and switch between them without restarting SISP.
(define (chatbot-register-current name)
  (setq *chatbot-models*
    (cons (list name *matrix-profile* *matrix-profile-name*
      *matrix-context* *matrix-dimension* *matrix-heads* *matrix-layers*
      *matrix-hidden* *matrix-model* *matrix-parameters*
      *matrix-vocabulary*)
      *chatbot-models*))
  name)

(define (chatbot-find-model name models)
  (cond ((eq models nil) nil)
        ((equal name (car (car models))) (car models))
        (t (chatbot-find-model name (cdr models)))))

(define (chatbot-model-names models)
  (cond ((eq models nil) nil)
        (t (cons (car (car models))
             (chatbot-model-names (cdr models))))))

(define (chatbot-use-model name)
  (let ((entry (chatbot-find-model name *chatbot-models*)))
    (cond ((eq entry nil)
      (print (list (quote unknown-model) name
        (quote available) (chatbot-model-names *chatbot-models*)))
      nil)
    (t
      (setq *chatbot-model-name* (nth 0 entry))
      (setq *matrix-profile* (nth 1 entry))
      (setq *matrix-profile-name* (nth 2 entry))
      (setq *matrix-context* (nth 3 entry))
      (setq *matrix-dimension* (nth 4 entry))
      (setq *matrix-heads* (nth 5 entry))
      (setq *matrix-layers* (nth 6 entry))
      (setq *matrix-hidden* (nth 7 entry))
      (setq *matrix-model* (nth 8 entry))
      (setq *matrix-parameters* (nth 9 entry))
      (setq *matrix-vocabulary* (nth 10 entry))
      (print (list (quote model) *chatbot-model-name*
        (quote profile) *matrix-profile*
        (quote context) *matrix-context*
        (quote parameters) (mparams-count *matrix-parameters*)))
      name))))

(define (chatbot-generate-walk history remaining)
  (cond ((= remaining 0) history)
        (t (chatbot-generate-walk
          (append history
            (list (mtransformer-next-token-top-k-top-p *matrix-model*
              history *chatbot-temperature* *chatbot-top-k*
              *chatbot-top-p*)))
          (- remaining 1)))))

(define (chatbot-complete-n prompt count)
  (let ((prompt-ids (llm-encode prompt *matrix-vocabulary*)))
    (matrix-checkpoint-require (> (length prompt-ids) 0)
      "chatbot prompt must not be empty")
    (let ((all-ids (chatbot-generate-walk prompt-ids count)))
      (llm-decode (drop all-ids (length prompt-ids)) *matrix-vocabulary*))))

(define (chatbot-next-token-id prompt)
  (let ((prompt-ids (llm-encode prompt *matrix-vocabulary*)))
    (matrix-checkpoint-require (> (length prompt-ids) 0)
      "chatbot prompt must not be empty")
    (mtransformer-next-token-top-k-top-p *matrix-model* prompt-ids 0.3 1 1.0)))

(define (chatbot-complete prompt)
  (chatbot-complete-n prompt *chatbot-count*))

(define (chatbot-newline)
  (nth (- (length *matrix-vocabulary*) 1) *matrix-vocabulary*))

(define (chatbot-code prompt)
  (let ((answer (chatbot-complete prompt)))
    (print answer)
    answer))

(define (chatbot-ask question)
  (let ((answer
    (chatbot-complete
      (cat "; prompt: "
        (cat question (cat (chatbot-newline) "; response:"))))))
    (print answer)
    answer))

(define (chatbot-help name)
  (let ((answer
    (chatbot-complete (cat "; help: " (cat name (chatbot-newline))))))
    (print answer)
    answer))

(define (chatbot-algorithm name)
  (let ((answer
    (chatbot-complete (cat "; algorithm: " (cat name (chatbot-newline))))))
    (print answer)
    answer))

(define (chatbot-sampling deterministic)
  (cond (deterministic
    (setq *chatbot-temperature* 0.3)
    (setq *chatbot-top-k* 1)
    (setq *chatbot-top-p* 1.0))
  (t
    (setq *chatbot-temperature* 0.8)
    (setq *chatbot-top-k* 20)
    (setq *chatbot-top-p* 0.95)))
  (list *chatbot-temperature* *chatbot-top-k* *chatbot-top-p*))

; Generate one complete agent action in-process.  read-from-string exposes the
; interpreter reader, so the old temporary-file scheduling bridge is not needed.
(define (chatbot-agent-generate remaining)
  (cond ((or (= remaining 0) *agent-action-complete*) nil)
        (t (agent-generation-step)
           (chatbot-agent-generate (- remaining 1)))))

(define (chatbot-agent-eval-action)
  (catch
    (let ((forms (read-from-string *agent-action-text*)))
      (cond ((and (consp forms) (eq (cdr forms) nil))
        (eval (car forms)))
      (t
        (setq *agent-action-recognized* nil)
        (setq *agent-observation* "invalid=>expected-one-form")
        nil)))
    (lambda (caught-error)
      (setq *agent-action-recognized* nil)
      (setq *agent-action-kind* "error")
      (setq *agent-last-value* caught-error)
      (setq *agent-observation*
        (cat "error=>" (agent-render (cadr caught-error) 20)))
      nil)))

(define (chatbot-agent-loop remaining)
  (cond ((or (= remaining 0) *agent-done*) *agent-final-answer*)
        (t
          (agent-prepare-action)
          (chatbot-agent-generate *agent-action-chars*)
          (agent-finish-action)
          (chatbot-agent-eval-action)
          (agent-validate-action)
          (agent-record-turn)
          (agent-advance-turn)
          (chatbot-agent-loop (- remaining 1)))))

(define (chatbot-agent goal)
  (write-file *chatbot-goal-path* goal)
  (define *agent-goal-path* *chatbot-goal-path*)
  (define *agent-memory-path* *chatbot-memory-path*)
  (define *agent-transcript-path* *chatbot-transcript-path*)
  (define *agent-action-path* *chatbot-action-path*)
  (define *agent-max-turns* *chatbot-agent-turns*)
  (define *agent-action-chars* *chatbot-agent-action-chars*)
  (define *agent-temperature* *chatbot-temperature*)
  (define *agent-top-k* *chatbot-top-k*)
  (define *agent-top-p* *chatbot-top-p*)
  (agent-start)
  (chatbot-agent-loop *agent-max-turns*)
  (agent-report))

(define (chatbot-status)
  (list (quote model) *chatbot-model-name*
    (quote available) (chatbot-model-names *chatbot-models*)
    (quote count) *chatbot-count*
    (quote sampling)
      (list *chatbot-temperature* *chatbot-top-k* *chatbot-top-p*)
    (quote memory) *chatbot-memory-path*))

(define (chatbot-ready)
  (print "=== native SISP chatbot ready ===")
  (print "chatbot-code: code completion")
  (print "chatbot-ask: SISP question")
  (print "chatbot-help: explain a name")
  (print "chatbot-algorithm: named algorithm")
  (print "chatbot-agent: tiny tool-using agent")
  (print "chatbot-use-model: switch loaded model")
  (print "(chatbot-status)                 current settings")
  t)
