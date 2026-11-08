; external-agent.lsp -- resumable external-model boundary for the SISP agent.
;
; The transport writes *external-agent-response-path* and calls
; external-agent-accept.  SISP owns prompt construction, action validation,
; tool execution, observations, memory, turn budgets, and transcripts.

(define *external-agent-request-path* "ml/agent/generated/external/request.txt")
(define *external-agent-response-path* "ml/agent/generated/external/response.lsp")
(define *external-agent-status-path* "ml/agent/generated/external/status.txt")
(define *external-agent-final-path* "ml/agent/generated/external/final.txt")
(define *external-agent-goal-limit* 1024)
(define *external-agent-observation-limit* 512)
(define *external-agent-memory-limit* 1024)

(define *external-agent-tools*
  (quote (agent-eval agent-recall agent-remember agent-think agent-answer)))

(define (external-agent-tool-symbol-p value)
  (and (symbolp value) (in value *external-agent-tools*)))

(define (external-agent-tool-count form)
  (cond ((external-agent-tool-symbol-p form) 1)
        ((consp form)
          (+ (external-agent-tool-count (car form))
             (external-agent-tool-count (cdr form))))
        (t 0)))

(define (external-agent-direct-action-p form)
  (and (consp form)
       (external-agent-tool-symbol-p (car form))
       (= (external-agent-tool-count form) 1)))

(define (external-agent-build-prompt)
  (agent-context
    (list-to-str (list
      ";S:You are the model inside a SISP agent. Return exactly one action and no prose."
      *agent-newline*
      ";G:" (agent-prompt-field *agent-goal* *external-agent-goal-limit*)
      *agent-newline*
      ";O:" (agent-prompt-field *agent-observation*
               *external-agent-observation-limit*)
      *agent-newline*
      ";M:" (agent-prompt-field *agent-memory-text*
               *external-agent-memory-limit*)
      *agent-newline*
      ";A:(agent-eval 'FORM)|(agent-recall KEY)|(agent-remember KEY VALUE)|(agent-think TEXT)|(agent-answer VALUE)"
      *agent-newline*))))

(define (external-agent-reset-action)
  (setq *agent-action-text* "")
  (setq *agent-action-count* 0)
  (setq *agent-action-complete* nil)
  (setq *agent-tool-count* 0)
  (setq *agent-action-recognized* nil)
  (setq *agent-action-kind* "pending")
  (setq *agent-last-value* nil)
  nil)

(define (external-agent-write-status)
  (cond (*agent-done*
    (write-file *external-agent-status-path* "done")
    (write-file *external-agent-final-path* *agent-final-text*)
    "done")
  (t
    (write-file *external-agent-status-path* "continue")
    "continue")))

(define (external-agent-request)
  (cond (*agent-done* (external-agent-write-status))
        (t
          (agent-memory-refresh)
          (external-agent-reset-action)
          (setq *agent-prompt* (external-agent-build-prompt))
          (write-file *external-agent-request-path* *agent-prompt*)
          (write-file *external-agent-status-path* "request")
          (print (list (quote external-model-request)
            (quote turn) *agent-turn*
            (quote prompt-bytes) (length *agent-prompt*)
            (quote path) *external-agent-request-path*))
          *external-agent-request-path*)))

(define (external-agent-reject observation)
  (setq *agent-action-recognized* nil)
  (setq *agent-action-kind* "invalid")
  (setq *agent-last-value* nil)
  (setq *agent-observation* observation)
  nil)

(define (external-agent-evaluate-action)
  (catch
    (let ((forms (read-from-string *agent-action-text*)))
      (cond ((not (and (consp forms) (eq (cdr forms) nil)))
              (external-agent-reject "invalid=>expected-one-form"))
            ((not (external-agent-direct-action-p (car forms)))
              (external-agent-reject "invalid=>expected-one-direct-tool"))
            (t (eval (car forms)))))
    (lambda (caught-error)
      (setq *agent-action-recognized* nil)
      (setq *agent-action-kind* "error")
      (setq *agent-last-value* caught-error)
      (setq *agent-observation*
        (cat "error=>" (agent-render (cadr caught-error) 20)))
      nil)))

(define (external-agent-accept)
  (cond (*agent-done* (external-agent-write-status))
        (t
          (setq *agent-action-text*
            (read-file *external-agent-response-path*))
          (setq *agent-action-count* (length *agent-action-text*))
          (setq *agent-tool-count* 0)
          (setq *agent-action-recognized* nil)
          (setq *agent-action-kind* "pending")
          (print (list (quote external-model-response)
            (quote turn) *agent-turn*
            (quote bytes) *agent-action-count*
            (quote action) *agent-action-text*))
          (external-agent-evaluate-action)
          (agent-validate-action)
          (agent-record-turn)
          (agent-advance-turn)
          (external-agent-write-status))))

(define (external-agent-start)
  (agent-start)
  (write-file *external-agent-final-path* "")
  (write-file *external-agent-status-path* "started")
  t)

