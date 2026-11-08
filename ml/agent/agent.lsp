; agent.lsp — a tiny model-driven SISP research agent.
;
; The model emits one ordinary SISP form. The launcher writes that text to a
; temporary .lsp file and presents it to SISP as the next top-level input.
; Recognized agent tool functions update the observation consumed by the next
; model turn. This intentionally accepts the risks of executing experimental
; model output.

(progn
(print "=== tiny SISP research agent loaded ===")

(define *agent-goal* "")
(define *agent-observation* "start")
(define *agent-memory-text* "")
(define *agent-prompt* "")
(define *agent-generated-ids* nil)
(define *agent-action-text* "")
(define *agent-action-count* 0)
(define *agent-action-complete* nil)
(define *agent-action-depth* 0)
(define *agent-action-started* nil)
(define *agent-action-in-string* nil)
(define *agent-tool-count* 0)
(define *agent-action-recognized* nil)
(define *agent-action-kind* "none")
(define *agent-last-value* nil)
(define *agent-final-answer* nil)
(define *agent-final-text* "")
(define *agent-turn* 0)
(define *agent-done* nil)
(define *agent-newline*
  (nth (- (length *matrix-vocabulary*) 1) *matrix-vocabulary*))

(define (agent-tail text maximum)
  (cond ((<= (length text) maximum) text)
        (t (substr (- (length text) maximum) maximum text))))

(define (agent-context text)
  (agent-tail text *matrix-context*))

; A field occupies one comment line in the policy prompt.  Replace embedded
; newlines so memory or tool output cannot create a forged ;A:/;O: protocol line.
(define (agent-prompt-safe-characters characters)
  (cond ((eq characters nil) nil)
        ((equal (car characters) *agent-newline*)
          (cons " " (agent-prompt-safe-characters (cdr characters))))
        (t (cons (car characters)
             (agent-prompt-safe-characters (cdr characters))))))

(define (agent-drop-leading-spaces characters)
  (cond ((eq characters nil) nil)
        ((equal (car characters) " ")
          (agent-drop-leading-spaces (cdr characters)))
        (t characters)))

(define (agent-prompt-field text maximum)
  (list-to-str
    (reverse (agent-drop-leading-spaces (reverse
      (agent-prompt-safe-characters (str-to-list (agent-tail text maximum))))))))

(define (agent-render-list values remaining)
  (cond ((eq values nil) "")
        ((= remaining 0) "...")
        (t (cat (agent-render (car values) (- remaining 1))
          (cond ((eq (cdr values) nil) "")
                (t (cat " "
                  (agent-render-list (cdr values) (- remaining 1)))))))))

(define (agent-render value remaining)
  (cond ((eq value nil) "nil")
        ((eq value t) "t")
        ((numberp value) (num-to-str value))
        ((equal (typeof value) "string") value)
        ((equal (typeof value) "matrix")
          (cat "#<matrix "
            (cat (num-to-str (mat-rows value))
              (cat "x" (cat (num-to-str (mat-cols value)) ">")))))
        ((and (consp value) (> remaining 0))
          (cat "(" (cat (agent-render-list value remaining) ")")))
        ((symbolp value) "<symbol>")
        (t "<object>")))

(define (agent-memory-refresh)
  (setq *agent-memory-text* (read-file *agent-memory-path*)))

(define (agent-memory-latest-line characters current prefix latest)
  (cond ((eq characters nil)
    (let ((line (list-to-str (reverse current))))
      (cond ((equal (str-find line prefix) 0) line) (t latest))))
    ((equal (car characters) *agent-newline*)
      (let ((line (list-to-str (reverse current))))
        (agent-memory-latest-line (cdr characters) nil prefix
          (cond ((equal (str-find line prefix) 0) line) (t latest)))))
    (t (agent-memory-latest-line (cdr characters)
         (cons (car characters) current) prefix latest))))

(define (agent-memory-window key)
  (let ((line (agent-memory-latest-line
    (str-to-list *agent-memory-text*) nil (cat key "=") nil)))
    (cond ((eq line nil) "not-found")
          (t (agent-tail line 32)))))

; Tools available to a generated action form.
(define (agent-tool-begin kind)
  (setq *agent-tool-count* (+ *agent-tool-count* 1))
  (cond ((= *agent-tool-count* 1)
    (setq *agent-action-recognized* t)
    (setq *agent-action-kind* kind)
    t)
    (t
      (setq *agent-action-recognized* nil)
      (setq *agent-action-kind* "multiple")
      (setq *agent-observation* "invalid=>multiple-tools")
      nil)))

(define (agent-eval form)
  (cond ((agent-tool-begin "eval")
    (catch
      (progn
        (setq *agent-last-value* (eval form))
        (cond ((= *agent-tool-count* 1)
          (setq *agent-observation*
            (cat "eval=>" (agent-render *agent-last-value* 20)))) (t nil))
        (print (list (quote agent-tool) (quote eval) *agent-last-value*))
        *agent-last-value*)
      (lambda (caught-error)
        (setq *agent-last-value* caught-error)
        (setq *agent-observation*
          (cat "eval-error=>" (agent-render (cadr caught-error) 20)))
        (print (list (quote agent-tool) (quote eval-error) caught-error))
        nil)))
    (t nil)))

(define (agent-remember key value)
  (cond ((agent-tool-begin "remember")
  (let ((key-text (agent-prompt-field (agent-render key 10) 24))
        (value-text (agent-prompt-field (agent-render value 20) 40)))
    (append-file *agent-memory-path*
      (cat key-text (cat "=" (cat value-text *agent-newline*))))
    (agent-memory-refresh)
    (setq *agent-observation* (cat "remembered:" key-text))
    *agent-observation*))
    (t nil)))

(define (agent-recall key)
  (cond ((agent-tool-begin "recall")
  (agent-memory-refresh)
  (let ((key-text (agent-render key 10)))
    (setq *agent-observation*
      (cat "recall=>" (agent-memory-window key-text)))
    *agent-observation*))
    (t nil)))

(define (agent-think text)
  (cond ((agent-tool-begin "think")
    (setq *agent-observation* (cat "thought:" (agent-render text 20)))
    *agent-observation*)
    (t nil)))

(define (agent-answer answer)
  (cond ((agent-tool-begin "answer")
  (setq *agent-final-answer* answer)
  (setq *agent-final-text* (agent-render answer 30))
  (setq *agent-observation* (cat "answer=>" *agent-final-text*))
  (setq *agent-done* t)
  answer)
  (t nil)))

(define (agent-validate-action)
  (cond (*agent-done* nil)
    (*agent-action-recognized* t)
    (t
      (setq *agent-action-kind* "invalid")
      (setq *agent-last-value* nil)
      (setq *agent-observation*
        (cat "invalid=>" (agent-tail *agent-action-text* 40)))
      (print (list (quote agent-action-invalid)
        (quote turn) *agent-turn* (quote action) *agent-action-text*))
      nil)))

(define (agent-build-prompt)
  (list-to-str (list
    ";G:" (agent-prompt-field *agent-goal* 70) *agent-newline*
    ";O:" (agent-prompt-field *agent-observation* 40) *agent-newline*
    ";M:" (agent-prompt-field *agent-memory-text* 24) *agent-newline*
    ";A:(agent-eval 'F)|(agent-recall K)|(agent-remember K V)|(agent-think T)|(agent-answer X)"
    *agent-newline*)))

(define (agent-start)
  (setq *agent-goal* (read-file *agent-goal-path*))
  (agent-memory-refresh)
  (setq *agent-observation* "start")
  (setq *agent-turn* 0)
  (setq *agent-done* nil)
  (setq *agent-final-answer* nil)
  (setq *agent-final-text* "")
  (append-file *agent-transcript-path*
    (list-to-str (list *agent-newline* "=== agent run ==="
      *agent-newline* "goal:" *agent-newline* *agent-goal* *agent-newline*)))
  (print (list (quote agent-start) (quote goal) *agent-goal*
    (quote turns) *agent-max-turns* (quote action-chars) *agent-action-chars*))
  t)

(define (agent-prepare-action)
  (cond (*agent-done*
    ; The shell has already scheduled every possible turn.  Clear the next
    ; action slot so an early answer cannot replay the previous generated file.
    (setq *agent-action-count* 0)
    (write-file *agent-action-path* "nil")
    nil)
    (t
      (setq *agent-prompt* (agent-context (agent-build-prompt)))
      (setq *agent-action-recognized* nil)
      (setq *agent-action-kind* "pending")
      (setq *agent-last-value* nil)
      (setq *agent-generated-ids*
        (llm-encode *agent-prompt* *matrix-vocabulary*))
      (setq *agent-action-text* "")
      (setq *agent-action-count* 0)
      (setq *agent-action-complete* nil)
      (setq *agent-action-depth* 0)
      (setq *agent-action-started* nil)
      (setq *agent-action-in-string* nil)
      (setq *agent-tool-count* 0)
      t)))

(define (agent-observe-action-character character)
  (cond (*agent-action-in-string*
    (cond ((equal character (nth 2 *matrix-vocabulary*))
      (setq *agent-action-in-string* nil)) (t nil)))
    ((equal character (nth 2 *matrix-vocabulary*))
      (setq *agent-action-in-string* t))
    ((equal character "(")
      (setq *agent-action-started* t)
      (setq *agent-action-depth* (+ *agent-action-depth* 1)))
    ((equal character ")")
      (setq *agent-action-depth* (max 0 (- *agent-action-depth* 1)))
      (cond ((and *agent-action-started* (= *agent-action-depth* 0))
        (setq *agent-action-complete* t)) (t nil)))
    ((equal character *agent-newline*)
      (setq *agent-action-complete* t))
    (t nil)))

; Called as a separate top-level form for every character so inference caches
; can be collected between tokens.
(define (agent-generation-step)
  (cond ((or *agent-done* *agent-action-complete*) nil)
    (t (let ((next
      (mtransformer-next-token-top-k-top-p *matrix-model*
        *agent-generated-ids* *agent-temperature* *agent-top-k* *agent-top-p*)))
      (let ((character (nth next *matrix-vocabulary*)))
        (setq *agent-generated-ids*
          (append *agent-generated-ids* (list next)))
        (setq *agent-action-count* (+ *agent-action-count* 1))
        (agent-observe-action-character character)
        next)))))

(define (agent-finish-action)
  (cond (*agent-done* nil)
    (t (let ((generated
      (llm-decode *agent-generated-ids* *matrix-vocabulary*)))
      (setq *agent-action-text*
        (substr (length *agent-prompt*) *agent-action-count* generated))
      (setq *agent-action-recognized* nil)
      (setq *agent-tool-count* 0)
      (setq *agent-action-kind* "invalid")
      (setq *agent-observation* "invalid-action")
      (write-file *agent-action-path* *agent-action-text*)
      (print (list (quote agent-proposed) (quote turn) *agent-turn*
        (quote action) *agent-action-text*))
      *agent-action-text*))))

(define (agent-record-turn)
  (cond ((= *agent-action-count* 0) nil)
    (t (append-file *agent-transcript-path*
      (list-to-str (list "turn=" (num-to-str *agent-turn*)
        *agent-newline* "action:" *agent-newline* *agent-action-text*
        *agent-newline* "observation:" *agent-newline*
        *agent-observation* *agent-newline*))))))

(define (agent-advance-turn)
  (cond (*agent-done* nil)
    (t
      (setq *agent-turn* (+ *agent-turn* 1))
      (cond ((>= *agent-turn* *agent-max-turns*)
        (setq *agent-done* t)
        (setq *agent-final-text* "step-budget-exhausted")
        (setq *agent-final-answer* *agent-final-text*))
        (t nil)))))

(define (agent-report)
  (print (list (quote agent-finished)
    (quote turns-used) *agent-turn*
    (quote done) *agent-done*
    (quote last-action) *agent-action-kind*
    (quote answer) *agent-final-answer*
    (quote observation) *agent-observation*
    (quote transcript) *agent-transcript-path*))
  *agent-final-answer*)
)
