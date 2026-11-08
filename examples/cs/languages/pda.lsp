; Pushdown automaton simulator.
; PDA = (states, alphabet, stack-alphabet, transitions, start, accept).
; Transition: (state, input, pop, new-state, push-list).
; Accept by final state and empty stack.
; Language: {a^n b^n | n >= 0}.

(define pda-states '(q0 q1 q2))
(define pda-alphabet '("a" "b"))
(define pda-stack-syms '("Z" "A"))
(define pda-start 'q0)
(define pda-accepting '(q2))
(define pda-initial-stack '("Z"))

(define (pda-transition state input pop)
  (cond
    ((and (eq state 'q0) (equal input "a") (equal pop "Z"))
     (list 'q0 (list "A" "Z")))
    ((and (eq state 'q0) (equal input "a") (equal pop "A"))
     (list 'q0 (list "A" "A")))
    ((and (eq state 'q0) (equal input "") (equal pop "Z"))
     (list 'q2 '()))
    ((and (eq state 'q0) (equal input "b") (equal pop "A"))
     (list 'q1 '()))
    ((and (eq state 'q1) (equal input "b") (equal pop "A"))
     (list 'q1 '()))
    ((and (eq state 'q1) (equal input "") (equal pop "Z"))
     (list 'q2 '()))
    (t nil)))

(define (pda-step config input-sym)
  (let ((state (car config))
        (stack (cadr config)))
    (cond ((eq stack nil) nil)
          (t (let ((top (car stack))
                   (rest (cdr stack)))
               (let ((tr (pda-transition state input-sym top)))
                 (cond ((eq tr nil) nil)
                       (t (list (car tr) (append (cadr tr) rest))))))))))

(define (pda-finish config)
  (cond ((eq config nil) nil)
        ((and (contains (car config) pda-accepting) (eq (cadr config) nil)) t)
        (t (let ((next (pda-step config "")))
             (cond ((eq next nil) nil)
                   ((equal next config) nil)
                   (t (pda-finish next)))))))

(define (pda-process config input-tail)
  (cond ((eq config nil) nil)
        ((eq input-tail nil) (pda-finish config))
        (t (pda-process (pda-step config (car input-tail)) (cdr input-tail)))))

(define (pda-accept input-string)
  (let ((chars (cond ((eq input-string "") nil)
                      ((equal (typeof input-string) "string") (str-to-list input-string))
                      (t input-string))))
    (let ((start-config (list pda-start pda-initial-stack)))
      (pda-process start-config chars))))

(print "=== Pushdown Automaton ===")
(print "Language: {a^n b^n | n >= 0}")
(print "Accept empty (n=0):") (print (pda-accept ""))
(print "Accept ab (n=1):") (print (pda-accept "ab"))
(print "Accept aabb (n=2):") (print (pda-accept "aabb"))
(print "Accept aaabbb (n=3):") (print (pda-accept "aaabbb"))
(print "Reject aab:") (print (pda-accept "aab"))
(print "Reject abb:") (print (pda-accept "abb"))
(print "Reject ba:")  (print (pda-accept "ba"))
(print "Reject aba:") (print (pda-accept "aba"))

; CFG equivalence: S -> aSb | epsilon generates {a^n b^n}
(print "--- CFG equivalence ---")
(define (cfg-anbn-derive n)
  (cond ((= n 0) "")
        (t (cat "a" (cat (cfg-anbn-derive (- n 1)) "b")))))
(print "CFG S->aSb|e generates:") (print (cfg-anbn-derive 3))
(print "PDA accepts it:") (print (pda-accept (cfg-anbn-derive 3)))
(print "=== PDA Done ===")
