; ============================================================
; 19 — Finite Automata via Set Theory
; DFA simulation — states/alphabet/transitions as sets
; UNIQUE: model automata purely via set-theoretic objects
; ============================================================

(print "=== Finite Automata ===")

; DFA that accepts strings with even number of 1's
(define states {even odd})
(define alphabet {0 1})
(define accept-states {even})
(print "DFA: even number of 1's")
(print "States:") (print states)
(print "Accept:") (print accept-states)

; Transition function
(define (delta state sym)
  (cond ((and (eq state 'even) (= sym 0)) 'even)
        ((and (eq state 'even) (= sym 1)) 'odd)
        ((and (eq state 'odd)  (= sym 0)) 'odd)
        ((and (eq state 'odd)  (= sym 1)) 'even)))

; Simulate DFA
(define (run-dfa input)
  (labels (((go state inp)
    (cond ((null inp) state)
          (t (go (delta state (car inp)) (cdr inp))))))
    (go 'even input)))

(define (accepts input)
  (in (run-dfa input) accept-states))

(print "--- Simulations ---")
(print "empty       →") (print (run-dfa nil))
(print "(0)         →") (print (run-dfa '(0)))
(print "(1)         →") (print (run-dfa '(1)))
(print "(1 1)       →") (print (run-dfa '(1 1)))
(print "(1 0 1)     →") (print (run-dfa '(1 0 1)))
(print "(1 1 1)     →") (print (run-dfa '(1 1 1)))
(print "(0 1 0 1 0) →") (print (run-dfa '(0 1 0 1 0)))

(print "--- Acceptance ---")
(print "empty accepted?")    (print (accepts nil))
(print "(1 1) accepted?")   (print (accepts '(1 1)))
(print "(1 1 1) accepted?") (print (accepts '(1 1 1)))
(print "(1 0 1) accepted?") (print (accepts '(1 0 1)))
(print "(1) accepted?")     (print (accepts '(1)))

; --- Subset construction: NFA → DFA ---
(print "--- NFA Subset Construction ---")
(print "NFA with states {q0 q1 q2}")
(define nfa-states {q0 q1 q2})
(define dfa-power-states (pow nfa-states))
(print "|P(Q)| =") (print (ord dfa-power-states))
(print "DFA may need up to 2^|Q| states")

; Verify powerset contains specific subsets
(print "{q0,q1} in P(Q)?") (print (in {q0 q1} dfa-power-states))
(print "{q0} in P(Q)?") (print (in {q0} dfa-power-states))

(print "=== Finite Automata Done ===")
