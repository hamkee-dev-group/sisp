; Two-way finite automaton (2DFA) simulation.
; Read head can move left (-1), right (+1), or stay (0).
; Accept when entering a final state.
; Language: strings containing substring "ab".
; This particular machine only ever moves right, so it behaves as a
; one-way DFA; the move field shows where a general 2DFA differs.

(print "=== Two-Way Finite Automaton (2DFA) ===")

(define twdfa-states '(q0 q1 q2 qf))
(define twdfa-start 'q0)
(define twdfa-accepting '(qf))

; Transition: (state, input-symbol) -> (new-state, move-direction)
; move-direction: -1 = left, +1 = right, 0 = stay
(define (twdfa-delta state sym)
  (cond
    ((and (eq state 'q0) (equal sym "a")) (list 'q1 1))
    ((and (eq state 'q0) (equal sym "b")) (list 'q0 1))
    ((and (eq state 'q1) (equal sym "b")) (list 'qf 1))
    ((and (eq state 'q1) (equal sym "a")) (list 'q1 1))
    ((eq state 'qf) (list 'qf 1))
    (t (list 'q0 1))))

(define (twdfa-sim state pos chars n steps)
  (cond ((contains state twdfa-accepting) (list 'accept steps))
        ((= steps 200) (list 'reject steps))
        ((or (< pos 0) (>= pos n)) (list 'reject steps))
        (t
          (let ((sym (nth pos chars)))
            (let ((tr (twdfa-delta state sym)))
              (let ((next (car tr))
                    (move (cadr tr)))
                (twdfa-sim next (+ pos move) chars n (+ steps 1))))))))

(define (twdfa-accept input)
  (let ((result (twdfa-sim twdfa-start 0 (str-to-list input)
                           (length input) 0)))
    (eq (car result) 'accept)))

; Shepherdson's construction: 2DFA equivalent to DFA.
; For substring "ab", the equivalent DFA is:
; q0 (last char not a), q1 (last char was a), q2 (saw ab -> accept).
(define (equiv-dfa-accept input)
  (let ((chars (str-to-list input)))
    (let ((final (foldl (lambda (s c)
                   (cond ((and (= s 0) (equal c "a")) 1)
                         ((and (= s 0) (equal c "b")) 0)
                         ((and (= s 1) (equal c "a")) 1)
                         ((and (= s 1) (equal c "b")) 2)
                         ((= s 2) 2)
                         (t s)))
                 0 chars)))
      (= final 2))))

; ---- checks ----

(check "2DFA accepts ab" (twdfa-accept "ab"))
(check "2DFA accepts aab" (twdfa-accept "aab"))
(check "2DFA accepts bab" (twdfa-accept "bab"))
(check "2DFA accepts xabx" (twdfa-accept "xabx"))
(check "2DFA rejects a" (not (twdfa-accept "a")))
(check "2DFA rejects b" (not (twdfa-accept "b")))
(check "2DFA rejects ba" (not (twdfa-accept "ba")))
(check "2DFA rejects empty" (not (twdfa-accept "")))

; 2DFA and equivalent DFA agree on every input
(check "2DFA = DFA on all probe strings"
       (every (lambda (s)
                (eq (twdfa-accept s) (equiv-dfa-accept s)))
              (list "" "a" "b" "ab" "ba" "aab" "bab" "aba" "bba"
                    "aa" "bb" "abab" "baba" "aabb" "bbaa" "xabx")))
