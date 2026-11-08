; ============================================================
; name: viterbi
; file: algorithms/ai/viterbi.lsp
; subject: ai
; capability: decode inference
; aliases: "viterbi" "hmm decoding" "most likely state sequence"
; signature: (viterbi states initial transitions emissions observations) -> state-path
; complexity: O(T * S^2)
; see-also: (none)
; derived-from: Viterbi 1967
; status: example (tested)
; checks: 2 - most-likely path on a known HMM
; ============================================================
; Viterbi decoding retains the highest-probability path ending in each state.
(define (vt-pair-get pairs key)
  (cadr (find (lambda (entry) (equal (car entry) key)) pairs)))
(define (vt-triple-get triples first second)
  (caddr (find (lambda (entry)
    (and (equal (car entry) first) (equal (cadr entry) second))) triples)))
(define (vt-start states initial emissions observation)
  (map (lambda (state) (list state
    (* (vt-pair-get initial state) (vt-triple-get emissions state observation))
    (list state))) states))
(define (vt-best-previous previous transitions state)
  (foldl (lambda (best entry)
    (let ((score (* (cadr entry) (vt-triple-get transitions (car entry) state))))
      (cond ((or (eq best nil) (> score (car best))) (list score entry)) (t best))))
    nil previous))
; Each dynamic-programming cell stores state, score, and its best complete path.
(define (vt-step states previous transitions emissions observation)
  (map (lambda (state)
    (let ((best (vt-best-previous previous transitions state)))
      (list state (* (car best) (vt-triple-get emissions state observation))
        (append (caddr (cadr best)) (list state))))) states))
(define (vt-run states transitions emissions observations paths)
  (cond ((eq observations nil) paths)
        (t (vt-run states transitions emissions (cdr observations)
             (vt-step states paths transitions emissions (car observations))))))
(define (viterbi states initial transitions emissions observations)
  (let ((paths (vt-run states transitions emissions (cdr observations)
                 (vt-start states initial emissions (car observations)))))
    (caddr (foldl (lambda (best entry)
      (cond ((or (eq best nil) (> (cadr entry) (cadr best))) entry) (t best))) nil paths))))
(viterbi '(healthy fever) '((healthy 3/5) (fever 2/5)) '((healthy healthy 7/10) (healthy fever 3/10) (fever healthy 2/5) (fever fever 3/5)) '((healthy normal 1/2) (healthy cold 2/5) (healthy dizzy 1/10) (fever normal 1/10) (fever cold 3/10) (fever dizzy 3/5)) '(normal cold dizzy)) ; => (healthy healthy fever)
(length (viterbi '(a b) '((a 1/2) (b 1/2)) '((a a 1/2) (a b 1/2) (b a 1/2) (b b 1/2)) '((a x 1) (b x 1)) '(x x))) ; => 2

; ---- checks ----
(check "viterbi: weather example -> (healthy healthy fever)"
       (equal (viterbi '(healthy fever) '((healthy 3/5) (fever 2/5))
               '((healthy healthy 7/10) (healthy fever 3/10)
                 (fever healthy 2/5) (fever fever 3/5))
               '((healthy normal 1/2) (healthy cold 2/5) (healthy dizzy 1/10)
                 (fever normal 1/10) (fever cold 3/10) (fever dizzy 3/5))
               '(normal cold dizzy))
              '(healthy healthy fever)))

(check "viterbi: trivial two-state path has length 2"
       (= (length (viterbi '(a b) '((a 1/2) (b 1/2))
                   '((a a 1/2) (a b 1/2) (b a 1/2) (b b 1/2))
                   '((a x 1) (b x 1)) '(x x)))
          2))
