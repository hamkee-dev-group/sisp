; pcp-verifier.lsp -- PCP verifier for 3SAT.
; Input: 3CNF formula, proof (variable assignment).
; Verifier: picks 3 random clauses, reads the proof bits for those variables.
; Accepts iff all 3 clauses are satisfied by the assignment.
; If SAT, accepts with prob 1. If UNSAT, accepts with prob < (1-1/m)^3 < 1.
; With repetition, gap amplifies: accept ≥ 0.5 if SAT, ≤ 0.25 if UNSAT.

(define (pcp-lit-var lit)
  (cond ((consp lit) (cadr lit)) (t lit)))

(define (pcp-lit-neg lit)
  (consp lit))

(define (pcp-eval-lit lit asgn)
  (let ((v (pcp-lit-var lit))
        (ng (pcp-lit-neg lit)))
    (let ((p (assoc v asgn)))
      (cond ((eq p nil) nil)
            (ng (eq (cadr p) nil))
            (t (eq (cadr p) t))))))

(define (pcp-clause-ok clause asgn)
  (any (lambda (lit) (pcp-eval-lit lit asgn)) clause))

(define (pcp-random-index m)
  (mod (floor (* (random) m)) m))

(define (pcp-pick-clauses formula k)
  (let ((m (length formula)))
    (pcp-pick-iter formula m k nil)))

(define (pcp-pick-iter formula m k acc)
  (cond ((= k 0) acc)
        (t (let ((i (pcp-random-index m)))
             (pcp-pick-iter formula m (- k 1)
               (cons (nth i formula) acc))))))

(define (pcp-verify-once formula proof)
  (let ((clauses (pcp-pick-clauses formula 3)))
    (every (lambda (c) (pcp-clause-ok c proof)) clauses)))

(define (pcp-count-accept formula proof remaining acc)
  (cond ((= remaining 0) acc)
        (t (pcp-count-accept formula proof (- remaining 1)
             (+ acc (cond ((pcp-verify-once formula proof) 1) (t 0)))))))

(define (pcp-accept-rate formula proof samples)
  (/ (pcp-count-accept formula proof samples 0) samples))

; --- test ---
; SAT: (a|b|c) & (~a|b|c) & (a|~b|c), satisfiable (a=t,b=t,c=t works)
(define pcp-sat '((a b c) ((not a) b c) (a (not b) c)))
(define pcp-sat-model '((a t) (b t) (c t)))

; UNSAT: (a) & (~a) — no assignment satisfies both
(define pcp-unsat '((a) ((not a))))
(define pcp-unsat-model '((a t)))

(define pcp-unsat2 '((a b) ((not a) b) (a (not b)) ((not a) (not b))))
(define pcp-unsat2-model '((a t) (b t)))

(let ((sat-rate (pcp-accept-rate pcp-sat pcp-sat-model 100))
      (unsat-rate (pcp-accept-rate pcp-unsat pcp-unsat-model 100))
      (unsat2-rate (pcp-accept-rate pcp-unsat2 pcp-unsat2-model 100)))
  (list
    (list (quote sat-formula) sat-rate (quote high) (> sat-rate 0.8))
    (list (quote unsat-formula) unsat-rate (quote low) (< unsat-rate 0.5))
    (list (quote unsat2-formula) unsat2-rate (quote low) (< unsat2-rate 0.5))))
