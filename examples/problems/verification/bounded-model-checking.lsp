; Bounded Model Checking: unroll program, encode as SAT.
; DPLL SAT solver. If SAT: counterexample found. If UNSAT: safe up to bound.

(define (bmc-pick lit)
  (cond ((and (listp lit) (eq (car lit) 'not)) (cadr lit)) (t lit)))

(define (bmc-neg lit)
  (cond ((and (listp lit) (eq (car lit) 'not)) (cadr lit)) (t (list 'not lit))))

(define (bmc-has lit cls)
  (any (lambda (l) (equal l lit)) cls))

(define (bmc-hasn lit cls)
  (any (lambda (l) (cond ((and (listp l) (eq (car l) 'not) (equal (cadr l) lit)) t) (t nil))) cls))

(define (bmc-unit clauses)
  (cond ((eq clauses nil) nil)
        ((= (length (car clauses)) 1) (car (car clauses)))
        (t (bmc-unit (cdr clauses)))))

(define (bmc-empty clauses)
  (cond ((eq clauses nil) nil) (t (any (lambda (c) (eq c nil)) clauses))))

(define (bmc-simplify lit sign clauses)
  (cond ((eq clauses nil) nil)
        (t (let* ((c (car clauses))
                  (has (bmc-has lit c))
                  (hasn (bmc-hasn lit c))
                  (rest (bmc-simplify lit sign (cdr clauses))))
             (cond ((cond (sign has) (t nil)) rest)
                   ((cond (sign hasn) (t nil))
                    (cons (remove (lambda (l) (cond ((and (listp l) (eq (car l) 'not) (equal (cadr l) lit)) t) (t nil))) c) rest))
                   ((cond (sign nil) (t hasn)) rest)
                   ((cond (sign nil) (t has))
                    (cons (remove (lambda (l) (equal l lit)) c) rest))
                   (t (cons c rest)))))))

(define (bmc-dpll clauses depth)
  (cond ((> depth 12) nil)
        ((eq clauses nil) t)
        ((bmc-empty clauses) nil)
        (t (let ((u (bmc-unit clauses)))
             (cond (u (let ((sign (cond ((and (listp u) (eq (car u) 'not)) nil) (t t))))
                        (bmc-dpll (bmc-simplify (bmc-pick u) sign clauses) (+ depth 1))))
                   (t (let ((lit (caar clauses)))
                        (cond ((bmc-dpll (bmc-simplify lit t clauses) (+ depth 1)) t)
                              (t (bmc-dpll (bmc-simplify lit nil clauses) (+ depth 1)))))))))))

(define (bmc-solve clauses) (bmc-dpll clauses 0))

; ---- checks (proper CNF literals: atoms a / (not a); a clause is a
;      list of literals, a formula a list of clauses) ----

; brute-force model existence over all assignments (independent oracle)
(define (bmc-brute-p formula vars)
  (bmc-brute-walk formula vars nil))

(define (bmc-brute-walk formula vars asgn)
  (cond ((null vars) (bmc-eval formula asgn))
        (t (or (bmc-brute-walk formula (cdr vars)
                               (cons (cons (car vars) t) asgn))
               (bmc-brute-walk formula (cdr vars)
                               (cons (cons (car vars) nil) asgn))))))

(define (bmc-eval formula asgn)
  (cond ((null formula) t)
        ((bmc-eval-clause (car formula) asgn)
         (bmc-eval (cdr formula) asgn))
        (t nil)))

(define (bmc-eval-clause clause asgn)
  (cond ((null clause) nil)
        ((bmc-eval-lit (car clause) asgn) t)
        (t (bmc-eval-clause (cdr clause) asgn))))

(define (bmc-eval-lit lit asgn)
  (cond ((and (listp lit) (eq (car lit) 'not))
         (not (bmc-lookup (cadr lit) asgn)))
        (t (bmc-lookup lit asgn))))

(define (bmc-lookup v asgn)
  (cond ((null asgn) nil)
        ((equal (caar asgn) v) (cdar asgn))
        (t (bmc-lookup v (cdr asgn)))))

(define (bmc-check formula vars expected)
  (and (eq (bmc-solve formula) expected)
       (eq (bmc-brute-p formula vars) expected)))

(check "bmc: unit clause is SAT"
       (bmc-check '((a)) '(a) t))

(check "bmc: contradictory units are UNSAT"
       (bmc-check '((a) ((not a))) '(a) nil))

(check "bmc: a OR b is SAT"
       (bmc-check '((a b)) '(a b) t))

(check "bmc: (a|b) and (~a) and (~b) is UNSAT"
       (bmc-check '((a b) ((not a)) ((not b))) '(a b) nil))

(check "bmc: empty clause is UNSAT"
       (bmc-check '(()) '(a) nil))

(check "bmc: two units are SAT"
       (bmc-check '((a) (b)) '(a b) t))

(check "bmc: (a|b)(~a|c)(~b|~c) is SAT (model a,c)"
       (bmc-check '((a b) ((not a) c) ((not b) (not c))) '(a b c) t))

(check "bmc: pigeonhole 2-in-1 is UNSAT"
       (bmc-check '((p1 p2) ((not p1)) ((not p2))) '(p1 p2) nil))

(check "bmc: xor chain is SAT"
       (bmc-check '((a b) ((not a) (not b)) (a (not b)))
                  '(a b) t))

(check "bmc: empty formula is SAT (vacuously)"
       (bmc-check nil '(a) t))
