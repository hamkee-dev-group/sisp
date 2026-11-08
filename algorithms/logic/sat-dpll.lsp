; ============================================================
; name: sat-dpll
; file: algorithms/logic/sat-dpll.lsp
; subject: logic
; capability: boolean satisfiability cnf dpll
; aliases: "dpll" "sat solver" "boolean satisfiability" "davis-putnam"
; signature: (st-solve formula) -> model | nil
; complexity: O(2^n) worst case; unit + pure-literal pruning in practice
; see-also: topological-sort backtracking
; derived-from: Davis, Logemann & Loveland 1962
; status: example (tested)
; checks: 14 checks; run: src/sisp <this file> from repo root
; ============================================================
; A propositional SAT solver.  A formula is built from symbols and the
; connectives not / and / or / implies / iff.  st-solve returns a model
; (an alist of ((var t-or-nil) ...) that makes the formula true) or nil
; when the formula is unsatisfiable.
;
; Pipeline: eliminate implies/iff -> push negations to atoms (NNF) ->
; distribute to CNF (list of clauses, each a list of literals) -> DPLL
; search with unit propagation, pure-literal elimination, and branching.
;
; Literals are symbols (positive) or (not symbol) (negative).  A sentinel
; (st-root t) binding seeds the search so a satisfiable-but-empty result
; is still a non-nil model, keeping nil unambiguously "unsatisfiable".

; ---- normalization: implies / iff elimination ----
(define (st-eliminate formula)
  (cond ((atom formula) formula)
        ((eq (car formula) 'not) (list 'not (st-eliminate (cadr formula))))
        ((eq (car formula) 'implies)
          (list 'or (list 'not (st-eliminate (cadr formula)))
                    (st-eliminate (caddr formula))))
        ((eq (car formula) 'iff)
          (let ((left (st-eliminate (cadr formula)))
                (right (st-eliminate (caddr formula))))
            (list 'and (list 'or (list 'not left) right)
                       (list 'or (list 'not right) left))))
        (t (cons (car formula) (map st-eliminate (cdr formula))))))

; ---- negation normal form: De Morgan pushes not down to atoms ----
(define (st-nnf formula negated)
  (cond ((atom formula) (cond (negated (list 'not formula)) (t formula)))
        ((eq (car formula) 'not) (st-nnf (cadr formula) (not negated)))
        ((eq (car formula) 'and)
          (cons (cond (negated 'or) (t 'and))
            (map (lambda (part) (st-nnf part negated)) (cdr formula))))
        ((eq (car formula) 'or)
          (cons (cond (negated 'and) (t 'or))
            (map (lambda (part) (st-nnf part negated)) (cdr formula))))))

; ---- CNF conversion via distribution ----
(define (st-literal-p formula)
  (or (atom formula)
      (and (eq (car formula) 'not) (atom (cadr formula)))))

; OR distributes by taking the Cartesian product of the two clause sets.
(define (st-distribute left right)
  (foldl (lambda (clauses left-clause)
    (append clauses
      (map (lambda (right-clause) (append left-clause right-clause)) right)))
    nil left))

(define (st-cnf-nnf formula)
  (cond ((st-literal-p formula) (list (list formula)))
        ((eq (car formula) 'and)
          (foldl (lambda (clauses part) (append clauses (st-cnf-nnf part)))
            nil (cdr formula)))
        ((eq (car formula) 'or)
          (foldl (lambda (clauses part)
                   (st-distribute clauses (st-cnf-nnf part)))
            '(()) (cdr formula)))))

(define (st-to-cnf formula)
  (st-cnf-nnf (st-nnf (st-eliminate formula) nil)))

; ---- literals and partial assignments ----
(define (st-negative-p literal)
  (and (consp literal) (eq (car literal) 'not)))
(define (st-variable literal)
  (cond ((st-negative-p literal) (cadr literal)) (t literal)))
(define (st-opposite literal)
  (cond ((st-negative-p literal) (cadr literal))
        (t (list 'not literal))))

(define (st-assignment-binding assignment variable)
  (find (lambda (binding) (eq (car binding) variable)) assignment))

(define (st-literal-state literal assignment)
  (let ((binding (st-assignment-binding assignment (st-variable literal))))
    (cond ((null binding) 'unknown)
          ((st-negative-p literal)
            (cond ((cadr binding) 'false) (t 'true)))
          ((cadr binding) 'true)
          (t 'false))))

(define (st-clause-satisfied-p clause assignment)
  (any (lambda (literal) (eq (st-literal-state literal assignment) 'true)) clause))

(define (st-reduce-clause clause assignment)
  (filter (lambda (literal)
            (eq (st-literal-state literal assignment) 'unknown)) clause))

; Satisfied clauses disappear; an empty remaining clause is a conflict.
(define (st-simplify cnf assignment)
  (cond ((null cnf) nil)
        ((st-clause-satisfied-p (car cnf) assignment)
          (st-simplify (cdr cnf) assignment))
        (t (cons (st-reduce-clause (car cnf) assignment)
                 (st-simplify (cdr cnf) assignment)))))

(define (st-conflict-p cnf)
  (any (lambda (clause) (null clause)) cnf))

; ---- DPLL heuristics ----
(define (st-unit-literal cnf)
  (let ((clause (find (lambda (candidate) (= (length candidate) 1)) cnf)))
    (cond ((null clause) nil) (t (car clause)))))

(define (st-all-literals cnf)
  (foldl (lambda (result clause) (append result clause)) nil cnf))

(define (st-literal-member-p literal literals)
  (any (lambda (candidate) (equal literal candidate)) literals))

(define (st-pure-literal-walk literals all-literals)
  (cond ((null literals) nil)
        ((not (st-literal-member-p (st-opposite (car literals)) all-literals))
          (car literals))
        (t (st-pure-literal-walk (cdr literals) all-literals))))

(define (st-pure-literal cnf)
  (let ((literals (st-all-literals cnf)))
    (st-pure-literal-walk literals literals)))

(define (st-assign-literal literal assignment)
  (cons (list (st-variable literal) (not (st-negative-p literal))) assignment))

; ---- DPLL search ----
(define (st-dpll cnf assignment)
  (let ((reduced (st-simplify cnf assignment)))
    (cond ((null reduced) assignment)
          ((st-conflict-p reduced) nil)
          (t (let ((unit (st-unit-literal reduced)))
               (cond (unit (st-dpll reduced (st-assign-literal unit assignment)))
                     (t (let ((pure (st-pure-literal reduced)))
                          (cond (pure
                                  (st-dpll reduced
                                    (st-assign-literal pure assignment)))
                                (t (let ((choice (caar reduced)))
                                     (let ((positive
                                       (st-dpll reduced
                                         (st-assign-literal choice assignment))))
                                       (cond (positive positive)
                                             (t (st-dpll reduced
                                                  (st-assign-literal
                                                    (st-opposite choice)
                                                    assignment))))))))))))))))

(define (st-remove-root assignment)
  (remove (lambda (binding) (eq (car binding) 'st-root)) assignment))

(define (st-solve-cnf cnf)
  (let ((result (st-dpll cnf '((st-root t)))))
    (cond ((null result) nil) (t (st-remove-root result)))))

(define (st-solve formula) (st-solve-cnf (st-to-cnf formula)))

; ---- semantic evaluation (independent of the CNF pipeline) ----
(define (st-value variable assignment)
  (let ((binding (st-assignment-binding assignment variable)))
    (cond ((null binding) nil) (t (cadr binding)))))

(define (st-evaluate formula assignment)
  (cond ((atom formula) (st-value formula assignment))
        ((eq (car formula) 'not) (not (st-evaluate (cadr formula) assignment)))
        ((eq (car formula) 'and)
          (every (lambda (part) (st-evaluate part assignment)) (cdr formula)))
        ((eq (car formula) 'or)
          (any (lambda (part) (st-evaluate part assignment)) (cdr formula)))
        ((eq (car formula) 'implies)
          (or (not (st-evaluate (cadr formula) assignment))
              (st-evaluate (caddr formula) assignment)))
        ((eq (car formula) 'iff)
          (<=> (st-evaluate (cadr formula) assignment)
               (st-evaluate (caddr formula) assignment)))))

; ---- check helpers: brute-force truth-table reference ----

; collect the distinct variable symbols mentioned in a formula
(define (st-vars formula)
  (cond ((atom formula)
          (cond ((eq formula nil) nil) ((eq formula t) nil) (t (list formula))))
        ((eq (car formula) 'not) (st-vars (cadr formula)))
        (t (foldl (lambda (acc part) (append acc (st-vars part))) nil (cdr formula)))))

; every 2^n truth assignment over vars, as ((var t-or-nil) ...) alists
(define (st-all-assignments vars)
  (cond ((null vars) (list nil))
        (t (let ((rest (st-all-assignments (cdr vars))))
             (append
               (map (lambda (a) (cons (list (car vars) t) a)) rest)
               (map (lambda (a) (cons (list (car vars) nil) a)) rest))))))

; reference satisfiability by exhaustive enumeration
(define (st-brute-sat formula)
  (any (lambda (a) (st-evaluate formula a))
       (st-all-assignments (dedup (st-vars formula)))))

; DPLL and brute force agree, AND any model DPLL returns really satisfies
(define (st-agree formula)
  (let ((model (st-solve formula)))
    (cond ((null model) (not (st-brute-sat formula)))
          (t (and (st-brute-sat formula) (st-evaluate formula model))))))

(define st-cross-formulas
  (list
    '(and (or a b) (or (not a) c) (or (not b) c))
    '(and a (not a))
    '(implies a (and b c))
    '(iff a b)
    '(or a (not a))
    '(and (or a b) (or (not a) (not b)))
    '(and (or a b c) (or (not a) (not b)) (or (not b) (not c)) (or (not a) (not c)))
    '(and a b c)
    '(and (or a b) (or (not a)) (or (not b)))
    '(iff (and a b) (or (not a) (not b)))
    '(and (implies a b) (implies b c) a (not c))
    '(or (and a b) (and (not a) (not b)))
    '(and (or a b c d) (or (not a) (not b)) (or (not c) (not d)) (or a c) (or b d))
    '(iff a (iff b c))
    '(and (or a b) (or b c) (or (not a) (not c)) (or a (not b) c))))

(define st-demo '(and (or a b) (or (not a) c) (or (not b) c)))

; ---- checks ----

(check "sat-dpll: CNF conversion of (implies a (and b c))"
       (equal (st-to-cnf '(implies a (and b c)))
              '(((not a) b) ((not a) c))))

(check "sat-dpll: NNF pushes negation through and/or (De Morgan)"
       (equal (st-nnf '(not (and a (or b c))) nil)
              '(or (not a) (and (not b) (not c)))))

(check "sat-dpll: satisfiable demo returns a model that satisfies it"
       (let ((model (st-solve st-demo)))
         (and (consp model) (st-evaluate st-demo model))))

(check "sat-dpll: demo model forces c to be true (only way to satisfy)"
       (eq (st-value 'c (st-solve st-demo)) t))

(check "sat-dpll: contradiction (a and not a) is UNSAT"
       (eq (st-solve '(and a (not a))) nil))

(check "sat-dpll: (iff X (not X)) is UNSAT"
       (eq (st-solve '(iff (and a b) (or (not a) (not b)))) nil))

(check "sat-dpll: tautology (or a (not a)) is SAT"
       (consp (st-solve '(or a (not a)))))

(check "sat-dpll: unit propagation chain a, a->b, b->c, not c is UNSAT"
       (eq (st-solve '(and (implies a b) (implies b c) a (not c))) nil))

(check "sat-dpll: all four clauses over two vars is UNSAT"
       (eq (st-solve '(and (or a b) (or (not a) b)
                           (or a (not b)) (or (not a) (not b)))) nil))

(check "sat-dpll: returned model for a 4-var formula satisfies it"
       (let ((f '(and (or a b c d) (or (not a) (not b))
                      (or (not c) (not d)) (or a c) (or b d))))
         (st-evaluate f (st-solve f))))

(check "sat-dpll: brute force confirms the demo is satisfiable"
       (eq (st-brute-sat st-demo) t))

(check "sat-dpll: brute force confirms (a and not a) is unsatisfiable"
       (eq (st-brute-sat '(and a (not a))) nil))

(check "sat-dpll: DPLL agrees with brute force on the demo formula"
       (st-agree st-demo))

(check "sat-dpll: DPLL agrees with brute force on all cross-check formulas"
       (every st-agree st-cross-formulas))
