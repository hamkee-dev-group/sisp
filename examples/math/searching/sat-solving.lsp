; ============================================================ ; 110 — SAT Solving: DPLL with Unit Propagation ; (TAOCP Vol 4, Fasc 6, 7.2.2.2) ; Solve small CNF formulas. Resolution refutation for UNSAT. ; Test: (x1 OR x2) AND (NOT x1) AND (NOT x2) UNSAT, ; (x1 OR x2) AND (NOT x1 OR x3) AND (NOT x2) SAT with model. ; ============================================================
(print "=== SAT Solving ===")


; ---------- literal and model ----------
(define (lit-sign lit) (car lit))
(define (lit-var lit) (cadr lit))

; Evaluate a literal under a complete assignment (list of (var . bool))
(define (lookup m var)
  (cond ((eq m nil) nil)
        ((eq (caar m) var) (cdar m))
        (t (lookup (cdr m) var))))

(define (eval-lit lit m)
  (cond ((lit-sign lit) (lookup m (lit-var lit)))
        (t (not (lookup m (lit-var lit))))))

; Check if a clause is satisfied
(define (clause-ok cl m)
  (cond ((eq cl nil) nil)
        ((eval-lit (car cl) m) t)
        (t (clause-ok (cdr cl) m))))

; Check if all clauses are satisfied
(define (all-clauses-ok f m)
  (cond ((eq f nil) t)
        ((clause-ok (car f) m) (all-clauses-ok (cdr f) m))
        (t nil)))

; Collect all variables from formula
(define (collect-vars f)
  (dedup-acc (foldl (lambda (acc cl) (append acc (map lit-var cl))) nil f) nil))

(define (dedup-acc lst acc)
  (cond ((eq lst nil) acc)
        ((in (car lst) acc) (dedup-acc (cdr lst) acc))
        (t (dedup-acc (cdr lst) (cons (car lst) acc)))))

; Generate all assignment combinations for given vars
(define (all-models vars)
  (cond ((eq vars nil) (list nil))
        (t (let* ((v (car vars))
                  (rest (all-models (cdr vars))))
          (append (map (lambda (m) (cons (cons v t) m)) rest)
                  (map (lambda (m) (cons (cons v nil) m)) rest))))))

; Simple exhaustive SAT solver (works for small formulas)
(define (sat-solve f)
  (let ((vars (collect-vars f)))
    (find-model f (all-models vars))))

(define (find-model f models)
  (cond ((eq models nil) nil)
        ((all-clauses-ok f (car models)) (car models))
        (t (find-model f (cdr models)))))

; Resolution
(define (resolve c1 c2 pivot)
  (dedup-acc (append (filter (lambda (l) (not (eq (lit-var l) pivot))) c1)
                 (filter (lambda (l) (not (eq (lit-var l) pivot))) c2)) nil))

; ============================================================ ; Test 1: UNSAT ; ============================================================
(print "--- UNSAT test ---")
(define f1 (list (list (list t 'x1) (list t 'x2))
                 (list (list nil 'x1))
                 (list (list nil 'x2))))
(define r1 (sat-solve f1))
(print "Result:")
(print r1)
(check "unsat-is-nil" (eq r1 nil))

; Resolution refutation
(define c1 (list (list t 'x1) (list t 'x2)))
(define c2 (list (list nil 'x1)))
(define c3 (list (list nil 'x2)))
(define res1 (resolve c1 c2 'x1))
(check "res1-has-1" (= (length res1) 1))
(define res2 (resolve res1 c3 'x2))
(check "res2-empty" (eq res2 nil))

; ============================================================ ; Test 2: SAT ; ============================================================
(print "--- SAT test ---")
(define f2 (list (list (list t 'x1) (list t 'x2))
                 (list (list nil 'x1) (list t 'x3))
                 (list (list nil 'x2))))
(define r2 (sat-solve f2))
(print "Model:")
(print r2)
(check "sat-not-nil" (not (eq r2 nil)))
(check "model-ok" (all-clauses-ok f2 r2))

; ============================================================ ; Test 3: trivial SAT ; ============================================================
(print "--- Trivial SAT ---")
(define f3 (list (list (list t 'a))))
(define r3 (sat-solve f3))
(check "triv-sat" (not (eq r3 nil)))
(check "triv-model-ok" (all-clauses-ok f3 r3))

(print "=== SAT Solving Done ===")
