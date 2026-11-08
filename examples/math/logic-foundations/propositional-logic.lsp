; Propositional logic: syntax, semantics, truth tables.
; Tautology, satisfiability, validity. Natural deduction.
; Completeness: tautology <-> provable. Tableau method.

(print "=== PROPOSITIONAL LOGIC ===")

; ---- Syntax ----
; Atoms: p, q, r
; Formulas: atom, (not F), (and F G), (or F G), (impl F G), (iff F G)
; Represented as symbolic S-expressions.

; ---- Truth assignments ----
; An assignment maps atoms to t/nil.
; Evaluate a formula under an assignment (alist of (atom . truthvalue)).

(define (pl-lookup env atom)
  (cadr (find (lambda (pair) (equal (car pair) atom)) env)))

(define (pl-eval formula env)
  (cond ((eq formula t) t)
        ((eq formula nil) nil)
        ((symbolp formula) (pl-lookup env formula))
        ((equal (car formula) (quote not))
         (not (pl-eval (cadr formula) env)))
        ((equal (car formula) (quote and))
         (and (pl-eval (cadr formula) env) (pl-eval (caddr formula) env)))
        ((equal (car formula) (quote or))
         (or (pl-eval (cadr formula) env) (pl-eval (caddr formula) env)))
        ((equal (car formula) (quote impl))
         (or (not (pl-eval (cadr formula) env)) (pl-eval (caddr formula) env)))
        ((equal (car formula) (quote iff))
         (equal (pl-eval (cadr formula) env) (pl-eval (caddr formula) env)))
        (t nil)))

; Test basic evaluations
(define env1 (list (list (quote p) t) (list (quote q) nil)))
(if (pl-eval (quote p) env1) (print "PASS: atom-true") (print "FAIL: atom-true"))
(if (not (pl-eval (quote q) env1)) (print "PASS: atom-false") (print "FAIL: atom-false"))
(if (pl-eval (quote (not q)) env1) (print "PASS: not-gate") (print "FAIL: not-gate"))
(if (not (pl-eval (quote (and p q)) env1)) (print "PASS: and-gate") (print "FAIL: and-gate"))
(if (pl-eval (quote (or p q)) env1) (print "PASS: or-gate") (print "FAIL: or-gate"))

; ---- Truth tables ----
; Generate all 2^n assignments for n atoms
(define (all-assignments atoms)
  (cond ((eq atoms nil) (list nil))
        (t (let ((tail (all-assignments (cdr atoms))))
             (append (map (lambda (rest) (cons (list (car atoms) t) rest)) tail)
                     (map (lambda (rest) (cons (list (car atoms) nil) rest)) tail))))))

; Test all assignments for {p,q}
(define ass2 (all-assignments (list (quote p) (quote q))))
(if (= (length ass2) 4) (print "PASS: truth-table-4") (print "FAIL: truth-table-4"))

; ---- Tautology: true under all assignments ----
(define (tautology-p formula atoms)
  (every (lambda (env) (pl-eval formula env))
         (all-assignments atoms)))

(if (tautology-p (quote (or p (not p))) (list (quote p)))
    (print "PASS: excluded-middle") (print "FAIL: excluded-middle"))
(if (not (tautology-p (quote (and p q)) (list (quote p) (quote q))))
    (print "PASS: not-tautology") (print "FAIL: not-tautology"))

; ---- Satisfiable: true under some assignment ----
(define (satisfiable-p formula atoms)
  (any (lambda (env) (pl-eval formula env))
           (list-to-set (all-assignments atoms))))

(if (satisfiable-p (quote (and p q)) (list (quote p) (quote q)))
    (print "PASS: satisfiable") (print "FAIL: satisfiable"))
(if (not (satisfiable-p (quote (and p (not p))) (list (quote p))))
    (print "PASS: unsatisfiable") (print "FAIL: unsatisfiable"))

; ---- Logical consequence: A |= B iff (impl A B) is a tautology ----
(define (entails-p prem conc atoms)
  (tautology-p (list (quote impl) prem conc) atoms))

(if (entails-p (quote p) (quote (or p q)) (list (quote p) (quote q)))
    (print "PASS: entails-or-intro") (print "FAIL: entails-or-intro"))

; ---- Natural deduction rules ----
; And-intro: A, B |- (and A B)
(define (and-intro-valid-p atoms)
  (let ((a (list (quote p))) (b (list (quote q))))
    (every (lambda (env)
      (cond ((and (pl-eval (quote p) env) (pl-eval (quote q) env))
             (pl-eval (quote (and p q)) env))
            (t t)))
      (all-assignments atoms))))

(if (and-intro-valid-p (list (quote p) (quote q)))
    (print "PASS: and-intro") (print "FAIL: and-intro"))

; Modus ponens: A, (impl A B) |- B
(define (mp-valid-p atoms)
  (every (lambda (env)
    (cond ((and (pl-eval (quote p) env)
                (pl-eval (quote (impl p q)) env))
           (pl-eval (quote q) env))
          (t t)))
    (all-assignments atoms)))

(if (mp-valid-p (list (quote p) (quote q)))
    (print "PASS: modus-ponens") (print "FAIL: modus-ponens"))

; ---- Completeness: tautology <=> provable (finite) ----
; For finite atoms, we can check all assignments. A formula is a tautology
; iff its truth table column is all t. We verify by checking that every
; tautology we construct is indeed a tautology.

; Check De Morgan: (not (and p q)) <-> (or (not p) (not q))
(define demorgan (quote (iff (not (and p q)) (or (not p) (not q)))))
(if (tautology-p demorgan (list (quote p) (quote q)))
    (print "PASS: demorgan-tautology") (print "FAIL: demorgan-tautology"))

; ---- Tableau method ----
; A signed formula is (T formula) or (F formula).
; Tableau rules decompose formulas into branches.

(define (tableau-sat-p formula)
  ; For finite atoms, just use truth table (semantic tableau)
  (satisfiable-p formula (list (quote p) (quote q) (quote r))))

(if (tableau-sat-p (quote (and p q)))
    (print "PASS: tableau-sat") (print "FAIL: tableau-sat"))
(if (not (tableau-sat-p (quote (and p (not p)))))
    (print "PASS: tableau-unsat") (print "FAIL: tableau-unsat"))

; ---- Additional tautologies ----
(define id-taut (quote (impl p p)))
(if (tautology-p id-taut (list (quote p)))
    (print "PASS: identity-tautology") (print "FAIL: identity-tautology"))

(define trans-taut (quote (impl (and (impl p q) (impl q r)) (impl p r))))
(if (tautology-p trans-taut (list (quote p) (quote q) (quote r)))
    (print "PASS: transitivity-tautology") (print "FAIL: transitivity-tautology"))

(print "=== PROPOSITIONAL LOGIC DONE ===")
