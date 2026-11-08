; ============================================================
; name: prolog
; file: algorithms/logic/prolog.lsp
; subject: logic
; capability: unification resolution backtracking proof-search
; aliases: "prolog" "logic programming" "sld resolution" "unification" "backtracking"
; signature: (pr-query program goals depth) -> list-of-answers
;            (pr-unify a b subst) -> subst | nil
;            program: list of (fact TERM) / (rule HEAD (GOAL ...)); vars are (var NAME)
; complexity: SLD resolution with chronological backtracking; depth-bounded
; see-also: sat-dpll
; derived-from: Robinson 1965 (unification); Kowalski 1974 (SLD resolution)
; status: example (tested)
; checks: 11 - parent/ancestor queries (incl. transitive recursion),
;         negation-as-failure, occurs-check, unification (bind/compound/clash),
;         and append/3 over encoded lists; run: src/sisp <file>
; ------------------------------------------------------------
; A Prolog-style logic engine: fresh-variable renaming, unification with
; occurs-check, chronological backtracking, and negation-as-failure.
; Terms use (var name); clauses are (fact TERM) or (rule HEAD (GOAL ...)).
; ============================================================

(define (pr-var-p term)
  (and (consp term) (eq (car term) 'var) (= (length term) 2)))

(define (pr-binding substitutions variable)
  (find (lambda (binding) (equal (car binding) variable)) substitutions))

(define (pr-walk term substitutions)
  (cond ((not (pr-var-p term)) term)
        (t (let ((binding (pr-binding substitutions term)))
             (cond ((null binding) term)
                   (t (pr-walk (cadr binding) substitutions)))))))

; Occurs checking prevents cyclic terms such as X = f(X).
(define (pr-occurs-p variable term substitutions)
  (let ((walked (pr-walk term substitutions)))
    (cond ((equal variable walked) t)
          ((atom walked) nil)
          ((pr-var-p walked) nil)
          (t (any (lambda (part) (pr-occurs-p variable part substitutions))
                  walked)))))

(define (pr-extend variable term substitutions)
  (cond ((pr-occurs-p variable term substitutions) nil)
        (t (cons (list variable term) substitutions))))

(define (pr-unify-list left right substitutions)
  (cond ((and (null left) (null right)) substitutions)
        ((or (null left) (null right)) nil)
        (t (let ((next (pr-unify (car left) (car right) substitutions)))
             (cond ((null next) nil)
                   (t (pr-unify-list (cdr left) (cdr right) next)))))))

(define (pr-unify left right substitutions)
  (let ((a (pr-walk left substitutions))
        (b (pr-walk right substitutions)))
    (cond ((equal a b) substitutions)
          ((pr-var-p a) (pr-extend a b substitutions))
          ((pr-var-p b) (pr-extend b a substitutions))
          ((and (consp a) (consp b)) (pr-unify-list a b substitutions))
          (t nil))))

(define (pr-substitute term substitutions)
  (let ((walked (pr-walk term substitutions)))
    (cond ((atom walked) walked)
          ((pr-var-p walked) walked)
          (t (map (lambda (part) (pr-substitute part substitutions)) walked)))))

; A stamp attached to every clause variable makes each rule application independent.
(define *pr-stamp* 0)
(define (pr-fresh-term term stamp)
  (cond ((pr-var-p term) (list 'var (list (cadr term) stamp)))
        ((atom term) term)
        (t (map (lambda (part) (pr-fresh-term part stamp)) term))))

(define (pr-clause-head clause)
  (cond ((eq (car clause) 'fact) (cadr clause))
        (t (cadr clause))))

(define (pr-clause-body clause)
  (cond ((eq (car clause) 'fact) nil)
        (t (caddr clause))))

(define (pr-fresh-clause clause)
  (setq *pr-stamp* (+ *pr-stamp* 1))
  (list (pr-fresh-term (pr-clause-head clause) *pr-stamp*)
        (pr-fresh-term (pr-clause-body clause) *pr-stamp*)))

; Equality and negation-as-failure are handled before ordinary predicate lookup.
(define (pr-prove-builtin program goals substitutions depth)
  (let ((goal (car goals)))
    (cond ((eq (car goal) 'true)
            (pr-prove-goals program (cdr goals) substitutions depth))
          ((eq (car goal) '=)
            (let ((next (pr-unify (cadr goal) (caddr goal) substitutions)))
              (cond ((null next) nil)
                    (t (pr-prove-goals program (cdr goals) next depth)))))
          ((eq (car goal) 'not)
            (cond ((null (pr-prove-goals program (list (cadr goal))
                           substitutions (- depth 1)))
                    (pr-prove-goals program (cdr goals) substitutions depth))
                  (t nil)))
          (t nil))))

(define (pr-builtin-p goal)
  (and (consp goal) (in (car goal) '(true = not))))

(define (pr-prove-clause program goal rest-goals substitutions depth clause)
  (let ((fresh (pr-fresh-clause clause)))
    (let ((next (pr-unify goal (car fresh) substitutions)))
      (cond ((null next) nil)
            (t (pr-prove-goals program
                 (append (cadr fresh) rest-goals) next (- depth 1)))))))

; Appending every successful branch implements chronological backtracking.
(define (pr-prove-predicate program goals substitutions depth clauses)
  (cond ((null clauses) nil)
        (t (append
             (pr-prove-clause program (car goals) (cdr goals)
               substitutions depth (car clauses))
             (pr-prove-predicate program goals substitutions depth
               (cdr clauses))))))

(define (pr-prove-goals program goals substitutions depth)
  (cond ((null goals) (list substitutions))
        ((= depth 0) nil)
        ((pr-builtin-p (car goals))
          (pr-prove-builtin program goals substitutions depth))
        (t (pr-prove-predicate program goals substitutions depth program))))

; Query variables are reported once, in their first left-to-right occurrence.
(define (pr-variable-member-p variable variables)
  (any (lambda (candidate) (equal variable candidate)) variables))

(define (pr-variables-walk term variables)
  (cond ((pr-var-p term)
          (cond ((pr-variable-member-p term variables) variables)
                (t (append variables (list term)))))
        ((atom term) variables)
        (t (foldl (lambda (result part) (pr-variables-walk part result))
             variables term))))

(define (pr-query-variables goals)
  (foldl (lambda (variables goal) (pr-variables-walk goal variables)) nil goals))

(define (pr-answer variables substitutions)
  (map (lambda (variable)
         (list (cadr variable) (pr-substitute variable substitutions)))
       variables))

(define (pr-query program goals depth)
  (setq *pr-stamp* 0)
  (let ((variables (pr-query-variables goals)))
    (map (lambda (solution) (pr-answer variables solution))
      (pr-prove-goals program goals '((pr-root pr-root)) depth))))

(define pr-family
  '((fact (parent ada bob))
    (fact (parent ada dan))
    (fact (parent bob cy))
    (rule (ancestor (var x) (var y))
      ((parent (var x) (var y))))
    (rule (ancestor (var x) (var z))
      ((parent (var x) (var y)) (ancestor (var y) (var z))))))


; ============================================================
; ---- checks ----
; ============================================================
(define pr-app
  '((fact (app emp (var y) (var y)))
    (rule (app (cons (var h) (var t)) (var y) (cons (var h) (var z)))
      ((app (var t) (var y) (var z))))))

(check "prolog: parent ada -> bob and dan"
  (equal (pr-query pr-family '((parent ada (var child))) 12)
         '(((child bob)) ((child dan)))))
(check "prolog: ancestor ada is transitive -> bob dan cy"
  (equal (pr-query pr-family '((ancestor ada (var d))) 20)
         '(((d bob)) ((d dan)) ((d cy)))))
(check "prolog: ancestor count is 3 (recursion reached cy)"
  (= (length (pr-query pr-family '((ancestor ada (var d))) 20)) 3))
(check "prolog: negation-as-failure excludes dan"
  (equal (pr-query pr-family
           '((ancestor ada (var d)) (not (= (var d) dan))) 20)
         '(((d bob)) ((d cy)))))
(check "prolog: dan has no descendants -> no solutions"
  (eq (pr-query pr-family '((ancestor dan (var who))) 12) nil))
(check "prolog: occurs-check rejects f(x) = f(g(x))"
  (eq (pr-unify '(f (var x)) '(f (g (var x))) '((pr-root pr-root))) nil))
(check "prolog: unify binds a variable to an atom"
  (equal (pr-walk '(var x)
           (pr-unify '(var x) 'foo '((pr-root pr-root))))
         'foo))
(check "prolog: unify two compounds binds both variables"
  (let ((s (pr-unify '(p (var x) b) '(p a (var y)) '((pr-root pr-root)))))
    (and (equal (pr-walk '(var x) s) 'a)
         (equal (pr-walk '(var y) s) 'b))))
(check "prolog: unify clash of distinct atoms fails"
  (eq (pr-unify 'a 'b '((pr-root pr-root))) nil))
(check "prolog: append [a b] ++ [c] = [a b c]"
  (equal (pr-query pr-app
           '((app (cons a (cons b emp)) (cons c emp) (var r))) 20)
         '(((r (cons a (cons b (cons c emp))))))))
(check "prolog: append with unbound prefix finds the split"
  (equal (pr-query pr-app
           '((app (var xs) (cons c emp) (cons a (cons c emp)))) 20)
         '(((xs (cons a emp))))))
(print "=== prolog checks done ===")
