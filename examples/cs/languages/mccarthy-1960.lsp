; Executable reconstruction of the symbolic-function core in McCarthy's 1960 Lisp paper.

; Conditional and recursive numerical examples used to introduce the formalism.
(define (m60-absolute value)
  (cond ((< value 0) (- 0 value)) (t value)))
(define (m60-delta left right)
  (cond ((eq left right) 1) (t 0)))
(define (m60-sign value)
  (cond ((< value 0) -1) ((= value 0) 0) (t 1)))

; Recursion replaces the paper's mathematical recurrence notation.
(define (m60-factorial value)
  (cond ((= value 0) 1) (t (* value (m60-factorial (- value 1))))))
(define (m60-gcd left right)
  (cond ((> left right) (m60-gcd right left))
        ((= (mod right left) 0) left)
        (t (m60-gcd (mod right left) left))))
(define (m60-sqrt value guess tolerance)
  (cond ((< (abs (- (* guess guess) value)) tolerance) guess)
        (t (m60-sqrt value (/ (+ guess (/ value guess)) 2.0) tolerance))))

; Recursive functions over atoms, pairs, trees, lists, and association lists.
; FIRST-ATOM repeatedly follows the left branch until it reaches an atom.
(define (m60-first-atom expression)
  (cond ((atom expression) expression)
        (t (m60-first-atom (car expression)))))
(define (m60-subst replacement wanted tree)
  (cond ((atom tree)
          (cond ((eq tree wanted) replacement) (t tree)))
        (t (cons (m60-subst replacement wanted (car tree))
                 (m60-subst replacement wanted (cdr tree))))))
(define (m60-same left right)
  (cond ((and (atom left) (atom right)) (eq left right))
        ((or (atom left) (atom right)) nil)
        (t (and (m60-same (car left) (car right))
                (m60-same (cdr left) (cdr right))))))
(define (m60-null value) (and (atom value) (eq value nil)))

; Lists remain ordinary cons trees; no host collection is used.
(define (m60-append left right)
  (cond ((m60-null left) right)
        (t (cons (car left) (m60-append (cdr left) right)))))
(define (m60-among value values)
  (and (not (m60-null values))
       (or (m60-same value (car values))
           (m60-among value (cdr values)))))
(define (m60-pair names values)
  (cond ((and (m60-null names) (m60-null values)) nil)
        (t (cons (list (car names) (car values))
                 (m60-pair (cdr names) (cdr values))))))
(define (m60-assoc name environment)
  (cond ((m60-null environment) nil)
        ((eq (caar environment) name) (cadar environment))
        (t (m60-assoc name (cdr environment)))))
(define (m60-sub2 substitutions atom-value)
  (cond ((m60-null substitutions) atom-value)
        ((eq (caar substitutions) atom-value) (cadar substitutions))
        (t (m60-sub2 (cdr substitutions) atom-value))))
(define (m60-sublis substitutions tree)
  (cond ((atom tree) (m60-sub2 substitutions tree))
        (t (cons (m60-sublis substitutions (car tree))
                 (m60-sublis substitutions (cdr tree))))))

; The universal S-function quotes arguments, then evaluates function data.
; APPQ constructs object-language QUOTE forms rather than evaluating in SISP.
(define (m60-appq arguments)
  (cond ((m60-null arguments) nil)
        (t (cons (list 'quote (car arguments))
                 (m60-appq (cdr arguments))))))
(define (m60-apply function arguments)
  (m60-eval (cons function (m60-appq arguments)) nil))
(define (m60-evlis expressions environment)
  (cond ((m60-null expressions) nil)
        (t (cons (list 'quote (m60-eval (car expressions) environment))
                 (m60-evlis (cdr expressions) environment)))))

; COND selects the first true clause and leaves later clauses unevaluated.
(define (m60-evcon clauses environment)
  (cond ((m60-eval (caar clauses) environment)
          (m60-eval (cadar clauses) environment))
        (t (m60-evcon (cdr clauses) environment))))

; Object-language operators are symbols; LABEL supplies recursive self-reference.
; This evaluator intentionally implements the paper's small language, not all SISP.
(define (m60-eval expression environment)
  (cond ((atom expression)
          (m60-eval (m60-assoc expression environment) environment))
        ((atom (car expression))
          (cond ((eq (car expression) 'quote) (cadr expression))
                ((eq (car expression) 'atom)
                  (atom (m60-eval (cadr expression) environment)))
                ((eq (car expression) 'eq)
                  (eq (m60-eval (cadr expression) environment)
                      (m60-eval (caddr expression) environment)))
                ((eq (car expression) 'cond)
                  (m60-evcon (cdr expression) environment))
                ((eq (car expression) 'car)
                  (car (m60-eval (cadr expression) environment)))
                ((eq (car expression) 'cdr)
                  (cdr (m60-eval (cadr expression) environment)))
                ((eq (car expression) 'cons)
                  (cons (m60-eval (cadr expression) environment)
                        (m60-eval (caddr expression) environment)))
                ; A named function is replaced by its associated function data.
                (t (m60-eval
                  (cons (m60-assoc (car expression) environment)
                        (m60-evlis (cdr expression) environment))
                  environment))))
        ((eq (caar expression) 'label)
          ; LABEL adds the complete labeled expression for recursive lookup.
          (m60-eval (cons (caddar expression) (cdr expression))
            (cons (list (cadar expression) (car expression)) environment)))
        ((eq (caar expression) 'lambda)
          ; Formal parameters receive quoted values, matching the 1960 a-list.
          (m60-eval (caddar expression)
            (m60-append
              (m60-pair (cadar expression)
                (m60-evlis (cdr expression) environment))
              environment)))))

; LABEL encodings of the paper's first-atom and substitution examples.
; They are plain list data consumed by M60-EVAL, not native SISP definitions.
(define m60-ff-code
  '(label ff
    (lambda (x)
      (cond ((atom x) x)
            ((quote t) (ff (car x)))))))
(define m60-subst-code
  '(label subst
    (lambda (x y z)
      (cond ((atom z)
              (cond ((eq z y) x) ((quote t) z)))
            ((quote t)
              (cons (subst x y (car z))
                    (subst x y (cdr z))))))))

; Functions as arguments operate on successive list suffixes, as in MAPLIST.
; Native FUNCALL is used here because this section demonstrates SISP-level use.
(define (m60-maplist values function)
  (cond ((m60-null values) nil)
        (t (cons (funcall function values)
                 (m60-maplist (cdr values) function)))))
(define (m60-search values predicate on-found on-missing)
  (cond ((m60-null values) (funcall on-missing))
        ((funcall predicate values) (funcall on-found values))
        (t (m60-search (cdr values) predicate on-found on-missing))))

; Symbolic differentiation demonstrates nested higher-order MAPLIST calls.
; PLUS, TIMES, ONE, and ZERO are symbolic output atoms, not arithmetic calls.
(define (m60-diff-product factors variable)
  (cons 'plus
    (m60-maplist factors
      (lambda (selected)
        (cons 'times
          (m60-maplist factors
            (lambda (remaining)
              (cond ((eq selected remaining)
                      (m60-diff (car remaining) variable))
                    (t (car remaining))))))))))
(define (m60-diff expression variable)
  (cond ((atom expression)
          (cond ((eq expression variable) 'one) (t 'zero)))
        ((eq (car expression) 'plus)
          (cons 'plus
            (m60-maplist (cdr expression)
              (lambda (remaining)
                (m60-diff (car remaining) variable)))))
        ((eq (car expression) 'times)
          (m60-diff-product (cdr expression) variable))))

; The results are grouped into a few lists, each followed by its expected value.
(define (m60-recursive-results)
  (list (m60-absolute -8) (m60-delta 'a 'b)
    (m60-sign -4) (m60-sign 0) (m60-sign 9)
    (m60-factorial 6) (m60-gcd 24 18)
    (m60-first-atom '(((a b) c) d))
    (m60-subst '(x a) 'b '((a b) c))
    (m60-same '(a (b c)) '(a (b c)))
    (m60-append '(a b) '(c d))
    (m60-among '(b c) '(a (b c) d))
    (m60-pair '(a b c) '(x (y z) u))
    (m60-sublis '((x (a b)) (y (b c))) '(a x y))))
(m60-recursive-results) ; => (8 0 -1 0 1 720 6 a ((a (x a)) c) t (a b c d) t ((a x) (b (y z)) (c u)) (a (a b) (b c)))

(list
  (m60-apply '(lambda (x y) (cons (car x) y)) '((a b) (c d)))
  (m60-apply m60-ff-code '((((a b) c) d)))
  (m60-apply m60-subst-code '((x a) b ((a b) c)))
  (m60-eval '(cond ((atom (quote (a b))) (quote wrong))
                   ((quote t) (car (quote (a b))))) nil)) ; => ((a c d) a ((a (x a)) c) a)

(list
  (< (abs (- (m60-sqrt 2 1.1 0.000001) 1.41421356)) 0.00001)
  (m60-maplist '(a b c) (lambda (suffix) (car suffix)))
  (m60-search '(a b c)
    (lambda (suffix) (eq (car suffix) 'b))
    (lambda (suffix) (car suffix))
    (lambda () 'none))
  (m60-diff '(times x (plus x a) y) 'x)) ; => (t (a b c) b (plus (times one (plus x a) y) (times x (plus one zero) y) (times x (plus x a) zero)))
