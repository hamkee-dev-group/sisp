; ============================================================
; SISP Test Suite — First-Class Primitives and Binding Guards
; Builtins as values (map/funcall/apply/define/let), interned
; runtime identifiers, and comprehension substitution staying
; symbolic in function position.
; ============================================================

(print "=== FIRST-CLASS PRIMITIVE TESTS ===")

; --- builtins as values ---
(if (equal (map car (list (list 1 2) (list 3 4))) (list 1 3)) (print "PASS: map-with-builtin") (print "FAIL: map-with-builtin"))
(if (equal (map cdr (list (list 1 2) (list 3 4))) (list (list 2) (list 4))) (print "PASS: map-with-builtin-cdr") (print "FAIL: map-with-builtin-cdr"))
(define kar car)
(if (equal (kar (list 7 8)) 7) (print "PASS: define-builtin-alias") (print "FAIL: define-builtin-alias"))
(if (equal (let ((f cdr)) (f (list 1 2 3))) (list 2 3)) (print "PASS: let-bound-builtin") (print "FAIL: let-bound-builtin"))
(if (equal (funcall car (list 5 6)) 5) (print "PASS: funcall-builtin") (print "FAIL: funcall-builtin"))
(if (equal (apply car (list (list 9 1))) 9) (print "PASS: apply-builtin") (print "FAIL: apply-builtin"))
(if (equal (eval (cons car (list (quote (list 5 6))))) 5) (print "PASS: embedded-primitive-head") (print "FAIL: embedded-primitive-head"))

; builtins flow through user-defined higher-order functions
(define (apply-twice f x) (f (f x)))
(if (equal (apply-twice cdr (list 1 2 3)) (list 3)) (print "PASS: builtin-through-user-hof") (print "FAIL: builtin-through-user-hof"))
(if (equal (filter numberp (list 1 (quote a) 2 (quote b))) (list 1 2)) (print "PASS: filter-with-builtin-predicate") (print "FAIL: filter-with-builtin-predicate"))

; prelude higher-order functions accept builtins now
(if (equal (map car {(1 2) (3 4)}) {1 3}) (print "PASS: map-set-with-builtin") (print "FAIL: map-set-with-builtin"))

; --- lambdas and closures unaffected ---
(define (mk-adder k) (lambda (y) (+ k y)))
(if (equal ((mk-adder 3) 4) 7) (print "PASS: closures-still-capture") (print "FAIL: closures-still-capture"))
(if (equal (map (lambda (x) (* x x)) (list 1 2 3)) (list 1 4 9)) (print "PASS: lambdas-still-map") (print "FAIL: lambdas-still-map"))
(if (equal (labels (((rec n) (cond ((= n 0) 0) (t (+ n (rec (- n 1))))))) (rec 4)) 10) (print "PASS: labels-still-work") (print "FAIL: labels-still-work"))

; --- comprehension substitution stays symbolic in function position ---
(define (above x) {tau : (> tau x)})
(if (equal (above 3) {tau : (> tau 3)}) (print "PASS: comprehension-keeps-operator-symbolic") (print "FAIL: comprehension-keeps-operator-symbolic"))
(if (equal (in 5 (above 3)) t) (print "PASS: comprehension-membership") (print "FAIL: comprehension-membership"))
(define (band lo hi) {tau : (and (> tau lo) (< tau hi))})
(if (equal (in 5 (band 3 10)) t) (print "PASS: comprehension-nested-capture") (print "FAIL: comprehension-nested-capture"))
(if (equal (in 12 (band 3 10)) nil) (print "PASS: comprehension-nested-out") (print "FAIL: comprehension-nested-out"))

; --- redefining builtin names is still refused ---
; (this errors on stderr and leaves the builtin intact)
(if (equal (car (list 4 5)) 4) (print "PASS: builtin-car-intact") (print "FAIL: builtin-car-intact"))

; --- shared interned identifiers keep closures independent ---
(define f1 (mk-adder 10))
(define f2 (mk-adder 20))
(if (and (equal (f1 1) 11) (equal (f2 1) 21)) (print "PASS: closures-independent") (print "FAIL: closures-independent"))

; sets/quotes built by the runtime still print and compare correctly
(if (equal (union {1} {2}) {1 2}) (print "PASS: symbolic-set-ops-alive") (print "FAIL: symbolic-set-ops-alive"))
(if (equal (in 1 (union {tau : (= tau 1)} {2})) t) (print "PASS: comprehension-union-alive") (print "FAIL: comprehension-union-alive"))


; --- builtin names are RESERVED in operator position (a deliberate rule) ---
; A builtin name can be bound and read as a value, but calling it still reaches
; the builtin. SISP is a Lisp-1 for values and builtin-wins for calls. These
; pin the rule so it cannot drift silently; see the note in eval_named_call
; and the `let` docstring.
(if (equal (let ((car 5)) car) 5)
    (print "PASS: builtin-name-binds-as-value") (print "FAIL: builtin-name-binds-as-value"))
(if (equal (let ((car (lambda (x) 42))) (car (quote (1 2)))) 1)
    (print "PASS: builtin-wins-in-operator-position") (print "FAIL: builtin-wins-in-operator-position"))
; only the C builtins are reserved -- user and prelude functions shadow normally
(define shadow-probe (lambda (x) 1))
(if (equal (let ((shadow-probe (lambda (x) 2))) (shadow-probe 0)) 2)
    (print "PASS: user-function-does-shadow") (print "FAIL: user-function-does-shadow"))
(if (equal (let ((count (lambda (a b) 99))) (count 1 2)) 99)
    (print "PASS: prelude-function-does-shadow") (print "FAIL: prelude-function-does-shadow"))

(print "=== FIRST-CLASS PRIMITIVE TESTS DONE ===")
