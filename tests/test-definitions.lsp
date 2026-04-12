; ============================================================
; SISP Test Suite — Definitions (define, let, labels, defmacro, lambda)
; ============================================================

(print "=== DEFINITION TESTS ===")

; --- define variable ---
(define x 42)
(if (equal x 42) (print "PASS: define-var") (print "FAIL: define-var"))

; --- define function ---
(define (add1 n) (+ n 1))
(if (equal (add1 5) 6) (print "PASS: define-func") (print "FAIL: define-func"))

; multi-arg function
(define (mysum a b c) (+ a (+ b c)))
(if (equal (mysum 1 2 3) 6) (print "PASS: define-multi-arg") (print "FAIL: define-multi-arg"))

; recursive function
(define (fact n) (if (equal n 0) 1 (* n (fact (+ n -1)))))
(if (equal (fact 0) 1) (print "PASS: fact-0") (print "FAIL: fact-0"))
(if (equal (fact 1) 1) (print "PASS: fact-1") (print "FAIL: fact-1"))
(if (equal (fact 5) 120) (print "PASS: fact-5") (print "FAIL: fact-5"))
(if (equal (fact 10) 3628800) (print "PASS: fact-10") (print "FAIL: fact-10"))

; --- define with lambda uses (define (name args) body) syntax ---
(define (sq x) (* x x))
(if (equal (sq 5) 25) (print "PASS: define-sq") (print "FAIL: define-sq"))
(if (equal (sq 0) 0) (print "PASS: define-sq-zero") (print "FAIL: define-sq-zero"))
(if (equal (sq -3) 9) (print "PASS: define-sq-neg") (print "FAIL: define-sq-neg"))

; function with multiple body forms (last is returned)
(define (test-multi x) (+ x 1) (* x 2))
(if (equal (test-multi 5) 10) (print "PASS: define-multi-body") (print "FAIL: define-multi-body"))

; --- let ---
(if (equal (let ((a 10) (b 20)) (+ a b)) 30) (print "PASS: let-basic") (print "FAIL: let-basic"))

; let should not leak bindings
(define a 999)
(let ((a 1)) a)
(if (equal a 999) (print "PASS: let-no-leak") (print "FAIL: let-no-leak"))

; nested let
(if (equal (let ((x 10)) (let ((y 20)) (+ x y))) 30)
    (print "PASS: let-nested") (print "FAIL: let-nested"))

; let with multiple body expressions
(if (equal (let ((x 1)) (+ x 1) (+ x 2) (+ x 3)) 4)
    (print "PASS: let-multi-body") (print "FAIL: let-multi-body"))

; ============================================================
; let: simultaneous binding (CL-compatible)
; ============================================================
; `let` evaluates ALL init forms before binding any variable,
; so a sibling init form must NOT see bindings from the same `let`.

; Case 1: sibling init referencing an outer binding
;   b must see the OUTER a-outer (100), not the sibling a-outer (1).
(define a-outer 100)
(if (equal (let ((a-outer 1) (b a-outer)) b) 100)
    (print "PASS: let-simultaneous-sibling")
    (print "FAIL: let-simultaneous-sibling"))

; Case 2: sibling init with expression referencing outer binding
(define a-outer2 100)
(if (equal (let ((a-outer2 1) (b (+ a-outer2 1))) b) 101)
    (print "PASS: let-simultaneous-expr")
    (print "FAIL: let-simultaneous-expr"))

; Case 3: three bindings, last references first — must not see sibling
(define x-outer 50)
(if (equal (let ((x-outer 10) (y 20) (c x-outer)) c) 50)
    (print "PASS: let-simultaneous-triple")
    (print "FAIL: let-simultaneous-triple"))

; Case 4: independent bindings that DON'T reference siblings
(if (equal (let ((p 3) (q 7)) (+ p q)) 10)
    (print "PASS: let-independent-binds")
    (print "FAIL: let-independent-binds"))

; Case 5: let restores shadowed globals after exit
(define a-outer3 100)
(let ((a-outer3 1)) a-outer3)
(if (equal a-outer3 100)
    (print "PASS: let-restore-global")
    (print "FAIL: let-restore-global"))

; ============================================================
; let*: sequential binding
; ============================================================
; `let*` evaluates and installs each binding before the next,
; so later init forms CAN see earlier siblings.

; Case 1: sibling sees earlier binding
(if (equal (let* ((a 1) (b a)) b) 1)
    (print "PASS: let*-sequential-sibling")
    (print "FAIL: let*-sequential-sibling"))

; Case 2: expression referencing earlier binding
(if (equal (let* ((a 1) (b (+ a 1))) b) 2)
    (print "PASS: let*-sequential-expr")
    (print "FAIL: let*-sequential-expr"))

; Case 3: let* restores shadowed globals after exit
(define letstar-tmp 99)
(let* ((letstar-tmp 1)) letstar-tmp)
(if (equal letstar-tmp 99)
    (print "PASS: let*-restore-global")
    (print "FAIL: let*-restore-global"))

(undef a-outer a-outer2 a-outer3 x-outer letstar-tmp)

; ============================================================
; Lexical scope regressions
; ============================================================

; Case 1: top-level free variable sees current global value
;   scope-adder is defined at top level (no local env to capture),
;   so free variable scope-var resolves from the global binding.
(define scope-var 10)
(define (scope-adder x) (+ x scope-var))
(define scope-var 20)
(if (equal (scope-adder 1) 21)
    (print "PASS: lexical-scope-global-free-var")
    (print "FAIL: lexical-scope-global-free-var"))

; Case 2: function defined at top level does NOT see let bindings
;   helper is a top-level lambda with no captured env.
;   Calling it inside a let does NOT expose the let's val.
(define val 0)
(define (helper x) (+ x val))
(if (equal (let ((val 5)) (helper 1)) 1)
    (print "PASS: lexical-scope-let-call (top-level fn ignores let)")
    (print "FAIL: lexical-scope-let-call (top-level fn ignores let)"))

; Case 3: function defined inside let captures let's environment
;   escaped-fn closes over escaped-var=42 from the let.
(define escaped-var 0)
(let ((escaped-var 42)) (define (escaped-fn x) (+ x escaped-var)))
(if (equal (escaped-fn 1) 43)
    (print "PASS: lexical-scope-escaped  (closure captures escaped-var=42)")
    (print "FAIL: lexical-scope-escaped  (closure captures escaped-var=42)"))

; Case 4: closure regression — make-adder / add5
(define (make-adder x) (define (adder y) (+ x y)) adder)
(define add5 (make-adder 5))
(if (equal (add5 3) 8)
    (print "PASS: closure-make-adder")
    (print "FAIL: closure-make-adder"))

(undef scope-var scope-adder val escaped-var escaped-fn make-adder add5 adder)

; --- labels (local recursive functions) ---
(if (equal (labels (((myfact n) (if (equal n 0) 1 (* n (myfact (+ n -1)))))) (myfact 5)) 120)
    (print "PASS: labels-fact") (print "FAIL: labels-fact"))

; labels with fibonacci
(if (equal (labels (((fib n) (if (<= n 1) n (+ (fib (+ n -1)) (fib (+ n -2)))))) (fib 10)) 55)
    (print "PASS: labels-fib") (print "FAIL: labels-fib"))

; labels should not leak
(define myfact 'original)
(labels (((myfact n) (if (equal n 0) 1 (* n (myfact (+ n -1)))))) (myfact 3))
(if (equal myfact 'original) (print "PASS: labels-no-leak") (print "FAIL: labels-no-leak"))

; --- defmacro (expands and evaluates in one call) ---
(defmacro (mywhen c b) (if ,c ,b nil))

; basic macro call — no explicit eval needed
(if (equal (mywhen t 42) 42) (print "PASS: defmacro-when-t") (print "FAIL: defmacro-when-t"))
(if (equal (mywhen nil 42) nil) (print "PASS: defmacro-when-nil") (print "FAIL: defmacro-when-nil"))

; macro args are un-evaluated: (/ 1 0) must not execute when branch is not selected
(if (equal (mywhen nil (/ 1 0)) nil)
    (print "PASS: defmacro-unevaluated-arg")
    (print "FAIL: defmacro-unevaluated-arg"))

; expansion evaluated exactly once: side effect fires once
(define eval-counter 0)
(define (bump-counter) (define eval-counter (+ eval-counter 1)) eval-counter)
(defmacro (do-bump) (bump-counter))
(do-bump)
(if (equal eval-counter 1)
    (print "PASS: defmacro-eval-once")
    (print "FAIL: defmacro-eval-once"))
(undef eval-counter bump-counter do-bump)

; --- macroexpand-1 ---
(defmacro (keep-form x) x)
(if (equal (macroexpand-1 '(keep-form (+ 1 2))) '(+ 1 2))
    (print "PASS: macroexpand-1-identity")
    (print "FAIL: macroexpand-1-identity"))
(undef keep-form)

; --- redefine variable ---
(define x 1)
(define x 2)
(if (equal x 2) (print "PASS: redefine-var") (print "FAIL: redefine-var"))

; --- eval ---
(if (equal (eval '(+ 1 2)) 3) (print "PASS: eval-basic") (print "FAIL: eval-basic"))
(if (equal (eval '(* 3 4)) 12) (print "PASS: eval-mul") (print "FAIL: eval-mul"))

; --- undef ---
(define temp 42)
(undef temp)
(if (equal (undef mywhen) t) (print "PASS: undef") (print "FAIL: undef"))

; --- subst: (subst old new body) — find old, replace with new ---
(if (equal (subst 'a 'x '(a b a c a)) '(x b x c x))
    (print "PASS: subst-basic") (print "FAIL: subst-basic"))
; multi-subst uses dotted pairs: ((old1 . new1) (old2 . new2))
(if (equal (subst '((a . x) (b . y)) '(a b c)) '(x y c))
    (print "PASS: subst-multi") (print "FAIL: subst-multi"))
(if (equal (subst 1 99 '(1 2 1 3)) '(99 2 99 3))
    (print "PASS: subst-integer") (print "FAIL: subst-integer"))

; ============================================================
; Namespace separation: value bindings do not clobber function bindings
; ============================================================

; Core acceptance test: f holds value 99 while (f 1) still calls the function
(define (f x) (+ x 1))
(define f 99)
(if (equal f 99)
    (print "PASS: ns-value-binding")
    (print "FAIL: ns-value-binding"))
(if (equal (f 1) 2)
    (print "PASS: ns-func-still-callable")
    (print "FAIL: ns-func-still-callable"))

; Value redefinition does not affect function
(define f 200)
(if (equal f 200)
    (print "PASS: ns-value-redefine")
    (print "FAIL: ns-value-redefine"))
(if (equal (f 10) 11)
    (print "PASS: ns-func-after-value-redefine")
    (print "FAIL: ns-func-after-value-redefine"))

; Function redefinition does not affect value
(define (f x) (* x 2))
(if (equal f 200)
    (print "PASS: ns-value-after-func-redefine")
    (print "FAIL: ns-value-after-func-redefine"))
(if (equal (f 10) 20)
    (print "PASS: ns-func-redefine")
    (print "FAIL: ns-func-redefine"))

; CL-style aliases
(setq ns-sv 42)
(if (equal ns-sv 42)
    (print "PASS: setq-alias")
    (print "FAIL: setq-alias"))
(defun ns-df (x) (+ x 10))
(if (equal (ns-df 5) 15)
    (print "PASS: defun-alias")
    (print "FAIL: defun-alias"))

; setq does not clobber a function with the same name
(define (ns-g x) (+ x 1))
(setq ns-g 50)
(if (equal ns-g 50)
    (print "PASS: setq-no-clobber-func")
    (print "FAIL: setq-no-clobber-func"))
(if (equal (ns-g 1) 2)
    (print "PASS: func-survives-setq")
    (print "FAIL: func-survives-setq"))

; undef removes both namespaces
(undef f ns-sv ns-df ns-g)

; cleanup
(undef x a add1 mysum fact sq test-multi myfact)

(print "=== DEFINITION TESTS DONE ===")
