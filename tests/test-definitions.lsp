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

(if (= (labels (((f f) f)) (f 7)) 7)
    (print "PASS: labels-name-may-also-be-parameter")
    (print "FAIL: labels-name-may-also-be-parameter"))
(if (eq (catch (labels (((1 x) x)) x) (lambda (e) 'raised)) 'raised)
    (print "PASS: labels-rejects-non-symbol-name")
    (print "FAIL: labels-rejects-non-symbol-name"))
(if (eq (catch (labels (((f x . y) x)) (f 1)) (lambda (e) 'raised)) 'raised)
    (print "PASS: labels-rejects-improper-parameters")
    (print "FAIL: labels-rejects-improper-parameters"))
(if (eq (catch (labels (((f x x) x)) (f 1 2)) (lambda (e) 'raised)) 'raised)
    (print "PASS: labels-rejects-duplicate-parameters")
    (print "FAIL: labels-rejects-duplicate-parameters"))
(if (eq (catch (labels (((f x))) (f 1)) (lambda (e) 'raised)) 'raised)
    (print "PASS: labels-rejects-missing-body")
    (print "FAIL: labels-rejects-missing-body"))

; --- improper parameter lists are rejected everywhere, not only in labels ---
; SISP has no rest parameters, so a dotted or non-list parameter list could
; never be called correctly. It is rejected at definition time, uniformly
; across the binding forms, rather than failing later at call time.
(if (eq (catch (define (dotted-define a . b) a) (lambda (e) 'raised)) 'raised)
    (print "PASS: define-rejects-improper-parameters")
    (print "FAIL: define-rejects-improper-parameters"))
(if (eq (catch (lambda (a . b) a) (lambda (e) 'raised)) 'raised)
    (print "PASS: lambda-rejects-improper-parameters")
    (print "FAIL: lambda-rejects-improper-parameters"))
(if (eq (catch (eval (list 'define (cons 'atom-params 5) 1))
               (lambda (e) 'raised))
        'raised)
    (print "PASS: define-rejects-non-list-parameters")
    (print "FAIL: define-rejects-non-list-parameters"))
(if (eq (catch (eval (list 'define (list 'atom-params2) 1)) (lambda (e) 'raised))
        'raised)
    (print "FAIL: define-accepts-empty-parameter-list")
    (print "PASS: define-accepts-empty-parameter-list"))

; A definition must not alias the caller's parameter list: mutating the list
; afterwards must not rewrite the function that was already defined.
(define alias-params (list 'a 'b))
(eval (list 'define (cons 'alias-fn alias-params) '(+ a b)))
(set-car alias-params 'z)
(if (= (alias-fn 1 2) 3)
    (print "PASS: define-copies-its-parameter-list")
    (print "FAIL: define-copies-its-parameter-list"))

; --- defmacro (expands and evaluates in one call) ---
(defmacro (mywhen c b) `(if ,c ,b nil))

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

; --- macroexpand (iterative, top-level) ---
(defmacro (keep-form x) x)
(if (equal (macroexpand '(keep-form (+ 1 2))) '(+ 1 2))
    (print "PASS: macroexpand-identity")
    (print "FAIL: macroexpand-identity"))
(undef keep-form)

; expands repeatedly until the head is no longer a macro
(defmacro (m-inc x) `(+ ,x 1))
(defmacro (m-inc2 x) `(m-inc (m-inc ,x)))
(if (equal (macroexpand '(m-inc2 y)) '(+ (m-inc y) 1))
    (print "PASS: macroexpand-iterates")
    (print "FAIL: macroexpand-iterates"))
(undef m-inc m-inc2)

; a non-macro form is returned unchanged (no error)
(if (equal (macroexpand '(+ 1 2)) '(+ 1 2))
    (print "PASS: macroexpand-nonmacro")
    (print "FAIL: macroexpand-nonmacro"))

; --- redefine variable ---
(define x 1)
(define x 2)
(if (equal x 2) (print "PASS: redefine-var") (print "FAIL: redefine-var"))

; --- eval ---
(if (equal (eval '(+ 1 2)) 3) (print "PASS: eval-basic") (print "FAIL: eval-basic"))
(if (equal (eval '(* 3 4)) 12) (print "PASS: eval-mul") (print "FAIL: eval-mul"))

; --- direct lambda call and funcall ---
(if (equal ((lambda (x) (+ x 1)) 2) 3)
    (print "PASS: lambda-direct-call")
    (print "FAIL: lambda-direct-call"))
(if (equal (funcall (lambda (x) x) 5) 5)
    (print "PASS: funcall-identity")
    (print "FAIL: funcall-identity"))
(if (equal (let ((x 5)) (funcall (lambda (y) (+ x y)) 3)) 8)
    (print "PASS: funcall-captures-let")
    (print "FAIL: funcall-captures-let"))

; --- apply ---
(if (equal (apply (lambda (a b) (+ a b)) (quote (3 4))) 7)
    (print "PASS: apply-list")
    (print "FAIL: apply-list"))
(if (equal (apply (lambda (a b c) (+ a (+ b c))) 1 (quote (2 3))) 6)
    (print "PASS: apply-spread")
    (print "FAIL: apply-spread"))
(if (equal (apply (lambda (x) x) (quote (42))) 42)
    (print "PASS: apply-single")
    (print "FAIL: apply-single"))

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
(if (equal (subst 'a 'x nil) nil)
    (print "PASS: subst-empty") (print "FAIL: subst-empty"))

; ============================================================
; Lisp-1: one binding per name — value and function definitions
; replace each other (a name cannot be both a value and a function)
; ============================================================

; a function definition makes the name callable
(define (f x) (+ x 1))
(if (equal (f 1) 2)
    (print "PASS: ns-func-callable")
    (print "FAIL: ns-func-callable"))

; defining a value on the same name replaces the function
(define f 99)
(if (equal f 99)
    (print "PASS: ns-value-replaces-func")
    (print "FAIL: ns-value-replaces-func"))

; the value can be redefined
(define f 200)
(if (equal f 200)
    (print "PASS: ns-value-redefine")
    (print "FAIL: ns-value-redefine"))

; defining a function on the same name replaces the value
(define (f x) (* x 2))
(if (equal (f 10) 20)
    (print "PASS: ns-func-replaces-value")
    (print "FAIL: ns-func-replaces-value"))

; a local function shadows a global value of the same name, without leaking
(define shadowed 7)
(if (equal (labels (((shadowed n) (* n n))) (shadowed 4)) 16)
    (print "PASS: ns-local-func-shadows-value")
    (print "FAIL: ns-local-func-shadows-value"))
(if (equal shadowed 7)
    (print "PASS: ns-shadow-no-leak")
    (print "FAIL: ns-shadow-no-leak"))

; CL-style aliases
(setq ns-sv 42)
(if (equal ns-sv 42)
    (print "PASS: setq-alias")
    (print "FAIL: setq-alias"))

; setq changes the nearest lexical binding and leaves a shadowed global alone
(define setq-shadowed 100)
(if (equal (let ((setq-shadowed 1))
             (setq setq-shadowed 2)
             setq-shadowed)
           2)
    (print "PASS: setq-let-local")
    (print "FAIL: setq-let-local"))
(if (equal setq-shadowed 100)
    (print "PASS: setq-let-global-unchanged")
    (print "FAIL: setq-let-global-unchanged"))

; let* bindings use the same nearest-binding assignment rule
(if (equal (let* ((setq-letstar 1)
                  (original setq-letstar))
             (setq setq-letstar 3)
             (+ setq-letstar original))
           4)
    (print "PASS: setq-let*-local")
    (print "FAIL: setq-let*-local"))

; function parameters are lexical variables
(define (setq-parameter x) (setq x 2) x)
(if (equal (setq-parameter 9) 2)
    (print "PASS: setq-parameter")
    (print "FAIL: setq-parameter"))

; closures mutate the binding they captured, so state persists across calls
(define (make-setq-counter start)
  (lambda () (setq start (+ start 1)) start))
(define setq-counter (make-setq-counter 5))
(if (and (equal (setq-counter) 6)
         (equal (setq-counter) 7))
    (print "PASS: setq-captured-binding")
    (print "FAIL: setq-captured-binding"))

; assignment inside labels reaches an enclosing lexical value binding
(if (equal
      (let ((total 0))
        (labels (((sum-items items)
                  (cond ((eq items nil) total)
                        (t (setq total (+ total (car items)))
                           (sum-items (cdr items))))))
          (sum-items '(1 2 3))))
      6)
    (print "PASS: setq-inside-labels")
    (print "FAIL: setq-inside-labels"))

; setq replaces a function of the same name (Lisp-1: one binding)
(define (ns-g x) (+ x 1))
(setq ns-g 50)
(if (equal ns-g 50)
    (print "PASS: setq-replaces-func")
    (print "FAIL: setq-replaces-func"))

; undef removes the single binding
(undef f shadowed ns-sv ns-df ns-g setq-shadowed setq-parameter
       make-setq-counter setq-counter)

; cleanup
(undef x a add1 mysum fact sq test-multi myfact)

(print "=== DEFINITION TESTS DONE ===")
