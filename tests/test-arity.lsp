; ============================================================
; SISP Test Suite — Primitive arity
;
; Builtins read their arguments positionally with car/cadr/caddr, so before
; the arity check a missing argument silently read as nil and a surplus one
; was silently discarded.  That is how the old binary cat dropped its third
; string and cap ignored its third set: both looked like working programs.
; Every primitive call now goes through invoke_builtin, which checks the
; count against the builtin's spec.
;
; Three cases per shape: too few, exactly right, too many.
; ============================================================

(print "=== ARITY TESTS ===")

; --- fixed unary -------------------------------------------------
(if (eq (catch (car) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-car-rejects-zero")
    (print "FAIL: arity-car-rejects-zero"))
(if (equal (car (list 1 2)) 1)
    (print "PASS: arity-car-accepts-one")
    (print "FAIL: arity-car-accepts-one"))
(if (eq (catch (car (list 1 2) 'extra) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-car-rejects-two")
    (print "FAIL: arity-car-rejects-two"))

(if (eq (catch (length "ab" 'extra) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-strlen-rejects-extra")
    (print "FAIL: arity-strlen-rejects-extra"))
(if (eq (catch (typeof 1 'extra) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-typeof-rejects-extra")
    (print "FAIL: arity-typeof-rejects-extra"))
(if (eq (catch (null) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-null-rejects-zero")
    (print "FAIL: arity-null-rejects-zero"))

; --- fixed binary ------------------------------------------------
(if (eq (catch (cons 1) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-cons-rejects-one")
    (print "FAIL: arity-cons-rejects-one"))
(if (equal (cons 1 nil) (list 1))
    (print "PASS: arity-cons-accepts-two")
    (print "FAIL: arity-cons-accepts-two"))
(if (eq (catch (cons 1 nil 'extra) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-cons-rejects-three")
    (print "FAIL: arity-cons-rejects-three"))

(if (eq (catch (nth 0 (list 1 2) 'extra) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-nth-rejects-extra")
    (print "FAIL: arity-nth-rejects-extra"))
(if (eq (catch (mod 7) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-mod-rejects-one")
    (print "FAIL: arity-mod-rejects-one"))

; --- fixed ternary / quaternary ----------------------------------
(if (eq (catch (substr 0 1) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-substr-rejects-two")
    (print "FAIL: arity-substr-rejects-two"))
(if (equal (substr 0 1 "abc") "a")
    (print "PASS: arity-substr-accepts-three")
    (print "FAIL: arity-substr-accepts-three"))

; --- optional trailing argument ----------------------------------
; make-matrix takes rows and cols, with an optional fill value.
(if (eq (catch (make-matrix 2) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-make-matrix-rejects-one")
    (print "FAIL: arity-make-matrix-rejects-one"))
(if (= (mat-rows (make-matrix 2 3)) 2)
    (print "PASS: arity-make-matrix-accepts-two")
    (print "FAIL: arity-make-matrix-accepts-two"))
(if (= (mat-ref (make-matrix 2 3 7.5) 0 0) 7.5)
    (print "PASS: arity-make-matrix-accepts-optional-fill")
    (print "FAIL: arity-make-matrix-accepts-optional-fill"))
(if (eq (catch (make-matrix 2 3 7.5 'extra) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-make-matrix-rejects-four")
    (print "FAIL: arity-make-matrix-rejects-four"))

; --- genuinely variadic: must NOT be restricted ------------------
(if (= (+ 1 2 3 4 5 6 7 8) 36)
    (print "PASS: arity-plus-is-variadic")
    (print "FAIL: arity-plus-is-variadic"))
(if (= (+) 0)
    (print "PASS: arity-plus-accepts-zero")
    (print "FAIL: arity-plus-accepts-zero"))
(if (equal (cat "a" "b" "c" "d") "abcd")
    (print "PASS: arity-cat-is-variadic")
    (print "FAIL: arity-cat-is-variadic"))
(if (= (length (list 1 2 3 4 5 6)) 6)
    (print "PASS: arity-list-is-variadic")
    (print "FAIL: arity-list-is-variadic"))

; --- set operations ----------------------------------------------
; cap is N-ary with a floor of two; diff is strictly binary.
(if (eq (catch (cap {1 2}) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-cap-rejects-one")
    (print "FAIL: arity-cap-rejects-one"))
(if (equal (cap {1 2 3} {2 3} {3}) {3})
    (print "PASS: arity-cap-is-n-ary")
    (print "FAIL: arity-cap-is-n-ary"))
(if (eq (catch (diff {1} {2} {3}) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-diff-rejects-three")
    (print "FAIL: arity-diff-rejects-three"))

; --- a primitive reached as a VALUE is checked too ---------------
; (map car lst) calls car through apply_function, not as a named form.
(if (equal (map car (list (list 1 2) (list 3 4))) (list 1 3))
    (print "PASS: arity-primitive-as-value-still-works")
    (print "FAIL: arity-primitive-as-value-still-works"))
(if (eq (catch (apply car (list (list 1 2) 'extra)) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-primitive-as-value-is-checked")
    (print "FAIL: arity-primitive-as-value-is-checked"))

; --- the error names the builtin and the bound -------------------
(if (eq (car (catch (car (list 1) 'extra) (lambda (e) e))) 'error)
    (print "PASS: arity-error-is-catchable")
    (print "FAIL: arity-error-is-catchable"))

; --- an improper argument list is malformed, not "missing an argument" ---
; list_length stops at the first non-cons, so without an explicit properness
; check (car x . 5) silently drops its tail and (cons 1 . 2) reports a
; missing argument rather than the dotted list that is actually wrong.
(if (eq (catch (car (quote (1 2)) . 5) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-rejects-dotted-argument-list")
    (print "FAIL: arity-rejects-dotted-argument-list"))
(if (eq (catch (cons 1 . 2) (lambda (e) 'raised)) 'raised)
    (print "PASS: arity-rejects-dotted-tail-on-binary")
    (print "FAIL: arity-rejects-dotted-tail-on-binary"))

; --- aliases of one C implementation stay equal -------------------
(if (eq (equal car cdr) nil)
    (print "PASS: arity-distinct-builtins-differ")
    (print "FAIL: arity-distinct-builtins-differ"))

(print "=== ARITY TESTS DONE ===")
