; ============================================================
; SISP Test Suite — Equality Hierarchy (eq, equal, =)
; ============================================================
;
; eq      — identity for cons/set/string; value for numbers/identifiers
; equal   — deep structural comparison (original SISP equality)
; =       — numeric only; signals error for non-numeric arguments

(print "=== EQUALITY TESTS ===")

; --- eq: CL-compat cases (these match CL semantics) ---
; eq on singletons nil and t
(if (equal (eq t t) t) (print "PASS: eq-t") (print "FAIL: eq-t"))
(if (equal (eq nil nil) t) (print "PASS: eq-nil") (print "FAIL: eq-nil"))
(if (equal (eq t nil) nil) (print "PASS: eq-t-nil") (print "FAIL: eq-t-nil"))
; eq on symbols (interned identifiers)
(if (equal (eq 'a 'a) t) (print "PASS: eq-symbol") (print "FAIL: eq-symbol"))
(if (equal (eq 'a 'b) nil) (print "PASS: eq-symbol-diff") (print "FAIL: eq-symbol-diff"))
; eq on case-folded symbols: FOO and foo should be eq after reader folding
(if (equal (eq 'foo 'foo) t) (print "PASS: eq-symbol-same-case") (print "FAIL: eq-symbol-same-case"))

; --- eq: SISP-specific behavior ---
; SISP eq compares integers/rationals by value (CL eq is identity-only for these).
(if (equal (eq 1 1) t) (print "PASS: eq-int-value") (print "FAIL: eq-int-value"))
(if (equal (eq 1/2 1/2) t) (print "PASS: eq-rat-value") (print "FAIL: eq-rat-value"))
(if (equal (eq 1 2) nil) (print "PASS: eq-int-diff") (print "FAIL: eq-int-diff"))
; Two distinct lists with same structure are NOT eq (matches CL)
(if (equal (eq '(1 2) '(1 2)) nil) (print "PASS: eq-list-not-identical") (print "FAIL: eq-list-not-identical"))
; Two distinct sets with same elements are NOT eq
(if (equal (eq {1 2} {1 2}) nil) (print "PASS: eq-set-not-identical") (print "FAIL: eq-set-not-identical"))
; Two distinct strings are NOT eq, whatever they contain: eq is identity for
; strings, as its docstring says. The reader does not intern string literals,
; so (eq "a" "a") and (eq "a" (cat "a")) agree: both are nil.
(if (equal (eq "hello" "hello") nil) (print "PASS: eq-string-not-identical") (print "FAIL: eq-string-not-identical"))
(if (equal (eq "hello" (cat "hello")) nil) (print "PASS: eq-string-computed") (print "FAIL: eq-string-computed"))
; equal is the one that looks at content, and is unaffected.
(if (equal (equal "hello" "hello") t) (print "PASS: equal-string-content") (print "FAIL: equal-string-content"))
; a string is still eq to itself
(if (equal (let ((s "hello")) (eq s s)) t) (print "PASS: eq-string-self") (print "FAIL: eq-string-self"))

; eq already compares numbers by value (eq-int-value above), so SISP has no
; separate eql.

; --- no eql: neither the function nor the assoc/member test selector ---
(if (eq (catch (eql 1 1) (lambda (e) 'raised)) 'raised)
    (print "PASS: eql-function-removed")
    (print "FAIL: eql-function-removed"))
(if (eq (catch (in 2 '(1 2 3) 'eql) (lambda (e) 'raised)) 'raised)
    (print "PASS: eql-test-selector-removed")
    (print "FAIL: eql-test-selector-removed"))
(if (equal (in 2 '(1 2 3) 'eq) t)
    (print "PASS: eq-test-selector-works")
    (print "FAIL: eq-test-selector-works"))

; --- = : CL-compat numeric equality ---
(if (equal (= 1 1) t) (print "PASS: numeq-int") (print "FAIL: numeq-int"))
(if (equal (= 1 2) nil) (print "PASS: numeq-int-false") (print "FAIL: numeq-int-false"))
(if (equal (= 1/2 1/2) t) (print "PASS: numeq-rat") (print "FAIL: numeq-rat"))
; cross-type: integer equals rational with same value
(if (equal (= 1 1/1) t) (print "PASS: numeq-cross-type") (print "FAIL: numeq-cross-type"))
(if (equal (= 0 0) t) (print "PASS: numeq-zero") (print "FAIL: numeq-zero"))
(if (equal (= -1 -1) t) (print "PASS: numeq-neg") (print "FAIL: numeq-neg"))
(if (equal (= -1/2 -1/2) t) (print "PASS: numeq-neg-rat") (print "FAIL: numeq-neg-rat"))
; = signals error for non-numeric arguments (CL-compat: type error)
(if (eq (catch (= '(1 2) '(1 2)) (lambda (e) 'raised)) 'raised)
    (print "PASS: numeq-list-error")
    (print "FAIL: numeq-list-error"))
(if (eq (catch (= "a" "b") (lambda (e) 'raised)) 'raised)
    (print "PASS: numeq-string-error")
    (print "FAIL: numeq-string-error"))

; --- equal: deep structural comparison ---
(if (equal (equal '(1 2 3) '(1 2 3)) t) (print "PASS: equal-list") (print "FAIL: equal-list"))
(if (equal (equal '(1 2) '(1 3)) nil) (print "PASS: equal-list-diff") (print "FAIL: equal-list-diff"))
(if (equal (equal "hello" "hello") t) (print "PASS: equal-string") (print "FAIL: equal-string"))
(if (equal (equal "hello" "world") nil) (print "PASS: equal-string-diff") (print "FAIL: equal-string-diff"))
(if (equal (equal {1 2 3} {3 2 1}) t) (print "PASS: equal-set-reorder") (print "FAIL: equal-set-reorder"))
(if (equal (equal {1 2} {1 3}) nil) (print "PASS: equal-set-diff") (print "FAIL: equal-set-diff"))
(if (equal (equal 1 1) t) (print "PASS: equal-int") (print "FAIL: equal-int"))
(if (equal (equal t t) t) (print "PASS: equal-t") (print "FAIL: equal-t"))
(if (equal (equal nil nil) t) (print "PASS: equal-nil") (print "FAIL: equal-nil"))
; nested structures
(if (equal (equal '((1 2) (3 4)) '((1 2) (3 4))) t) (print "PASS: equal-nested") (print "FAIL: equal-nested"))


; --- no equalp: neither the function nor the assoc/in test selector ---
; Case-folding string comparison and cross-type numeric comparison are two
; unrelated concerns, so SISP does not combine them in one predicate.
(if (eq (catch (equalp "A" "a") (lambda (e) 'raised)) 'raised)
    (print "PASS: equalp-function-removed")
    (print "FAIL: equalp-function-removed"))
(if (eq (catch (assoc "HI" '(("hi" found)) 'equalp) (lambda (e) 'raised)) 'raised)
    (print "PASS: equalp-test-selector-removed")
    (print "FAIL: equalp-test-selector-removed"))
; equal is case-sensitive
(if (equal (equal "A" "a") nil) (print "PASS: equal-still-case-sensitive")
    (print "FAIL: equal-still-case-sensitive"))
