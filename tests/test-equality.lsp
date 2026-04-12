; ============================================================
; SISP Test Suite — Equality Hierarchy (eq, eql, equal, equalp, =)
; ============================================================
;
; eq      — identity for cons/set/string; value for numbers/identifiers
; eql     — same as eq for SISP's type system
; equal   — deep structural comparison (original SISP equality)
; equalp  — like equal but case-insensitive strings, cross-type numeric
; =       — numeric only; signals error for non-numeric arguments
; sisp-equal — compatibility alias for equal

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
; Interned strings with same content ARE eq (parser interns strings)
(if (equal (eq "hello" "hello") t) (print "PASS: eq-string-interned") (print "FAIL: eq-string-interned"))

; --- eql: same as eq for SISP ---
(if (equal (eql 1 1) t) (print "PASS: eql-int") (print "FAIL: eql-int"))
(if (equal (eql '(1 2) '(1 2)) nil) (print "PASS: eql-list-not-identical") (print "FAIL: eql-list-not-identical"))
(if (equal (eql t t) t) (print "PASS: eql-t") (print "FAIL: eql-t"))
(if (equal (eql "hello" "hello") t) (print "PASS: eql-string-interned") (print "FAIL: eql-string-interned"))

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
(define numeq-list-flag "PASS")
(progn (= '(1 2) '(1 2)) (define numeq-list-flag "FAIL"))
(print (cat numeq-list-flag ": numeq-list-error"))
(define numeq-str-flag "PASS")
(progn (= "a" "b") (define numeq-str-flag "FAIL"))
(print (cat numeq-str-flag ": numeq-string-error"))

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

; --- equalp: case-insensitive strings, cross-type numeric ---
(if (equal (equalp "Hello" "hello") t) (print "PASS: equalp-case-insensitive") (print "FAIL: equalp-case-insensitive"))
(if (equal (equalp "ABC" "abc") t) (print "PASS: equalp-case-upper") (print "FAIL: equalp-case-upper"))
(if (equal (equalp "hello" "world") nil) (print "PASS: equalp-string-diff") (print "FAIL: equalp-string-diff"))
(if (equal (equalp '(1 2 3) '(1 2 3)) t) (print "PASS: equalp-list") (print "FAIL: equalp-list"))
(if (equal (equalp {1 2 3} {3 2 1}) t) (print "PASS: equalp-set") (print "FAIL: equalp-set"))
(if (equal (equalp 1 1) t) (print "PASS: equalp-int") (print "FAIL: equalp-int"))
; equalp case-insensitive in nested structures
(if (equal (equalp '("Hello" "World") '("hello" "world")) t)
    (print "PASS: equalp-nested-case") (print "FAIL: equalp-nested-case"))

; --- sisp-equal: compatibility alias for equal ---
(if (equal (sisp-equal '(1 2 3) '(1 2 3)) t) (print "PASS: sisp-equal-list") (print "FAIL: sisp-equal-list"))
(if (equal (sisp-equal "hello" "hello") t) (print "PASS: sisp-equal-string") (print "FAIL: sisp-equal-string"))
(if (equal (sisp-equal {1 2} {2 1}) t) (print "PASS: sisp-equal-set") (print "FAIL: sisp-equal-set"))

; cleanup
(undef numeq-list-flag numeq-str-flag)

(print "=== EQUALITY TESTS DONE ===")
