; ============================================================
; SISP Test Suite — String Operations (length, cat, substr)
; Includes the empty string "" and escaped characters.
; ============================================================

(print "=== STRING TESTS ===")

; --- length of a string ---
(if (equal (length "hello") 5) (print "PASS: strlen-basic") (print "FAIL: strlen-basic"))
(if (equal (length "a") 1) (print "PASS: strlen-single") (print "FAIL: strlen-single"))
(if (equal (length "hello world") 11) (print "PASS: strlen-space") (print "FAIL: strlen-space"))
(if (equal (length "12345678901234567890") 20) (print "PASS: strlen-twenty") (print "FAIL: strlen-twenty"))

; --- cat ---
(if (equal (cat "hello" " world") "hello world") (print "PASS: cat-basic") (print "FAIL: cat-basic"))
(if (equal (cat "a" "b") "ab") (print "PASS: cat-single-chars") (print "FAIL: cat-single-chars"))
(if (equal (length (cat "abc" "def")) 6) (print "PASS: cat-len") (print "FAIL: cat-len"))
(if (equal (cat "foo" "bar") "foobar") (print "PASS: cat-foobar") (print "FAIL: cat-foobar"))

; chained cat
(if (equal (cat (cat "a" "b") "c") "abc") (print "PASS: cat-chained") (print "FAIL: cat-chained"))

; --- substr ---
(if (equal (substr 0 3 "hello") "hel") (print "PASS: substr-basic") (print "FAIL: substr-basic"))
(if (equal (substr 0 5 "hello") "hello") (print "PASS: substr-full") (print "FAIL: substr-full"))
(if (equal (substr 1 3 "hello") "ell") (print "PASS: substr-mid") (print "FAIL: substr-mid"))
(if (equal (substr 0 1 "hello") "h") (print "PASS: substr-single") (print "FAIL: substr-single"))
(if (equal (substr 4 1 "hello") "o") (print "PASS: substr-last") (print "FAIL: substr-last"))

; substr with overflow (start + len > string len, should clamp)
(if (equal (substr 3 10 "hello") "lo") (print "PASS: substr-clamp") (print "FAIL: substr-clamp"))

; invalid indices are catchable errors
(if (eq (catch (substr -1 3 "hello") (lambda (e) 'caught)) 'caught) (print "PASS: substr-neg-start") (print "FAIL: substr-neg-start"))

(if (eq (catch (substr 0 -1 "hello") (lambda (e) 'caught)) 'caught) (print "PASS: substr-neg-len") (print "FAIL: substr-neg-len"))

; escaped text prints and reads without losing quotes, slashes, or controls
(if (equal (car (read-from-string "\"a\\n\\\"b\\\\c\"")) "a\n\"b\\c") (print "PASS: string-escape-roundtrip") (print "FAIL: string-escape-roundtrip"))
(if (eq (catch (code-char 0) (lambda (e) 'caught)) 'caught) (print "PASS: string-rejects-nul") (print "FAIL: string-rejects-nul"))

; substr length check
(if (equal (length (substr 0 3 "hello")) 3) (print "PASS: substr-strlen") (print "FAIL: substr-strlen"))

; string equality
(if (equal (equal "abc" "abc") t) (print "PASS: str-eq-same") (print "FAIL: str-eq-same"))
(if (equal (equal "abc" "abd") nil) (print "PASS: str-eq-diff") (print "FAIL: str-eq-diff"))
(if (equal (equal "abc" "ab") nil) (print "PASS: str-eq-difflen") (print "FAIL: str-eq-difflen"))

; long strings
(define long1 "abcdefghijklmnopqrstuvwxyz")
(define long2 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(if (equal (length long1) 26) (print "PASS: strlen-alpha") (print "FAIL: strlen-alpha"))
(if (equal (length (cat long1 long2)) 52) (print "PASS: cat-long") (print "FAIL: cat-long"))

(undef long1 long2)

; --- empty string ---
(if (equal (length "") 0) (print "PASS: strlen-empty") (print "FAIL: strlen-empty"))
(if (equal (cat "" "abc") "abc") (print "PASS: cat-empty-left") (print "FAIL: cat-empty-left"))
(if (equal (cat "abc" "") "abc") (print "PASS: cat-empty-right") (print "FAIL: cat-empty-right"))
(if (equal (cat "" "") "") (print "PASS: cat-empty-both") (print "FAIL: cat-empty-both"))
(if (equal (substr 0 0 "hello") "") (print "PASS: substr-empty") (print "FAIL: substr-empty"))
(if (equal "" "") (print "PASS: eq-empty") (print "FAIL: eq-empty"))

; --- mixed-case string preservation ---
(if (equal "AbC" "AbC") (print "PASS: str-mixed-case-roundtrip") (print "FAIL: str-mixed-case-roundtrip"))
(if (equal "ABC" "ABC") (print "PASS: str-upper-roundtrip") (print "FAIL: str-upper-roundtrip"))
(if (equal (equal "abc" "ABC") nil) (print "PASS: str-case-sensitive-eq") (print "FAIL: str-case-sensitive-eq"))
(if (equal (length "AbCdEf") 6) (print "PASS: strlen-mixed-case") (print "FAIL: strlen-mixed-case"))
(if (equal (cat "Ab" "Cd") "AbCd") (print "PASS: cat-mixed-case") (print "FAIL: cat-mixed-case"))
(if (equal (substr 0 2 "AbCd") "Ab") (print "PASS: substr-mixed-case") (print "FAIL: substr-mixed-case"))

; --- Reader case: symbol folding vs string preservation ---
; The reader lowercases identifiers (symbol folding) but preserves string case.
; These tests distinguish the two behaviors so a future reader-case change
; (e.g. CL readtable-case) has explicit regression targets.

; Symbol folding: all case variants resolve to the same binding
(define Foo 42)
(if (equal foo 42) (print "PASS: id-case-fold-lower") (print "FAIL: id-case-fold-lower"))
(if (equal FOO 42) (print "PASS: id-case-fold-upper") (print "FAIL: id-case-fold-upper"))
; Quoted symbols also fold: 'Foo and 'foo are eq
(if (equal (eq 'Foo 'foo) t) (print "PASS: sym-fold-quoted") (print "FAIL: sym-fold-quoted"))
(if (equal (eq 'BAR 'bar) t) (print "PASS: sym-fold-quoted-upper") (print "FAIL: sym-fold-quoted-upper"))
(undef foo)

; String preservation: strings are NOT folded
(if (equal (equal "Foo" "foo") nil) (print "PASS: str-no-fold") (print "FAIL: str-no-fold"))
(if (equal (equal "BAR" "bar") nil) (print "PASS: str-no-fold-upper") (print "FAIL: str-no-fold-upper"))
; String content survives round-trip through define
(define str-mixed "AbCdEf")
(if (equal str-mixed "AbCdEf") (print "PASS: str-define-preserves-case") (print "FAIL: str-define-preserves-case"))
(undef str-mixed)

; --- Namespace smoke test: single object table ---
; SISP uses one shared object table for both function and value bindings.
; Defining a function shadows a variable of the same name, and vice versa.
; This documents the current collision behavior so a later function/value
; namespace split (CL-style Lisp-2) has an explicit regression target.
(define ns-x 10)
(define (ns-x n) (+ n 1))
; Redefining ns-x as a function replaces its value binding
(if (equal (ns-x 5) 6) (print "PASS: ns-func-shadows-var") (print "FAIL: ns-func-shadows-var"))
; Redefine as value again — function binding is replaced
(define ns-x 99)
(if (equal ns-x 99) (print "PASS: ns-var-shadows-func") (print "FAIL: ns-var-shadows-func"))
(undef ns-x)

; --- string <-> list/number conversions and search (char = 1-char string) ---
(if (equal (str-to-list "abc") '("a" "b" "c")) (print "PASS: str-to-list") (print "FAIL: str-to-list"))
(if (equal (str-to-list "") nil) (print "PASS: str-to-list-empty") (print "FAIL: str-to-list-empty"))
(if (equal (list-to-str '("a" "b" "c")) "abc") (print "PASS: list-to-str") (print "FAIL: list-to-str"))
(if (equal (list-to-str '("he" "llo")) "hello") (print "PASS: list-to-str-multi") (print "FAIL: list-to-str-multi"))
(if (equal (list-to-str nil) "") (print "PASS: list-to-str-empty") (print "FAIL: list-to-str-empty"))
(if (equal (list-to-str (str-to-list "roundtrip")) "roundtrip") (print "PASS: str-list-roundtrip") (print "FAIL: str-list-roundtrip"))
(if (equal (str-to-num "42") 42) (print "PASS: str-to-num-int") (print "FAIL: str-to-num-int"))
(if (equal (str-to-num "-7") -7) (print "PASS: str-to-num-neg") (print "FAIL: str-to-num-neg"))
(if (equal (str-to-num "3/4") 3/4) (print "PASS: str-to-num-rat") (print "FAIL: str-to-num-rat"))
(if (equal (str-to-num "3.5") 3.5) (print "PASS: str-to-num-float") (print "FAIL: str-to-num-float"))
(if (equal (str-to-num "abc") nil) (print "PASS: str-to-num-bad") (print "FAIL: str-to-num-bad"))
(if (equal (str-to-num "12x") nil) (print "PASS: str-to-num-partial") (print "FAIL: str-to-num-partial"))
(if (equal (num-to-str 42) "42") (print "PASS: num-to-str-int") (print "FAIL: num-to-str-int"))
(if (equal (num-to-str 1/2) "1/2") (print "PASS: num-to-str-rat") (print "FAIL: num-to-str-rat"))
(if (equal (num-to-str 3.14) "3.14") (print "PASS: num-to-str-float") (print "FAIL: num-to-str-float"))
(if (equal (str-to-num (num-to-str 12345)) 12345) (print "PASS: num-str-roundtrip") (print "FAIL: num-str-roundtrip"))
(if (equal (str-find "hello" "lo") 3) (print "PASS: str-find") (print "FAIL: str-find"))
(if (equal (str-find "hello" "xy") nil) (print "PASS: str-find-miss") (print "FAIL: str-find-miss"))
(if (equal (str-find "hello" "he") 0) (print "PASS: str-find-start") (print "FAIL: str-find-start"))

(print "=== STRING TESTS DONE ===")
