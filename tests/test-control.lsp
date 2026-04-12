; ============================================================
; SISP Test Suite — Control Flow (if, cond, progn, prog1, prog2)
; ============================================================

(print "=== CONTROL FLOW TESTS ===")

; --- if ---
(if (equal (if t 1 2) 1) (print "PASS: if-true") (print "FAIL: if-true"))
(if (equal (if nil 1 2) 2) (print "PASS: if-false") (print "FAIL: if-false"))
(if (equal (if t 42) 42) (print "PASS: if-no-else-true") (print "FAIL: if-no-else-true"))
(if (equal (if nil 42) nil) (print "PASS: if-no-else-false") (print "FAIL: if-no-else-false"))

; if with expressions
(if (equal (if (equal 1 1) 'yes 'no) 'yes) (print "PASS: if-expr-cond") (print "FAIL: if-expr-cond"))
(if (equal (if (equal 1 2) 'yes 'no) 'no) (print "PASS: if-expr-cond2") (print "FAIL: if-expr-cond2"))

; if with multi-expression false branch (progn-like)
(if (equal (if nil 'skip (+ 1 1) (+ 2 2) (+ 3 3)) 6)
    (print "PASS: if-multi-false") (print "FAIL: if-multi-false"))

; nested if
(if (equal (if t (if t 'deep 'wrong) 'wrong) 'deep)
    (print "PASS: if-nested") (print "FAIL: if-nested"))
(if (equal (if nil 'wrong (if nil 'wrong 'right)) 'right)
    (print "PASS: if-nested-false") (print "FAIL: if-nested-false"))

; --- cond ---
(if (equal (cond (nil 'no) (t 'yes)) 'yes)
    (print "PASS: cond-basic") (print "FAIL: cond-basic"))
(if (equal (cond (t 'first) (t 'second)) 'first)
    (print "PASS: cond-first-match") (print "FAIL: cond-first-match"))
(if (equal (cond (nil 'no) (nil 'no) (t 'yes)) 'yes)
    (print "PASS: cond-skip-nil") (print "FAIL: cond-skip-nil"))
(if (equal (cond ((equal 1 1) 'one) ((equal 2 2) 'two)) 'one)
    (print "PASS: cond-expr") (print "FAIL: cond-expr"))
(if (equal (cond ((equal 1 2) 'no) ((equal 2 2) 'yes)) 'yes)
    (print "PASS: cond-second") (print "FAIL: cond-second"))

; cond with multi-body
(if (equal (cond (t (+ 1 1) (+ 2 2))) 4)
    (print "PASS: cond-multi-body") (print "FAIL: cond-multi-body"))

; --- progn ---
(if (equal (progn 1 2 3) 3) (print "PASS: progn-basic") (print "FAIL: progn-basic"))
(if (equal (progn (+ 1 1) (+ 2 2) (+ 3 3)) 6) (print "PASS: progn-exprs") (print "FAIL: progn-exprs"))
(if (equal (progn 42) 42) (print "PASS: progn-single") (print "FAIL: progn-single"))

; --- prog1 ---
(if (equal (prog1 1 2 3) 1) (print "PASS: prog1-basic") (print "FAIL: prog1-basic"))
(if (equal (prog1 (+ 1 1) (+ 2 2) (+ 3 3)) 2) (print "PASS: prog1-expr") (print "FAIL: prog1-expr"))
(if (equal (prog1 42) 42) (print "PASS: prog1-single") (print "FAIL: prog1-single"))

; --- prog2 ---
(if (equal (prog2 1 2 3) 2) (print "PASS: prog2-basic") (print "FAIL: prog2-basic"))
(if (equal (prog2 (+ 1 1) (+ 2 2) (+ 3 3)) 4) (print "PASS: prog2-expr") (print "FAIL: prog2-expr"))
(if (equal (prog2 42 99) 99) (print "PASS: prog2-two") (print "FAIL: prog2-two"))

; --- Side effects in progn ---
(define side-test 0)
(progn (define side-test 1) (define side-test 2) (define side-test 3))
(if (equal side-test 3) (print "PASS: progn-side-effects") (print "FAIL: progn-side-effects"))

; prog1 still evaluates all but returns first
(define counter 0)
(prog1 (define counter 10) (define counter 20))
(if (equal counter 20) (print "PASS: prog1-side-effects") (print "FAIL: prog1-side-effects"))

(undef side-test counter)

(print "=== CONTROL FLOW TESTS DONE ===")
