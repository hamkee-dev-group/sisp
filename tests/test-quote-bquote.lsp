; ============================================================
; SISP Test Suite — Quote, Backquote, Comma
; ============================================================

(print "=== QUOTE/BQUOTE TESTS ===")

; --- quote ---
(if (equal (quote a) 'a) (print "PASS: quote-basic") (print "FAIL: quote-basic"))
(if (equal '(1 2 3) (quote (1 2 3))) (print "PASS: quote-list") (print "FAIL: quote-list"))
(if (equal 'a 'a) (print "PASS: quote-id") (print "FAIL: quote-id"))

; quote prevents evaluation
(define myvar 42)
(if (equal 'myvar 'myvar) (print "PASS: quote-no-eval") (print "FAIL: quote-no-eval"))
(if (equal (equal 'myvar 42) nil) (print "PASS: quote-not-value") (print "FAIL: quote-not-value"))

; --- backquote with comma ---
(define x 10)
(if (equal `(,x 2 3) '(10 2 3)) (print "PASS: bquote-comma") (print "FAIL: bquote-comma"))
(if (equal `(1 ,x 3) '(1 10 3)) (print "PASS: bquote-comma-mid") (print "FAIL: bquote-comma-mid"))
(if (equal `(,(+ 1 2)) '(3)) (print "PASS: bquote-comma-expr") (print "FAIL: bquote-comma-expr"))

; backquote without comma (pure quote)
(if (equal `(a b c) '(a b c)) (print "PASS: bquote-no-comma") (print "FAIL: bquote-no-comma"))

; nested backquote
(define y 20)
(if (equal `(,x ,y) '(10 20)) (print "PASS: bquote-two-commas") (print "FAIL: bquote-two-commas"))

; backquote with identifier (returns as-is)
(if (equal `a 'a) (print "PASS: bquote-identifier") (print "FAIL: bquote-identifier"))

; --- splicing with ,@ ---
(define lst '(2 3 4))
(if (equal `(1 ,@lst 5) '(1 2 3 4 5)) (print "PASS: bquote-splice-mid") (print "FAIL: bquote-splice-mid"))
(if (equal `(,@lst 5) '(2 3 4 5)) (print "PASS: bquote-splice-head") (print "FAIL: bquote-splice-head"))
(if (equal `(1 ,@lst) '(1 2 3 4)) (print "PASS: bquote-splice-tail") (print "FAIL: bquote-splice-tail"))
(if (equal `(,@lst) '(2 3 4)) (print "PASS: bquote-splice-only") (print "FAIL: bquote-splice-only"))
(if (equal `(,@lst ,@lst) '(2 3 4 2 3 4)) (print "PASS: bquote-splice-twice") (print "FAIL: bquote-splice-twice"))

; splicing nil contributes no elements
(if (equal `(1 ,@nil 2) '(1 2)) (print "PASS: bquote-splice-nil") (print "FAIL: bquote-splice-nil"))

; splice mixed with regular comma
(if (equal `(,x ,@lst ,y) '(10 2 3 4 20)) (print "PASS: bquote-splice-mixed") (print "FAIL: bquote-splice-mixed"))

; splice an expression
(if (equal `(0 ,@(list 1 2) 3) '(0 1 2 3)) (print "PASS: bquote-splice-expr") (print "FAIL: bquote-splice-expr"))

; splicing inside a nested list preserves outer structure
(if (equal `(a (b ,@lst) c) '(a (b 2 3 4) c)) (print "PASS: bquote-splice-nested") (print "FAIL: bquote-splice-nested"))

; cleanup
(undef myvar x y lst)

(print "=== QUOTE/BQUOTE TESTS DONE ===")
