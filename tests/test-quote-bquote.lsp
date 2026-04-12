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

; cleanup
(undef myvar x y)

(print "=== QUOTE/BQUOTE TESTS DONE ===")
