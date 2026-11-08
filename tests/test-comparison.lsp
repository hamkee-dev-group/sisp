; ============================================================
; SISP Test Suite — Comparison Operations (<, <=, >, >=, =)
; ============================================================

(print "=== COMPARISON TESTS ===")

; --- Less than (<) ---
(if (equal (< 1 2) t) (print "PASS: less-true") (print "FAIL: less-true"))
(if (equal (< 2 1) nil) (print "PASS: less-false") (print "FAIL: less-false"))
(if (equal (< 1 1) nil) (print "PASS: less-equal") (print "FAIL: less-equal"))
(if (equal (< -1 0) t) (print "PASS: less-neg") (print "FAIL: less-neg"))
(if (equal (< -5 -3) t) (print "PASS: less-neg-neg") (print "FAIL: less-neg-neg"))
(if (equal (< 0 0) nil) (print "PASS: less-zero") (print "FAIL: less-zero"))

; rational comparisons
(if (equal (< 1/3 1/2) t) (print "PASS: less-rational") (print "FAIL: less-rational"))
(if (equal (< 1/2 1/3) nil) (print "PASS: less-rational-false") (print "FAIL: less-rational-false"))
(if (equal (< 1/2 1/2) nil) (print "PASS: less-rational-eq") (print "FAIL: less-rational-eq"))

; mixed int/rational
(if (equal (< 0 1/2) t) (print "PASS: less-int-rat") (print "FAIL: less-int-rat"))
(if (equal (< 1 1/2) nil) (print "PASS: less-int-rat-false") (print "FAIL: less-int-rat-false"))
(if (equal (< 1/2 1) t) (print "PASS: less-rat-int") (print "FAIL: less-rat-int"))

; --- Less or equal (<=) ---
(if (equal (<= 1 2) t) (print "PASS: leq-less") (print "FAIL: leq-less"))
(if (equal (<= 2 1) nil) (print "PASS: leq-greater") (print "FAIL: leq-greater"))
(if (equal (<= 1 1) t) (print "PASS: leq-equal") (print "FAIL: leq-equal"))
(if (equal (<= -1 -1) t) (print "PASS: leq-neg-equal") (print "FAIL: leq-neg-equal"))
(if (equal (<= 1/2 1/2) t) (print "PASS: leq-rational-eq") (print "FAIL: leq-rational-eq"))
(if (equal (<= 1/3 1/2) t) (print "PASS: leq-rational") (print "FAIL: leq-rational"))

; --- Greater than (>) ---
(if (equal (> 2 1) t) (print "PASS: great-true") (print "FAIL: great-true"))
(if (equal (> 1 2) nil) (print "PASS: great-false") (print "FAIL: great-false"))
(if (equal (> 1 1) nil) (print "PASS: great-equal") (print "FAIL: great-equal"))
(if (equal (> -1 -5) t) (print "PASS: great-neg") (print "FAIL: great-neg"))
(if (equal (> 1/2 1/3) t) (print "PASS: great-rational") (print "FAIL: great-rational"))
(if (equal (> 1 1/2) t) (print "PASS: great-int-rat") (print "FAIL: great-int-rat"))

; --- Greater or equal (>=) ---
(if (equal (>= 2 1) t) (print "PASS: geq-greater") (print "FAIL: geq-greater"))
(if (equal (>= 1 2) nil) (print "PASS: geq-less") (print "FAIL: geq-less"))
(if (equal (>= 1 1) t) (print "PASS: geq-equal") (print "FAIL: geq-equal"))
(if (equal (>= 1/2 1/2) t) (print "PASS: geq-rational-eq") (print "FAIL: geq-rational-eq"))
(if (equal (>= 1/2 1/3) t) (print "PASS: geq-rational") (print "FAIL: geq-rational"))

; --- Numeric equality (=) ---
(if (equal (= 1 1) t) (print "PASS: eq-int") (print "FAIL: eq-int"))
(if (equal (= 1 2) nil) (print "PASS: eq-int-false") (print "FAIL: eq-int-false"))
(if (equal (= 0 0) t) (print "PASS: eq-zero") (print "FAIL: eq-zero"))
(if (equal (= -1 -1) t) (print "PASS: eq-neg") (print "FAIL: eq-neg"))
(if (equal (= 1/2 1/2) t) (print "PASS: eq-rational") (print "FAIL: eq-rational"))
(if (equal (= 1/2 2/4) t) (print "PASS: eq-rational-reduced") (print "FAIL: eq-rational-reduced"))

; --- Singleton equality ---
(if (equal (equal t t) t) (print "PASS: eq-t") (print "FAIL: eq-t"))
(if (equal (equal nil nil) t) (print "PASS: eq-nil") (print "FAIL: eq-nil"))
(if (equal (equal t nil) nil) (print "PASS: eq-t-nil") (print "FAIL: eq-t-nil"))

; --- SISP-specific: deep structural equality via equal on strings/lists/sets ---
; In CL these would use equal/equalp; here equal is the SISP deep comparator.
(if (equal (equal "hello" "hello") t) (print "PASS: eq-string") (print "FAIL: eq-string"))
(if (equal (equal "hello" "world") nil) (print "PASS: eq-string-false") (print "FAIL: eq-string-false"))
(if (equal (equal '(1 2 3) '(1 2 3)) t) (print "PASS: eq-list") (print "FAIL: eq-list"))
(if (equal (equal '(1 2) '(1 3)) nil) (print "PASS: eq-list-false") (print "FAIL: eq-list-false"))
(if (equal (equal {1 2 3} {1 2 3}) t) (print "PASS: eq-set") (print "FAIL: eq-set"))
(if (equal (equal {1 2 3} {3 2 1}) t) (print "PASS: eq-set-reorder") (print "FAIL: eq-set-reorder"))
(if (equal (equal {1 2} {1 3}) nil) (print "PASS: eq-set-false") (print "FAIL: eq-set-false"))

; edge: large numbers
(if (equal (< 999999 1000000) t) (print "PASS: less-large") (print "FAIL: less-large"))
(if (equal (> 1000000 999999) t) (print "PASS: great-large") (print "FAIL: great-large"))

(print "=== COMPARISON TESTS DONE ===")
