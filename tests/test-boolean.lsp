; ============================================================
; SISP Test Suite — Boolean Logic (and, or, not, xor, =>, <=>)
; ============================================================

(print "=== BOOLEAN TESTS ===")

; --- AND ---
(if (equal (and t t) t) (print "PASS: and-tt") (print "FAIL: and-tt"))
(if (equal (and t nil) nil) (print "PASS: and-tn") (print "FAIL: and-tn"))
(if (equal (and nil t) nil) (print "PASS: and-nt") (print "FAIL: and-nt"))
(if (equal (and nil nil) nil) (print "PASS: and-nn") (print "FAIL: and-nn"))
(if (equal (and t t t) t) (print "PASS: and-variadic-t") (print "FAIL: and-variadic-t"))
(if (equal (and t t nil) nil) (print "PASS: and-variadic-n") (print "FAIL: and-variadic-n"))
(if (equal (and t t t t t) t) (print "PASS: and-5t") (print "FAIL: and-5t"))
(if (equal (and nil t t t t) nil) (print "PASS: and-short-circuit") (print "FAIL: and-short-circuit"))

; --- OR ---
(if (equal (or t t) t) (print "PASS: or-tt") (print "FAIL: or-tt"))
(if (equal (or t nil) t) (print "PASS: or-tn") (print "FAIL: or-tn"))
(if (equal (or nil t) t) (print "PASS: or-nt") (print "FAIL: or-nt"))
(if (equal (or nil nil) nil) (print "PASS: or-nn") (print "FAIL: or-nn"))
(if (equal (or nil nil nil t) t) (print "PASS: or-variadic") (print "FAIL: or-variadic"))
(if (equal (or nil nil nil nil) nil) (print "PASS: or-all-nil") (print "FAIL: or-all-nil"))
(if (equal (or t nil nil nil nil) t) (print "PASS: or-short-circuit") (print "FAIL: or-short-circuit"))

; --- NOT ---
(if (equal (not t) nil) (print "PASS: not-t") (print "FAIL: not-t"))
(if (equal (not nil) t) (print "PASS: not-nil") (print "FAIL: not-nil"))
(if (equal (not (not t)) t) (print "PASS: not-double") (print "FAIL: not-double"))
(if (equal (not (not nil)) nil) (print "PASS: not-double-nil") (print "FAIL: not-double-nil"))

; --- XOR ---
(if (equal (xor t t) nil) (print "PASS: xor-tt") (print "FAIL: xor-tt"))
(if (equal (xor t nil) t) (print "PASS: xor-tn") (print "FAIL: xor-tn"))
(if (equal (xor nil t) t) (print "PASS: xor-nt") (print "FAIL: xor-nt"))
(if (equal (xor nil nil) nil) (print "PASS: xor-nn") (print "FAIL: xor-nn"))
(if (equal (xor t t t) t) (print "PASS: xor-3t") (print "FAIL: xor-3t"))
(if (equal (xor t nil t) nil) (print "PASS: xor-tnt") (print "FAIL: xor-tnt"))

; --- Implication (=>) ---
(if (equal (=> t t) t) (print "PASS: impl-tt") (print "FAIL: impl-tt"))
(if (equal (=> t nil) nil) (print "PASS: impl-tn") (print "FAIL: impl-tn"))
(if (equal (=> nil t) t) (print "PASS: impl-nt") (print "FAIL: impl-nt"))
(if (equal (=> nil nil) t) (print "PASS: impl-nn") (print "FAIL: impl-nn"))

; --- Biconditional (<=>) ---
(if (equal (<=> t t) t) (print "PASS: iff-tt") (print "FAIL: iff-tt"))
(if (equal (<=> t nil) nil) (print "PASS: iff-tn") (print "FAIL: iff-tn"))
(if (equal (<=> nil t) nil) (print "PASS: iff-nt") (print "FAIL: iff-nt"))
(if (equal (<=> nil nil) t) (print "PASS: iff-nn") (print "FAIL: iff-nn"))
(if (equal (<=> t t t) t) (print "PASS: iff-3t") (print "FAIL: iff-3t"))
(if (equal (<=> t nil t) nil) (print "PASS: iff-tnt") (print "FAIL: iff-tnt"))

; --- Combined logic ---
(if (equal (and (or t nil) (not nil)) t) (print "PASS: logic-combined1") (print "FAIL: logic-combined1"))
(if (equal (or (and t nil) (and t t)) t) (print "PASS: logic-combined2") (print "FAIL: logic-combined2"))
(if (equal (not (and t nil)) t) (print "PASS: logic-combined3") (print "FAIL: logic-combined3"))
(if (equal (=> (and t t) (or nil t)) t) (print "PASS: logic-combined4") (print "FAIL: logic-combined4"))
(if (equal (<=> (not nil) t) t) (print "PASS: logic-combined5") (print "FAIL: logic-combined5"))

; De Morgan's laws
(if (equal (equal (not (and t nil)) (or (not t) (not nil))) t)
    (print "PASS: demorgan1") (print "FAIL: demorgan1"))
(if (equal (equal (not (or t nil)) (and (not t) (not nil))) t)
    (print "PASS: demorgan2") (print "FAIL: demorgan2"))

(print "=== BOOLEAN TESTS DONE ===")
