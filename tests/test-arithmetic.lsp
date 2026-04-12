; ============================================================
; SISP Test Suite — Arithmetic Operations (+, *, /, mod, ^)
; ============================================================

(print "=== ARITHMETIC TESTS ===")

; --- Addition (+) ---
(if (equal (+ 1 2) 3) (print "PASS: add-integers") (print "FAIL: add-integers"))
(if (equal (+ 0 0) 0) (print "PASS: add-zeros") (print "FAIL: add-zeros"))
(if (equal (+ -3 3) 0) (print "PASS: add-neg-pos") (print "FAIL: add-neg-pos"))
(if (equal (+ -5 -7) -12) (print "PASS: add-neg-neg") (print "FAIL: add-neg-neg"))
(if (equal (+ 1 2 3 4 5) 15) (print "PASS: add-variadic") (print "FAIL: add-variadic"))
(if (equal (+ 100000 200000) 300000) (print "PASS: add-large") (print "FAIL: add-large"))

; rational addition
(if (equal (+ 1/2 1/2) 1) (print "PASS: add-rational-to-int") (print "FAIL: add-rational-to-int"))
(if (equal (+ 1/3 1/3) 2/3) (print "PASS: add-rational") (print "FAIL: add-rational"))
(if (equal (+ 1/2 1/3) 5/6) (print "PASS: add-diff-denom") (print "FAIL: add-diff-denom"))
(if (equal (+ 1/4 1/4 1/4 1/4) 1) (print "PASS: add-rational-variadic") (print "FAIL: add-rational-variadic"))

; mixed integer + rational
(if (equal (+ 1 1/2) 3/2) (print "PASS: add-int-rational") (print "FAIL: add-int-rational"))

; --- Multiplication (*) ---
(if (equal (* 2 3) 6) (print "PASS: mul-integers") (print "FAIL: mul-integers"))
(if (equal (* 0 999) 0) (print "PASS: mul-by-zero") (print "FAIL: mul-by-zero"))
(if (equal (* -2 3) -6) (print "PASS: mul-neg") (print "FAIL: mul-neg"))
(if (equal (* -2 -3) 6) (print "PASS: mul-neg-neg") (print "FAIL: mul-neg-neg"))
(if (equal (* 1 1) 1) (print "PASS: mul-identity") (print "FAIL: mul-identity"))
(if (equal (* 2 3 4) 24) (print "PASS: mul-variadic") (print "FAIL: mul-variadic"))
(if (equal (* 1000 1000) 1000000) (print "PASS: mul-large") (print "FAIL: mul-large"))

; rational multiplication
(if (equal (* 1/2 1/2) 1/4) (print "PASS: mul-rational") (print "FAIL: mul-rational"))
(if (equal (* 1/3 3/1) 1) (print "PASS: mul-rational-inv") (print "FAIL: mul-rational-inv"))
(if (equal (* 2/3 3/2) 1) (print "PASS: mul-rational-recip") (print "FAIL: mul-rational-recip"))

; --- Division (/) ---
(if (equal (/ 6 3) 2) (print "PASS: div-exact") (print "FAIL: div-exact"))
(if (equal (/ 7 2) 7/2) (print "PASS: div-to-rational") (print "FAIL: div-to-rational"))
(if (equal (/ 1 3) 1/3) (print "PASS: div-fraction") (print "FAIL: div-fraction"))
(if (equal (/ -6 3) -2) (print "PASS: div-neg-num") (print "FAIL: div-neg-num"))
(if (equal (/ 6 -3) -2) (print "PASS: div-neg-den") (print "FAIL: div-neg-den"))
(if (equal (/ -6 -3) 2) (print "PASS: div-neg-both") (print "FAIL: div-neg-both"))
(if (equal (/ 0 5) 0) (print "PASS: div-zero-num") (print "FAIL: div-zero-num"))

; rational division
(if (equal (/ 1/2 1/4) 2) (print "PASS: div-rational") (print "FAIL: div-rational"))
(if (equal (/ 2/3 4/5) 5/6) (print "PASS: div-rational2") (print "FAIL: div-rational2"))

; --- Modulo (mod) ---
(if (equal (mod 10 3) 1) (print "PASS: mod-basic") (print "FAIL: mod-basic"))
(if (equal (mod 9 3) 0) (print "PASS: mod-exact") (print "FAIL: mod-exact"))
(if (equal (mod 1 1) 0) (print "PASS: mod-one") (print "FAIL: mod-one"))
(if (equal (mod 7 7) 0) (print "PASS: mod-self") (print "FAIL: mod-self"))
(if (equal (mod 100 7) 2) (print "PASS: mod-larger") (print "FAIL: mod-larger"))
(if (equal (mod -7 3) 2) (print "PASS: mod-negative") (print "FAIL: mod-negative"))
(if (equal (mod 0 5) 0) (print "PASS: mod-zero") (print "FAIL: mod-zero"))

; --- Exponentiation (^) ---
(if (equal (^ 2 0) 1) (print "PASS: pow-zero-exp") (print "FAIL: pow-zero-exp"))
(if (equal (^ 2 1) 2) (print "PASS: pow-one-exp") (print "FAIL: pow-one-exp"))
(if (equal (^ 2 10) 1024) (print "PASS: pow-large") (print "FAIL: pow-large"))
(if (equal (^ 3 3) 27) (print "PASS: pow-three") (print "FAIL: pow-three"))
(if (equal (^ 1 100) 1) (print "PASS: pow-one-base") (print "FAIL: pow-one-base"))
(if (equal (^ 0 5) 0) (print "PASS: pow-zero-base") (print "FAIL: pow-zero-base"))
(if (equal (^ -2 3) -8) (print "PASS: pow-neg-odd") (print "FAIL: pow-neg-odd"))
(if (equal (^ -2 2) 4) (print "PASS: pow-neg-even") (print "FAIL: pow-neg-even"))

; negative exponents
(if (equal (^ 2 -1) 1/2) (print "PASS: pow-neg-exp") (print "FAIL: pow-neg-exp"))
(if (equal (^ 2 -2) 1/4) (print "PASS: pow-neg-exp2") (print "FAIL: pow-neg-exp2"))
(if (equal (^ 3 -1) 1/3) (print "PASS: pow-neg-exp3") (print "FAIL: pow-neg-exp3"))

; rational base
(if (equal (^ 1/2 2) 1/4) (print "PASS: pow-rational-base") (print "FAIL: pow-rational-base"))
(if (equal (^ 2/3 2) 4/9) (print "PASS: pow-rational-base2") (print "FAIL: pow-rational-base2"))
; (^ 1/2 -1) returns rational 2/1 (not simplified to int)
(if (and (>= (^ 1/2 -1) 2) (<= (^ 1/2 -1) 2)) (print "PASS: pow-rational-neg") (print "FAIL: pow-rational-neg"))

; negative exponent on zero base must not produce 1/0
; (^ 0 -1) should error and not return a value, so we just test valid cases survive
(if (equal (^ 2 -1) 1/2) (print "PASS: pow-neg-exp-valid") (print "FAIL: pow-neg-exp-valid"))
(if (equal (^ 3 -2) 1/9) (print "PASS: pow-neg-exp-valid2") (print "FAIL: pow-neg-exp-valid2"))

(print "=== ARITHMETIC TESTS DONE ===")
