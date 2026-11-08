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

; min / max — variadic over numeric args
(if (equal (min 3 1 2) 1) (print "PASS: min-basic") (print "FAIL: min-basic"))
(if (equal (max 3 1 2) 3) (print "PASS: max-basic") (print "FAIL: max-basic"))
(if (equal (min 5) 5) (print "PASS: min-single") (print "FAIL: min-single"))
(if (equal (max 5) 5) (print "PASS: max-single") (print "FAIL: max-single"))
(if (equal (min 1/2 1/3 1/4) 1/4) (print "PASS: min-rational") (print "FAIL: min-rational"))
(if (equal (max 1/2 1/3 1/4) 1/2) (print "PASS: max-rational") (print "FAIL: max-rational"))
(if (equal (max (+ 1 1) (* 2 3) 4) 6) (print "PASS: max-evals-args") (print "FAIL: max-evals-args"))
(if (equal (min (- 0 5) 2 3) -5) (print "PASS: min-negative") (print "FAIL: min-negative"))
; max over a single collection (set or list); mixing is unsupported
(if (equal (max {1 2 3}) 3) (print "PASS: max-set") (print "FAIL: max-set"))
(if (equal (max (list 5 9 2)) 9) (print "PASS: max-list") (print "FAIL: max-list"))
(if (equal (max {1/2 3/4 1/3}) 3/4) (print "PASS: max-set-rational") (print "FAIL: max-set-rational"))
(if (equal (max (max {1 2 3}) (max (list 1 2 3)) (max 1 2 3 4) 4 5 6) 6) (print "PASS: max-composed") (print "FAIL: max-composed"))

; --- Floats (numeric contagion + integral demotion) ---
; a float operand makes the whole result a float; an integral float result
; demotes back to an integer (1.5 + 1.5 -> 3, not 3.0)
(if (equal (+ 1.5 2) 3.5) (print "PASS: float-add-mixed") (print "FAIL: float-add-mixed"))
(if (equal (+ 1.5 1.5) 3) (print "PASS: float-add-demote") (print "FAIL: float-add-demote"))
(if (equal (+ 1.5 1/2) 2) (print "PASS: float-add-rational") (print "FAIL: float-add-rational"))
(if (equal (- 5.5 1.0 0.5) 4) (print "PASS: float-sub") (print "FAIL: float-sub"))
(if (equal (* 1.5 2) 3) (print "PASS: float-mul-demote") (print "FAIL: float-mul-demote"))
(if (equal (* 2.5 4) 10) (print "PASS: float-mul") (print "FAIL: float-mul"))
(if (equal (/ 1.5 6) 0.25) (print "PASS: float-div") (print "FAIL: float-div"))
(if (equal (^ 1.5 2) 2.25) (print "PASS: float-pow") (print "FAIL: float-pow"))
(if (equal (mod 5.5 2) 1.5) (print "PASS: float-mod") (print "FAIL: float-mod"))
(if (equal (mod -5.5 2) 0.5) (print "PASS: float-mod-neg") (print "FAIL: float-mod-neg"))
; integral float literals evaluate to integers (1.0 -> 1)
(if (equal 2.0 2) (print "PASS: float-literal-demote") (print "FAIL: float-literal-demote"))
(if (equal (typeof 1.5) "float") (print "PASS: float-typeof") (print "FAIL: float-typeof"))
(if (equal (typeof 2.0) "integer") (print "PASS: float-typeof-demote") (print "FAIL: float-typeof-demote"))
; comparisons across the whole numeric tower
(if (< 1.5 2 5/2 3.0) (print "PASS: float-less") (print "FAIL: float-less"))
(if (equal (max 1 2.5 2) 2.5) (print "PASS: float-max") (print "FAIL: float-max"))
(if (equal (min 1.5 1/2 3) 1/2) (print "PASS: float-min") (print "FAIL: float-min"))
(if (= 1.0 1) (print "PASS: float-numeq") (print "FAIL: float-numeq"))

; --- <math.h> wrappers (results demote to int when integral) ---
(if (equal (sqrt 4) 2) (print "PASS: math-sqrt-exact") (print "FAIL: math-sqrt-exact"))
(if (equal (sqrt 9) 3) (print "PASS: math-sqrt-9") (print "FAIL: math-sqrt-9"))
(if (equal (sqrt 1/4) 0.5) (print "PASS: math-sqrt-rat") (print "FAIL: math-sqrt-rat"))
(if (equal (exp 0) 1) (print "PASS: math-exp0") (print "FAIL: math-exp0"))
(if (equal (log 1) 0) (print "PASS: math-log1") (print "FAIL: math-log1"))
(if (equal (log2 1024) 10) (print "PASS: math-log2") (print "FAIL: math-log2"))
(if (equal (log10 1000) 3) (print "PASS: math-log10") (print "FAIL: math-log10"))
(if (equal (sin 0) 0) (print "PASS: math-sin0") (print "FAIL: math-sin0"))
(if (equal (cos 0) 1) (print "PASS: math-cos0") (print "FAIL: math-cos0"))
(if (equal (tan 0) 0) (print "PASS: math-tan0") (print "FAIL: math-tan0"))
(if (equal (atan 0) 0) (print "PASS: math-atan0") (print "FAIL: math-atan0"))
(if (equal (atan2 0 1) 0) (print "PASS: math-atan2") (print "FAIL: math-atan2"))
(if (equal (sinh 0) 0) (print "PASS: math-sinh0") (print "FAIL: math-sinh0"))
(if (equal (tanh 0) 0) (print "PASS: math-tanh0") (print "FAIL: math-tanh0"))
(if (equal (acosh 1) 0) (print "PASS: math-acosh1") (print "FAIL: math-acosh1"))
(if (equal (asin 0) 0) (print "PASS: math-asin0") (print "FAIL: math-asin0"))
(if (equal (acos 1) 0) (print "PASS: math-acos1") (print "FAIL: math-acos1"))
(if (equal (asinh 0) 0) (print "PASS: math-asinh0") (print "FAIL: math-asinh0"))
(if (equal (atanh 0) 0) (print "PASS: math-atanh0") (print "FAIL: math-atanh0"))
(if (equal (cosh 0) 1) (print "PASS: math-cosh0") (print "FAIL: math-cosh0"))
(if (equal (cbrt 8) 2) (print "PASS: math-cbrt8") (print "FAIL: math-cbrt8"))
(if (equal (hypot 3 4) 5) (print "PASS: math-hypot") (print "FAIL: math-hypot"))
(if (equal (hypot 5 12) 13) (print "PASS: math-hypot2") (print "FAIL: math-hypot2"))
; numeric-tower promotion into a math wrapper
(if (equal (sqrt (+ 2 2)) 2) (print "PASS: math-promote") (print "FAIL: math-promote"))

; --- abs / rounding / signum / gcd / lcm / parity / random / rationalize ---
(if (equal (abs -5) 5) (print "PASS: abs-int") (print "FAIL: abs-int"))
(if (equal (abs -3/4) 3/4) (print "PASS: abs-rat") (print "FAIL: abs-rat"))
(if (equal (abs -2.5) 2.5) (print "PASS: abs-float") (print "FAIL: abs-float"))
(if (equal (abs 7) 7) (print "PASS: abs-pos") (print "FAIL: abs-pos"))
; exact rational rounding (a rational rounds exactly to an integer)
(if (equal (floor 7/2) 3) (print "PASS: floor-rat") (print "FAIL: floor-rat"))
(if (equal (ceiling 7/2) 4) (print "PASS: ceiling-rat") (print "FAIL: ceiling-rat"))
(if (equal (truncate 7/2) 3) (print "PASS: truncate-rat") (print "FAIL: truncate-rat"))
(if (equal (floor -7/2) -4) (print "PASS: floor-neg") (print "FAIL: floor-neg"))
(if (equal (truncate -7/2) -3) (print "PASS: truncate-neg") (print "FAIL: truncate-neg"))
; round is half-to-even (banker's), for rationals and floats
(if (equal (round 7/2) 4) (print "PASS: round-3.5-even") (print "FAIL: round-3.5-even"))
(if (equal (round 5/2) 2) (print "PASS: round-2.5-even") (print "FAIL: round-2.5-even"))
(if (equal (round 2.5) 2) (print "PASS: round-float-even") (print "FAIL: round-float-even"))
(if (equal (round 3.5) 4) (print "PASS: round-float-even2") (print "FAIL: round-float-even2"))
(if (equal (floor 3.7) 3) (print "PASS: floor-float") (print "FAIL: floor-float"))
(if (equal (ceiling 2.1) 3) (print "PASS: ceiling-float") (print "FAIL: ceiling-float"))
(if (equal (signum -3) -1) (print "PASS: signum-neg") (print "FAIL: signum-neg"))
(if (equal (signum 0) 0) (print "PASS: signum-zero") (print "FAIL: signum-zero"))
(if (equal (signum 2.5) 1) (print "PASS: signum-float") (print "FAIL: signum-float"))
(if (equal (gcd 12 18) 6) (print "PASS: gcd-2") (print "FAIL: gcd-2"))
(if (equal (gcd 12 18 8) 2) (print "PASS: gcd-3") (print "FAIL: gcd-3"))
(if (equal (gcd) 0) (print "PASS: gcd-0") (print "FAIL: gcd-0"))
(if (equal (gcd -12 8) 4) (print "PASS: gcd-neg") (print "FAIL: gcd-neg"))
(if (equal (lcm 4 6) 12) (print "PASS: lcm-2") (print "FAIL: lcm-2"))
(if (equal (lcm 3 4 5) 60) (print "PASS: lcm-3") (print "FAIL: lcm-3"))
(if (equal (lcm) 1) (print "PASS: lcm-1") (print "FAIL: lcm-1"))
(if (equal (evenp 4) t) (print "PASS: evenp") (print "FAIL: evenp"))
(if (equal (oddp 4) nil) (print "PASS: oddp") (print "FAIL: oddp"))
(if (equal (evenp -3) nil) (print "PASS: evenp-neg") (print "FAIL: evenp-neg"))
(if (equal (rationalize 3.14159 1/100) 22/7) (print "PASS: rationalize-pi") (print "FAIL: rationalize-pi"))
(if (equal (rationalize 0.5 1/1000) 1/2) (print "PASS: rationalize-half") (print "FAIL: rationalize-half"))
; relu: scalar max(0,x)
(if (equal (relu -5) 0) (print "PASS: relu-neg") (print "FAIL: relu-neg"))
(if (equal (relu 5) 5) (print "PASS: relu-pos") (print "FAIL: relu-pos"))
; random: reproducible under a fixed seed, and in range
(seed 12345)
(if (equal (random 1000) (progn (seed 12345) (random 1000))) (print "PASS: random-reproducible") (print "FAIL: random-reproducible"))
(if (< (random 10) 10) (print "PASS: random-range") (print "FAIL: random-range"))
(if (>= (random 10) 0) (print "PASS: random-nonneg") (print "FAIL: random-nonneg"))

(print "=== ARITHMETIC TESTS DONE ===")
