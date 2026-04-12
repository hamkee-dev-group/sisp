; ============================================================
; SISP Test Suite — Comprehension Sets {tau : condition}
; ============================================================

(print "=== COMPREHENSION SET TESTS ===")

; --- Basic comprehension ---
(define evens {tau : (equal (mod tau 2) 0)})
(if (equal (in 2 evens) t) (print "PASS: comp-even-2") (print "FAIL: comp-even-2"))
(if (equal (in 4 evens) t) (print "PASS: comp-even-4") (print "FAIL: comp-even-4"))
(if (equal (in 3 evens) nil) (print "PASS: comp-odd-3") (print "FAIL: comp-odd-3"))
(if (equal (in 0 evens) t) (print "PASS: comp-even-0") (print "FAIL: comp-even-0"))

; --- Comprehension with comparison ---
(define pos {tau : (> tau 0)})
(if (equal (in 1 pos) t) (print "PASS: comp-pos-1") (print "FAIL: comp-pos-1"))
(if (equal (in 100 pos) t) (print "PASS: comp-pos-100") (print "FAIL: comp-pos-100"))
(if (equal (in 0 pos) nil) (print "PASS: comp-pos-0") (print "FAIL: comp-pos-0"))
(if (equal (in -1 pos) nil) (print "PASS: comp-pos-neg") (print "FAIL: comp-pos-neg"))

; --- Compound condition ---
(define small-pos {tau : (and (> tau 0) (< tau 5))})
(if (equal (in 1 small-pos) t) (print "PASS: comp-range-1") (print "FAIL: comp-range-1"))
(if (equal (in 4 small-pos) t) (print "PASS: comp-range-4") (print "FAIL: comp-range-4"))
(if (equal (in 5 small-pos) nil) (print "PASS: comp-range-5") (print "FAIL: comp-range-5"))
(if (equal (in 0 small-pos) nil) (print "PASS: comp-range-0") (print "FAIL: comp-range-0"))

; --- Subset with comprehension ---
(if (equal (subset {1 2 3} small-pos) t) (print "PASS: subset-comp") (print "FAIL: subset-comp"))
(if (equal (subset {1 2 5} small-pos) nil) (print "PASS: subset-comp-fail") (print "FAIL: subset-comp-fail"))

; --- Union of comprehension sets ---
(define neg {tau : (< tau 0)})
(define nonzero (union pos neg))
(if (equal (in 5 nonzero) t) (print "PASS: union-comp-pos") (print "FAIL: union-comp-pos"))
(if (equal (in -3 nonzero) t) (print "PASS: union-comp-neg") (print "FAIL: union-comp-neg"))
(if (equal (in 0 nonzero) nil) (print "PASS: union-comp-zero") (print "FAIL: union-comp-zero"))

; --- Intersection of comprehension sets ---
(define big {tau : (> tau 10)})
(define big-even (cap evens big))
(if (equal (in 12 big-even) t) (print "PASS: cap-comp-12") (print "FAIL: cap-comp-12"))
(if (equal (in 11 big-even) nil) (print "PASS: cap-comp-11") (print "FAIL: cap-comp-11"))
(if (equal (in 8 big-even) nil) (print "PASS: cap-comp-8") (print "FAIL: cap-comp-8"))

; --- Complement of comprehension ---
(define not-evens (comp evens))
(if (equal (in 3 not-evens) t) (print "PASS: comp-not-even-3") (print "FAIL: comp-not-even-3"))
(if (equal (in 2 not-evens) nil) (print "PASS: comp-not-even-2") (print "FAIL: comp-not-even-2"))

; --- Diff of comprehension sets ---
(define pos-odd (\ pos evens))
(if (equal (in 3 pos-odd) t) (print "PASS: diff-comp-3") (print "FAIL: diff-comp-3"))
(if (equal (in 2 pos-odd) nil) (print "PASS: diff-comp-2") (print "FAIL: diff-comp-2"))
(if (equal (in -1 pos-odd) nil) (print "PASS: diff-comp-neg") (print "FAIL: diff-comp-neg"))

; --- Symdiff of comprehension sets ---
(define A {tau : (and (> tau 0) (< tau 10))})
(define B {tau : (and (> tau 5) (< tau 15))})
(define sd (symdiff A B))
(if (equal (in 3 sd) t) (print "PASS: symdiff-comp-3") (print "FAIL: symdiff-comp-3"))
(if (equal (in 12 sd) t) (print "PASS: symdiff-comp-12") (print "FAIL: symdiff-comp-12"))
(if (equal (in 7 sd) nil) (print "PASS: symdiff-comp-7") (print "FAIL: symdiff-comp-7"))

; --- Cartesian product of comprehension sets ---
(define cp (prod pos evens))
; (3 . 2) should be in: 3 > 0 and 2 is even
(if (equal (in '(3 . 2) cp) t) (print "PASS: prod-comp-in") (print "FAIL: prod-comp-in"))
; (3 . 3) should not be in: 3 is odd
(if (equal (in '(3 . 3) cp) nil) (print "PASS: prod-comp-notin") (print "FAIL: prod-comp-notin"))

; --- Power set of comprehension ---
(define pw (pow pos))
; {1 2} is a subset of pos
(if (equal (in {1 2} pw) t) (print "PASS: pow-comp-subset") (print "FAIL: pow-comp-subset"))
; {-1} is not a subset of pos
(if (equal (in {-1} pw) nil) (print "PASS: pow-comp-not-subset") (print "FAIL: pow-comp-not-subset"))

; --- Mixed comprehension/extension operands ---

; union: comprehension + extension
(define u1 (union pos {0}))
(if (equal (in 5 u1) t) (print "PASS: mixed-union-comp-ext-pos") (print "FAIL: mixed-union-comp-ext-pos"))
(if (equal (in 0 u1) t) (print "PASS: mixed-union-comp-ext-zero") (print "FAIL: mixed-union-comp-ext-zero"))
(if (equal (in -1 u1) nil) (print "PASS: mixed-union-comp-ext-neg") (print "FAIL: mixed-union-comp-ext-neg"))

; union: extension + comprehension
(define u2 (union {0} pos))
(if (equal (in 0 u2) t) (print "PASS: mixed-union-ext-comp-zero") (print "FAIL: mixed-union-ext-comp-zero"))
(if (equal (in 5 u2) t) (print "PASS: mixed-union-ext-comp-pos") (print "FAIL: mixed-union-ext-comp-pos"))

; cap: comprehension ∩ extension
(define c1 (cap evens {1 2 3 4 5 6}))
(if (equal (in 2 c1) t) (print "PASS: mixed-cap-comp-ext-even") (print "FAIL: mixed-cap-comp-ext-even"))
(if (equal (in 3 c1) nil) (print "PASS: mixed-cap-comp-ext-odd") (print "FAIL: mixed-cap-comp-ext-odd"))
(if (equal (in 7 c1) nil) (print "PASS: mixed-cap-comp-ext-outside") (print "FAIL: mixed-cap-comp-ext-outside"))

; cap: extension ∩ comprehension
(define c2 (cap {1 2 3 4 5 6} evens))
(if (equal (in 4 c2) t) (print "PASS: mixed-cap-ext-comp-even") (print "FAIL: mixed-cap-ext-comp-even"))
(if (equal (in 5 c2) nil) (print "PASS: mixed-cap-ext-comp-odd") (print "FAIL: mixed-cap-ext-comp-odd"))

; diff: comprehension \ extension (acceptance criterion)
(define d1 (diff {tau : (> tau 0)} {2}))
(if (equal (in 2 d1) nil) (print "PASS: mixed-diff-comp-ext-excluded") (print "FAIL: mixed-diff-comp-ext-excluded"))
(if (equal (in 3 d1) t) (print "PASS: mixed-diff-comp-ext-kept") (print "FAIL: mixed-diff-comp-ext-kept"))

; diff: extension \ comprehension
(define d2 (diff {1 2 3 4 5} evens))
(if (equal (in 1 d2) t) (print "PASS: mixed-diff-ext-comp-odd") (print "FAIL: mixed-diff-ext-comp-odd"))
(if (equal (in 2 d2) nil) (print "PASS: mixed-diff-ext-comp-even") (print "FAIL: mixed-diff-ext-comp-even"))

; symdiff: comprehension △ extension (acceptance criterion)
(define s1 (symdiff {tau : (eq (mod tau 2) 0)} {1}))
(if (equal (in 1 s1) t) (print "PASS: mixed-symdiff-comp-ext-odd") (print "FAIL: mixed-symdiff-comp-ext-odd"))
(if (equal (in 2 s1) t) (print "PASS: mixed-symdiff-comp-ext-even") (print "FAIL: mixed-symdiff-comp-ext-even"))
; 0 is even and not in {1}, so it's in exactly one set → in symdiff
(if (equal (in 0 s1) t) (print "PASS: mixed-symdiff-comp-ext-zero") (print "FAIL: mixed-symdiff-comp-ext-zero"))

; symdiff: extension △ comprehension
(define s2 (symdiff {1} evens))
(if (equal (in 1 s2) t) (print "PASS: mixed-symdiff-ext-comp-odd") (print "FAIL: mixed-symdiff-ext-comp-odd"))
(if (equal (in 4 s2) t) (print "PASS: mixed-symdiff-ext-comp-even") (print "FAIL: mixed-symdiff-ext-comp-even"))

(undef u1 u2 c1 c2 d1 d2 s1 s2)

; cleanup
(undef evens pos small-pos neg nonzero big big-even not-evens pos-odd A B sd cp pw)

(print "=== COMPREHENSION SET TESTS DONE ===")
