; ============================================================
; SISP Test Suite — Set Operations
; (union, cap, diff, symdiff, subset, complement, member/in,
;  notin, pow, prod)
; ============================================================

(print "=== SET TESTS ===")

; --- Basic set creation ---
(if (equal {1 2 3} {1 2 3}) (print "PASS: set-create") (print "FAIL: set-create"))
(if (equal {1 2 3} {3 2 1}) (print "PASS: set-order-indep") (print "FAIL: set-order-indep"))
(if (equal {1 1 2 2 3 3} {1 2 3}) (print "PASS: set-dedup") (print "FAIL: set-dedup"))

; --- member / in ---
(if (equal (in 1 {1 2 3}) t) (print "PASS: in-found") (print "FAIL: in-found"))
(if (equal (in 4 {1 2 3}) nil) (print "PASS: in-not-found") (print "FAIL: in-not-found"))
(if (equal (in "hello" {"hello" "world"}) t) (print "PASS: in-string") (print "FAIL: in-string"))
(if (equal (in 1/2 {1/2 3/4}) t) (print "PASS: in-rational") (print "FAIL: in-rational"))
(if (equal (in t {t nil}) t) (print "PASS: in-t") (print "FAIL: in-t"))
(if (equal (in nil {t nil}) t) (print "PASS: in-nil") (print "FAIL: in-nil"))

; identifiers in sets are stored as-is (not evaluated)
(if (equal (memberp 'x '(x y z)) t) (print "PASS: in-id-list") (print "FAIL: in-id-list"))

; member on lists
(if (equal (memberp 2 '(1 2 3)) t) (print "PASS: memberp-list") (print "FAIL: memberp-list"))
(if (equal (memberp 5 '(1 2 3)) nil) (print "PASS: memberp-not-found") (print "FAIL: memberp-not-found"))

; --- notin ---
(if (equal (notin 4 {1 2 3}) t) (print "PASS: notin-true") (print "FAIL: notin-true"))
(if (equal (notin 1 {1 2 3}) nil) (print "PASS: notin-false") (print "FAIL: notin-false"))

; --- union ---
(if (equal (union {1 2} {3 4}) {1 2 3 4}) (print "PASS: union-disjoint") (print "FAIL: union-disjoint"))
(if (equal (union {1 2 3} {2 3 4}) {1 2 3 4}) (print "PASS: union-overlap") (print "FAIL: union-overlap"))
(if (equal (union {1 2} {1 2}) {1 2}) (print "PASS: union-same") (print "FAIL: union-same"))
(if (equal (union {1} {2} {3}) {1 2 3}) (print "PASS: union-triple") (print "FAIL: union-triple"))

; --- cap (intersection) ---
(if (equal (cap {1 2 3} {2 3 4}) {2 3}) (print "PASS: cap-basic") (print "FAIL: cap-basic"))
; cap of disjoint sets returns empty — verify via notin
(if (equal (notin 1 (cap {1 2} {3 4})) t) (print "PASS: cap-disjoint") (print "FAIL: cap-disjoint"))
(if (equal (cap {1 2 3} {1 2 3}) {1 2 3}) (print "PASS: cap-same") (print "FAIL: cap-same"))
(if (equal (cap {1} {1}) {1}) (print "PASS: cap-single") (print "FAIL: cap-single"))

; --- diff (set difference \ ) ---
(if (equal (\ {1 2 3} {2}) {1 3}) (print "PASS: diff-basic") (print "FAIL: diff-basic"))
(if (equal (\ {1 2 3} {4 5}) {1 2 3}) (print "PASS: diff-disjoint") (print "FAIL: diff-disjoint"))
; diff of identical sets returns empty
(if (equal (notin 1 (\ {1 2 3} {1 2 3})) t) (print "PASS: diff-same") (print "FAIL: diff-same"))
(if (equal (diff {1 2 3 4} {2 4}) {1 3}) (print "PASS: diff-named") (print "FAIL: diff-named"))

; --- symdiff ---
(if (equal (symdiff {1 2 3} {2 3 4}) {1 4}) (print "PASS: symdiff-basic") (print "FAIL: symdiff-basic"))
(if (equal (symdiff {1 2} {3 4}) {1 2 3 4}) (print "PASS: symdiff-disjoint") (print "FAIL: symdiff-disjoint"))
; symdiff of identical sets returns empty
(if (equal (notin 1 (symdiff {1 2 3} {1 2 3})) t) (print "PASS: symdiff-same") (print "FAIL: symdiff-same"))

; --- subset ---
(if (equal (subset {1 2} {1 2 3}) t) (print "PASS: subset-true") (print "FAIL: subset-true"))
(if (equal (subset {1 4} {1 2 3}) nil) (print "PASS: subset-false") (print "FAIL: subset-false"))
(if (equal (subset {1 2 3} {1 2 3}) t) (print "PASS: subset-equal") (print "FAIL: subset-equal"))

; --- pow (power set) ---
(define ps (pow {1 2}))
(if (equal (in {1} ps 'equal) t) (print "PASS: pow-has-1") (print "FAIL: pow-has-1"))
(if (equal (in {2} ps 'equal) t) (print "PASS: pow-has-2") (print "FAIL: pow-has-2"))
(if (equal (in {1 2} ps 'equal) t) (print "PASS: pow-has-12") (print "FAIL: pow-has-12"))
(if (equal (ord ps) 4) (print "PASS: pow-card-4") (print "FAIL: pow-card-4"))

; power set of 3 elements has 8 subsets
(define ps3 (pow {1 2 3}))
(if (equal (ord ps3) 8) (print "PASS: pow-card-8") (print "FAIL: pow-card-8"))

; --- prod (cartesian product) ---
(define cp (prod {1 2} {3 4}))
(if (equal (ord cp) 4) (print "PASS: prod-card-4") (print "FAIL: prod-card-4"))
; product elements are lists: {(1 3) (1 4) (2 3) (2 4)}
(if (equal (in '(1 3) cp 'equal) t) (print "PASS: prod-13") (print "FAIL: prod-13"))
(if (equal (in '(1 4) cp 'equal) t) (print "PASS: prod-14") (print "FAIL: prod-14"))
(if (equal (in '(2 3) cp 'equal) t) (print "PASS: prod-23") (print "FAIL: prod-23"))
(if (equal (in '(2 4) cp 'equal) t) (print "PASS: prod-24") (print "FAIL: prod-24"))

; --- Sets with mixed types ---
(define mixed {1 "hello" 1/2})
(if (equal (in 1 mixed) t) (print "PASS: mixed-int") (print "FAIL: mixed-int"))
(if (equal (in "hello" mixed) t) (print "PASS: mixed-str") (print "FAIL: mixed-str"))
(if (equal (in 1/2 mixed) t) (print "PASS: mixed-rat") (print "FAIL: mixed-rat"))

; --- Sets with nested sets ---
(define nested {{1 2} {3 4}})
(if (equal (in {1 2} nested 'equal) t) (print "PASS: nested-set-in") (print "FAIL: nested-set-in"))
(if (equal (in {2 1} nested 'equal) t) (print "PASS: nested-set-reorder") (print "FAIL: nested-set-reorder"))
(if (equal (in {5} nested 'equal) nil) (print "PASS: nested-set-notin") (print "FAIL: nested-set-notin"))

; --- Union/cap/diff on lists (via append) ---
(if (equal (append '(1 2) '(3 4)) '(1 2 3 4)) (print "PASS: append-list") (print "FAIL: append-list"))

; --- Complement (comprehension set) ---
(define A {tau : (and (> tau 0) (< tau 10))})
(define compA (comp A))
(if (equal (in -1 compA) t) (print "PASS: comp-member-neg") (print "FAIL: comp-member-neg"))
(if (equal (in 5 compA) nil) (print "PASS: comp-nonmember-5") (print "FAIL: comp-nonmember-5"))

; --- Mixed comprehension/extension operands ---
(define pos {tau : (> tau 0)})
(define evens {tau : (equal (mod tau 2) 0)})

; union: comp U ext
(if (equal (in 0 (union pos {0})) t) (print "PASS: union-mixed-comp-ext") (print "FAIL: union-mixed-comp-ext"))
(if (equal (in 5 (union pos {0})) t) (print "PASS: union-mixed-comp-ext-pos") (print "FAIL: union-mixed-comp-ext-pos"))
; union: ext U comp
(if (equal (in 5 (union {0} pos)) t) (print "PASS: union-mixed-ext-comp") (print "FAIL: union-mixed-ext-comp"))
(if (equal (in 0 (union {0} pos)) t) (print "PASS: union-mixed-ext-comp-zero") (print "FAIL: union-mixed-ext-comp-zero"))

; cap: comp ∩ ext
(if (equal (in 2 (cap evens {1 2 3 4})) t) (print "PASS: cap-mixed-comp-ext") (print "FAIL: cap-mixed-comp-ext"))
(if (equal (in 3 (cap evens {1 2 3 4})) nil) (print "PASS: cap-mixed-comp-ext-odd") (print "FAIL: cap-mixed-comp-ext-odd"))
; cap: ext ∩ comp
(if (equal (in 4 (cap {1 2 3 4} evens)) t) (print "PASS: cap-mixed-ext-comp") (print "FAIL: cap-mixed-ext-comp"))
(if (equal (in 1 (cap {1 2 3 4} evens)) nil) (print "PASS: cap-mixed-ext-comp-odd") (print "FAIL: cap-mixed-ext-comp-odd"))

; diff: comp \ ext
(if (equal (in 2 (diff pos {2})) nil) (print "PASS: diff-mixed-comp-ext-excluded") (print "FAIL: diff-mixed-comp-ext-excluded"))
(if (equal (in 3 (diff pos {2})) t) (print "PASS: diff-mixed-comp-ext-kept") (print "FAIL: diff-mixed-comp-ext-kept"))
; diff: ext \ comp
(if (equal (in 1 (diff {1 2 3} pos)) nil) (print "PASS: diff-mixed-ext-comp-excluded") (print "FAIL: diff-mixed-ext-comp-excluded"))

; symdiff: comp △ ext
(if (equal (in 1 (symdiff evens {1})) t) (print "PASS: symdiff-mixed-comp-ext") (print "FAIL: symdiff-mixed-comp-ext"))
(if (equal (in 2 (symdiff evens {1})) t) (print "PASS: symdiff-mixed-comp-ext-even") (print "FAIL: symdiff-mixed-comp-ext-even"))
; symdiff: ext △ comp
(if (equal (in 1 (symdiff {1} evens)) t) (print "PASS: symdiff-mixed-ext-comp") (print "FAIL: symdiff-mixed-ext-comp"))

(undef pos evens)

; --- Rational canonicalization in sets ---
; Equivalent rationals deduplicate
(if (equal {1/2 2/4} {1/2}) (print "PASS: rat-dedup-half") (print "FAIL: rat-dedup-half"))
(if (equal {3/6 1/2} {1/2}) (print "PASS: rat-dedup-sixth") (print "FAIL: rat-dedup-sixth"))
; Rationals reducing to integer deduplicate with integers
(if (equal {3/3 1} {1}) (print "PASS: rat-dedup-int") (print "FAIL: rat-dedup-int"))
; equal works for sets with equivalent rationals
(if (equal {1/2} {2/4}) (print "PASS: rat-equal-set") (print "FAIL: rat-equal-set"))
; in finds equivalent rationals
(if (equal (in 1/2 {2/4 3/4}) t) (print "PASS: rat-in-equiv") (print "FAIL: rat-in-equiv"))
(if (equal (in 2/4 {1/2 3/4}) t) (print "PASS: rat-in-equiv2") (print "FAIL: rat-in-equiv2"))
; subset with equivalent rationals
(if (equal (subset {2/4} {1/2 3/4}) t) (print "PASS: rat-subset") (print "FAIL: rat-subset"))
; union deduplicates equivalent rationals
(if (equal (union {1/2} {2/4}) {1/2}) (print "PASS: rat-union") (print "FAIL: rat-union"))
; cap finds equivalent rationals
(if (equal (cap {1/2 3/4} {2/4 5/6}) {1/2}) (print "PASS: rat-cap") (print "FAIL: rat-cap"))
; diff removes equivalent rationals
(if (equal (diff {1/2 3/4} {2/4}) {3/4}) (print "PASS: rat-diff") (print "FAIL: rat-diff"))
; symdiff with equivalent rationals
(if (equal (symdiff {1/2 3/4} {2/4 5/6}) {3/4 5/6}) (print "PASS: rat-symdiff") (print "FAIL: rat-symdiff"))

; cleanup
(undef ps ps3 cp mixed nested A compA)

(print "=== SET TESTS DONE ===")
