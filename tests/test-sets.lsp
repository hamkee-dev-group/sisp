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

; --- SISP extension protection: symbolic and structured members ---
; These protect set semantics against CL-compat changes to eq, consp,
; or reader case that could otherwise silently alter identifier or
; cons-cell comparison inside set operations.

; Symbolic members: union / cap / diff / symdiff
(if (equal (union {a b} {b c}) {a b c}) (print "PASS: ext-union-sym") (print "FAIL: ext-union-sym"))
(if (equal (cap {a b c} {b c d}) {b c}) (print "PASS: ext-cap-sym") (print "FAIL: ext-cap-sym"))
(if (equal (diff {a b c} {b}) {a c}) (print "PASS: ext-diff-sym") (print "FAIL: ext-diff-sym"))
(if (equal (symdiff {a b c} {b c d}) {a d}) (print "PASS: ext-symdiff-sym") (print "FAIL: ext-symdiff-sym"))
(if (equal (in 'a {a b c}) t) (print "PASS: ext-in-sym") (print "FAIL: ext-in-sym"))
(if (equal (in 'd {a b c}) nil) (print "PASS: ext-in-sym-miss") (print "FAIL: ext-in-sym-miss"))

; Symbolic members: pow / prod
(define sym-pow (pow {a b}))
(if (equal (ord sym-pow) 4) (print "PASS: ext-pow-sym-card") (print "FAIL: ext-pow-sym-card"))
(if (equal (in {a} sym-pow 'equal) t) (print "PASS: ext-pow-sym-has-a") (print "FAIL: ext-pow-sym-has-a"))
(if (equal (in {a b} sym-pow 'equal) t) (print "PASS: ext-pow-sym-has-ab") (print "FAIL: ext-pow-sym-has-ab"))

(define sym-prod (prod {a b} {x y}))
(if (equal (ord sym-prod) 4) (print "PASS: ext-prod-sym-card") (print "FAIL: ext-prod-sym-card"))
(if (equal (in '(a x) sym-prod 'equal) t) (print "PASS: ext-prod-sym-ax") (print "FAIL: ext-prod-sym-ax"))
(if (equal (in '(b y) sym-prod 'equal) t) (print "PASS: ext-prod-sym-by") (print "FAIL: ext-prod-sym-by"))

; Structured (proper-list tuple) members: union / cap / diff / symdiff
(define tup-a {(1 2) (3 4) (5 6)})
(define tup-b {(3 4) (7 8)})
(if (equal (union tup-a tup-b) {(1 2) (3 4) (5 6) (7 8)}) (print "PASS: ext-union-tuple") (print "FAIL: ext-union-tuple"))
(if (equal (cap tup-a tup-b) {(3 4)}) (print "PASS: ext-cap-tuple") (print "FAIL: ext-cap-tuple"))
(if (equal (diff tup-a tup-b) {(1 2) (5 6)}) (print "PASS: ext-diff-tuple") (print "FAIL: ext-diff-tuple"))
(if (equal (symdiff tup-a tup-b) {(1 2) (5 6) (7 8)}) (print "PASS: ext-symdiff-tuple") (print "FAIL: ext-symdiff-tuple"))
(if (equal (in '(3 4) tup-a 'equal) t) (print "PASS: ext-in-tuple") (print "FAIL: ext-in-tuple"))
(if (equal (in '(9 9) tup-a 'equal) nil) (print "PASS: ext-in-tuple-miss") (print "FAIL: ext-in-tuple-miss"))

; Nested-set members: union / cap / diff / symdiff
(define ns-a {{1 2} {3 4} {5 6}})
(define ns-b {{3 4} {7 8}})
(if (equal (cap ns-a ns-b) {{3 4}}) (print "PASS: ext-cap-nested") (print "FAIL: ext-cap-nested"))
(if (equal (diff ns-a ns-b) {{1 2} {5 6}}) (print "PASS: ext-diff-nested") (print "FAIL: ext-diff-nested"))
(if (equal (symdiff ns-a ns-b) {{1 2} {5 6} {7 8}}) (print "PASS: ext-symdiff-nested") (print "FAIL: ext-symdiff-nested"))

(undef sym-pow sym-prod tup-a tup-b ns-a ns-b)

; --- Nested comprehension-set members: equality, `in`, and dedup
;     must not silently false-reject or retain duplicates ---
(if (equal {tau : (> tau 0)} {tau : (> tau 0)}) (print "PASS: comp-equal-self") (print "FAIL: comp-equal-self"))
(if (equal (equal {tau : (> tau 0)} {tau : (< tau 0)}) nil) (print "PASS: comp-equal-diff") (print "FAIL: comp-equal-diff"))
(if (equal {{tau : (> tau 0)}} {{tau : (> tau 0)}}) (print "PASS: nested-comp-equal") (print "FAIL: nested-comp-equal"))
(if (equal (equal {{tau : (> tau 0)}} {{tau : (< tau 0)}}) nil) (print "PASS: nested-comp-equal-diff") (print "FAIL: nested-comp-equal-diff"))
(if (equal (in {tau : (> tau 0)} {{tau : (> tau 0)}} 'equal) t) (print "PASS: nested-comp-in") (print "FAIL: nested-comp-in"))
(if (equal (in {tau : (< tau 0)} {{tau : (> tau 0)}} 'equal) nil) (print "PASS: nested-comp-in-miss") (print "FAIL: nested-comp-in-miss"))
(if (equal (ord {{tau : (> tau 0)} {tau : (> tau 0)}}) 1) (print "PASS: nested-comp-dedup") (print "FAIL: nested-comp-dedup"))
(if (equal {{tau : (> tau 0)} {tau : (> tau 0)}} {{tau : (> tau 0)}}) (print "PASS: nested-comp-dedup-equal") (print "FAIL: nested-comp-dedup-equal"))

; --- CL-friendly set predicates ---
(if (equal (setp {1 2 3}) t) (print "PASS: setp-ext") (print "FAIL: setp-ext"))
(if (equal (setp {tau : (> tau 0)}) t) (print "PASS: setp-comp") (print "FAIL: setp-comp"))
(if (equal (setp '(1 2 3)) nil) (print "PASS: setp-list") (print "FAIL: setp-list"))
(if (equal (setp 42) nil) (print "PASS: setp-int") (print "FAIL: setp-int"))
(if (equal (setp nil) nil) (print "PASS: setp-nil") (print "FAIL: setp-nil"))

(if (equal (ext-set-p {1 2 3}) t) (print "PASS: ext-set-p-ext") (print "FAIL: ext-set-p-ext"))
(if (equal (ext-set-p {tau : (> tau 0)}) nil) (print "PASS: ext-set-p-comp") (print "FAIL: ext-set-p-comp"))
(if (equal (ext-set-p '(1 2 3)) nil) (print "PASS: ext-set-p-list") (print "FAIL: ext-set-p-list"))

(if (equal (comp-set-p {tau : (> tau 0)}) t) (print "PASS: comp-set-p-comp") (print "FAIL: comp-set-p-comp"))
(if (equal (comp-set-p {1 2 3}) nil) (print "PASS: comp-set-p-ext") (print "FAIL: comp-set-p-ext"))
(if (equal (comp-set-p '(1 2 3)) nil) (print "PASS: comp-set-p-list") (print "FAIL: comp-set-p-list"))

; --- CL-friendly aliases for existing set operators ---
(if (equal (intersection {1 2 3} {2 3 4}) (cap {1 2 3} {2 3 4})) (print "PASS: intersection-alias") (print "FAIL: intersection-alias"))
(if (equal (set-difference {1 2 3} {2}) (diff {1 2 3} {2})) (print "PASS: set-difference-alias") (print "FAIL: set-difference-alias"))
(if (equal (symmetric-difference {1 2 3} {2 3 4}) (symdiff {1 2 3} {2 3 4})) (print "PASS: symmetric-difference-alias") (print "FAIL: symmetric-difference-alias"))
(define alias-ps (powerset {1 2}))
(if (equal (ord alias-ps) (ord (pow {1 2}))) (print "PASS: powerset-alias-card") (print "FAIL: powerset-alias-card"))
(if (equal (in {1 2} alias-ps 'equal) t) (print "PASS: powerset-alias-member") (print "FAIL: powerset-alias-member"))
(undef alias-ps)
(if (equal (cartesian-product {1 2} {3 4}) (prod {1 2} {3 4})) (print "PASS: cartesian-product-alias") (print "FAIL: cartesian-product-alias"))
(define aliasA {tau : (and (> tau 0) (< tau 10))})
(if (equal (in -1 (complement aliasA)) (in -1 (comp aliasA))) (print "PASS: complement-alias") (print "FAIL: complement-alias"))
(if (equal (in 5 (complement aliasA)) (in 5 (comp aliasA))) (print "PASS: complement-alias-nonmember") (print "FAIL: complement-alias-nonmember"))
(undef aliasA)

; cleanup
(undef ps ps3 cp mixed nested A compA)

(print "=== SET TESTS DONE ===")
