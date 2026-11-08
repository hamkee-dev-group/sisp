; ============================================================
; SISP Test Suite — Set/Relation Algebra Prelude + list-to-set
; (set-to-list, list-to-set, map/filter/every/any/find/count on sets,
;  enumerate, pairs-such-that, k-subsets, rel-* algebra,
;  function-p, injective-p, lfp, transitive-closure)
; ============================================================

(load "lib/collections/relations.lsp")

(print "=== RELATION ALGEBRA TESTS ===")

; --- list <-> set conversion ---
(if (equal (list-to-set (list 1 2 2 3)) {1 2 3}) (print "PASS: list-to-set-dedup") (print "FAIL: list-to-set-dedup"))
(if (equal (list-to-set nil) {}) (print "PASS: list-to-set-empty") (print "FAIL: list-to-set-empty"))
(if (equal (list-to-set {1 2}) {1 2}) (print "PASS: list-to-set-idempotent") (print "FAIL: list-to-set-idempotent"))
(if (equal (set-to-list {5}) (list 5)) (print "PASS: set-to-list-singleton") (print "FAIL: set-to-list-singleton"))
(if (equal (set-to-list {}) nil) (print "PASS: set-to-list-empty") (print "FAIL: set-to-list-empty"))
(if (equal (list-to-set (set-to-list {3 1 2})) {1 2 3}) (print "PASS: set-roundtrip") (print "FAIL: set-roundtrip"))

; --- functional set operations ---
(if (equal (filter (lambda (x) (> x 1)) {1 2 3}) {2 3}) (print "PASS: filter-set") (print "FAIL: filter-set"))
(if (equal (map (lambda (x) (* x x)) {1 2 3}) {1 4 9}) (print "PASS: map-set") (print "FAIL: map-set"))
(if (equal (map (lambda (x) (mod x 2)) {1 2 3 4}) {0 1}) (print "PASS: map-set-collapses") (print "FAIL: map-set-collapses"))
(if (equal (every (lambda (x) (> x 0)) {1 2 3}) t) (print "PASS: every-set-true") (print "FAIL: every-set-true"))
(if (equal (every (lambda (x) (> x 1)) {1 2 3}) nil) (print "PASS: every-set-false") (print "FAIL: every-set-false"))
(if (equal (any (lambda (x) (= x 2)) {1 2 3}) t) (print "PASS: any-set-true") (print "FAIL: any-set-true"))
(if (equal (any (lambda (x) (= x 9)) {1 2 3}) nil) (print "PASS: any-set-false") (print "FAIL: any-set-false"))
(if (equal (find (lambda (x) (> x 1)) {1 2 3}) 2) (print "PASS: find-set") (print "FAIL: find-set"))
(if (equal (count (lambda (x) (oddp x)) {1 2 3 4 5}) 3) (print "PASS: count-set") (print "FAIL: count-set"))
(if (equal (remove evenp {1 2 3 4}) {1 3}) (print "PASS: remove-set") (print "FAIL: remove-set"))
(if (equal (foldl + 0 {1 2 3}) 6) (print "PASS: foldl-set") (print "FAIL: foldl-set"))
(if (equal (foldr + 0 {1 2 3}) 6) (print "PASS: foldr-set") (print "FAIL: foldr-set"))
(if (equal (map car {}) {}) (print "PASS: map-empty-set") (print "FAIL: map-empty-set"))
(if (equal (filter evenp {}) {}) (print "PASS: filter-empty-set") (print "FAIL: filter-empty-set"))
(if (equal (every evenp {}) t) (print "PASS: every-empty-set") (print "FAIL: every-empty-set"))
(if (equal (any evenp {}) nil) (print "PASS: any-empty-set") (print "FAIL: any-empty-set"))
(if (equal (find evenp {}) nil) (print "PASS: find-empty-set") (print "FAIL: find-empty-set"))
(if (equal (count evenp {}) 0) (print "PASS: count-empty-set") (print "FAIL: count-empty-set"))

; --- bounded enumeration of comprehension sets ---
(if (equal (enumerate {tau : (> tau 2)} {1 2 3 4 5}) {3 4 5}) (print "PASS: enumerate-comprehension") (print "FAIL: enumerate-comprehension"))
(if (equal (enumerate {2 4} {1 2 3 4 5}) {2 4}) (print "PASS: enumerate-extension") (print "FAIL: enumerate-extension"))
(define (interval a b) {tau : (and (>= tau a) (<= tau b))})
(if (equal (enumerate (interval 2 4) {1 2 3 4 5 6}) {2 3 4}) (print "PASS: enumerate-captured-locals") (print "FAIL: enumerate-captured-locals"))

; --- pairs and k-subsets ---
(if (equal (pairs-such-that (lambda (x y) (< x y)) {1 2} {1 2 3}) {(1 2) (1 3) (2 3)}) (print "PASS: pairs-such-that") (print "FAIL: pairs-such-that"))
(if (equal (k-subsets {1 2 3} 2) {{1 2} {1 3} {2 3}}) (print "PASS: k-subsets") (print "FAIL: k-subsets"))
(if (equal (k-subsets {1 2 3} 0) {{}}) (print "PASS: k-subsets-zero") (print "FAIL: k-subsets-zero"))

; --- relation algebra (relations are sets of two-lists) ---
(define R {(1 2) (2 3) (1 3)})
(if (equal (rel-domain R) {1 2}) (print "PASS: rel-domain") (print "FAIL: rel-domain"))
(if (equal (rel-range R) {2 3}) (print "PASS: rel-range") (print "FAIL: rel-range"))
(if (equal (rel-inverse R) {(2 1) (3 2) (3 1)}) (print "PASS: rel-inverse") (print "FAIL: rel-inverse"))
(if (equal (rel-image {(1 a) (2 b) (3 c)} {1 3}) {a c}) (print "PASS: rel-image") (print "FAIL: rel-image"))
(if (equal (rel-compose {(1 2) (2 3)} {(2 9) (3 7)}) {(1 9) (2 7)}) (print "PASS: rel-compose") (print "FAIL: rel-compose"))
(if (equal (rel-compose {(1 2)} {(9 9)}) {}) (print "PASS: rel-compose-empty") (print "FAIL: rel-compose-empty"))

; inverse of inverse is the original
(if (equal (rel-inverse (rel-inverse R)) R) (print "PASS: rel-inverse-involution") (print "FAIL: rel-inverse-involution"))

; --- function properties ---
(if (equal (function-p {(1 2) (2 3)}) t) (print "PASS: function-p-true") (print "FAIL: function-p-true"))
(if (equal (function-p {(1 2) (1 3)}) nil) (print "PASS: function-p-false") (print "FAIL: function-p-false"))
(if (equal (injective-p {(1 2) (2 3)}) t) (print "PASS: injective-p-true") (print "FAIL: injective-p-true"))
(if (equal (injective-p {(1 2) (2 2)}) nil) (print "PASS: injective-p-false") (print "FAIL: injective-p-false"))

; --- fixpoints and closure ---
(if (equal (lfp (lambda (s) (union s {1})) {}) {1}) (print "PASS: lfp-reaches-fixpoint") (print "FAIL: lfp-reaches-fixpoint"))
(if (equal (transitive-closure {(1 2) (2 3) (3 4)})
           {(1 2) (2 3) (3 4) (1 3) (2 4) (1 4)})
    (print "PASS: transitive-closure") (print "FAIL: transitive-closure"))
(if (equal (transitive-closure {(1 1)}) {(1 1)}) (print "PASS: transitive-closure-loop") (print "FAIL: transitive-closure-loop"))

; --- regressions pinned from the code review ---
; list-tail/drop must not diverge on a negative or oversized count
(if (equal (drop (list 1 2) -1) (list 1 2)) (print "PASS: drop-negative") (print "FAIL: drop-negative"))
(if (equal (drop (list 1 2) 5) nil) (print "PASS: drop-past-end") (print "FAIL: drop-past-end"))
; comprehension sets capture locals nested anywhere in the predicate
(define (band lo hi) {tau : (and (> tau lo) (< tau hi))})
(if (equal (in 5 (band 3 10)) t) (print "PASS: comprehension-nested-capture") (print "FAIL: comprehension-nested-capture"))
(if (equal (in 12 (band 3 10)) nil) (print "PASS: comprehension-nested-capture-out") (print "FAIL: comprehension-nested-capture-out"))
; all bound identifiers substitute, not only the last
(define (above2 a b) {tau : (> a b)})
(if (equal (above2 5 3) {tau : (> 5 3)}) (print "PASS: comprehension-multi-substitution") (print "FAIL: comprehension-multi-substitution"))
; lexical function bindings shadow same-named global functions
(define (shadow-probe x) 1)
(if (equal (let ((shadow-probe (lambda (y) 99))) (shadow-probe 42)) 99)
    (print "PASS: local-shadows-global-function") (print "FAIL: local-shadows-global-function"))
(if (equal (shadow-probe 42) 1) (print "PASS: global-survives-shadowing") (print "FAIL: global-survives-shadowing"))
; matrices honor structural equality under :test dispatch
(if (equal (in [1 2] (list [3 4]) (quote equal)) nil) (print "PASS: matrix-memberp-unequal") (print "FAIL: matrix-memberp-unequal"))
(if (equal (in [1 2] (list [1 2]) (quote equal)) t) (print "PASS: matrix-memberp-equal") (print "FAIL: matrix-memberp-equal"))

(print "=== RELATION ALGEBRA TESTS DONE ===")
