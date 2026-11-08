; ============================================================
; 57 — Relation Algebra and Closure Operators
; Relations as sets of pairs; composition, inverse, image,
; function properties, and least-fixpoint closures.
; native prelude relation algebra over extension sets
; ============================================================

(load "lib/collections/relations.lsp")

(print "=== Relation Algebra ===")


; A relation is a set of two-element lists (a b), read "a relates to b".
; Here: direct-flight routes between cities.
(define flights {(rome paris) (paris london) (london oslo) (rome berlin)})

(print "-- basic algebra --")
(print (list (quote domain) (rel-domain flights)))
(print (list (quote range) (rel-range flights)))
(print (list (quote inverse) (rel-inverse flights)))

(check "domain" (equal (rel-domain flights) {rome paris london}))
(check "range" (equal (rel-range flights) {paris london oslo berlin}))
(check "inverse-involution" (equal (rel-inverse (rel-inverse flights)) flights))

; Composition: two-leg trips.
(define two-leg (rel-compose flights flights))
(print (list (quote two-leg-trips) two-leg))
(check "compose" (equal two-leg {(rome london) (paris oslo)}))

; Image: everywhere reachable from a set of origins in one flight.
(check "image" (equal (rel-image flights {rome}) {paris berlin}))

; Reachability = transitive closure, computed as a least fixpoint:
; lfp iterates R -> R ∪ (R ∘ flights) until nothing new appears.
(define reachable (transitive-closure flights))
(print (list (quote reachable) reachable))
(check "closure-contains-base" (subset flights reachable))
(check "rome-reaches-oslo" (in (list (quote rome) (quote oslo)) reachable))
(check "oslo-reaches-nothing" (equal (rel-image reachable {oslo}) {}))

; Equivalence closure: reflexive + symmetric + transitive.
(define (reflexive-closure r carrier)
  (union r (map (lambda (x) (list x x)) carrier)))
(define (symmetric-closure r) (union r (rel-inverse r)))
(define (equivalence-closure r carrier)
  (transitive-closure
    (symmetric-closure (reflexive-closure r carrier))))

(define partners {(a b) (c d)})
(define eqrel (equivalence-closure partners {a b c d}))
(check "equivalence-reflexive" (in (list (quote a) (quote a)) eqrel))
(check "equivalence-symmetric" (in (list (quote b) (quote a)) eqrel))
(check "equivalence-blocks-separate" (notin (list (quote a) (quote c)) eqrel))

; Functions are single-valued relations.
(define square-table {(1 1) (2 4) (3 9)})
(check "function" (function-p square-table))
(check "injective" (injective-p square-table))
(check "not-function" (equal (function-p {(1 2) (1 3)}) nil))
(check "not-injective" (equal (injective-p {(1 5) (2 5)}) nil))

; Comprehension sets restrict to finite carriers via enumerate.
(define (divisor-of n) {tau : (= 0 (mod n tau))})
(define divisors-of-12 (enumerate (divisor-of 12) {1 2 3 4 5 6 7 8 9 10 11 12}))
(print (list (quote divisors-of-12) divisors-of-12))
(check "enumerate-comprehension" (equal divisors-of-12 {1 2 3 4 6 12}))

; Divisibility as a relation built by a predicate over pairs.
(define divides
  (pairs-such-that (lambda (a b) (= 0 (mod b a))) {1 2 3 6} {1 2 3 6}))
(check "divides-reflexive"
  (every (lambda (x) (in (list x x) divides)) {1 2 3 6}))
(check "divides-transitive" (equal (transitive-closure divides) divides))

(print "=== Relation Algebra Done ===")
