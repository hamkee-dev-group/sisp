; ============================================================
; name: crdt-gcounter
; subject: data-structure
; capability: distributed counter
; status: healthy
; canonical-home: algorithms/data-structures/crdt-gcounter.lsp
; notes: All CRDT laws checked with exact states.
; ============================================================
; G-Counter CRDT: inc, merge = element-wise max. State-based. MONOTONIC.
(print "=== CRDT G-Counter ===")
(seed 42)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

(define (mk-gcounter n) (map (lambda (x) 0) (seq 1 n)))

(define (gc-inc gc replica)
  (map (lambda (i) (cond ((= i replica) (+ (nth i gc) 1)) (t (nth i gc)))) (seq 0 (- (length gc) 1))))

(define (gc-merge a b)
  (map (lambda (i) (max (nth i a) (nth i b))) (seq 0 (- (length a) 1))))

(define (gc-value gc)
  (define (sum-list l acc)
    (cond ((null l) acc) (t (sum-list (cdr l) (+ acc (car l))))))
  (sum-list gc 0))

; --- Test merge properties ---
(define R 4)
(define a (mk-gcounter R))
(define b (mk-gcounter R))
(define c (mk-gcounter R))

(define a3 (gc-inc (gc-inc (gc-inc a 0) 0) 2))
(define b3 (gc-inc (gc-inc (gc-inc b 1) 1) 3))
(define c3 (gc-inc (gc-inc (gc-inc c 0) 2) 3))

(define m-ab (gc-merge a3 b3))
(define m-ba (gc-merge b3 a3))

(ck "merge-commutative" (equal m-ab m-ba))
(ck "merge-commutative-value" (= (gc-value m-ab) (gc-value m-ba)))

; Associativity: merge(a, merge(b,c)) == merge(merge(a,b), c)
(define m-ab-c (gc-merge (gc-merge a3 b3) c3))
(define m-a-bc (gc-merge a3 (gc-merge b3 c3)))
(ck "merge-associative" (equal m-ab-c m-a-bc))

; Idempotent: merge(x, x) == x
(define m-aa (gc-merge a3 a3))
(ck "merge-idempotent" (equal m-aa a3))

; Monotonicity: after merge, values are >= original
(define (ge-list xs ys)
  (cond ((null xs) t)
        ((>= (car xs) (car ys)) (ge-list (cdr xs) (cdr ys)))
        (t nil)))

(ck "merge-monotonic-a" (ge-list m-ab a3))
(ck "merge-monotonic-b" (ge-list m-ab b3))

(print (list 'a3-value (gc-value a3)))
(print (list 'b3-value (gc-value b3)))
(ck "value-correct" (= (gc-value a3) 3))
(ck "merged-lte-sum" (<= (gc-value m-ab) (+ (gc-value a3) (gc-value b3))))

; --- Multiple rounds of merging ---
(define x (mk-gcounter R))
(define x2 (gc-inc (gc-inc x 0) 0))
(define y (mk-gcounter R))
(define y3 (gc-inc (gc-inc (gc-inc y 3) 3) 1))
(define m1 (gc-merge x2 y3))
(define m4 (gc-merge (gc-inc (gc-inc m1 0) 1) y3))

(print (list 'final-merged (gc-value m4)))

; The multi-round schedule above is fully determined: x2=(2 0 0 0), y3=(0 1 0 2),
; m1=(2 1 0 2), then +1 at replica 0 and 1, then re-merge with the stale y3.
(ck "multi-round-merge-state" (equal m4 (list 3 2 0 2)))
(ck "multi-round-merge-value" (= (gc-value m4) 7))

; Convergence: a replica that applies the same increments in a different merge
; order must reach the identical state (delivery order must not matter).
(define alt (gc-merge (gc-inc (gc-inc (gc-merge y3 x2) 1) 0) y3))
(ck "multi-round-merge-consistent" (equal alt m4))

; Re-merging a state already subsumed changes nothing (stable under stale gossip).
(ck "stale-remerge-is-noop" (equal (gc-merge m4 y3) m4))

(print "=== CRDT G-Counter Done ===")
