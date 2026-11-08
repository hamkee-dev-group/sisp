; ============================================================
; name: crdt-gcounter
; file: algorithms/data-structures/crdt-gcounter.lsp
; subject: data-structure
; capability: distributed counter convergent
; aliases: "g-counter" "grow-only counter" "crdt"
; signature: (gc-new n) (gc-inc gc replica) (gc-merge a b)
;            (gc-value gc) (gc-le a b)
; complexity: O(n) per operation, n = number of replicas
; see-also: vector-clock
; derived-from: Shapiro, Preguica, Baquero & Zawirski 2011; see also
;               examples/problems/distributed/crdt-gcounter.lsp
; status: example (tested)
; checks: 12 checks incl. all CRDT laws; run: src/sisp <this file>
; ============================================================
; G-Counter CRDT: a state-based grow-only counter.  Each replica owns
; one slot of a vector and only ever increments its own slot; merge is
; elementwise max.  Merge is commutative, associative, idempotent, and
; inflationary, so replicas always converge (strong eventual
; consistency).

(define (gc-new n)
  (cond ((<= n 0) (error "GC-NEW: N MUST BE POSITIVE" n))
        (t (map (lambda (i) 0) (seq 1 n)))))

(define (gc-inc gc replica)
  (map (lambda (i)
         (cond ((= i replica) (+ (nth i gc) 1)) (t (nth i gc))))
       (seq 0 (- (length gc) 1))))

(define (gc-merge a b)
  (map (lambda (i) (max (nth i a) (nth i b)))
       (seq 0 (- (length a) 1))))

(define (gc-value gc)
  (foldl (lambda (acc x) (+ acc x)) 0 gc))

; partial order: a <= b elementwise
(define (gc-le a b)
  (cond ((null a) t)
        ((> (car a) (car b)) nil)
        (t (gc-le (cdr a) (cdr b)))))

; ---- checks ----

(check "gcounter: new is all zeros"
       (equal (gc-new 3) (list 0 0 0)))

(check "gcounter: increment touches only the replica's slot"
       (equal (gc-inc (gc-inc (gc-new 3) 0) 0) (list 2 0 0)))

(check "gcounter: value is the sum of slots"
       (= (gc-value (gc-inc (gc-inc (gc-inc (gc-new 3) 0) 0) 2)) 3))

(check "gcounter: merge takes elementwise max"
       (equal (gc-merge (list 3 0 1) (list 1 2 0)) (list 3 2 1)))

(check "gcounter: merge is commutative"
       (equal (gc-merge (list 3 0 1) (list 1 2 0))
              (gc-merge (list 1 2 0) (list 3 0 1))))

(check "gcounter: merge is associative"
       (equal (gc-merge (gc-merge (list 3 0 1) (list 1 2 0)) (list 0 1 2))
              (gc-merge (list 3 0 1) (gc-merge (list 1 2 0) (list 0 1 2)))))

(check "gcounter: merge is idempotent"
       (equal (gc-merge (list 3 0 1) (list 3 0 1)) (list 3 0 1)))

(check "gcounter: merge is inflationary (result dominates both sides)"
       (let ((m (gc-merge (list 3 0 1) (list 1 2 0))))
         (and (gc-le (list 3 0 1) m)
              (gc-le (list 1 2 0) m))))

(check "gcounter: concurrent replicas converge under reordering"
       (let* ((a (gc-inc (gc-inc (gc-inc (gc-new 4) 0) 0) 2))
              (b (gc-inc (gc-inc (gc-inc (gc-new 4) 1) 1) 3))
              (c (gc-inc (gc-inc (gc-inc (gc-new 4) 0) 2) 3))
              (m1 (gc-merge (gc-merge a b) c))
              (m2 (gc-merge (gc-merge c a) b))
              (m3 (gc-merge b (gc-merge a c))))
         ; merged state = elementwise max = (2 2 1 1), value 6
         (and (equal m1 (list 2 2 1 1))
              (equal m1 m2)
              (equal m2 m3)
              (= (gc-value m1) 6))))

(check "gcounter: re-merging a stale state is a no-op"
       (let* ((a (gc-inc (gc-inc (gc-new 2) 0) 0))
              (b (gc-inc (gc-new 2) 1))
              (m (gc-merge a b)))
         (equal (gc-merge m a) m)))

(check "gcounter: partial order is antisymmetric via merge"
       (let ((a (list 1 2 3))
             (b (list 3 2 1)))
         (and (not (gc-le a b))
              (not (gc-le b a))
              (gc-le a (gc-merge a b))
              (gc-le b (gc-merge a b)))))

(check "gcounter: value never decreases under merge"
       (let ((a (list 2 0))
             (b (list 0 3)))
         (and (>= (gc-value (gc-merge a b)) (gc-value a))
              (>= (gc-value (gc-merge a b)) (gc-value b)))))
