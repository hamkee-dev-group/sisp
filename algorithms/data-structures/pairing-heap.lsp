; ============================================================
; name: pairing-heap
; file: algorithms/data-structures/pairing-heap.lsp
; subject: data-structure
; capability: priority queue merge insert extract-min
; aliases: "pairing heap" "mergeable heap" "priority queue"
; signature: (ph-new) (ph-insert h x) (ph-merge a b) (ph-find-min h)
;            (ph-extract-min h) (ph-empty-p h) (ph-from-list xs)
; complexity: O(1) insert/merge/find-min, amortized O(log n) extract-min
; see-also: heap (lib/collections/heap.lsp) binomial-heap
; derived-from: Fredman, Sedgewick, Sleator & Tarjan 1986; Okasaki
;               1998, 5.5 (two-pass pairing)
; status: example (tested)
; checks: 12 checks; run: src/sisp <this file> from repo root
; ============================================================
; Pairing heap (Fredman-Sedgewick-Sleator-Tarjan), purely functional.
; Node = (min children-list); empty = nil.  merge puts the larger root
; under the smaller; extract-min deletes the root and merges the
; children pairwise left-to-right, then right-to-left (Okasaki's
; two-pass pairing).

(define (ph-new) nil)

(define (ph-empty-p h) (null h))

(define (ph-find-min h)
  (cond ((null h) (error "PH-FIND-MIN: EMPTY HEAP" h))
        (t (car h))))

(define (ph-merge a b)
  (cond ((null a) b)
        ((null b) a)
        ((<= (car a) (car b)) (list (car a) (cons b (cadr a))))
        (t (list (car b) (cons a (cadr b))))))

(define (ph-insert h x) (ph-merge (list x nil) h))

; two-pass: pair up neighbors left to right, then fold right to left
(define (ph-merge-pairs hs)
  (cond ((null hs) nil)
        ((null (cdr hs)) (car hs))
        (t (ph-merge (ph-merge (car hs) (cadr hs))
                     (ph-merge-pairs (cddr hs))))))

(define (ph-extract-min h)
  "returns (min new-heap) as a 2-element list"
  (cond ((null h) (error "PH-EXTRACT-MIN: EMPTY HEAP" h))
        (t (list (car h) (ph-merge-pairs (cadr h))))))

(define (ph-from-list xs)
  (foldl (lambda (h x) (ph-insert h x)) nil xs))

; ---- check helpers (defined before use) ----

(define (ph-drain h acc)
  (cond ((null h) (reverse acc))
        (t (let ((r (ph-extract-min h)))
             (ph-drain (cadr r) (cons (car r) acc))))))

(define (ph-heap-order-p h)
  (cond ((null h) t)
        (t (and (ph-all-ge (cadr h) (car h))
                (ph-all-heaps (cadr h))))))

(define (ph-all-ge hs m)
  (cond ((null hs) t)
        ((< (caar hs) m) nil)
        (t (and (ph-all-ge (cadr (car hs)) m)
                (ph-all-ge (cdr hs) m)))))

(define (ph-all-heaps hs)
  (cond ((null hs) t)
        (t (and (ph-heap-order-p (car hs))
                (ph-all-heaps (cdr hs))))))

; ---- checks ----

(check "pairing-heap: new is empty"
       (ph-empty-p (ph-new)))

(check "pairing-heap: insert then find-min"
       (= (ph-find-min (ph-insert (ph-new) 5)) 5))

(check "pairing-heap: min of several"
       (= (ph-find-min (ph-from-list (list 5 3 7 1 4))) 1))

(check "pairing-heap: extract-min returns min and keeps order"
       (let ((r (ph-extract-min (ph-from-list (list 5 3 7 1 4)))))
         (and (= (car r) 1)
              (ph-heap-order-p (cadr r)))))

(check "pairing-heap: drain yields sorted order"
       (equal (ph-drain (ph-from-list (list 9 3 7 1 5 8 2 6 4)) nil)
              (list 1 2 3 4 5 6 7 8 9)))

(check "pairing-heap: merge two heaps"
       (equal (ph-drain (ph-merge (ph-from-list (list 1 5 9))
                                  (ph-from-list (list 2 6 7)))
                        nil)
              (list 1 2 5 6 7 9)))

(check "pairing-heap: with duplicates"
       (equal (ph-drain (ph-from-list (list 3 1 4 1 5)) nil)
              (list 1 1 3 4 5)))

(check "pairing-heap: singleton"
       (let ((r (ph-extract-min (ph-insert (ph-new) 42))))
         (and (= (car r) 42)
              (ph-empty-p (cadr r)))))

(check "pairing-heap: heap order invariant after many inserts"
       (ph-heap-order-p (ph-from-list (list 4 9 1 7 3 10 2 8 5 6))))

(check "pairing-heap: drain of 100 elements is sorted"
       (equal (ph-drain (ph-from-list (reverse (seq 1 100))) nil)
              (seq 1 100)))

(check "pairing-heap: matches builtin sort on a random-ish spread"
       (equal (ph-drain (ph-from-list (list 42 17 93 5 61 28 74 39 86 10)) nil)
              (sort (lambda (a b) (< a b))
                    (list 42 17 93 5 61 28 74 39 86 10))))

(check "pairing-heap: error on empty find-min is catchable"
       (equal (car (catch (ph-find-min (ph-new)) (lambda (e) e))) (quote error)))
