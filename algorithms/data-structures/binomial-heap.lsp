; ============================================================
; name: binomial-heap
; file: algorithms/data-structures/binomial-heap.lsp
; subject: data-structure
; capability: priority queue merge insert extract-min
; aliases: "binomial queue" "mergeable heap"
; signature: (bh-new) (bh-insert h x) (bh-merge a b) (bh-find-min h)
;            (bh-extract-min h) (bh-from-list xs)
; complexity: O(log n) insert/merge/extract-min, O(log n) find-min
; see-also: pairing-heap heap
; derived-from: CLRS (2nd ed.) 19; Okasaki 1998, 3.2
; status: example (tested)
; checks: 11 checks; run: src/sisp <this file> from repo root
; ============================================================
; Binomial heap, purely functional.  A heap is a list of binomial
; trees sorted by increasing rank.  A binomial tree of rank r is
; (rank val children), where children are trees of ranks r-1..0 in
; that order.  Insert is binary increment with linking as carry;
; merge is binary addition of the two rank-sorted lists.

(define (bh-new) nil)

(define (bh-tree rank v children) (list rank v children))
(define (bh-rank tr) (car tr))
(define (bh-val tr) (cadr tr))
(define (bh-children tr) (caddr tr))

; link two trees of equal rank: the smaller root becomes the parent
(define (bh-link a b)
  (cond ((<= (bh-val a) (bh-val b))
         (bh-tree (+ (bh-rank a) 1) (bh-val a) (cons b (bh-children a))))
        (t (bh-link b a))))

; insert a tree into a rank-sorted tree list (carry on equal ranks)
(define (bh-ins-tree h tr)
  (cond ((null h) (list tr))
        ((< (bh-rank tr) (bh-rank (car h))) (cons tr h))
        (t (bh-ins-tree (cdr h) (bh-link tr (car h))))))

(define (bh-insert h x)
  (bh-ins-tree h (bh-tree 0 x nil)))

; merge two rank-sorted tree lists (binary addition)
(define (bh-merge a b)
  (cond ((null a) b)
        ((null b) a)
        (t (let ((t1 (car a)) (t2 (car b)))
             (cond ((< (bh-rank t1) (bh-rank t2))
                    (cons t1 (bh-merge (cdr a) b)))
                   ((> (bh-rank t1) (bh-rank t2))
                    (cons t2 (bh-merge a (cdr b))))
                   (t (bh-ins-tree (bh-merge (cdr a) (cdr b))
                                   (bh-link t1 t2))))))))

(define (bh-find-min h)
  (cond ((null h) (error "BH-FIND-MIN: EMPTY HEAP" h))
        (t (bh-min-roots h (bh-val (car h))))))

(define (bh-min-roots h best)
  (cond ((null h) best)
        ((< (bh-val (car h)) best) (bh-min-roots (cdr h) (bh-val (car h))))
        (t (bh-min-roots (cdr h) best))))

; remove the minimum tree and return (rest-tree-list min-tree)
(define (bh-remove-min h)
  (cond ((null (cdr h)) (list nil (car h)))
        (t (let* ((r (bh-remove-min (cdr h)))
                  (rest (car r))
                  (min-tr (cadr r)))
             (cond ((< (bh-val (car h)) (bh-val min-tr))
                    (list (cdr h) (car h)))
                   (t (list (cons (car h) rest) min-tr)))))))

(define (bh-extract-min h)
  "returns (min new-heap) as a 2-element list"
  (cond ((null h) (error "BH-EXTRACT-MIN: EMPTY HEAP" h))
        (t (let* ((r (bh-remove-min h))
                  (rest (car r))
                  (min-tr (cadr r))
                  ; children are in decreasing rank order; reverse them
                  (children-heap (reverse (bh-children min-tr))))
             (list (bh-val min-tr) (bh-merge rest children-heap))))))

(define (bh-from-list xs)
  (foldl (lambda (h x) (bh-insert h x)) nil xs))

; ---- check helpers (defined before use) ----

(define (bh-drain h acc)
  (cond ((null h) (reverse acc))
        (t (let ((r (bh-extract-min h)))
             (bh-drain (cadr r) (cons (car r) acc))))))

(define (bh-tree-order-p tr)
  (and (bh-children-ge (bh-children tr) (bh-val tr))
       (bh-children-ordered (bh-children tr))))

(define (bh-children-ge children m)
  (cond ((null children) t)
        ((< (bh-val (car children)) m) nil)
        (t (and (bh-tree-order-p (car children))
                (bh-children-ge (cdr children) m)))))

(define (bh-children-ordered children)
  (cond ((or (null children) (null (cdr children))) t)
        (t (and (> (bh-rank (car children)) (bh-rank (cadr children)))
                (bh-children-ordered (cdr children))))))

(define (bh-heap-valid-p h)
  (and (bh-ranks-increasing h)
       (bh-all-trees-ordered h)))

(define (bh-ranks-increasing h)
  (cond ((or (null h) (null (cdr h))) t)
        (t (and (< (bh-rank (car h)) (bh-rank (cadr h)))
                (bh-ranks-increasing (cdr h))))))

(define (bh-all-trees-ordered h)
  (cond ((null h) t)
        (t (and (bh-tree-order-p (car h))
                (bh-all-trees-ordered (cdr h))))))

; ---- checks ----

(check "binomial-heap: insert then find-min"
       (= (bh-find-min (bh-insert (bh-new) 5)) 5))

(check "binomial-heap: min of several"
       (= (bh-find-min (bh-from-list (list 5 3 7 1 4))) 1))

(check "binomial-heap: extract-min returns min, keeps structure valid"
       (let ((r (bh-extract-min (bh-from-list (list 5 3 7 1 4)))))
         (and (= (car r) 1)
              (bh-heap-valid-p (cadr r)))))

(check "binomial-heap: drain yields sorted order"
       (equal (bh-drain (bh-from-list (list 9 3 7 1 5 8 2 6 4)) nil)
              (list 1 2 3 4 5 6 7 8 9)))

(check "binomial-heap: merge two heaps"
       (equal (bh-drain (bh-merge (bh-from-list (list 1 5 9))
                                  (bh-from-list (list 2 6 7)))
                        nil)
              (list 1 2 5 6 7 9)))

(check "binomial-heap: with duplicates"
       (equal (bh-drain (bh-from-list (list 3 1 4 1 5)) nil)
              (list 1 1 3 4 5)))

(check "binomial-heap: structure valid after many inserts"
       (bh-heap-valid-p (bh-from-list (list 4 9 1 7 3 10 2 8 5 6))))

(check "binomial-heap: ranks are strictly increasing after 10 inserts"
       (bh-ranks-increasing (bh-from-list (seq 1 10))))

(check "binomial-heap: drain of 64 elements is sorted"
       (equal (bh-drain (bh-from-list (reverse (seq 1 64))) nil)
              (seq 1 64)))

(check "binomial-heap: matches builtin sort"
       (equal (bh-drain (bh-from-list (list 42 17 93 5 61 28 74 39 86 10)) nil)
              (sort (lambda (a b) (< a b))
                    (list 42 17 93 5 61 28 74 39 86 10))))

(check "binomial-heap: error on empty find-min is catchable"
       (equal (car (catch (bh-find-min (bh-new)) (lambda (e) e))) (quote error)))
