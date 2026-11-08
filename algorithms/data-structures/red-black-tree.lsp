; ============================================================
; name: red-black-tree
; file: algorithms/data-structures/red-black-tree.lsp
; subject: data-structure
; capability: balanced sorted map insert find
; aliases: "rbt" "okasaki red-black" "balanced bst"
; signature: (rb-new) (rb-insert t x) (rb-find t x) (rb-inorder t)
;            (rb-valid-p t)
; complexity: O(log n) insert/find; delete not provided (see notes)
; see-also: avl-tree binary-search-tree
; derived-from: Okasaki 1999 ("Red-black trees in a functional
;               setting"); invariants as in CLRS 13
; status: example (tested)
; checks: 10 checks incl. exhaustive 120-permutation invariant sweep;
;         run: src/sisp <this file> from repo root
; ============================================================
; Red-black tree, purely functional, after Okasaki (1999).
; Node = (color val left right), color in {red black}; empty = nil.
; Insertion colors the new node red and rebalances on the way up with
; the four rotation patterns; the root is then blackened.
; Invariants: no red node has a red child; every root-to-leaf path has
; the same number of black nodes (black-height).  Deletion is the hard
; part of the functional literature and is deliberately not provided
; (use avl-tree when delete is needed).

(define (rb-new) nil)

(define (rb-mk c v l r) (list c v l r))

; the four balance patterns (Okasaki): rotate when a red parent has a
; red child, in any of the four mirror configurations
(define (rb-balance color v l r)
  (cond ((and (eq color (quote black)) (rb-red-p l) (rb-red-p (rb-l l)))
         (rb-mk (quote red) (rb-v l)
                (rb-mk (quote black) (rb-v (rb-l l)) (rb-l (rb-l l)) (rb-r (rb-l l)))
                (rb-mk (quote black) v (rb-r l) r)))
        ((and (eq color (quote black)) (rb-red-p l) (rb-red-p (rb-r l)))
         (rb-mk (quote red) (rb-v (rb-r l))
                (rb-mk (quote black) (rb-v l) (rb-l l) (rb-l (rb-r l)))
                (rb-mk (quote black) v (rb-r (rb-r l)) r)))
        ((and (eq color (quote black)) (rb-red-p r) (rb-red-p (rb-l r)))
         (rb-mk (quote red) (rb-v (rb-l r))
                (rb-mk (quote black) v l (rb-l (rb-l r)))
                (rb-mk (quote black) (rb-v r) (rb-r (rb-l r)) (rb-r r))))
        ((and (eq color (quote black)) (rb-red-p r) (rb-red-p (rb-r r)))
         (rb-mk (quote red) (rb-v r)
                (rb-mk (quote black) v l (rb-l r))
                (rb-mk (quote black) (rb-v (rb-r r)) (rb-l (rb-r r)) (rb-r (rb-r r)))))
        (t (rb-mk color v l r))))

(define (rb-red-p nd)
  (and (not (null nd)) (eq (car nd) (quote red))))

(define (rb-v nd) (cadr nd))
(define (rb-l nd) (caddr nd))
(define (rb-r nd) (cadddr nd))

(define (rb-ins nd x)
  (cond ((null nd) (rb-mk (quote red) x nil nil))
        ((< x (rb-v nd)) (rb-balance (car nd) (rb-v nd) (rb-ins (rb-l nd) x) (rb-r nd)))
        ((> x (rb-v nd)) (rb-balance (car nd) (rb-v nd) (rb-l nd) (rb-ins (rb-r nd) x)))
        (t nd)))

(define (rb-insert nd x)
  (let ((t2 (rb-ins nd x)))
    (rb-mk (quote black) (rb-v t2) (rb-l t2) (rb-r t2))))

(define (rb-find nd x)
  (cond ((null nd) nil)
        ((< x (rb-v nd)) (rb-find (rb-l nd) x))
        ((> x (rb-v nd)) (rb-find (rb-r nd) x))
        (t t)))

(define (rb-inorder-acc nd acc)
  (cond ((null nd) acc)
        (t (rb-inorder-acc (rb-l nd)
                           (cons (rb-v nd) (rb-inorder-acc (rb-r nd) acc))))))

(define (rb-inorder nd) (rb-inorder-acc nd nil))

; ---- invariant validator ----

; no red node has a red child
(define (rb-no-red-red nd)
  (cond ((null nd) t)
        ((and (rb-red-p nd)
              (or (rb-red-p (rb-l nd)) (rb-red-p (rb-r nd)))) nil)
        (t (and (rb-no-red-red (rb-l nd)) (rb-no-red-red (rb-r nd))))))

; black-height: same count of black nodes on every path, or -1 if broken
(define (rb-bh nd)
  (cond ((null nd) 0)
        (t (let ((l (rb-bh (rb-l nd)))
                 (r (rb-bh (rb-r nd))))
             (cond ((or (= l -1) (= r -1) (not (= l r))) -1)
                   (t (+ l (cond ((eq (car nd) (quote black)) 1) (t 0)))))))))

; BST ordering over an open interval (lo, hi)
(define (rb-bst-p nd lo hi)
  (cond ((null nd) t)
        (t (and (or (eq lo nil) (< lo (rb-v nd)))
                (or (eq hi nil) (< (rb-v nd) hi))
                (rb-bst-p (rb-l nd) lo (rb-v nd))
                (rb-bst-p (rb-r nd) (rb-v nd) hi)))))

(define (rb-valid-p nd)
  (and (rb-no-red-red nd)
       (> (rb-bh nd) -1)
       (rb-bst-p nd nil nil)
       (cond ((null nd) t) (t (eq (car nd) (quote black))))))

; ---- exhaustive harness ----

(load "lib/math/combinatorics.lsp")

(define (rb-fill nd xs)
  (cond ((null xs) nd) (t (rb-fill (rb-insert nd (car xs)) (cdr xs)))))

(define (rb-check-perm perm)
  (let ((t2 (rb-fill (rb-new) perm)))
    (and (equal (rb-inorder t2) (list 1 2 3 4 5))
         (rb-valid-p t2)
         (rb-all-found t2 perm))))

; content check: every inserted key is findable (the shape invariants
; alone would not catch a lost key)
(define (rb-all-found nd xs)
  (cond ((null xs) t)
        ((rb-find nd (car xs)) (rb-all-found nd (cdr xs)))
        (t nil)))

(define (rb-check-perm-4 perm)
  (let ((t2 (rb-fill (rb-new) perm)))
    (and (equal (rb-inorder t2) (list 1 2 3 4))
         (rb-valid-p t2)
         (rb-all-found t2 perm))))

; ---- checks ----

(check "red-black: inorder is sorted"
       (equal (rb-inorder (rb-fill (rb-new) (list 5 3 7 1 4 6 8)))
              (list 1 3 4 5 6 7 8)))

(check "red-black: find present and absent"
       (let ((t2 (rb-fill (rb-new) (list 5 3 7))))
         (and (rb-find t2 3)
              (eq (rb-find t2 4) nil))))

(check "red-black: root is black"
       (eq (car (rb-insert (rb-new) 5)) (quote black)))

(check "red-black: invariants hold after a build"
       (rb-valid-p (rb-fill (rb-new) (list 5 3 7 1 4 6 8))))

(check "red-black: sorted input stays valid (no degeneration)"
       (let ((t2 (rb-fill (rb-new) (list 1 2 3 4 5 6 7 8))))
         (and (rb-valid-p t2)
              (equal (rb-inorder t2) (list 1 2 3 4 5 6 7 8)))))

(check "red-black: reverse-sorted input stays valid"
       (rb-valid-p (rb-fill (rb-new) (list 8 7 6 5 4 3 2 1))))

(check "red-black: duplicate insert keeps one copy"
       (equal (rb-inorder (rb-insert (rb-insert (rb-new) 5) 5)) (list 5)))

(check "red-black: exhaustive over all 120 permutations of 5"
       (every rb-check-perm (permutations (list 1 2 3 4 5))))

(check "red-black: exhaustive over all 24 permutations of 4"
       (every rb-check-perm-4 (permutations (list 1 2 3 4))))

(check "red-black: larger build (1..10 shuffled) valid and complete"
       (let ((t2 (rb-fill (rb-new) (list 4 9 1 7 3 10 2 8 5 6))))
         (and (rb-valid-p t2)
              (equal (rb-inorder t2) (list 1 2 3 4 5 6 7 8 9 10))
              (rb-all-found t2 (list 1 2 3 4 5 6 7 8 9 10)))))
