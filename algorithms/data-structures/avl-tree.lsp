; ============================================================
; name: avl-tree
; file: algorithms/data-structures/avl-tree.lsp
; subject: data-structure
; capability: balanced sorted map insert delete
; aliases: "avl" "balanced bst" "height-balanced tree"
; signature: (avl-new) (avl-insert t x) (avl-delete t x) (avl-find t x)
;            (avl-inorder t) (avl-height t) (avl-balanced-p t)
; complexity: O(log n) insert/delete/find
; see-also: binary-search-tree red-black-tree
; derived-from: Adelson-Velsky & Landis 1962; TAOCP 6.2.3
; status: example (tested)
; checks: 10 checks incl. exhaustive 120-permutation insert/delete
;         invariant sweep; run: src/sisp <this file> from repo root
; ============================================================
; AVL tree (TAOCP 6.2.3), purely functional.
; Node = (val left right height); height is maintained by avl-mk.
; Rebalance case split (bf = right height - left height):
;   bf > 1 : right-right, or right-left when the right child leans left
;   bf < -1: left-left, or left-right when the left child leans right

(define (avl-new) nil)

(define (avl-h nd) (cond ((null nd) 0) (t (cadddr nd))))
(define (avl-bf nd) (cond ((null nd) 0) (t (- (avl-h (caddr nd)) (avl-h (cadr nd))))))
(define (avl-mk v l r)
  (list v l r (+ 1 (max (avl-h l) (avl-h r)))))

(define (avl-rot-ll y)
  (let ((x (cadr y)))
    (avl-mk (car x) (cadr x)
            (avl-mk (car y) (caddr x) (caddr y)))))

(define (avl-rot-rr x)
  (let ((y (caddr x)))
    (avl-mk (car y) (avl-mk (car x) (cadr x) (cadr y))
            (caddr y))))

(define (avl-rot-lr nd)
  (avl-rot-ll (avl-mk (car nd) (avl-rot-rr (cadr nd)) (caddr nd))))

(define (avl-rot-rl nd)
  (avl-rot-rr (avl-mk (car nd) (cadr nd) (avl-rot-ll (caddr nd)))))

(define (avl-rebal v l r)
  (let ((nd (avl-mk v l r)))
    (let ((b (avl-bf nd)))
      (cond ((> b 1)
             (cond ((< (avl-bf (caddr nd)) 0) (avl-rot-rl nd))
                   (t (avl-rot-rr nd))))
            ((< b -1)
             (cond ((> (avl-bf (cadr nd)) 0) (avl-rot-lr nd))
                   (t (avl-rot-ll nd))))
            (t nd)))))

(define (avl-insert nd val)
  (cond ((null nd) (avl-mk val nil nil))
        ((< val (car nd)) (avl-rebal (car nd) (avl-insert (cadr nd) val) (caddr nd)))
        ((> val (car nd)) (avl-rebal (car nd) (cadr nd) (avl-insert (caddr nd) val)))
        (t nd)))

(define (avl-min nd)
  (cond ((null (cadr nd)) (car nd))
        (t (avl-min (cadr nd)))))

(define (avl-delete nd val)
  (cond ((null nd) nil)
        ((< val (car nd)) (avl-rebal (car nd) (avl-delete (cadr nd) val) (caddr nd)))
        ((> val (car nd)) (avl-rebal (car nd) (cadr nd) (avl-delete (caddr nd) val)))
        (t (cond ((null (cadr nd)) (caddr nd))
                 ((null (caddr nd)) (cadr nd))
                 (t (let ((sv (avl-min (caddr nd))))
                      (avl-rebal sv (cadr nd) (avl-delete (caddr nd) sv))))))))

(define (avl-find nd val)
  (cond ((null nd) nil)
        ((< val (car nd)) (avl-find (cadr nd) val))
        ((> val (car nd)) (avl-find (caddr nd) val))
        (t t)))

(define (avl-inorder-acc nd acc)
  (cond ((null nd) acc)
        (t (avl-inorder-acc (cadr nd)
                            (cons (car nd) (avl-inorder-acc (caddr nd) acc))))))

(define (avl-inorder nd) (avl-inorder-acc nd nil))

(define (avl-height nd) (avl-h nd))

; stored heights must match actual heights and |bf| <= 1 everywhere
(define (avl-balanced-p nd)
  (cond ((null nd) t)
        (t (and (= (avl-h nd)
                   (+ 1 (max (avl-h (cadr nd)) (avl-h (caddr nd)))))
                (<= (avl-abs (avl-bf nd)) 1)
                (avl-balanced-p (cadr nd))
                (avl-balanced-p (caddr nd))))))

(define (avl-abs x) (cond ((< x 0) (- 0 x)) (t x)))

; ---- exhaustive harness: every permutation of (1 2 3 4 5) ----

(load "lib/math/combinatorics.lsp")

(define (avl-fill nd xs)
  (cond ((null xs) nd) (t (avl-fill (avl-insert nd (car xs)) (cdr xs)))))

(define (avl-check-insert-perm perm)
  (and (equal (avl-inorder (avl-fill (avl-new) perm)) (list 1 2 3 4 5))
       (avl-balanced-p (avl-fill (avl-new) perm))))

(define (avl-delete-all nd xs)
  (cond ((null xs) nd) (t (avl-delete-all (avl-delete nd (car xs)) (cdr xs)))))

(define (avl-check-delete-perm perm)
  (let ((full (avl-fill (avl-new) perm)))
    (and (avl-delete-balanced full perm)
         (eq (avl-delete-all full perm) nil))))

(define (avl-delete-balanced nd xs)
  (cond ((null xs) t)
        (t (let ((nd2 (avl-delete nd (car xs))))
             (and (avl-balanced-p nd2)
                  (avl-sorted-p (avl-inorder nd2))
                  (avl-delete-balanced nd2 (cdr xs)))))))

(define (avl-sorted-p xs)
  (cond ((or (null xs) (null (cdr xs))) t)
        (t (and (< (car xs) (cadr xs)) (avl-sorted-p (cdr xs))))))

(define (avl-check-insert-perm-4 perm)
  (and (equal (avl-inorder (avl-fill (avl-new) perm)) (list 1 2 3 4))
       (avl-balanced-p (avl-fill (avl-new) perm))))

; ---- checks ----

(check "avl: inorder of a build is sorted"
       (equal (avl-inorder (avl-fill (avl-new) (list 5 3 7 1 4 6 8)))
              (list 1 3 4 5 6 7 8)))

(check "avl: sorted input does NOT degenerate (height stays log)"
       (let ((nd (avl-fill (avl-new) (list 1 2 3 4 5 6 7))))
         (and (<= (avl-height nd) 3)
              (equal (car nd) 4))))

(check "avl: reverse-sorted input stays balanced"
       (avl-balanced-p (avl-fill (avl-new) (list 7 6 5 4 3 2 1))))

(check "avl: find present and absent"
       (let ((nd (avl-fill (avl-new) (list 5 3 7))))
         (and (avl-find nd 3)
              (eq (avl-find nd 4) nil))))

(check "avl: delete keeps order and balance"
       (let ((nd (avl-delete (avl-fill (avl-new) (list 5 3 7 1 4 6 8)) 5)))
         (and (equal (avl-inorder nd) (list 1 3 4 6 7 8))
              (avl-balanced-p nd))))

(check "avl: exhaustive insert over all 120 permutations of 5"
       (every avl-check-insert-perm (permutations (list 1 2 3 4 5))))

(check "avl: exhaustive delete over all 120 permutations of 5"
       (every avl-check-delete-perm (permutations (list 1 2 3 4 5))))

(check "avl: duplicate insert keeps one copy"
       (equal (avl-inorder (avl-insert (avl-insert (avl-new) 5) 5)) (list 5)))

(check "avl: delete absent key is a no-op"
       (equal (avl-inorder (avl-delete (avl-fill (avl-new) (list 3 1 4)) 9))
              (list 1 3 4)))

(check "avl: exhaustive over all 24 permutations of 4 incl. reverse delete"
       (every avl-check-insert-perm-4 (permutations (list 1 2 3 4))))
