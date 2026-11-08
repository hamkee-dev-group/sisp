; ============================================================
; name: binary-search-tree
; file: algorithms/data-structures/binary-search-tree.lsp
; subject: data-structure
; capability: sorted map insert find delete
; aliases: "bst" "ordered tree"
; signature: (bst-new) (bst-insert tr x) (bst-find tr x) (bst-delete tr x)
;            (bst-inorder tr) (bst-min tr) (bst-max tr)
; complexity: O(h) per operation; O(n) worst case (unbalanced)
; see-also: avl-tree red-black-tree
; status: example (tested)
; checks: 12 checks at bottom; run: src/sisp <this file> from repo root
; ============================================================
; Unbalanced binary search tree, purely functional.
; Node = (val left right); empty tree = nil.  Insert/find/delete are
; O(h); inorder traversal yields the sorted sequence.  Deleting a
; two-child node promotes the inorder successor.

(define (bst-new) nil)

(define (bst-insert tr x)
  (cond ((null tr) (list x nil nil))
        ((< x (car tr)) (list (car tr) (bst-insert (cadr tr) x) (caddr tr)))
        ((> x (car tr)) (list (car tr) (cadr tr) (bst-insert (caddr tr) x)))
        (t tr)))

(define (bst-find tr x)
  (cond ((null tr) nil)
        ((< x (car tr)) (bst-find (cadr tr) x))
        ((> x (car tr)) (bst-find (caddr tr) x))
        (t t)))

(define (bst-min tr)
  (cond ((null tr) (error "BST-MIN: EMPTY TREE" tr))
        ((null (cadr tr)) (car tr))
        (t (bst-min (cadr tr)))))

(define (bst-max tr)
  (cond ((null tr) (error "BST-MAX: EMPTY TREE" t))
        ((null (caddr tr)) (car tr))
        (t (bst-max (caddr tr)))))

(define (bst-delete tr x)
  (cond ((null tr) nil)
        ((< x (car tr)) (list (car tr) (bst-delete (cadr tr) x) (caddr tr)))
        ((> x (car tr)) (list (car tr) (cadr tr) (bst-delete (caddr tr) x)))
        (t (bst-delete-node tr))))

(define (bst-delete-node tr)
  (cond ((null (cadr tr)) (caddr tr))
        ((null (caddr tr)) (cadr tr))
        (t (let ((sv (bst-min (caddr tr))))
             (list sv (cadr tr) (bst-delete (caddr tr) sv))))))

(define (bst-inorder-acc tr acc)
  (cond ((null tr) acc)
        (t (bst-inorder-acc (cadr tr)
                            (cons (car tr)
                                  (bst-inorder-acc (caddr tr) acc))))))

(define (bst-inorder tr) (bst-inorder-acc tr nil))

(define (bst-fill xs tr)
  (cond ((null xs) tr) (t (bst-fill (cdr xs) (bst-insert tr (car xs))))))

; ---- checks ----

(check "bst: insert and inorder is sorted"
       (equal (bst-inorder (bst-fill (list 5 3 7 1 4 6 8) (bst-new)))
              (list 1 3 4 5 6 7 8)))

(check "bst: find present"
       (bst-find (bst-fill (list 5 3 7) (bst-new)) 3))

(check "bst: find absent -> nil"
       (eq (bst-find (bst-fill (list 5 3 7) (bst-new)) 4) nil))

(check "bst: duplicate insert keeps one copy"
       (equal (bst-inorder (bst-insert (bst-insert (bst-new) 5) 5)) (list 5)))

(check "bst: min and max"
       (let ((tr (bst-fill (list 5 3 7 1 4 6 8) (bst-new))))
         (and (= (bst-min tr) 1) (= (bst-max tr) 8))))

(check "bst: delete a leaf"
       (equal (bst-inorder (bst-delete (bst-fill (list 5 3 7) (bst-new)) 3))
              (list 5 7)))

(check "bst: delete a one-child node"
       (equal (bst-inorder (bst-delete (bst-fill (list 5 3 7 6) (bst-new)) 7))
              (list 3 5 6)))

(check "bst: delete a two-child node promotes successor"
       (equal (bst-inorder (bst-delete (bst-fill (list 5 3 7 6 8) (bst-new)) 5))
              (list 3 6 7 8)))

(check "bst: delete absent key is a no-op"
       (equal (bst-inorder (bst-delete (bst-fill (list 5 3 7) (bst-new)) 4))
              (list 3 5 7)))

(check "bst: delete all leaves empty"
       (eq (bst-delete (bst-delete (bst-insert (bst-new) 1) 1) 1) nil))

(check "bst: sorted insert degrades to a chain but stays correct"
       (equal (bst-inorder (bst-fill (list 1 2 3 4 5) (bst-new)))
              (list 1 2 3 4 5)))

(check "bst: find after delete sequence"
       (let ((tr (bst-fill (list 5 3 7 1 4 6 8) (bst-new))))
         (let ((t2 (bst-delete (bst-delete tr 5) 3)))
           (and (bst-find t2 6)
                (eq (bst-find t2 5) nil)
                (equal (bst-inorder t2) (list 1 4 6 7 8))))))
