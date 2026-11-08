; ============================================================
; name: threaded-bst
; subject: data-structure
; capability: bst traversal
; status: incomplete
; canonical-home: none
; notes: Mutable threading has no functional counterpart; correct on its one tree.
; ============================================================
; ============================================================
; 85 — Threaded Binary Trees (TAOCP Vol 1, 2.3.1)
; Build a BST, thread it: left-thread -> inorder predecessor,
; right-thread -> inorder successor. Inorder traversal via threads
; (no recursion, no stack). Compare with recursive inorder.
; ============================================================

(print "=== Threaded Binary Trees ===")

(define (test name ok)
  (cond (ok (print (list (quote pass) name)))
        (t (print (list (quote FAIL) name)))))

; ---------- node: (val left right ltag rtag) ----------
(define (t-val nd) (car nd))
(define (t-left nd) (cadr nd))
(define (t-right nd) (caddr nd))
(define (t-ltag nd) (cadddr nd))
(define (t-rtag nd) (car (cddddr nd)))
(define (t-set-left nd v) (set-car (cdr nd) v))
(define (t-set-right nd v) (set-car (cddr nd) v))
(define (t-set-ltag nd v) (set-car (cdddr nd) v))
(define (t-set-rtag nd v) (set-car (cddddr nd) v))
(define (make-node val) (list val nil nil nil nil))

; ---------- BST insert ----------
(define (bst-insert root val)
  (cond ((eq root nil) (make-node val))
        ((< val (t-val root))
         (progn (t-set-left root (bst-insert (t-left root) val)) root))
        (t (progn (t-set-right root (bst-insert (t-right root) val)) root))))

; ---------- thread the BST (single inorder pass) ----------
; thread-node walks the tree in inorder and returns nil; *t-prev* holds
; the previously visited node in inorder
(setq *t-prev* nil)

(define (thread-tree root)
  (setq *t-prev* nil)
  (thread-node root)
  nil)

(define (thread-node nd)
  (cond ((eq nd nil) nil)
        (t (progn
             ; process left subtree
             (cond ((not (t-ltag nd))
                    (thread-node (t-left nd))))
             ; process current node: prev's right thread points here
             (cond ((and (not (eq *t-prev* nil)) (t-rtag *t-prev*))
                    (t-set-right *t-prev* nd)))
             ; set left thread if no left child
             (cond ((eq (t-left nd) nil)
                    (progn (t-set-left nd *t-prev*)
                           (t-set-ltag nd t)))
                   (t (t-set-ltag nd nil)))
             ; mark current as prev for next
             (setq *t-prev* nd)
             ; process right subtree
             (cond ((not (t-rtag nd))
                    (thread-node (t-right nd))))
             ; mark right as thread if no right child
             (cond ((eq (t-right nd) nil)
                    (t-set-rtag nd t)))
             nil))))

; ---------- inorder using threads ----------
(define (threaded-inorder root)
  (cond ((eq root nil) nil)
        (t (threaded-walk (leftmost root) nil))))

(define (leftmost nd)
  (cond ((t-ltag nd) nd)
        (t (leftmost (t-left nd)))))

(define (threaded-walk nd acc)
  (cond ((eq nd nil) (reverse-acc acc nil))
        (t (let ((new-acc (cons (t-val nd) acc)))
          (cond ((t-rtag nd) (threaded-walk (t-right nd) new-acc))
                (t (threaded-walk (leftmost (t-right nd)) new-acc)))))))

(define (reverse-acc lst acc)
  (cond ((eq lst nil) acc)
        (t (reverse-acc (cdr lst) (cons (car lst) acc)))))

; ---------- recursive inorder (called BEFORE threading) ----------
; simple recursive traversal using append
(define (rec-inorder root)
  (cond ((eq root nil) nil)
        (t (append (rec-inorder (t-left root))
                   (cons (t-val root)
                         (rec-inorder (t-right root)))))))

; ---------- build tree, compute recursive inorder BEFORE threading ----------
(define bst nil)
(setq bst (bst-insert bst 5))
(setq bst (bst-insert bst 3))
(setq bst (bst-insert bst 7))
(setq bst (bst-insert bst 2))
(setq bst (bst-insert bst 4))
(setq bst (bst-insert bst 6))
(setq bst (bst-insert bst 8))

(define rec-result (rec-inorder bst))
(print (list (quote recursive-order) rec-result))

; now thread it
(thread-tree bst)

(print (list (quote threaded-order) (threaded-inorder bst)))

(test "threaded-inorder-matches-recursive"
  (equal (threaded-inorder bst) rec-result))
(test "inorder-is-sorted-ascending"
  (equal (threaded-inorder bst) (list 2 3 4 5 6 7 8)))

(print "=== Threaded Binary Trees Done ===")
