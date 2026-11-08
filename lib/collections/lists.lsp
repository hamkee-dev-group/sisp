; ============================================================
; lib/collections/lists.lsp -- list utilities
;
; Public:   (take-while pred lst) -> list
;           (drop-while pred lst) -> list
;           (split-at n lst) -> (head . tail) dotted pair
;           (group-by key lst) -> alist of (key . group-elements)
;           (transpose rows) -> columns (list of lists)
; Private:  group-by-acc
; Depends:  none (uses prelude functions)
; Assumes:  list arguments are proper lists
; ============================================================

(define (take-while pred lst)
  "return the longest prefix of lst where every element satisfies pred"
  (cond ((null lst) nil)
        ((pred (car lst)) (cons (car lst) (take-while pred (cdr lst))))
        (t nil)))

(define (drop-while pred lst)
  "remove the longest prefix of lst where every element satisfies pred"
  (cond ((null lst) nil)
        ((pred (car lst)) (drop-while pred (cdr lst)))
        (t lst)))

(define (split-at n lst)
  "return a dotted pair (first-n . rest)"
  (cond ((= n 0) (cons nil lst))
        ((null lst) (cons nil nil))
        (t (let ((rec (split-at (- n 1) (cdr lst))))
             (cons (cons (car lst) (car rec)) (cdr rec))))))

(define (group-by key lst)
  "partition lst into an alist mapping key values to lists of elements with that key"
  (group-by-acc key lst nil))

; The lookup and the removal below must agree on what "same key" means.
; assoc defaults to eq, which for a string means identity, while remove uses
; equal -- so a string key found the old group with remove but not with assoc,
; and the accumulated elements were dropped: grouping (list "a" "b" "a") kept
; only the last "a". This was already wrong for computed strings before the
; reader stopped interning literals; it merely became visible for literals too.
(define (group-by-acc key lst acc)
  (cond ((null lst) (reverse acc))
        (t (let* ((k (key (car lst)))
                  (entry (assoc k acc (quote equal)))
                  (rest (remove (lambda (p) (equal (car p) k)) acc)))
             (group-by-acc key (cdr lst)
               (cons (cons k (append (cond ((eq entry nil) nil) (t (cdr entry)))
                                     (list (car lst))))
                     rest))))))

(define (transpose rows)
  "transpose a list of equal-length lists (matrix as list of rows)"
  (cond ((null rows) nil)
        ((null (car rows)) nil)
        (t (cons (map car rows)
                 (transpose (map cdr rows))))))
