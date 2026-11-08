; ============================================================
; name: zipper
; file: algorithms/data-structures/zipper.lsp
; subject: data-structure
; capability: cursor navigation editing
; aliases: "list zipper" "cursor" "focus"
; signature: (zip-from-list xs) (zip-to-list z) (zip-left z) (zip-right z)
;            (zip-read z) (zip-write z x) (zip-insert-right z x)
;            (zip-delete-right z) (zip-start-p z) (zip-end-p z)
; complexity: O(1) per cursor move or local edit
; see-also: linked-list deque
; derived-from: Huet 1997 ("The Zipper", J. Functional Programming)
; status: example (tested)
; checks: 11 checks; run: src/sisp <this file> from repo root
; ============================================================
; Huet's list zipper: a focus within a sequence, as a pair
; (reversed-left right).  Moving left/right and editing at the focus
; are O(1); converting from/to a plain list is O(n).

(define (zip-from-list xs) (cons nil xs))
(define (zip-to-list z) (append (reverse (car z)) (cdr z)))

(define (zip-start-p z) (null (car z)))
(define (zip-end-p z) (null (cdr z)))

(define (zip-left z)
  (cond ((null (car z)) (error "ZIP-LEFT: AT START" z))
        (t (cons (cdr (car z)) (cons (car (car z)) (cdr z))))))

(define (zip-right z)
  (cond ((null (cdr z)) (error "ZIP-RIGHT: AT END" z))
        (t (cons (cons (car (cdr z)) (car z)) (cdr (cdr z))))))

(define (zip-read z)
  (cond ((null (cdr z)) (error "ZIP-READ: AT END" z))
        (t (car (cdr z)))))

(define (zip-write z x)
  (cond ((null (cdr z)) (error "ZIP-WRITE: AT END" z))
        (t (cons (car z) (cons x (cdr (cdr z)))))))

(define (zip-insert-right z x)
  (cons (car z) (cons x (cdr z))))

(define (zip-delete-right z)
  (cond ((null (cdr z)) (error "ZIP-DELETE-RIGHT: AT END" z))
        (t (cons (car z) (cdr (cdr z))))))

; ---- checks ----

(check "zipper: from-list starts at the left end"
       (and (zip-start-p (zip-from-list (list 1 2 3)))
            (= (zip-read (zip-from-list (list 1 2 3))) 1)))

(check "zipper: round-trip to-list"
       (equal (zip-to-list (zip-from-list (list 1 2 3 4))) (list 1 2 3 4)))

(check "zipper: right then read"
       (= (zip-read (zip-right (zip-from-list (list 1 2 3)))) 2))

(check "zipper: right twice, left once -> middle"
       (= (zip-read (zip-left (zip-right (zip-right (zip-from-list (list 1 2 3)))))) 2))

(check "zipper: write changes the focus element"
       (equal (zip-to-list (zip-write (zip-right (zip-from-list (list 1 2 3))) 9))
              (list 1 9 3)))

(check "zipper: insert-right"
       (equal (zip-to-list (zip-insert-right (zip-right (zip-from-list (list 1 3))) 2))
              (list 1 2 3)))

(check "zipper: delete-right removes the focus"
       (equal (zip-to-list (zip-delete-right (zip-right (zip-from-list (list 1 2 3)))))
              (list 1 3)))

(check "zipper: end detection"
       (zip-end-p (zip-right (zip-right (zip-right (zip-from-list (list 1 2 3)))))))

(check "zipper: start detection after moving left to boundary"
       (zip-start-p (zip-left (zip-right (zip-from-list (list 1 2))))))

(check "zipper: navigation is reversible (to-list unchanged after tour)"
       (let ((z (zip-from-list (list 1 2 3 4))))
         (equal (zip-to-list (zip-left (zip-left (zip-right (zip-right z)))))
                (list 1 2 3 4))))

(check "zipper: error moving right past the end is catchable"
       (equal (car (catch (zip-right (zip-right (zip-from-list (list 1)))) (lambda (e) e)))
              (quote error)))
