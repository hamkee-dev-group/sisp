; ============================================================
; name: linked-list
; file: algorithms/data-structures/linked-list.lsp
; subject: data-structure
; capability: sequence cons traversal
; aliases: "singly linked list" "cons list" "forward list"
; signature: (ll-new) (ll-cons x l) (ll-head l) (ll-tail l)
;            (ll-nth l i) (ll-length l) (ll-append a b) (ll-reverse l)
;            (ll-to-list l) (ll-from-list xs)
; complexity: O(1) cons/head/tail, O(n) nth/append
; see-also: stack queue
; status: example (tested)
; checks: 10 checks at bottom; run: src/sisp <this file> from repo root
; ============================================================
; The singly linked list is SISP's native cons structure: each cell is
; (value . next), the empty list is nil.  This file makes the pointer
; model explicit -- the same operations textbooks name -- and shows the
; classic traversals (iterative reverse, tail-recursive map).

(define (ll-new) nil)
(define (ll-cons x l) (cons x l))
(define (ll-head l) (car l))
(define (ll-tail l) (cdr l))
(define (ll-empty-p l) (null l))

(define (ll-nth l i)
  (cond ((null l) (error "LL-NTH: OUT OF RANGE" i))
        ((= i 0) (car l))
        (t (ll-nth (cdr l) (- i 1)))))

(define (ll-length l)
  (cond ((null l) 0) (t (+ 1 (ll-length (cdr l))))))

(define (ll-append a b)
  (cond ((null a) b)
        (t (cons (car a) (ll-append (cdr a) b)))))

; the classic O(n) reverse through an accumulator
(define (ll-reverse-acc l acc)
  (cond ((null l) acc)
        (t (ll-reverse-acc (cdr l) (cons (car l) acc)))))

(define (ll-reverse l) (ll-reverse-acc l nil))

(define (ll-to-list l) l)
(define (ll-from-list xs) xs)

; ---- checks ----

(check "linked-list: new is empty"
       (ll-empty-p (ll-new)))

(check "linked-list: cons/head/tail"
       (let ((l (ll-cons 1 (ll-cons 2 (ll-new)))))
         (and (= (ll-head l) 1)
              (= (ll-head (ll-tail l)) 2))))

(check "linked-list: nth walks the chain"
       (= (ll-nth (ll-from-list (list 10 20 30)) 2) 30))

(check "linked-list: length"
       (= (ll-length (ll-from-list (list 1 2 3 4))) 4))

(check "linked-list: append preserves order"
       (equal (ll-append (list 1 2) (list 3 4)) (list 1 2 3 4)))

(check "linked-list: reverse"
       (equal (ll-reverse (list 1 2 3)) (list 3 2 1)))

(check "linked-list: reverse of empty"
       (eq (ll-reverse nil) nil))

(check "linked-list: double reverse is identity"
       (equal (ll-reverse (ll-reverse (list 1 2 3 4))) (list 1 2 3 4)))

(check "linked-list: append with empty left"
       (equal (ll-append nil (list 1 2)) (list 1 2)))

(check "linked-list: nth out of range is catchable"
       (equal (car (catch (ll-nth (list 1) 5) (lambda (e) e))) (quote error)))
