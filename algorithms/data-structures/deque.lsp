; ============================================================
; name: deque
; file: algorithms/data-structures/deque.lsp
; subject: data-structure
; capability: double-ended queue push pop
; aliases: "double-ended queue" "two-list deque"
; signature: (deque-new) (deque-push-front d x) (deque-push-back d x)
;            (deque-pop-front d) (deque-pop-back d) (deque-front d)
;            (deque-back d) (deque-empty-p d) (deque-size d)
; complexity: amortized O(1) per operation (two-list deque)
; see-also: stack queue
; status: example (tested)
; checks: 9 checks at bottom; run: src/sisp <this file> from repo root
; ============================================================
; Purely functional double-ended queue as a pair (front . back), front
; in order, back reversed.  Pushes and pops on either end are O(1) until
; one side empties, then half of the other side is reversed over
; (Okasaki's balancing).  Access on an empty deque aborts the form.

(define (deque-new) (cons nil nil))

(define (deque-empty-p d) (and (null (car d)) (null (cdr d))))

; invariant: after every operation both sides are non-empty (or the
; deque is).  When one side empties, move half of the other side over.
(define (deque-normalize d)
  (cond ((and (null (car d)) (not (null (cdr d))))
         ; everything sits in the back list (reversed); move the front
         ; half over: take keeps the back-most k, the rest reverts
         (let* ((n (length (cdr d)))
                (k (truncate (/ n 2))))
           (cons (reverse (drop (cdr d) k)) (take (cdr d) k))))
        ((and (null (cdr d)) (not (null (car d))))
         ; everything sits in the front list; move the back half over
         (let* ((n (length (car d)))
                (k (truncate (/ (+ n 1) 2))))
           (cons (take (car d) k) (reverse (drop (car d) k)))))
        (t d)))

(define (deque-push-front d x)
  (deque-normalize (cons (cons x (car d)) (cdr d))))

(define (deque-push-back d x)
  (deque-normalize (cons (car d) (cons x (cdr d)))))

(define (deque-front d)
  (cond ((deque-empty-p d) (error "DEQUE-FRONT: EMPTY DEQUE" d))
        ((null (car d)) (car (reverse (cdr d))))
        (t (car (car d)))))

(define (deque-back d)
  (cond ((deque-empty-p d) (error "DEQUE-BACK: EMPTY DEQUE" d))
        ((null (cdr d)) (car (reverse (car d))))
        (t (car (cdr d)))))

(define (deque-pop-front d)
  (cond ((deque-empty-p d) (error "DEQUE-POP-FRONT: EMPTY DEQUE" d))
        ((null (car d)) (cons nil nil))   ; single element
        (t (deque-normalize (cons (cdr (car d)) (cdr d))))))

(define (deque-pop-back d)
  (cond ((deque-empty-p d) (error "DEQUE-POP-BACK: EMPTY DEQUE" d))
        ((null (cdr d)) (cons nil nil))   ; single element
        (t (deque-normalize (cons (car d) (cdr (cdr d)))))))

(define (deque-size d) (+ (length (car d)) (length (cdr d))))

; ---- check helpers (defined before use) ----

(define (deque-fill-back d xs)
  (cond ((null xs) d) (t (deque-fill-back (deque-push-back d (car xs)) (cdr xs)))))

; ---- checks ----

(check "deque: new is empty"
       (deque-empty-p (deque-new)))

(check "deque: push-front then front"
       (= (deque-front (deque-push-front (deque-new) 1)) 1))

(check "deque: push-back then back"
       (= (deque-back (deque-push-back (deque-new) 1)) 1))

(check "deque: front and back see opposite ends"
       (let ((d (deque-push-back (deque-push-back (deque-push-front (deque-new) 1) 2) 3)))
         (and (= (deque-front d) 1)
              (= (deque-back d) 3))))

(check "deque: pop-front then pop-back order"
       (let* ((d (deque-push-back (deque-push-back (deque-push-front (deque-new) 1) 2) 3))
              (d2 (deque-pop-front d)))
         (and (= (deque-front d2) 2)
              (= (deque-back (deque-pop-back d2)) 2))))

(check "deque: size"
       (= (deque-size (deque-push-back (deque-push-front (deque-new) 1) 2)) 2))

(check "deque: empty after draining both ends"
       (and (deque-empty-p (deque-pop-back (deque-push-back (deque-new) 1)))
            (deque-empty-p (deque-pop-front (deque-push-front (deque-new) 1)))))

(check "deque: rebalance serves many front pops"
       (let ((d (deque-fill-back (deque-new) (list 1 2 3 4 5))))
         (and (= (deque-front d) 1)
              (= (deque-front (deque-pop-front d)) 2)
              (= (deque-front (deque-pop-front (deque-pop-front d))) 3))))

(check "deque: error on empty front is catchable"
       (equal (car (catch (deque-front (deque-new)) (lambda (e) e))) (quote error)))
