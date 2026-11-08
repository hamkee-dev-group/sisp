; ============================================================
; name: queue
; file: algorithms/data-structures/queue.lsp
; subject: data-structure
; capability: fifo enqueue dequeue
; aliases: "fifo" "two-list queue" "okasaki queue"
; signature: (queue-new) (queue-enqueue q x) (queue-dequeue q)
;            (queue-front q) (queue-empty-p q) (queue-size q)
; complexity: amortized O(1) per operation (two-list queue)
; see-also: stack deque
; status: example (tested)
; checks: 9 checks at bottom; run: src/sisp <this file> from repo root
; ============================================================
; Purely functional queue as a pair (front . back): elements are
; enqueued onto the front of back, dequeued from the front of front;
; when front empties, back is reversed into it.  Okasaki's amortized
; O(1) queue.  dequeue/front on an empty queue abort the calling form.

(define (queue-new) (cons nil nil))

(define (queue-empty-p q) (and (null (car q)) (null (cdr q))))

; rebalance: if front is empty, reverse back into it
(define (queue-normalize q)
  (cond ((null (car q)) (cons (reverse (cdr q)) nil))
        (t q)))

(define (queue-enqueue q x)
  (queue-normalize (cons (car q) (cons x (cdr q)))))

(define (queue-front q)
  (cond ((queue-empty-p q) (error "QUEUE-FRONT: EMPTY QUEUE" q))
        (t (car (car q)))))

(define (queue-dequeue q)
  (cond ((queue-empty-p q) (error "QUEUE-DEQUEUE: EMPTY QUEUE" q))
        (t (queue-normalize (cons (cdr (car q)) (cdr q))))))

(define (queue-size q) (+ (length (car q)) (length (cdr q))))

; ---- check helpers (defined before use: SISP evaluates top-level
;      forms in order) ----

(define (queue-fill q xs)
  (cond ((null xs) q) (t (queue-fill (queue-enqueue q (car xs)) (cdr xs)))))

(define (queue-drain q acc)
  (cond ((queue-empty-p q) (reverse acc))
        (t (queue-drain (queue-dequeue q) (cons (queue-front q) acc)))))

; ---- checks ----

(check "queue: new is empty"
       (queue-empty-p (queue-new)))

(check "queue: enqueue then front"
       (= (queue-front (queue-enqueue (queue-new) 1)) 1))

(check "queue: fifo order over three"
       (let ((q (queue-enqueue (queue-enqueue (queue-enqueue (queue-new) 1) 2) 3)))
         (and (= (queue-front q) 1)
              (= (queue-front (queue-dequeue q)) 2)
              (= (queue-front (queue-dequeue (queue-dequeue q))) 3))))

(check "queue: empty after draining"
       (queue-empty-p (queue-dequeue (queue-enqueue (queue-new) 1))))

(check "queue: interleaved enqueue/dequeue keeps order"
       (let* ((q1 (queue-enqueue (queue-enqueue (queue-new) 1) 2))
              (q2 (queue-dequeue q1))
              (q3 (queue-enqueue (queue-enqueue q2 3) 4)))
         (and (= (queue-front q3) 2)
              (= (queue-front (queue-dequeue q3)) 3)
              (= (queue-front (queue-dequeue (queue-dequeue q3))) 4))))

(check "queue: size"
       (= (queue-size (queue-enqueue (queue-enqueue (queue-new) 1) 2)) 2))

(check "queue: rebalance after full drain of front"
       (let* ((q1 (queue-enqueue (queue-enqueue (queue-enqueue (queue-new) 1) 2) 3))
              (q2 (queue-dequeue (queue-dequeue (queue-dequeue q1))))
              (q3 (queue-enqueue q2 4)))
         (= (queue-front q3) 4)))

(check "queue: long sequence comes out in order"
       (equal (queue-drain (queue-fill (queue-new) (list 1 2 3 4 5)) nil)
              (list 1 2 3 4 5)))

(check "queue: error on empty front is catchable"
       (equal (car (catch (queue-front (queue-new)) (lambda (e) e))) (quote error)))
