; ============================================================
; lib/collections/heap.lsp -- min-heap as breadth-first list
;
; Public:   (heapify lst) -> heap
;           (heap-insert h x) -> heap
;           (heap-extract-min h) -> (min new-heap)  ; 2-element list
;           (heapsort lst) -> sorted list
; Private:  heap-sift-up, heap-sift-down, heap-list-set,
;           heap-parent, heap-left, heap-right, heap-build,
;           heap-extract-all
; Depends:  none (uses prelude: nth, length, last, butlast)
; Derived-from: examples/math/sorting/heaps-heapsort.lsp
; Assumes:  elements are comparable with < and =
; ============================================================

(define (heap-parent i) (truncate (/ (- i 1) 2)))
(define (heap-left i) (+ (* 2 i) 1))
(define (heap-right i) (+ (* 2 i) 2))

(define (heap-list-set xs i v)
  (cond ((= i 0) (cons v (cdr xs)))
        (t (cons (car xs) (heap-list-set (cdr xs) (- i 1) v)))))

(define (heap-sift-up h i)
  (cond ((= i 0) h)
        (t (let ((p (heap-parent i)))
             (cond ((< (nth i h) (nth p h))
                    (let* ((vi (nth i h)) (vp (nth p h)))
                      (heap-sift-up
                        (heap-list-set (heap-list-set h p vi) i vp) p)))
                   (t h))))))

(define (heap-insert h x)
  (let ((n (length h)))
    (heap-sift-up (append h (list x)) n)))

(define (heap-sift-down h i n)
  (let ((l (heap-left i)) (r (heap-right i)))
    (cond ((>= l n) h)
          (t (let ((smallest
                    (cond ((or (>= r n) (<= (nth l h) (nth r h))) l)
                          (t r))))
               (cond ((< (nth smallest h) (nth i h))
                      (let* ((vi (nth i h)) (vs (nth smallest h)))
                        (heap-sift-down
                          (heap-list-set (heap-list-set h i vs) smallest vi)
                          smallest n)))
                     (t h)))))))

(define (heap-extract-min h)
  (cond ((eq h nil) (list nil nil))
        ((eq (cdr h) nil) (list (car h) nil))
        (t (let ((min-val (car h))
                 (last-val (last h))
                 (h2 (butlast h)))
             (let ((h3 (heap-list-set h2 0 last-val)))
               (list min-val (heap-sift-down h3 0 (length h3))))))))

(define (heapify lst)
  (let ((n (length lst)))
    (heap-build lst (- (truncate (/ n 2)) 1))))

(define (heap-build h i)
  (cond ((< i 0) h)
        (t (heap-build (heap-sift-down h i (length h)) (- i 1)))))

(define (heapsort lst)
  (heap-extract-all (heapify lst) nil))

(define (heap-extract-all h acc)
  (cond ((eq h nil) (reverse acc))
        (t (let ((r (heap-extract-min h)))
             (heap-extract-all (cadr r) (cons (car r) acc))))))
