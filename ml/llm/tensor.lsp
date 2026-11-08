; tensor.lsp — shape-checked list vectors/matrices for scalar Values.

(define (llm-map2 f a b)
  (cond ((and (eq a nil) (eq b nil)) nil)
        ((or (eq a nil) (eq b nil)) (error "vector length mismatch"))
        (t (cons (f (car a) (car b))
                 (llm-map2 f (cdr a) (cdr b))))))

(define (llm-vector-add a b) (llm-map2 g+ a b))
(define (llm-vector-sub a b) (llm-map2 g- a b))
(define (llm-vector-tanh a) (map gtanh a))
(define (llm-vector-relu a) (map grelu a))
(define (llm-vector-scale a scalar-value)
  (map (lambda (x) (g* x scalar-value)) a))
(define (llm-vector-scale-number a scalar)
  (llm-vector-scale a (val scalar)))
(define (llm-vector-data a) (map vdata a))

(define (llm-scale-vectors weights vectors)
  (cond ((and (eq weights nil) (eq vectors nil)) nil)
        ((or (eq weights nil) (eq vectors nil)) (error "vector length mismatch"))
        (t (cons (llm-vector-scale (car vectors) (car weights))
                 (llm-scale-vectors (cdr weights) (cdr vectors))))))

(define (llm-vector-sum-pairs vectors)
  (cond ((eq vectors nil) nil)
        ((eq (cdr vectors) nil) vectors)
        (t (cons (llm-vector-add (car vectors) (cadr vectors))
                 (llm-vector-sum-pairs (cddr vectors))))))

(define (llm-sum-vectors vectors)
  (cond ((eq vectors nil) nil)
        ((eq (cdr vectors) nil) (car vectors))
        (t (llm-sum-vectors (llm-vector-sum-pairs vectors)))))

(define (llm-weighted-vector-sum weights vectors)
  (llm-sum-vectors (llm-scale-vectors weights vectors)))

(define (llm-flatten rows)
  (cond ((eq rows nil) nil)
        (t (append (car rows) (llm-flatten (cdr rows))))))

(define (llm-make-param-vector n scale)
  (repeat-call n (lambda () (randw-scale scale))))
(define (llm-make-zero-vector n)
  (repeat-call n (lambda () (val 0.0))))
(define (llm-make-param-matrix rows cols scale)
  (repeat-call rows (lambda () (llm-make-param-vector cols scale))))

(define (llm-last xs)
  (cond ((eq (cdr xs) nil) (car xs))
        (t (llm-last (cdr xs)))))

(define (llm-enumerate-map f xs index)
  (cond ((eq xs nil) nil)
        (t (cons (f (car xs) index)
                 (llm-enumerate-map f (cdr xs) (+ index 1))))))

; Plain-number matrix helpers retained for forward-only demonstrations.
(define (llm-dot-numbers a b)
  (cond ((and (eq a nil) (eq b nil)) 0.0)
        ((or (eq a nil) (eq b nil)) (error "dot product length mismatch"))
        (t (+ (* (car a) (car b))
              (llm-dot-numbers (cdr a) (cdr b))))))

(define (llm-transpose matrix)
  (cond ((eq matrix nil) nil)
        ((eq (car matrix) nil) nil)
        (t (cons (map car matrix)
                 (llm-transpose (map cdr matrix))))))

(define (llm-matmul-numbers a b)
  (let ((bt (llm-transpose b)))
    (map (lambda (row)
      (map (lambda (column) (llm-dot-numbers row column)) bt)) a)))
