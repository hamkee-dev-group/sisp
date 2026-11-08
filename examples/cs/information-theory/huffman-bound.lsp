; Huffman coding meets entropy.
; Tree: leaf = (symbol . weight), internal = ((left-subtree . right-subtree) . weight)
; Internal node's car is (left . right) — both subtrees in node form.

(define (entropy dist)
  (- 0 (foldl + 0 (map (lambda (p) (cond ((= p 0) 0) (t (* p (log2 p))))) dist))))

(define (hb-leaf-p node)
  (atom (car node)))

(define (hb-insert node nodes)
  (cond ((null nodes) (list node))
        ((< (cdr node) (cdar nodes)) (cons node nodes))
        (t (cons (car nodes) (hb-insert node (cdr nodes))))))

(define (hb-build queue)
  (cond ((null (cdr queue)) (car queue))
        (t (let ((a (car queue)) (b (cadr queue)))
          (hb-build (hb-insert
            (cons (cons a b) (+ (cdr a) (cdr b)))
            (cddr queue)))))))

(define (huffman-tree freqs)
  (hb-build
    (foldl (lambda (q p) (hb-insert (cons (car p) (cadr p)) q))
      nil
      (sort (lambda (x y) (< (cadr x) (cadr y))) freqs))))

(define (cl-walk tree depth)
  (cond ((hb-leaf-p tree) (list (list (car tree) depth)))
        (t (let ((pair (car tree)))
             (append (cl-walk (car pair) (+ depth 1))
                     (cl-walk (cdr pair) (+ depth 1)))))))

(define (expected-length freqs)
  (let* ((total (foldl + 0 (map cadr freqs)))
         (tree (huffman-tree freqs))
         (lengths (cl-walk tree 0)))
    (/ (foldl (lambda (sum entry)
         (+ sum (* (cadr entry)
                   (cadr (find (lambda (l) (equal (car l) (car entry))) lengths)))))
       0 freqs) total)))

(define (huffman-verify freqs)
  (let* ((total (foldl + 0 (map cadr freqs)))
         (probs (map (lambda (p) (/ (cadr p) total)) freqs))
         (H (entropy probs))
         (L (expected-length freqs)))
    (list 'H H 'L L 'bound (and (<= H L) (< L (+ H 1))))))

; -- Tests on multiple distributions --

; Uniform: p = (1/4, 1/4, 1/4, 1/4), H = 2, L should be 2
(print (huffman-verify '((a 1) (b 1) (c 1) (d 1))))

; Skewed: p = (1/2, 1/4, 1/8, 1/8), H = 1.75
(print (huffman-verify '((a 4) (b 2) (c 1) (d 1))))

; Two symbols: p = (0.9, 0.1), H ≈ 0.469, L = 1
(print (huffman-verify '((x 9) (y 1))))

; Three symbols: p = (0.6, 0.3, 0.1)
(print (huffman-verify '((p 6) (q 3) (r 1))))

; Single symbol: H = 0, L = 0
(print (huffman-verify '((x 1))))

; Five symbols: uniform
(print (huffman-verify '((a 1) (b 1) (c 1) (d 1) (e 1))))
