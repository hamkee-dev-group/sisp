; Content-based recommendation compares an averaged preference profile with item vectors.
(define (cr-dot left right)
  (cond ((eq left nil) 0)
        (t (+ (* (car left) (car right)) (cr-dot (cdr left) (cdr right))))))
(define (cr-norm vector) (sqrt (cr-dot vector vector)))
(define (cr-cosine left right)
  (cond ((or (= (cr-norm left) 0) (= (cr-norm right) 0)) 0)
        (t (/ (cr-dot left right) (* (cr-norm left) (cr-norm right))))))
(define (cr-indexes count index)
  (cond ((= index count) nil) (t (cons index (cr-indexes count (+ index 1))))))
(define (cr-profile liked-vectors)
  (map (lambda (index) (/ (foldl (lambda (sum vector) (+ sum (nth index vector)))
                          0 liked-vectors) (length liked-vectors)))
    (cr-indexes (length (car liked-vectors)) 0)))
(define (content-rank items liked-vectors)
  (let ((profile (cr-profile liked-vectors)))
    (sort (lambda (a b) (> (cadr a) (cadr b)))
      (map (lambda (item) (list (car item) (cr-cosine profile (cadr item)))) items))))
(car (car (content-rank '((compiler (1 1 0)) (cooking (0 0 1)) (algebra (1 0 0))) '((1 1 0) (1 0 0))))) ; => compiler
(cr-profile '((1 0 1) (0 1 1))) ; => (1/2 1/2 1)


; ---- checks ----
(check "content-recommendation: compiler ranks first"
       (eq (car (car (content-rank '((compiler (1 1 0)) (cooking (0 0 1)) (algebra (1 0 0)))
                                   '((1 1 0) (1 0 0)))))
           'compiler))

(check "content-recommendation: profile averages liked vectors"
       (equal (cr-profile '((1 0 1) (0 1 1))) (list (/ 1 2) (/ 1 2) 1)))
