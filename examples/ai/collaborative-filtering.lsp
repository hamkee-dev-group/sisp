; User-based collaborative filtering predicts unseen ratings from correlated neighbours.
(define (cf-user ratings name)
  (cadr (find (lambda (entry) (equal (car entry) name)) ratings)))
(define (cf-rating user-ratings item)
  (let ((entry (find (lambda (pair) (equal (car pair) item)) user-ratings)))
    (cond ((eq entry nil) nil) (t (cadr entry)))))
(define (cf-mean user-ratings)
  (/ (foldl (lambda (sum entry) (+ sum (cadr entry))) 0 user-ratings)
     (length user-ratings)))
(define (cf-shared left right)
  (filter (lambda (entry) (not (eq (cf-rating right (car entry)) nil))) left))
; Pearson similarity uses only items rated by both users.
(define (cf-pearson left right)
  (let ((shared (cf-shared left right)))
    (cond ((< (length shared) 2) 0)
          (t (let ((left-mean (/ (foldl (lambda (sum entry) (+ sum (cadr entry))) 0 shared)
                                  (length shared)))
                   (right-mean (/ (foldl (lambda (sum entry)
                     (+ sum (cf-rating right (car entry)))) 0 shared) (length shared))))
            (let ((numerator (foldl (lambda (sum entry)
                    (+ sum (* (- (cadr entry) left-mean)
                              (- (cf-rating right (car entry)) right-mean)))) 0 shared))
                  (left-square (foldl (lambda (sum entry)
                    (+ sum (^ (- (cadr entry) left-mean) 2))) 0 shared))
                  (right-square (foldl (lambda (sum entry)
                    (+ sum (^ (- (cf-rating right (car entry)) right-mean) 2))) 0 shared)))
              (cond ((or (= left-square 0) (= right-square 0)) 0)
                    (t (/ numerator (sqrt (* left-square right-square)))))))))))
; Neighbour deviations are weighted by signed similarity around the target mean.
(define (cf-predict ratings user item)
  (let ((target (cf-user ratings user)))
    (let ((weighted (foldl (lambda (state neighbour)
      (let ((other (cadr neighbour)))
        (let ((rating (cf-rating other item)) (similarity (cf-pearson target other)))
          (cond ((or (eq rating nil) (= similarity 0)) state)
                (t (list (+ (car state) (* similarity (- rating (cf-mean other))))
                         (+ (cadr state) (abs similarity))))))))
      '(0 0) (remove (lambda (entry) (equal (car entry) user)) ratings))))
      (+ (cf-mean target) (/ (car weighted) (cadr weighted))))))
(define cf-ratings '((ann ((a 5) (b 1))) (bob ((a 4) (b 1) (c 5))) (cara ((a 1) (b 5) (c 1)))))
(> (cf-predict cf-ratings 'ann 'c) 4) ; => t
(< (abs (- (cf-pearson (cf-user cf-ratings 'ann) (cf-user cf-ratings 'cara)) -1)) 0.000001) ; => t

; ---- checks ----
(check "collaborative-filtering: prediction for ann on c > 4"
       (> (cf-predict cf-ratings 'ann 'c) 4))

(check "collaborative-filtering: ann/cara pearson = -1"
       (< (abs (- (cf-pearson (cf-user cf-ratings 'ann) (cf-user cf-ratings 'cara)) -1))
          0.000001))
