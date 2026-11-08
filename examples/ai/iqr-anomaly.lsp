; Tukey fences use the interquartile range without assuming a normal distribution.
(define (iq-median sorted)
  (let ((count (length sorted)))
    (cond ((oddp count) (nth (truncate (/ count 2)) sorted))
          (t (/ (+ (nth (- (/ count 2) 1) sorted) (nth (/ count 2) sorted)) 2)))))
(define (iq-prefix values count)
  (cond ((= count 0) nil) (t (cons (car values) (iq-prefix (cdr values) (- count 1))))))
(define (iq-suffix values count)
  (cond ((= count 0) values) (t (iq-suffix (cdr values) (- count 1)))))
(define (iq-quartiles values)
  (let ((sorted (sort (lambda (a b) (< a b)) values)))
    (let ((half (truncate (/ (length sorted) 2))))
      (list (iq-median (iq-prefix sorted half))
            (iq-median (iq-suffix sorted (- (length sorted) half)))))))
(define (iqr-anomalies values multiplier)
  (let ((quartiles (iq-quartiles values)))
    (let ((spread (- (cadr quartiles) (car quartiles))))
      (filter (lambda (value)
        (or (< value (- (car quartiles) (* multiplier spread)))
            (> value (+ (cadr quartiles) (* multiplier spread))))) values))))
(iq-quartiles '(1 2 3 4 5 6 7 8)) ; => (5/2 13/2)
(iqr-anomalies '(1 2 2 3 3 4 100 3) 3/2) ; => (100)


; ---- checks ----
(check "iqr: quartiles of 1..8 are (5/2 13/2)"
       (equal (iq-quartiles '(1 2 3 4 5 6 7 8)) (list (/ 5 2) (/ 13 2))))

(check "iqr-anomaly: 100 is the only outlier"
       (equal (iqr-anomalies '(1 2 2 3 3 4 100 3) (/ 3 2)) '(100)))
