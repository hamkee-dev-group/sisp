; Z-score detection flags observations far from the population mean.
(define (za-mean values)
  (/ (foldl (lambda (sum value) (+ sum value)) 0 values) (length values)))
(define (za-standard-deviation values)
  (let ((mean (za-mean values)))
    (sqrt (/ (foldl (lambda (sum value) (+ sum (^ (- value mean) 2))) 0 values)
             (length values)))))
(define (zscore value values)
  (/ (- value (za-mean values)) (za-standard-deviation values)))
(define (zscore-anomalies values threshold)
  (filter (lambda (value) (> (abs (zscore value values)) threshold)) values))
(zscore-anomalies '(10 10 11 9 10 50) 2) ; => (50)
(< (abs (zscore 10 '(9 10 11))) 0.000001) ; => t


; ---- checks ----
(check "zscore-anomaly: 50 is the outlier"
       (equal (zscore-anomalies '(10 10 11 9 10 50) 2) '(50)))

(check "zscore: the mean has zero score"
       (< (abs (zscore 10 '(9 10 11))) 0.000001))
