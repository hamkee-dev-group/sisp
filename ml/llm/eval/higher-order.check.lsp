(define bench-double-after-inc (bench-compose (lambda (x) (* x 2)) (lambda (x) (+ x 1))))
(print (= (bench-double-after-inc 4) 10))
