(define bench-common-result (bench-common {1 2 3} {2 3 4}))
(print (and (= (length bench-common-result) 2) (in 2 bench-common-result) (in 3 bench-common-result)))
