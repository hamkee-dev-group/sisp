(print (and (= (bench-find '(2 4 6 8 10) 8) 3) (eq (bench-find '(2 4 6) 5) nil)))
