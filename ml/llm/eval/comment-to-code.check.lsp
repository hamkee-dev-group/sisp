(print (and (= (bench-clamp -2 0 10) 0) (= (bench-clamp 20 0 10) 10) (= (bench-clamp 4 0 10) 4)))
