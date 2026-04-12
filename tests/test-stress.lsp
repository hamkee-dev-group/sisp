; ============================================================
; SISP Test Suite — Stress Tests
; ============================================================

(print "=== STRESS TESTS ===")

; --- Large list creation ---
(define biglist (seq 1 500))
(if (equal (ord biglist) 500) (print "PASS: stress-seq-500") (print "FAIL: stress-seq-500"))
(if (equal (nth 1 biglist) 1) (print "PASS: stress-nth-first") (print "FAIL: stress-nth-first"))
(if (equal (nth 500 biglist) 500) (print "PASS: stress-nth-last") (print "FAIL: stress-nth-last"))
(if (equal (nth 250 biglist) 250) (print "PASS: stress-nth-mid") (print "FAIL: stress-nth-mid"))

; --- Deep recursion ---
(define (sum-to n) (if (equal n 0) 0 (+ n (sum-to (+ n -1)))))
(if (equal (sum-to 100) 5050) (print "PASS: stress-sum-100") (print "FAIL: stress-sum-100"))
(if (equal (sum-to 500) 125250) (print "PASS: stress-sum-500") (print "FAIL: stress-sum-500"))

; --- Fibonacci (deep tree recursion) ---
(define (fib n) (if (<= n 1) n (+ (fib (+ n -1)) (fib (+ n -2)))))
(if (equal (fib 15) 610) (print "PASS: stress-fib-15") (print "FAIL: stress-fib-15"))
(if (equal (fib 20) 6765) (print "PASS: stress-fib-20") (print "FAIL: stress-fib-20"))

; --- Large arithmetic ---
(if (equal (* 99999 99999) 9999800001) (print "PASS: stress-mul-large") (print "FAIL: stress-mul-large"))
(if (equal (+ 999999999 1) 1000000000) (print "PASS: stress-add-large") (print "FAIL: stress-add-large"))
(if (equal (^ 2 20) 1048576) (print "PASS: stress-pow-2-20") (print "FAIL: stress-pow-2-20"))
(if (equal (^ 2 30) 1073741824) (print "PASS: stress-pow-2-30") (print "FAIL: stress-pow-2-30"))

; --- Rational stress ---
(if (equal (/ 1 999999) 1/999999) (print "PASS: stress-rat-large-denom") (print "FAIL: stress-rat-large-denom"))
(if (equal (+ 1/7 1/11 1/13) 311/1001) (print "PASS: stress-rat-add-3") (print "FAIL: stress-rat-add-3"))
(if (equal (* 1/2 1/3 1/5 1/7) 1/210) (print "PASS: stress-rat-mul-4") (print "FAIL: stress-rat-mul-4"))

; --- Map over large list ---
(define (id x) x)
(define mapped (map id (seq 1 200)))
(if (equal (ord mapped) 200) (print "PASS: stress-map-200") (print "FAIL: stress-map-200"))
(if (equal (nth 1 mapped) 1) (print "PASS: stress-map-first") (print "FAIL: stress-map-first"))
(if (equal (nth 200 mapped) 200) (print "PASS: stress-map-last") (print "FAIL: stress-map-last"))

; --- Append large lists ---
(define big1 (seq 1 200))
(define big2 (seq 201 400))
(define combined (append big1 big2))
(if (equal (ord combined) 400) (print "PASS: stress-append-400") (print "FAIL: stress-append-400"))
(if (equal (nth 200 combined) 200) (print "PASS: stress-append-200th") (print "FAIL: stress-append-200th"))
(if (equal (nth 201 combined) 201) (print "PASS: stress-append-201st") (print "FAIL: stress-append-201st"))

; --- Set operations on larger sets ---
(define s1 {1 2 3 4 5 6 7 8 9 10})
(define s2 {6 7 8 9 10 11 12 13 14 15})
(if (equal (ord (union s1 s2)) 15) (print "PASS: stress-union-15") (print "FAIL: stress-union-15"))
(if (equal (ord (cap s1 s2)) 5) (print "PASS: stress-cap-5") (print "FAIL: stress-cap-5"))
(if (equal (ord (\ s1 s2)) 5) (print "PASS: stress-diff-5") (print "FAIL: stress-diff-5"))
(if (equal (ord (symdiff s1 s2)) 10) (print "PASS: stress-symdiff-10") (print "FAIL: stress-symdiff-10"))

; --- Power set of 4 elements (16 subsets) ---
(define ps4 (pow {1 2 3 4}))
(if (equal (ord ps4) 16) (print "PASS: stress-pow4-16") (print "FAIL: stress-pow4-16"))

; --- Nested let scoping ---
(if (equal (let ((a 1))
          (let ((b 2))
            (let ((c 3))
              (+ a (+ b c))))) 6)
    (print "PASS: stress-nested-let") (print "FAIL: stress-nested-let"))

; --- Labels with recursion ---
(if (equal (labels (((ack m n)
            (cond ((equal m 0) (+ n 1))
                  ((equal n 0) (ack (+ m -1) 1))
                  (t (ack (+ m -1) (ack m (+ n -1)))))))
        (ack 3 3)) 61)
    (print "PASS: stress-ackermann-3-3") (print "FAIL: stress-ackermann-3-3"))

; --- Many defines and undefs ---
(define t1 1) (define t2 2) (define t3 3) (define t4 4) (define t5 5)
(define t6 6) (define t7 7) (define t8 8) (define t9 9) (define t10 10)
(if (equal (+ t1 (+ t2 (+ t3 (+ t4 (+ t5 (+ t6 (+ t7 (+ t8 (+ t9 t10))))))))) 55)
    (print "PASS: stress-10-vars") (print "FAIL: stress-10-vars"))
(undef t1 t2 t3 t4 t5 t6 t7 t8 t9 t10)

; --- Deeply nested list ---
(define deep-list '(1 (2 (3 (4 (5))))))
(if (equal (car deep-list) 1) (print "PASS: stress-deep-car") (print "FAIL: stress-deep-car"))
(if (equal (car (car (cdr deep-list))) 2) (print "PASS: stress-deep-l2") (print "FAIL: stress-deep-l2"))

; --- String concatenation stress ---
(define s1s (cat "aaa" "bbb"))
(define s2s (cat s1s (cat "ccc" "ddd")))
(define s3s (cat s2s (cat "eee" "fff")))
(if (equal (strlen s3s) 18) (print "PASS: stress-cat-chain") (print "FAIL: stress-cat-chain"))
(if (equal s3s "aaabbbcccdddeeefff") (print "PASS: stress-cat-value") (print "FAIL: stress-cat-value"))

; --- GC trigger ---
(gc t)
(print "PASS: stress-gc-manual")

; --- eval of complex expression ---
(if (equal (eval '(+ (+ 1 2) (+ 3 4))) 10) (print "PASS: stress-eval-nested") (print "FAIL: stress-eval-nested"))

; cleanup
(undef biglist sum-to fib id mapped big1 big2 combined s1 s2 ps4 deep-list s1s s2s s3s)

(print "=== STRESS TESTS DONE ===")
