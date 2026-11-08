; ============================================================
; 80 - Linear Probing and Parking Functions (TAOCP Vol 3, 6.4)
; Cars choose preferred spaces and scan right, exactly like hash
; keys under linear probing. Exhaustive generation verifies the
; sorted criterion and the count (n+1)^(n-1) through n=5.
; ============================================================

(print "=== Linear Probing and Parking Functions ===")


(define (parking-range n)
  (cond ((= n 1) (list 1)) (t (seq 1 n))))

(define (parking-empty-table n)
  (map (lambda (ignored) nil) (parking-range n)))

(define (parking-set table index value)
  (cond ((= index 1) (cons value (cdr table)))
        (t (cons (car table) (parking-set (cdr table) (- index 1) value)))))

; Non-circular probing: a car fails if no space at or right of preference.
(define (parking-first-free table index)
  (cond ((> index (length table)) nil)
        ((eq (nth (- index 1) table) nil) index)
        (t (parking-first-free table (+ index 1)))))

(define (parking-run-walk preferences table car)
  (cond ((eq preferences nil) table)
        (t (let ((space (parking-first-free table (car preferences))))
          (cond ((eq space nil) nil)
                (t (parking-run-walk (cdr preferences)
                     (parking-set table space car) (+ car 1))))))))

(define (parking-run preferences)
  (parking-run-walk preferences (parking-empty-table (length preferences)) 1))

(define (parking-success-p preferences)
  (not (eq (parking-run preferences) nil)))

; A preference vector parks successfully iff its sorted values b_i obey b_i <= i.
(define (parking-criterion-walk sorted index)
  (cond ((eq sorted nil) t)
        ((<= (car sorted) index)
         (parking-criterion-walk (cdr sorted) (+ index 1)))
        (t nil)))

(define (parking-criterion-p preferences)
  (parking-criterion-walk (sort (lambda (left right) (< left right)) preferences) 1))

; All length-k words over 1..n, generated without deep-flattening structure.
(define (preference-vectors n k)
  (cond ((= k 0) (list nil))
        (t (foldl (lambda (vectors preference)
             (append vectors
               (map (lambda (tail) (cons preference tail))
                 (preference-vectors n (- k 1)))))
             nil (parking-range n)))))

(define (parking-functions n)
  (filter (lambda (preferences) (parking-success-p preferences))
    (preference-vectors n n)))

(define (parking-car-position table car index)
  (cond ((= (car table) car) index)
        (t (parking-car-position (cdr table) car (+ index 1)))))

(define (parking-displacement preferences)
  (let ((table (parking-run preferences)))
    (foldl (lambda (total car)
      (+ total (- (parking-car-position table car 1)
                  (nth (- car 1) preferences))))
      0 (parking-range (length preferences)))))

(define parking-example '(2 2 1 4))
(define parking-failure '(2 3 4 4))
(check "example-parks-as-3-1-2-4"
  (equal (parking-run parking-example) '(3 1 2 4)))
(check "blocked-tail-really-fails" (eq (parking-run parking-failure) nil))

(define vectors4 (preference-vectors 4 4))
(define functions4 (parking-functions 4))
(check "all-4-to-the-4-vectors-generated" (= (length vectors4) 256))
(check "sorted-criterion-is-equivalent-to-simulation"
  (every (lambda (preferences)
    (eq (parking-success-p preferences) (parking-criterion-p preferences)))
    vectors4))
(check "four-cars-have-125-parking-functions" (= (length functions4) 125))
(check "count-is-five-cubed" (= (length functions4) (^ 5 3)))

; The exact average extra distance over all 125 successful vectors.
(define displacement4
  (/ (foldl (lambda (total preferences)
       (+ total (parking-displacement preferences))) 0 functions4)
     (length functions4)))
(print (list 'average-displacement-four-cars displacement4))
(check "average-displacement-is-exact" (= displacement4 222/125))

; Exhaustively verify the classical count (n+1)^(n-1), not just n=4.
(define parking-counts
  (map (lambda (n) (length (parking-functions n))) '(1 2 3 4 5)))
(print (list 'parking-function-counts parking-counts))
(check "counts-through-five" (equal parking-counts '(1 3 16 125 1296)))
(check "counts-match-n-plus-one-to-n-minus-one"
  (every (lambda (n)
    (= (length (parking-functions n)) (^ (+ n 1) (- n 1)))) '(1 2 3 4 5)))

(print "=== Linear Probing and Parking Functions Done ===")
