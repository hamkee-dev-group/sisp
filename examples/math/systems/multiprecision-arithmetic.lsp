; ============================================================
; 81 - Multiple-Precision Arithmetic (TAOCP Vol 2, 4.3)
; Non-negative integers are little-endian decimal digit lists.
; Addition, subtraction, schoolbook multiplication, and long
; division operate on digits; native integers only verify results.
; ============================================================

(print "=== Multiple-Precision Arithmetic ===")


(define (big-drop-leading-zeroes msd)
  (cond ((eq (cdr msd) nil) msd)
        ((= (car msd) 0) (big-drop-leading-zeroes (cdr msd)))
        (t msd)))

(define (big-normalize digits)
  (cond ((eq digits nil) (list 0))
        (t (reverse (big-drop-leading-zeroes (reverse digits))))))

(define (big-from-integer n)
  (cond ((< n 10) (list n))
        (t (cons (mod n 10)
             (big-from-integer (truncate (/ n 10)))))))

(define (big-to-integer digits)
  (cond ((eq digits nil) 0)
        (t (+ (car digits) (* 10 (big-to-integer (cdr digits)))))))

(define (big-compare-msd left right)
  (cond ((eq left nil) 0)
        ((< (car left) (car right)) -1)
        ((> (car left) (car right)) 1)
        (t (big-compare-msd (cdr left) (cdr right)))))

(define (big-compare left right)
  (let ((a (big-normalize left)) (b (big-normalize right)))
    (cond ((< (length a) (length b)) -1)
          ((> (length a) (length b)) 1)
          (t (big-compare-msd (reverse a) (reverse b))))))

(define (big-add-carry left right carry)
  (cond ((and (eq left nil) (eq right nil))
         (cond ((= carry 0) nil) (t (list carry))))
        (t (let ((sum (+ carry
                         (cond ((eq left nil) 0) (t (car left)))
                         (cond ((eq right nil) 0) (t (car right))))))
          (cons (mod sum 10)
            (big-add-carry
              (cond ((eq left nil) nil) (t (cdr left)))
              (cond ((eq right nil) nil) (t (cdr right)))
              (truncate (/ sum 10))))))))

(define (big-add left right)
  (big-normalize (big-add-carry left right 0)))

; Precondition: left >= right.
(define (big-sub-borrow left right borrow)
  (cond ((eq left nil) nil)
        (t (let ((difference (- (car left) borrow
                              (cond ((eq right nil) 0) (t (car right))))))
          (cond ((< difference 0)
                 (cons (+ difference 10)
                   (big-sub-borrow (cdr left)
                     (cond ((eq right nil) nil) (t (cdr right))) 1)))
                (t (cons difference
                     (big-sub-borrow (cdr left)
                       (cond ((eq right nil) nil) (t (cdr right))) 0))))))))

(define (big-subtract left right)
  (big-normalize (big-sub-borrow left right 0)))

(define (big-multiply-digit digits digit carry)
  (cond ((eq digits nil)
         (cond ((= carry 0) nil) (t (big-from-integer carry))))
        (t (let ((product (+ (* (car digits) digit) carry)))
          (cons (mod product 10)
            (big-multiply-digit (cdr digits) digit
              (truncate (/ product 10))))))))

(define (big-shift digits places)
  (cond ((= places 0) digits)
        (t (big-shift (cons 0 digits) (- places 1)))))

(define (big-multiply-walk left right place product)
  (cond ((eq right nil) (big-normalize product))
        (t (big-multiply-walk left (cdr right) (+ place 1)
             (big-add product
               (big-shift (big-multiply-digit left (car right) 0) place))))))

(define (big-multiply left right)
  (big-multiply-walk left right 0 (list 0)))

; Largest decimal digit d such that divisor*d <= remainder.
(define (big-quotient-digit divisor remainder digit)
  (cond ((= digit 9) 9)
        ((<= (big-compare
               (big-multiply-digit divisor (+ digit 1) 0) remainder) 0)
         (big-quotient-digit divisor remainder (+ digit 1)))
        (t digit)))

(define (big-divide-walk msd-digits divisor remainder quotient-msd)
  (cond ((eq msd-digits nil)
         (list (big-normalize (reverse quotient-msd))
               (big-normalize remainder)))
        (t (let ((next-remainder
                   (big-normalize (cons (car msd-digits) remainder))))
          (let ((digit (big-quotient-digit divisor next-remainder 0)))
            (big-divide-walk (cdr msd-digits) divisor
              (big-subtract next-remainder
                (big-multiply-digit divisor digit 0))
              (append quotient-msd (list digit))))))))

(define (big-divide dividend divisor)
  (big-divide-walk (reverse (big-normalize dividend))
    (big-normalize divisor) (list 0) nil))

(define big-a (big-from-integer 12345))
(define big-b (big-from-integer 6789))
(define big-product (big-multiply big-a big-b))
(define big-division (big-divide big-product big-a))
(print (list 'product-digits-12345-by-6789 big-product))

(check "digit-round-trip"
  (every (lambda (n) (= (big-to-integer (big-from-integer n)) n))
    '(0 7 42 999 12345 987654321)))
(check "addition-with-long-carry"
  (= (big-to-integer
       (big-add (big-from-integer 999999) (big-from-integer 1))) 1000000))
(check "subtraction-with-long-borrow"
  (= (big-to-integer
       (big-subtract (big-from-integer 1000000) (big-from-integer 1))) 999999))
(check "schoolbook-product" (= (big-to-integer big-product) 83810205))
(check "long-division-quotient"
  (= (big-to-integer (car big-division)) 6789))
(check "long-division-remainder"
  (= (big-to-integer (cadr big-division)) 0))

; Exhaustively cross-check a finite battery against native exact arithmetic.
(define big-values '(1 2 7 11 37 99 123 456))
(define big-pairs
  (foldl (lambda (pairs a)
    (append pairs (map (lambda (b) (list a b)) big-values)))
    nil big-values))
(check "multiplication-battery"
  (every (lambda (pair)
    (= (big-to-integer
         (big-multiply (big-from-integer (car pair))
                       (big-from-integer (cadr pair))))
       (* (car pair) (cadr pair))))
    big-pairs))
(check "division-battery"
  (every (lambda (pair)
    (let ((division (big-divide (big-from-integer (car pair))
                                (big-from-integer (cadr pair)))))
      (and (= (big-to-integer (car division))
              (truncate (/ (car pair) (cadr pair))))
           (= (big-to-integer (cadr division))
              (mod (car pair) (cadr pair))))))
    big-pairs))

(print "=== Multiple-Precision Arithmetic Done ===")
