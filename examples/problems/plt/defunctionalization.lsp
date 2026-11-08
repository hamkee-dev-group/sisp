; Defunctionalization (Reynolds 1972): replace higher-order functions with data types.
; filter even with data type (Filter Even) instead of passing even function.

(define (df-even n) (= (mod n 2) 0))
(define (df-odd n) (= (mod n 2) 1))
(define (df-pos n) (> n 0))

; Functional version: filter with predicate function
(define (dfun f xs)
  (cond ((eq xs nil) nil)
        ((f (car xs)) (cons (car xs) (dfun f (cdr xs))))
        (t (dfun f (cdr xs)))))

; Data-based predicate interpretation
(define (df-apply-pred pred val)
  (cond ((eq pred 'is-even) (= (mod val 2) 0))
        ((eq pred 'is-odd) (= (mod val 2) 1))
        ((eq pred 'is-pos) (> val 0))
        ((eq pred 'is-zero) (= val 0))
        ((and (listp pred) (eq (car pred) 'and))
         (cond ((df-apply-pred (cadr pred) val) (df-apply-pred (caddr pred) val))
               (t nil)))
        ((and (listp pred) (eq (car pred) 'or))
         (or (df-apply-pred (cadr pred) val) (df-apply-pred (caddr pred) val)))
        ((and (listp pred) (eq (car pred) 'not)) (not (df-apply-pred (cadr pred) val)))
        (t nil)))

(define (df-apply-mapper mapper val)
  (cond ((eq mapper 'identity) val)
        ((eq mapper 'square) (* val val))
        ((eq mapper 'double) (* val 2))
        ((eq mapper 'negate) (- 0 val))
        (t val)))

; Defunctionalized filter
(define (df-filter xs pred)
  (cond ((eq xs nil) nil)
        ((df-apply-pred pred (car xs)) (cons (car xs) (df-filter (cdr xs) pred)))
        (t (df-filter (cdr xs) pred))))

; Defunctionalized map
(define (df-map xs mapper)
  (cond ((eq xs nil) nil)
        (t (cons (df-apply-mapper mapper (car xs)) (df-map (cdr xs) mapper)))))

(define df-data '(1 2 3 4 5 6 7 8 9 10))

; Equivalence: functional even = defunctionalized even
(define df-fun-even (dfun df-even df-data))
(define df-dat-even (df-filter df-data 'is-even))
(equal df-fun-even df-dat-even) t

; Equivalence: functional odd = defunctionalized odd
(define df-fun-odd (dfun df-odd df-data))
(define df-dat-odd (df-filter df-data 'is-odd))
(equal df-fun-odd df-dat-odd) t

; Composite predicate: (and is-even is-pos)
(define df-comp (df-filter df-data '(and is-even is-pos)))
(define df-comp-ref (dfun (lambda (x) (cond ((df-even x) (df-pos x)) (t nil))) df-data))
(equal df-comp df-comp-ref) t

; OR predicate
(define df-or (df-filter df-data '(or is-odd is-zero)))
(define df-or-ref (dfun (lambda (x) (or (df-odd x) (= x 0))) df-data))
(equal df-or df-or-ref) t

; Map equivalence
(define df-mdouble (df-map '(1 2 3) 'double))
(define df-mdouble-ref (map (lambda (x) (* x 2)) '(1 2 3)))
(equal df-mdouble df-mdouble-ref) t

(define df-msquare (df-map '(1 2 3 4) 'square))
(define df-msquare-ref (map (lambda (x) (* x x)) '(1 2 3 4)))
(equal df-msquare df-msquare-ref) t

; Edge cases
(equal (df-filter nil 'is-even) nil) t
(equal (df-filter '(2) 'is-even) '(2)) t
(equal (df-filter '(3) 'is-even) nil) t
(equal (df-filter '(0 1 2 3) 'is-zero) '(0)) t
(equal (df-filter '(-1 0 1) 'is-pos) '(1)) t
