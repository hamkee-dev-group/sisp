; A definition macro generates total functions with explicit precondition failure values.
; The condition and body remain ordinary SISP forms with normal parameter scope.
(define (contract-definition name parameters condition body)
  (list 'define (cons name parameters)
    (list 'cond
      (list condition body)
      (list t (list 'list
        (list 'quote 'contract-error)
        (list 'quote name))))))

(defmacro (define-checked name parameters condition body)
  (contract-definition name parameters condition body))

(define-checked checked-divide (numerator denominator)
  (/= denominator 0)
  (/ numerator denominator))

(define-checked checked-withdraw (balance amount)
  (and (>= amount 0) (<= amount balance))
  (- balance amount))

(define-checked checked-index (values index)
  (and (>= index 0) (< index (length values)))
  (nth index values))

(list (checked-divide 7 2) (checked-divide 7 0)) ; => (7/2 (contract-error checked-divide))
(list (checked-withdraw 100 35) (checked-withdraw 20 25)) ; => (65 (contract-error checked-withdraw))
(list (checked-index '(a b c) 1) (checked-index '(a b c) 5)) ; => (b (contract-error checked-index))
