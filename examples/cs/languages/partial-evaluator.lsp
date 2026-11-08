; A partial evaluator treats SISP forms as data, substitutes known bindings, and folds safe constants.
; Residual expressions remain ordinary SISP and can later be completed with more bindings.
(define (pe-binding symbol environment)
  (assoc symbol environment))

(define (pe-bound-p symbol environment)
  (not (eq (pe-binding symbol environment) nil)))

(define (pe-operator-p symbol)
  (in symbol '(+ - * / ^ mod min max = /= < <= > >= and or not)))

(define (pe-all-values-p expressions)
  (every (lambda (expression)
    (or (numberp expression) (eq expression t) (eq expression nil))) expressions))

(define (pe-specialize-list expressions environment)
  (map (lambda (expression) (pe-specialize expression environment)) expressions))

(define (pe-specialize-if expression environment)
  (let ((test (pe-specialize (cadr expression) environment)))
    (cond ((eq test t) (pe-specialize (caddr expression) environment))
          ((eq test nil) (pe-specialize (cadddr expression) environment))
          (t (list 'if test
               (pe-specialize (caddr expression) environment)
               (pe-specialize (cadddr expression) environment))))))

(define (pe-specialize expression environment)
  (cond ((symbolp expression)
         (cond ((pe-bound-p expression environment)
                (cadr (pe-binding expression environment)))
               (t expression)))
        ((atom expression) expression)
        ((eq (car expression) 'quote) expression)
        ((eq (car expression) 'if) (pe-specialize-if expression environment))
        (t (let ((arguments (pe-specialize-list (cdr expression) environment)))
          (cond ((and (pe-operator-p (car expression))
                      (pe-all-values-p arguments))
                 (eval (cons (car expression) arguments)))
                (t (cons (car expression) arguments)))))))

(define (pe-run expression environment)
  (eval (pe-specialize expression environment)))

(define pe-price-form
  '(if member (+ (* quantity unit-price) shipping) (+ (* quantity unit-price) 5)))
(define pe-residual
  (pe-specialize pe-price-form '((member t) (unit-price 3/2) (shipping 2))))

(list pe-residual) ; => ((+ (* quantity 3/2) 2))
(pe-run pe-residual '((quantity 4))) ; => 8
(pe-specialize '(+ (* x 0) (* 3 4) (- 9 2)) nil) ; => (+ (* x 0) 12 7)
