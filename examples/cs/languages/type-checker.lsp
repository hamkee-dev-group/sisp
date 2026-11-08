; Type checker covers literals, variables, arithmetic, conditionals, annotated lambdas, and applications.
; Function types are (function argument-type result-type).
(define (tc-lookup environment name)
  (cadr (find (lambda (binding) (equal (car binding) name)) environment)))
(define (tc-all-type-p expressions environment wanted)
  (foldl (lambda (valid expression) (and valid (equal (tc-type expression environment) wanted)))
    t expressions))
(define (tc-type expression environment)
  (cond ((numberp expression) 'number) ((or (eq expression t) (eq expression nil)) 'boolean)
        ((symbolp expression) (tc-lookup environment expression))
        ((in (car expression) '(+ - * /))
         (cond ((tc-all-type-p (cdr expression) environment 'number) 'number) (t 'type-error)))
        ((in (car expression) '(< > =))
         (cond ((tc-all-type-p (cdr expression) environment 'number) 'boolean) (t 'type-error)))
        ((eq (car expression) 'if)
         (let ((condition (tc-type (cadr expression) environment))
               (left (tc-type (caddr expression) environment))
               (right (tc-type (cadddr expression) environment)))
           (cond ((and (eq condition 'boolean) (equal left right)) left) (t 'type-error))))
        ((eq (car expression) 'lambda)
         (let ((parameter (caadr expression)) (parameter-type (cadadr expression)))
           (list 'function parameter-type
             (tc-type (caddr expression) (cons (list parameter parameter-type) environment)))))
        (t (let ((function-type (tc-type (car expression) environment))
                 (argument-type (tc-type (cadr expression) environment)))
          (cond ((and (listp function-type) (eq (car function-type) 'function)
                      (equal (cadr function-type) argument-type)) (caddr function-type))
                (t 'type-error))))))
(tc-type '(if (> x 0) (+ x 1) 0) '((x number))) ; => number
(tc-type '((lambda (x number) (+ x 1)) 4) nil) ; => number
(tc-type '(if 1 2 3) nil) ; => type-error
