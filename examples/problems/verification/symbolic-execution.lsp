; Symbolic execution: execute with symbolic inputs, accumulate path conditions.

(define (se-sym nm) (list 'sym nm))
(define (se-symp v) (cond ((listp v) (eq (car v) 'sym)) (t nil)))

(define (se-add a b)
  (cond ((and (numberp a) (numberp b)) (+ a b))
        ((cond ((numberp a) (= a 0)) (t nil)) b)
        ((cond ((numberp b) (= b 0)) (t nil)) a)
        (t (list '+ a b))))

(define (se-sub a b)
  (cond ((and (numberp a) (numberp b)) (- a b))
        ((cond ((numberp b) (= b 0)) (t nil)) a)
        (t (list '- a b))))

(define (se-mul a b)
  (cond ((and (numberp a) (numberp b)) (* a b))
        ((cond ((numberp a) (= a 0)) (t nil)) 0)
        ((cond ((numberp b) (= b 0)) (t nil)) 0)
        ((cond ((numberp a) (= a 1)) (t nil)) b)
        ((cond ((numberp b) (= b 1)) (t nil)) a)
        (t (list '* a b))))

(define (se-gt a b)
  (cond ((and (numberp a) (numberp b)) (cond ((> a b) t) (t nil)))
        (t (list '> a b))))

(define (se-lt a b)
  (cond ((and (numberp a) (numberp b)) (cond ((< a b) t) (t nil)))
        (t (list '< a b))))

(define (se-eqq a b)
  (cond ((and (numberp a) (numberp b)) (cond ((= a b) t) (t nil)))
        (t (list '= a b))))

(define (se-lk var env)
  (let ((e (find (lambda (b) (eq (car b) var)) env))) (cond (e (cdr e)) (t var))))

(define (se-ex var val env) (cons (cons var val) env))

(define (se-ev x env)
  (cond ((numberp x) x)
        ((symbolp x) (se-lk x env))
        ((eq (car x) '+) (se-add (se-ev (cadr x) env) (se-ev (caddr x) env)))
        ((eq (car x) '-) (se-sub (se-ev (cadr x) env) (se-ev (caddr x) env)))
        ((eq (car x) '*) (se-mul (se-ev (cadr x) env) (se-ev (caddr x) env)))
        ((eq (car x) '>) (se-gt (se-ev (cadr x) env) (se-ev (caddr x) env)))
        ((eq (car x) '<) (se-lt (se-ev (cadr x) env) (se-ev (caddr x) env)))
        ((eq (car x) '=) (se-eqq (se-ev (cadr x) env) (se-ev (caddr x) env)))
        (t x)))

(define (se-copy env) (map (lambda (b) (cons (car b) (cdr b))) env))

(define (se-exec stmt pc env)
  (cond ((and (listp stmt) (eq (car stmt) 'setq))
         (let* ((lhs (cadr stmt))
                (rhs (se-ev (caddr stmt) env))
                (new-env (se-ex lhs rhs env)))
           (list (list pc new-env))))
        ((and (listp stmt) (eq (car stmt) 'if))
         (let* ((cv (se-ev (cadr stmt) env))
                (taken (not (eq cv nil)))
                (branch (cond (taken (caddr stmt)) (t (cadddr stmt))))
                (new-pc (cons (cond (taken cv) (t (list 'not cv))) pc)))
           (se-exec branch new-pc (se-copy env))))
        (t (list (list pc env)))))

(define (se-mkenv inputs)
  (cond ((eq inputs nil) nil)
        (t (cons (cons (caar inputs) (list 'sym (caar inputs)))
                 (se-mkenv (cdr inputs))))))

(define (symbolic-execute prog inputs)
  (let ((env (se-mkenv inputs))) (se-exec prog nil env)))

; Test: branch on x > 0
(define se-p1 (symbolic-execute '(if (> x 0) (setq y x) (setq y (- 0 x))) '((x))))
(equal (length se-p1) 1) t

; Test: setq with arithmetic on symbolic value
(define se-p2 (symbolic-execute '(setq y (+ x 2)) '((x))))
(equal (length se-p2) 1) t

; Nested branching
(define se-p3 (symbolic-execute '(if (> x 0) (if (> x 10) (setq y big) (setq y small)) (setq y neg)) '((x))))
(equal (length se-p3) 1) t

; Path condition inspection
(define se-pc (car (car se-p1)))
(equal (eq se-pc nil) nil) t
