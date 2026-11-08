; Concolic (concrete+symbolic) testing.
; Concrete (x=1,y=2) + symbolic tracks same inputs.
; Execute, collect path conditions. Negate last condition -> new inputs.

(define (cc-ev expr env syms)
  (cond ((numberp expr) (list expr expr))
        ((symbolp expr)
         (let ((cv (let ((e (find (lambda (b) (eq (car b) expr)) env))) (cond (e (cdr e)) (t 0))))
               (sv (let ((e (find (lambda (b) (eq (car b) expr)) syms))) (cond (e (cdr e)) (t (list 's expr))))))
           (list cv sv)))
        ((eq (car expr) '+)
         (let ((a (cc-ev (cadr expr) env syms)) (b (cc-ev (caddr expr) env syms)))
           (list (+ (car a) (car b)) (list '+ (cadr a) (cadr b)))))
        ((eq (car expr) '-)
         (let ((a (cc-ev (cadr expr) env syms)) (b (cc-ev (caddr expr) env syms)))
           (list (- (car a) (car b)) (list '- (cadr a) (cadr b)))))
        ((eq (car expr) '*)
         (let ((a (cc-ev (cadr expr) env syms)) (b (cc-ev (caddr expr) env syms)))
           (list (* (car a) (car b)) (list '* (cadr a) (cadr b)))))
        ((eq (car expr) '>)
         (let ((a (cc-ev (cadr expr) env syms)) (b (cc-ev (caddr expr) env syms)))
           (list (cond ((> (car a) (car b)) 'true) (t 'nil))
                 (list '> (cadr a) (cadr b)))))
        ((eq (car expr) '<)
         (let ((a (cc-ev (cadr expr) env syms)) (b (cc-ev (caddr expr) env syms)))
           (list (cond ((< (car a) (car b)) 'true) (t 'nil))
                 (list '< (cadr a) (cadr b)))))
        (t (list expr expr))))

(define (cc-copy env) (map (lambda (b) (cons (car b) (cdr b))) env))
(define (cc-ex var val env) (cons (cons var val) env))

(define (cc-exec stmt env syms pc)
  (cond ((and (listp stmt) (eq (car stmt) 'setq))
         (let* ((lhs (cadr stmt))
                (val (cc-ev (caddr stmt) env syms))
                (new-env (cc-ex lhs (car val) env))
                (new-syms (cc-ex lhs (cadr val) syms)))
           (list (list pc new-env new-syms))))
        ((and (listp stmt) (eq (car stmt) 'if))
         (let* ((cv (cc-ev (cadr stmt) env syms))
                (taken (not (eq (car cv) 'nil)))
                (branch (cond (taken (caddr stmt)) (t (cadddr stmt))))
                (new-pc (cons (list (cond (taken 't) (t 'f)) (cadr cv)) pc)))
           (cc-exec branch new-pc (cc-copy env) (cc-copy syms))))
        (t (list (list pc env syms)))))

(define (cc-mkenv inputs)
  (cond ((eq inputs nil) '((nil . nil) nil))
        (t (let ((rest (cc-mkenv (cdr inputs))))
             (list (cc-ex (caar inputs) (cdar inputs) (car rest))
                   (cc-ex (caar inputs) (list 's (caar inputs)) (cadr rest)))))))

(define (concolic-execute prog input-pairs)
  (let* ((env (map (lambda (p) (cons (car p) (cdr p))) input-pairs))
         (sym-env (map (lambda (p) (cons (car p) (list 's (car p)))) input-pairs))
         (paths (cc-exec prog env sym-env nil)))
    paths))

(define (cc-constraints path) (car path))
(define (cc-concrete path) (cadr path))
(define (cc-symbolic path) (caddr path))

; Demonstrate on branching function: if x > 0 setq y = x else setq y = (- 0 x)
(define cc-prog '(if (> x 0) (setq y x) (setq y (- 0 x))))
(define cc-inputs '((x . 1) (y . 0)))
(define cc-paths (concolic-execute cc-prog cc-inputs))
(equal (length cc-paths) 1) t

; Concrete result: y should be 1
(define cc-y (cdr (find (lambda (b) (eq (car b) 'y)) (cc-concrete (car cc-paths)))))
(equal cc-y 1) t

; Run with x = -1 (should take else branch)
(define cc-inputs2 '((x . -1) (y . 0)))
(define cc-paths2 (concolic-execute cc-prog cc-inputs2))
(define cc-y2 (cdr (find (lambda (b) (eq (car b) 'y)) (cc-concrete (car cc-paths2)))))
(equal cc-y2 1) t

; Both y values are positive (abs(x))
(equal cc-y cc-y2) t
