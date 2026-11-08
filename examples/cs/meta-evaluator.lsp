; A tiny lexical evaluator for arithmetic, conditionals, and lambdas.

; environments are association lists; closures capture their defining environment
(define (mini-lookup name environment)
  (cond ((eq environment nil) nil)
        ((equal name (caar environment)) (cadar environment))
        (t (mini-lookup name (cdr environment)))))
(define (mini-bind names values environment)
  (cond ((eq names nil) environment)
        (t (cons (list (car names) (car values))
             (mini-bind (cdr names) (cdr values) environment)))))
(mini-lookup 'x '((x 4) (y 7))) ; => 4
(mini-bind '(x y) '(2 3) nil)   ; => ((x 2) (y 3))

; evaluate argument lists and apply supported binary primitives or closures
; primitive names remain symbols rather than first-class SISP builtins
(define (mini-eval-list expressions environment)
  (cond ((eq expressions nil) nil)
        (t (cons (mini-eval (car expressions) environment)
             (mini-eval-list (cdr expressions) environment)))))
(define (mini-primitive-p name)
  (in name '(+ - * / = < > cons)))
(define (mini-primitive-apply name values)
  (cond ((equal name '+) (+ (car values) (cadr values)))
        ((equal name '-) (- (car values) (cadr values)))
        ((equal name '*) (* (car values) (cadr values)))
        ((equal name '/) (/ (car values) (cadr values)))
        ((equal name '=) (= (car values) (cadr values)))
        ((equal name '<) (< (car values) (cadr values)))
        ((equal name '>) (> (car values) (cadr values)))
        (t (cons (car values) (cadr values)))))
(define (mini-apply procedure values)
  ; a closure stores parameters, body, and its lexical environment
  (cond ((symbolp procedure) (mini-primitive-apply procedure values))
        (t (mini-eval (caddr procedure)
             (mini-bind (cadr procedure) values (cadddr procedure))))))

; dispatch self-evaluating values, special forms, and applications
(define (mini-eval expression environment)
  (cond ((numberp expression) expression)
        ((symbolp expression) (mini-lookup expression environment))
        ((equal (car expression) 'quote) (cadr expression))
        ((equal (car expression) 'if)
         (cond ((mini-eval (cadr expression) environment)
                (mini-eval (caddr expression) environment))
               (t (mini-eval (cadddr expression) environment))))
        ((equal (car expression) 'lambda)
         (list 'closure (cadr expression) (caddr expression) environment))
        (t (mini-apply
          (cond ((mini-primitive-p (car expression)) (car expression))
                (t (mini-eval (car expression) environment)))
          (mini-eval-list (cdr expression) environment)))))
(mini-eval '(+ x (* y 2)) '((x 3) (y 4))) ; => 11
(mini-eval '(if (> x 0) (+ x 1) 0) '((x 4))) ; => 5
(mini-eval '((lambda (x) (* x x)) 6) nil) ; => 36
