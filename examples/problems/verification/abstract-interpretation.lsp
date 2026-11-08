; Abstract interpretation over the sign domain {bottom, neg, zero, pos, top}.
; Galois connection: alpha: Z -> Sign, gamma: Sign -> P(Z).
; Abstract transformer for +: alpha+(a,b).
; Widening for while loops.

(define (ai-abs n)
  (cond ((< n 0) 'neg) ((= n 0) 'zero) ((> n 0) 'pos)))

(define (ai-gamma s)
  (cond ((eq s 'bottom) {})
        ((eq s 'neg) {tau : (< tau 0)})
        ((eq s 'zero) {tau : (= tau 0)})
        ((eq s 'pos) {tau : (> tau 0)})
        ((eq s 'top) (union (ai-gamma 'neg) (union (ai-gamma 'zero) (ai-gamma 'pos))))))

(define (ai-leq a b)
  (cond ((eq a 'bottom) t)
        ((eq b 'top) t)
        ((eq a b) t)
        (t nil)))

(define (ai-join a b)
  (cond ((eq a b) a)
        ((eq a 'bottom) b)
        ((eq b 'bottom) a)
        (t 'top)))

(define (ai-meet a b)
  (cond ((eq a b) a)
        ((or (eq a 'bottom) (eq b 'bottom)) 'bottom)
        ((eq a 'top) b)
        ((eq b 'top) a)
        (t 'bottom)))

(define (ai-abstract-plus a b)
  (cond ((and (eq a 'pos) (eq b 'pos)) 'pos)
        ((and (eq a 'neg) (eq b 'neg)) 'neg)
        ((and (eq a 'zero) (eq b 'zero)) 'zero)
        ((or (eq a 'zero) (eq b 'zero)) (cond ((eq a 'zero) b) (t a)))
        (t 'top)))

; 0 - b = -b
(define (ai-abstract-minus a b)
  (cond ((eq a 'zero) (cond ((eq b 'pos) 'neg)
                            ((eq b 'neg) 'pos)
                            ((eq b 'zero) 'zero)
                            (t 'top)))
        ((eq b 'zero) a)
        ((and (eq a 'pos) (eq b 'neg)) 'pos)
        ((and (eq a 'neg) (eq b 'pos)) 'neg)
        (t 'top)))

(define (ai-abstract-mul a b)
  (cond ((or (eq a 'zero) (eq b 'zero)) 'zero)
        ((and (eq a 'pos) (eq b 'pos)) 'pos)
        ((and (eq a 'neg) (eq b 'neg)) 'pos)
        ((and (eq a 'pos) (eq b 'neg)) 'neg)
        ((and (eq a 'neg) (eq b 'pos)) 'neg)
        (t 'top)))

(define (ai-sign->string s)
  (cond ((eq s 'bottom) "bottom") ((eq s 'neg) "neg")
        ((eq s 'zero) "zero") ((eq s 'pos) "pos") ((eq s 'top) "top")))

(define (ai-abstract-eval expr env)
  (cond ((numberp expr) (ai-abs expr))
        ((symbolp expr)
         (let ((entry (find (lambda (b) (eq (car b) expr)) env)))
           (cond (entry (cadr entry)) (t 'bottom))))
        ((eq (car expr) '+)
         (ai-abstract-plus (ai-abstract-eval (cadr expr) env)
                           (ai-abstract-eval (caddr expr) env)))
        ((eq (car expr) '-)
         (ai-abstract-minus (ai-abstract-eval (cadr expr) env)
                            (ai-abstract-eval (caddr expr) env)))
        ((eq (car expr) '*)
         (ai-abstract-mul (ai-abstract-eval (cadr expr) env)
                          (ai-abstract-eval (caddr expr) env)))
        ((eq (car expr) '/)
         (ai-abstract-mul (ai-abstract-eval (cadr expr) env)
                          (ai-abstract-eval (caddr expr) env)))
        (t 'top)))

(define (ai-widen old new)
  (cond ((eq old 'bottom) new)
        ((ai-leq new old) old)
        (t 'top)))

(define (ai-while-fp start-body iterations)
  (ai-iterate start-body start-body iterations))

(define (ai-iterate state-env body-env remaining)
  (cond ((= remaining 0) body-env)
        (t (let ((next (ai-abstract-eval body-env state-env)))
             (cond ((equal next state-env) state-env)
                   (t (ai-iterate next body-env (- remaining 1))))))))

; Tests for abstract sign domain
(equal (ai-abs -5) 'neg) t
(equal (ai-abs 0) 'zero) t
(equal (ai-abs 42) 'pos) t

; Abstract addition
(equal (ai-abstract-plus 'pos 'pos) 'pos) t
(equal (ai-abstract-plus 'neg 'neg) 'neg) t
(equal (ai-abstract-plus 'zero 'zero) 'zero) t
(equal (ai-abstract-plus 'pos 'neg) 'top) t
(equal (ai-abstract-plus 'pos 'zero) 'pos) t
(equal (ai-abstract-plus 'neg 'top) 'top) t

; Abstract multiplication
(equal (ai-abstract-mul 'pos 'pos) 'pos) t
(equal (ai-abstract-mul 'neg 'neg) 'pos) t
(equal (ai-abstract-mul 'pos 'neg) 'neg) t
(equal (ai-abstract-mul 'pos 'zero) 'zero) t
(equal (ai-abstract-mul 'pos 'top) 'top) t

; Abstract subtraction
(equal (ai-abstract-minus 'pos 'neg) 'pos) t

; Join/meet
(check "join pos neg = top" (eq (ai-join 'pos 'neg) 'top))
(check "join pos pos = pos" (eq (ai-join 'pos 'pos) 'pos))
(check "meet pos top = pos (was wrongly top)" (eq (ai-meet 'pos 'top) 'pos))

; Widening
(check "widen pos pos = pos" (eq (ai-widen 'pos 'pos) 'pos))
(check "widen pos neg = top" (eq (ai-widen 'pos 'neg) 'top))

; Abstract evaluation
(define ai-env '((x pos) (y neg)))
(check "eval (+ x y) = top" (eq (ai-abstract-eval '(+ x y) ai-env) 'top))
(check "eval (* x y) = neg (env lookup was broken)" (eq (ai-abstract-eval '(* x y) ai-env) 'neg))
(check "eval (+ 1 2) = pos" (eq (ai-abstract-eval '(+ 1 2) nil) 'pos))
(check "eval (* 0 5) = zero" (eq (ai-abstract-eval '(* 0 5) nil) 'zero))
(check "minus: 0 - pos = neg" (eq (ai-abstract-minus 'zero 'pos) 'neg))
(check "minus: 0 - neg = pos" (eq (ai-abstract-minus 'zero 'neg) 'pos))

; Gamma sanity check
(define gamma-pos (ai-gamma 'pos))
(check "gamma of pos is a set" (setp gamma-pos))
