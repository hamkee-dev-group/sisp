; ============================================================
; 70 — The Metacircular Evaluator Tower
; A McCarthy-style evaluator for a SISP subset, written twice:
; once as ordinary SISP (level 1), and once as PURE DATA in its
; own subset with self-passing recursion (EVAL-SRC). The same
; program is then run at four levels — natively, interpreted,
; interpreted-by-an-interpreted-interpreter, and one deeper —
; and every level returns the same value. Interpretation is a
; tower you can climb, and each floor is checked.
; ============================================================

(print "=== The Metacircular Evaluator Tower ===")


; ---------- level-1 evaluator: ordinary SISP ----------
; Subset: numbers, t/nil, symbols, quote, cond, if, lambda,
; closures, and the primitives car cdr cons eq atom numberp + - * = <.

(define (menv-lookup x env)
  (cond ((eq env nil) nil)
        ((eq (car (car env)) x) (cdr (car env)))
        (t (menv-lookup x (cdr env)))))

(define (mbind params vals env)
  (cond ((eq params nil) env)
        (t (mbind (cdr params) (cdr vals)
             (cons (cons (car params) (car vals)) env)))))

(define (mevlis xs env)
  (cond ((eq xs nil) nil)
        (t (cons (meval (car xs) env) (mevlis (cdr xs) env)))))

(define (mprim-p s)
  (contains s (quote (car cdr cons eq atom numberp + - * = <))))

(define (mapply-prim s args)
  (cond ((eq s (quote car)) (car (car args)))
        ((eq s (quote cdr)) (cdr (car args)))
        ((eq s (quote cons)) (cons (car args) (car (cdr args))))
        ((eq s (quote eq)) (eq (car args) (car (cdr args))))
        ((eq s (quote atom)) (atom (car args)))
        ((eq s (quote numberp)) (numberp (car args)))
        ((eq s (quote +)) (+ (car args) (car (cdr args))))
        ((eq s (quote -)) (- (car args) (car (cdr args))))
        ((eq s (quote *)) (* (car args) (car (cdr args))))
        ((eq s (quote =)) (= (car args) (car (cdr args))))
        (t (< (car args) (car (cdr args))))))

(define (mevcond clauses env)
  (cond ((eq clauses nil) nil)
        ((meval (car (car clauses)) env)
          (meval (car (cdr (car clauses))) env))
        (t (mevcond (cdr clauses) env))))

(define (meval x env)
  (cond ((numberp x) x)
        ((eq x nil) nil)
        ((eq x t) t)
        ((atom x) (menv-lookup x env))
        ((eq (car x) (quote quote)) (car (cdr x)))
        ((eq (car x) (quote cond)) (mevcond (cdr x) env))
        ((eq (car x) (quote if))
          (cond ((meval (car (cdr x)) env)
                  (meval (car (cdr (cdr x))) env))
                (t (meval (car (cdr (cdr (cdr x)))) env))))
        ((eq (car x) (quote lambda))
          (list (quote closure) (car (cdr x)) (car (cdr (cdr x))) env))
        ((and (atom (car x)) (mprim-p (car x)))
          (mapply-prim (car x) (mevlis (cdr x) env)))
        (t (let ((f (meval (car x) env)))
             (meval (car (cdr (cdr f)))
               (mbind (car (cdr f)) (mevlis (cdr x) env)
                 (car (cdr (cdr (cdr f))))))))))

; ---------- the test programs (written in the subset) ----------
; recursion via self-passing, as in program 43
(define fact-src
  (quote ((lambda (self n) (self self n))
          (lambda (self n)
            (cond ((= n 0) 1) (t (* n (self self (- n 1))))))
          5)))

(define twice-src
  (quote ((lambda (f x) (f (f x))) (lambda (y) (+ y 1)) 3)))

(define capture-src
  (quote (((lambda (k) (lambda (y) (+ k y))) 10) 5)))

; ---------- level 0 vs level 1 ----------
(define (fact n) (cond ((= n 0) 1) (t (* n (fact (- n 1))))))
(define t0-start (runtime))
(define level0 (fact 5))
(define t0 (- (runtime) t0-start))
(define t1-start (runtime))
(define level1 (meval fact-src nil))
(define t1 (- (runtime) t1-start))

(check "level-0-native" (= level0 120))
(check "level-1-interpreted" (= level1 120))
(check "level-1-higher-order" (= (meval twice-src nil) 5))
(check "level-1-lexical-capture" (= (meval capture-src nil) 15))

; ---------- EVAL-SRC: the evaluator as pure data ----------
; The same algorithm, written in the subset it interprets. It has
; no define: recursion is self-passing through ev, environment
; extension and argument evaluation are immediately-applied
; lambdas (a let with no let), and cond walks itself by
; reconstructing a smaller cond.

(define eval-src (quote
  (lambda (ev x env)
    (cond
      ((numberp x) x)
      ((eq x nil) nil)
      ((eq x t) t)
      ((atom x)
        (cond ((eq env nil) nil)
              ((eq (car (car env)) x) (cdr (car env)))
              (t (ev ev x (cdr env)))))
      ((eq (car x) (quote quote)) (car (cdr x)))
      ((eq (car x) (quote cond))
        (cond ((eq (cdr x) nil) nil)
              ((ev ev (car (car (cdr x))) env)
                (ev ev (car (cdr (car (cdr x)))) env))
              (t (ev ev (cons (quote cond) (cdr (cdr x))) env))))
      ((eq (car x) (quote lambda))
        (cons (quote closure)
          (cons (car (cdr x))
            (cons (car (cdr (cdr x))) (cons env nil)))))
      ((eq (car x) (quote car)) (car (ev ev (car (cdr x)) env)))
      ((eq (car x) (quote cdr)) (cdr (ev ev (car (cdr x)) env)))
      ((eq (car x) (quote atom)) (atom (ev ev (car (cdr x)) env)))
      ((eq (car x) (quote numberp)) (numberp (ev ev (car (cdr x)) env)))
      ((eq (car x) (quote cons))
        (cons (ev ev (car (cdr x)) env)
              (ev ev (car (cdr (cdr x))) env)))
      ((eq (car x) (quote eq))
        (eq (ev ev (car (cdr x)) env)
            (ev ev (car (cdr (cdr x))) env)))
      ((eq (car x) (quote +))
        (+ (ev ev (car (cdr x)) env) (ev ev (car (cdr (cdr x))) env)))
      ((eq (car x) (quote -))
        (- (ev ev (car (cdr x)) env) (ev ev (car (cdr (cdr x))) env)))
      ((eq (car x) (quote *))
        (* (ev ev (car (cdr x)) env) (ev ev (car (cdr (cdr x))) env)))
      ((eq (car x) (quote =))
        (= (ev ev (car (cdr x)) env) (ev ev (car (cdr (cdr x))) env)))
      ((eq (car x) (quote <))
        (< (ev ev (car (cdr x)) env) (ev ev (car (cdr (cdr x))) env)))
      (t
        ((lambda (clo args)
           (ev ev (car (cdr (cdr clo)))
             ((lambda (bind)
                (bind bind (car (cdr clo)) args
                  (car (cdr (cdr (cdr clo))))))
              (lambda (bind params vals e)
                (cond ((eq params nil) e)
                      (t (bind bind (cdr params) (cdr vals)
                           (cons (cons (car params) (car vals)) e))))))))
         (ev ev (car x) env)
         ((lambda (evl) (evl evl (cdr x)))
          (lambda (evl xs)
            (cond ((eq xs nil) nil)
                  (t (cons (ev ev (car xs) env)
                           (evl evl (cdr xs)))))))))))))

; the interpreter interprets ITS OWN SOURCE applied to a program:
(define (tower-expr program)
  (list eval-src eval-src (list (quote quote) program)
        (list (quote quote) nil)))

; ---------- level 2: meval runs EVAL-SRC running fact ----------
(define t2-start (runtime))
(define level2 (meval (tower-expr fact-src) nil))
(define t2 (- (runtime) t2-start))
(check "level-2-interpreter-in-interpreter" (= level2 120))
(check "level-2-higher-order" (= (meval (tower-expr twice-src) nil) 5))

; ---------- level 3: three interpreters deep ----------
(define mul-src (quote (* 6 7)))
(define t3-start (runtime))
(define level3 (meval (tower-expr (tower-expr mul-src)) nil))
(define t3 (- (runtime) t3-start))
(check "level-3-tower" (= level3 42))
(check "level-3-agrees-with-level-0" (= level3 (* 6 7)))

; ---------- the price of a floor ----------
(print (list (quote seconds)
  (quote level0) t0 (quote level1) t1
  (quote level2) t2 (quote level3) t3))
(check "each-level-costs-more"
  (and (< t1 t2) (<= t2 t3)))

(print "=== Evaluator Tower Done ===")
