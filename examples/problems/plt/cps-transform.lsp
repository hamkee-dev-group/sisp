; CPS (Continuation-Passing Style) transformation for lambda terms.
; (cps '(lambda (x) x)) -> (lambda (k) (k (lambda (x k2) (k2 x))))

(define cp-count 0)
(define (cp-rst) (setq cp-count 0))
(define (cp-fr)
  (setq cp-count (+ cp-count 1))
  (cat "k" (num-to-str cp-count)))

(define (cp-atomp x)
  (or (numberp x) (and (symbolp x) (eq x 'true)) (and (symbolp x) (eq x 'nil))))

(define (cp-simplep x)
  (or (cp-atomp x) (and (listp x) (eq (car x) 'quote))))

(define (cp-t expr k)
  (cond ((cp-simplep expr) (list k expr))
        ((and (listp expr) (eq (car expr) 'lambda))
         (let* ((arg (caadr expr))
                (body (caddr expr))
                (nk (cp-fr)))
           (list k (list 'lambda (list arg nk)
                         (cp-t body (list 'var nk))))))
        ((and (listp expr) (eq (car expr) 'app))
         (let ((fun (cadr expr)) (arg (caddr expr)))
           (let* ((rk (cp-fr)) (fk (cp-fr)))
             (cp-t fun (list 'lambda (list fk)
                       (cp-t arg (list 'lambda (list rk)
                             (list 'app (list 'var fk) (list 'var rk) k))))))))
        (t (list k expr))))

(define (cps expr)
  (cp-rst)
  (let ((kend (cp-fr)))
    (list 'lambda (list kend) (cp-t expr (list 'var kend)))))

; Tests
(define cr1 (cps '(lambda (x) x)))
(equal (car cr1) 'lambda) t

(define cr2 (cps '(lambda (x) (app (lambda (y) y) x))))
(equal (car cr2) 'lambda) t

(define cr3 (cps 42))
(equal (car cr3) 'lambda) t

; Check structure: CPS of identity has a continuation parameter
(define cr4 (cps '(lambda (x) x)))
(equal (car (caddr cr4)) 'app) nil
