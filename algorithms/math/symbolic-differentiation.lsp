; ============================================================
; name: symbolic-differentiation
; file: algorithms/math/symbolic-differentiation.lsp
; subject: algebra
; capability: differentiate simplify expand evaluate
; aliases: "symbolic differentiation" "computer algebra" "symbolic math" "cas"
; signature: (sm-derivative expr var) -> expr
;            (sm-simplify expr) -> expr
;            (sm-expand expr) -> expr
;            (sm-to-polynomial expr var) -> ascending-coefficient-list
;            expr: number | symbol | (op L R), op in + - * / ^, unary sin/cos/exp/log
; complexity: linear in expression-tree size per pass
; see-also: groebner-basis
; derived-from: SICP 2.3.2 (symbolic differentiation)
; status: example (tested)
; checks: 13 - known derivatives (power/product/quotient/chain), simplify
;         identities, expansion, substitution, polynomial round-trip, the
;         quadratic solver, and an INDEPENDENT cross-oracle: the symbolic
;         derivative converted to a polynomial must equal the polynomial
;         differentiator's result at every sample point; run: src/sisp <file>
; ------------------------------------------------------------
; A small computer-algebra core: expression simplification, symbolic
; differentiation (sum/product/quotient/power/chain rules + sin/cos/exp/log),
; evaluation, substitution, distributive expansion, and a coefficient-list
; polynomial layer (multiply, power, derivative, Horner evaluation).
; ============================================================
(define (sm-zero-p expression) (and (numberp expression) (= expression 0)))
(define (sm-one-p expression) (and (numberp expression) (= expression 1)))
(define (sm-apply operator left right)
  (cond ((eq operator '+) (+ left right)) ((eq operator '-) (- left right))
        ((eq operator '*) (* left right)) ((eq operator '/) (/ left right))
        (t (^ left right))))
(define (sm-unary-p operator) (in operator '(sin cos exp log)))
(define (sm-apply-unary operator value)
  (cond ((eq operator 'sin) (sin value)) ((eq operator 'cos) (cos value))
        ((eq operator 'exp) (exp value)) (t (log value))))
(define (sm-simplify expression)
  (cond ((atom expression) expression)
        ((sm-unary-p (car expression))
          (let ((inside (sm-simplify (cadr expression))))
            (cond ((numberp inside) (sm-apply-unary (car expression) inside))
                  (t (list (car expression) inside)))))
        (t (let ((operator (car expression)) (left (sm-simplify (cadr expression)))
                 (right (sm-simplify (caddr expression))))
          (cond ((and (numberp left) (numberp right)) (sm-apply operator left right))
                ((and (eq operator '+) (sm-zero-p left)) right)
                ((and (eq operator '+) (sm-zero-p right)) left)
                ((and (eq operator '-) (sm-zero-p right)) left)
                ((and (eq operator '*) (or (sm-zero-p left) (sm-zero-p right))) 0)
                ((and (eq operator '*) (equal left right)) (list '^ left 2))
                ((and (eq operator '*) (sm-one-p left)) right)
                ((and (eq operator '*) (sm-one-p right)) left)
                ((and (eq operator '/) (sm-zero-p left)) 0)
                ((and (eq operator '/) (sm-one-p right)) left)
                ((and (eq operator '^) (sm-one-p right)) left)
                ((and (eq operator '^) (sm-zero-p right)) 1)
                (t (list operator left right)))))))

(define (sm-derivative expression variable)
  (cond ((numberp expression) 0) ((symbolp expression) (cond ((eq expression variable) 1) (t 0)))
        ((eq (car expression) '+) (sm-simplify (list '+ (sm-derivative (cadr expression) variable)
                                                       (sm-derivative (caddr expression) variable))))
        ((eq (car expression) '-) (sm-simplify (list '- (sm-derivative (cadr expression) variable)
                                                       (sm-derivative (caddr expression) variable))))
        ((eq (car expression) '*) (sm-simplify (list '+
          (list '* (sm-derivative (cadr expression) variable) (caddr expression))
          (list '* (cadr expression) (sm-derivative (caddr expression) variable)))))
        ((eq (car expression) '/) (sm-simplify (list '/
          (list '-
            (list '* (sm-derivative (cadr expression) variable) (caddr expression))
            (list '* (cadr expression) (sm-derivative (caddr expression) variable)))
          (list '^ (caddr expression) 2))))
        ((and (eq (car expression) '^) (numberp (caddr expression)))
         (sm-simplify (list '* (caddr expression)
           (list '* (list '^ (cadr expression) (- (caddr expression) 1))
             (sm-derivative (cadr expression) variable)))))
        ((eq (car expression) 'sin)
          (sm-simplify (list '* (list 'cos (cadr expression))
            (sm-derivative (cadr expression) variable))))
        ((eq (car expression) 'cos)
          (sm-simplify (list '* -1 (list '* (list 'sin (cadr expression))
            (sm-derivative (cadr expression) variable)))))
        ((eq (car expression) 'exp)
          (sm-simplify (list '* (list 'exp (cadr expression))
            (sm-derivative (cadr expression) variable))))
        ((eq (car expression) 'log)
          (sm-simplify (list '/ (sm-derivative (cadr expression) variable)
            (cadr expression))))
        (t (list 'derivative expression variable))))
(define (sm-lookup environment variable)
  (cadr (find (lambda (binding) (equal (car binding) variable)) environment)))
(define (sm-evaluate expression environment)
  (cond ((numberp expression) expression)
        ((symbolp expression) (sm-lookup environment expression))
        ((sm-unary-p (car expression))
          (sm-apply-unary (car expression)
            (sm-evaluate (cadr expression) environment)))
        (t (sm-apply (car expression) (sm-evaluate (cadr expression) environment)
             (sm-evaluate (caddr expression) environment)))))

; Substitution and expansion transform expression trees without evaluating them.
(define (sm-substitute expression variable replacement)
  (cond ((atom expression)
          (cond ((eq expression variable) replacement) (t expression)))
        (t (map (lambda (part) (sm-substitute part variable replacement)) expression))))

(define (sm-expand expression)
  (cond ((atom expression) expression)
        ((sm-unary-p (car expression))
          (list (car expression) (sm-expand (cadr expression))))
        (t (let ((operator (car expression))
                 (left (sm-expand (cadr expression)))
                 (right (sm-expand (caddr expression))))
             (cond ((and (eq operator '*) (consp left) (eq (car left) '+))
                     (sm-simplify (list '+
                       (sm-expand (list '* (cadr left) right))
                       (sm-expand (list '* (caddr left) right)))))
                   ((and (eq operator '*) (consp right) (eq (car right) '+))
                     (sm-simplify (list '+
                       (sm-expand (list '* left (cadr right)))
                       (sm-expand (list '* left (caddr right))))))
                   (t (sm-simplify (list operator left right))))))))

; Polynomials are coefficient lists in ascending power order.
(define (sm-poly-add-left left right)
  (cond ((null left) right)
        ((null right) left)
        (t (cons (+ (car left) (car right))
             (sm-poly-add-left (cdr left) (cdr right))))))

(define (sm-poly-scale polynomial scalar)
  (map (lambda (coefficient) (* coefficient scalar)) polynomial))

(define (sm-poly-shift polynomial count)
  (cond ((= count 0) polynomial)
        (t (cons 0 (sm-poly-shift polynomial (- count 1))))))

(define (sm-poly-multiply-walk left right power result)
  (cond ((null left) result)
        (t (sm-poly-multiply-walk (cdr left) right (+ power 1)
             (sm-poly-add-left result
               (sm-poly-shift (sm-poly-scale right (car left)) power))))))

(define (sm-poly-multiply left right)
  (sm-poly-multiply-walk left right 0 nil))

(define (sm-poly-power polynomial exponent)
  (cond ((= exponent 0) '(1))
        (t (sm-poly-multiply polynomial
             (sm-poly-power polynomial (- exponent 1))))))

(define (sm-to-polynomial expression variable)
  (cond ((numberp expression) (list expression))
        ((symbolp expression)
          (cond ((eq expression variable) '(0 1)) (t nil)))
        ((eq (car expression) '+)
          (let ((left (sm-to-polynomial (cadr expression) variable))
                (right (sm-to-polynomial (caddr expression) variable)))
            (cond ((or (null left) (null right)) nil)
                  (t (sm-poly-add-left left right)))))
        ((eq (car expression) '-)
          (let ((left (sm-to-polynomial (cadr expression) variable))
                (right (sm-to-polynomial (caddr expression) variable)))
            (cond ((or (null left) (null right)) nil)
                  (t (sm-poly-add-left left (sm-poly-scale right -1))))))
        ((eq (car expression) '*)
          (let ((left (sm-to-polynomial (cadr expression) variable))
                (right (sm-to-polynomial (caddr expression) variable)))
            (cond ((or (null left) (null right)) nil)
                  (t (sm-poly-multiply left right)))))
        ((and (eq (car expression) '^) (numberp (caddr expression))
              (>= (caddr expression) 0)
              (= (caddr expression) (truncate (caddr expression))))
          (let ((base (sm-to-polynomial (cadr expression) variable)))
            (cond ((null base) nil)
                  (t (sm-poly-power base (caddr expression))))))
        (t nil)))

(define (sm-poly-derivative-tail polynomial power)
  (cond ((null (cdr polynomial)) nil)
        (t (cons (* power (cadr polynomial))
             (sm-poly-derivative-tail (cdr polynomial) (+ power 1))))))

(define (sm-poly-derivative polynomial power)
  (let ((result (sm-poly-derivative-tail polynomial power)))
    (cond ((null result) '(0)) (t result))))

(define (sm-poly-evaluate polynomial value)
  (foldr (lambda (coefficient result) (+ coefficient (* value result)))
    0 polynomial))

(define (sm-solve-quadratic a b c)
  (cond ((= a 0)
          (cond ((= b 0) nil)
                (t (list (rationalize (/ (- c) b))))))
        (t
          (let ((discriminant (- (* b b) (* 4 a c))))
            (cond ((< discriminant 0) nil)
                  (t
                    (let ((root (sqrt discriminant))
                          (denominator (* 2 a)))
                      (list
                        (rationalize (/ (+ (- b) root) denominator))
                        (rationalize (/ (- (- b) root) denominator))))))))))


; ============================================================
; ---- checks ----
; ============================================================

; --- known-answer derivatives ---
(check "symdiff: d/dx (x^3 + 2x) = 3x^2 + 2"
  (equal (sm-derivative '(+ (^ x 3) (* 2 x)) 'x) '(+ (* 3 (^ x 2)) 2)))
(check "symdiff: product rule d/dx (x*x) = x + x"
  (equal (sm-derivative '(* x x) 'x) '(+ x x)))
(check "symdiff: chain rule d/dx sin(2x) = cos(2x)*2"
  (equal (sm-derivative '(sin (* 2 x)) 'x) '(* (cos (* 2 x)) 2)))
(check "symdiff: d/dx cos(x) = -1 * (sin(x) * 1)  (sign + chain)"
  (equal (sm-derivative '(cos x) 'x) '(* -1 (sin x))))
(check "symdiff: d/dx exp(x) = exp(x)"
  (equal (sm-derivative '(exp x) 'x) '(exp x)))
(check "symdiff: constant has zero derivative"
  (= (sm-derivative '(+ 3 4) 'x) 0))

; --- simplification identities ---
(check "symdiff: simplify x + 0 -> x"
  (eq (sm-simplify '(+ x 0)) 'x))
(check "symdiff: simplify x * 1 -> x"
  (eq (sm-simplify '(* x 1)) 'x))
(check "symdiff: simplify x * 0 -> 0"
  (= (sm-simplify '(* x 0)) 0))

; --- expansion / substitution / polynomial ---
(check "symdiff: expand (x+2)(x+3)"
  (equal (sm-expand '(* (+ x 2) (+ x 3)))
         '(+ (+ (^ x 2) (* x 3)) (+ (* 2 x) 6))))
(check "symdiff: substitute x := (z+1) in x^2 + y"
  (equal (sm-substitute '(+ (^ x 2) y) 'x '(+ z 1))
         '(+ (^ (+ z 1) 2) y)))
(check "symdiff: quadratic x^2 - 1 = 0 has roots 1 and -1"
  (equal (sm-solve-quadratic 1 0 -1) '(1 -1)))

; --- independent cross-oracle: two derivative paths must agree everywhere ---
; sm-derivative (tree rules) vs sm-poly-derivative (coefficient calculus).
; If either differentiator is wrong they disagree at some sample point.
(check "symdiff: symbolic and polynomial derivatives agree at 0,1,2,3,4"
  (let ((f '(+ (+ (^ x 3) (* 2 x)) 7)))
    (let ((via-sym  (sm-to-polynomial (sm-derivative f 'x) 'x))
          (via-poly (sm-poly-derivative (sm-to-polynomial f 'x) 1)))
      (every (lambda (pt)
               (= (sm-poly-evaluate via-sym pt)
                  (sm-poly-evaluate via-poly pt)))
             '(0 1 2 3 4)))))
(print "=== symbolic-differentiation checks done ===")
