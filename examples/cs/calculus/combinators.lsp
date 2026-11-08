; SKI combinator calculus.
; S x y z = x z (y z)
; K x y = x
; I x = x (I = S K K)
; Bracket abstraction: translate lambda terms to combinators.
; Reduce combinator expressions to normal form.

(define (comb-var-p term)
  (and (symbolp term) (not (contains term '(S K I)))))

(define (comb-p term)
  (or (comb-var-p term) (contains term '(S K I))))

; Combinator reduction rules
; S x y z -> x z (y z), K x y -> x, I x -> x
(define (comb-step term)
  (cond ((atom term) term)
        ((and (>= (length term) 4) (eq (car term) 'S))
         (let ((x (cadr term)) (y (caddr term)) (z (cadddr term)))
           (cond ((>= (length term) 4)
                  (append (list (list x z) (list y z)) (cddddr term)))
                 (t term))))
        ((and (>= (length term) 3) (eq (car term) 'K))
         (let ((x (cadr term)) (y (caddr term)))
           (cond ((>= (length term) 3)
                  (cons x (cdddr term)))
                 (t term))))
        ((and (>= (length term) 2) (eq (car term) 'I))
         (let ((x (cadr term)))
           (cdr term)))
        (t term)))

; Helper: apply combinator arg
(define (comb-apply comb arg)
  (cond ((eq comb 'I) arg)
        ((eq comb 'K) (list 'K arg))
        ((eq comb 'S) (list 'S arg))
        ((and (consp comb) (eq (car comb) 'K) (= (length comb) 2))
         (cadr comb))
        ((and (consp comb) (eq (car comb) 'K) (= (length comb) 1))
         (list 'K arg))
        ((and (consp comb) (eq (car comb) 'S) (= (length comb) 2))
         (list 'S (cadr comb) arg))
        ((and (consp comb) (eq (car comb) 'S) (= (length comb) 3))
         (comb-apply (comb-apply (cadr comb) arg) (list (caddr comb) arg)))
        (t (cons comb (list arg)))))

; Reduction to normal form
(define (comb-reduce term budget)
  (cond ((= budget 0) term)
        ((comb-var-p term) term)
        ((eq term 'I) 'I)
        ((eq term 'K) 'K)
        ((eq term 'S) 'S)
        ((consp term)
         (let ((reduced (foldl (lambda (acc x) (comb-apply acc x))
                        (car term) (cdr term))))
           (cond ((equal reduced term) term)
                 (t (comb-reduce reduced (- budget 1))))))
        (t term)))

; SKK x ->* x
(define (test-skk x)
  (comb-reduce (list 'S 'K 'K x) 20))

; S (K S) K f g x ->* f (g x)  -- the B combinator (composition),
; NOT a swap (the old label claimed swap with only two arguments,
; which is a stuck partial application)
(define (test-compose f g x)
  (comb-reduce (list 'S (list 'K 'S) 'K f g x) 30))

; SKI combinator interpreter (left-associative application)
(define (comb-normalize expr budget)
  (cond ((= budget 0) expr)
        (t (comb-normalize (comb-reduce expr (+ budget 1)) (- budget 1)))))

; Simple string representation
(define (comb-str term)
  (cond ((eq term 'S) "S")
        ((eq term 'K) "K")
        ((eq term 'I) "I")
        ((symbolp term) (sym-to-str term))
        ((consp term) (str-join (map comb-str term) " "))
        (t "?")))

(define (sym-to-str s)
  (cond ((eq s 'x) "x")
        ((eq s 'y) "y")
        ((eq s 'z) "z")
        (t "?")))

(define (str-join strs sep)
  (cond ((eq strs nil) "")
        ((eq (cdr strs) nil) (car strs))
        (t (cat (car strs) (cat sep (str-join (cdr strs) sep))))))

(print "=== SKI Combinator Calculus ===")
(print "I = SKK") (print (comb-reduce (list 'S 'K 'K) 5))
(print "S K K x ->* x") (print (test-skk 'x))
(print "S (K S) K f g x ->* f (g x) (B composition)")
(print (test-compose 'x 'y 'z))

; K combinator properties
(print "K a b = a") (print (comb-reduce (list 'K 'a 'b) 5))
(print "I x = x")  (print (comb-reduce (list 'I 'x) 5))

; S combinator property: S K K = I
(print "S K K reduces to I:") (print (comb-reduce (list 'S 'K 'K) 10))

(print "=== Combinators Done ===")
