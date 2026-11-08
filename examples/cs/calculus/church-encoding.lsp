; Church encoding of data in lambda calculus.
; Numbers: 0 = lam f lam x x, n = lam f lam x f^n(x).
; succ = lam n lam f lam x f (n f x)
; add = lam m lam n lam f lam x m f (n f x)
; mul = lam m lam n lam f m (n f)
; Booleans: true = lam x lam y x, false = lam x lam y y.
; Pairs: pair = lam x lam y lam f f x y, fst = lam p p true, snd = lam p p false.

; Encode Church numerals as functions on a list counter
(define (church-num n)
  (cond ((= n 0) (lambda (f x) x))
        (t (lambda (f x) (f ((church-num (- n 1)) f x))))))

(define (church-to-int cn)
  (cn (lambda (k) (+ k 1)) 0))

(define (church-zero f x) x)
(define (church-one f x) (f x))
(define (church-two f x) (f (f x)))
(define (church-three f x) (f (f (f x))))
(define (church-five f x) (f (f (f (f (f x))))))
(define (church-six f x) (f (f (f (f (f (f x)))))))

(define (church-succ n)
  (lambda (f x) (f (n f x))))

(define (church-add m n)
  (lambda (f x) (m f (n f x))))

(define (church-mul m n)
  (lambda (f x) (m (lambda (y) (n f y)) x)))

; Booleans
(define church-true (lambda (x y) x))
(define church-false (lambda (x y) y))

(define (church-bool-to-bool b)
  (b t nil))

; Pairs
(define (church-pair x y)
  (lambda (f) (f x y)))

(define (church-fst p)
  (p church-true))

(define (church-snd p)
  (p church-false))

(print "=== Church Encoding ===")

(print "--- Numbers ---")
(print "0 =") (print (church-to-int church-zero))
(print "1 =") (print (church-to-int church-one))
(print "2 =") (print (church-to-int church-two))
(print "3 =") (print (church-to-int church-three))
(print "succ 2 =") (print (church-to-int (church-succ church-two)))
(print "add 2 3 =") (print (church-to-int (church-add church-two church-three)))
(print "Expected 5:") (print (church-to-int church-five))
(print "add 2 3 = 5?") (print (= (church-to-int (church-add church-two church-three)) 5))
(print "mul 2 3 =") (print (church-to-int (church-mul church-two church-three)))
(print "Expected 6:") (print (church-to-int church-six))
(print "mul 2 3 = 6?") (print (= (church-to-int (church-mul church-two church-three)) 6))

(print "--- Booleans ---")
(print "true:") (print (church-bool-to-bool church-true))
(print "false:") (print (church-bool-to-bool church-false))

(print "--- Pairs ---")
(define my-pair (church-pair 'a 'b))
(print "fst (pair a b) = a?") (print (eq (church-fst my-pair) 'a))
(print "snd (pair a b) = b?") (print (eq (church-snd my-pair) 'b))

(print "--- Additional verification ---")
(print "succ (add 1 2) = 4?") (print (= (church-to-int (church-succ (church-add church-one church-two))) 4))
(print "mul 0 5 = 0?") (print (= (church-to-int (church-mul church-zero church-five)) 0))
(print "mul 1 5 = 5?") (print (= (church-to-int (church-mul church-one church-five)) 5))

(print "=== Church Encoding Done ===")
