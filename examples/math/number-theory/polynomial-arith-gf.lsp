; ============================================================
; 123 — Polynomial Arithmetic over GF(7) and GF(3)
; Add, multiply, divide, GCD, evaluation.
; GF(3): squarefree decomposition example.
; ============================================================

(print "=== Polynomial Arithmetic over GF(p) ===")


(define P 7)
(define (gf x) (mod x P))

(define (poly-add a b)
  (cond ((equal a nil) b)
        ((equal b nil) a)
        (t (cons (gf (+ (car a) (car b))) (poly-add (cdr a) (cdr b))))))

(define (poly-neg p) (map (lambda (x) (gf (- x))) p))
(define (poly-sub a b) (poly-add a (poly-neg b)))

(define (poly-scale p c) (map (lambda (x) (gf (* x c))) p))

(define (poly-shift p k)
  (cond ((= k 0) p) (t (cons 0 (poly-shift p (- k 1))))))

(define (poly-mul a b)
  (poly-mul-iter a b 0 nil))

(define (poly-mul-iter a b idx acc)
  (cond ((equal a nil) acc)
        (t (poly-mul-iter (cdr a) b (+ idx 1)
             (poly-add acc (poly-shift (poly-scale b (car a)) idx))))))

(define (poly-zero-p p)
  (cond ((equal p nil) t)
        ((= (car p) 0) (poly-zero-p (cdr p)))
        (t nil)))

(define (poly-eval p x)
  (poly-eval-iter p x 0))

(define (poly-eval-iter p x acc)
  (cond ((equal p nil) acc)
        (t (poly-eval-iter (cdr p) x (gf (+ (car p) (* x acc)))))))

; Inverse table for GF(7)
(define inv7 '(0 1 4 5 2 3 6))
(define (gf-inv c) (car (drop inv7 c)))

; --- polynomial long division and GCD over GF(7) ---
; polynomials are coefficient lists, constant term first.
(define (poly-trim p)                          ; drop high-degree zero coeffs
  (cond ((equal p nil) nil)
        ((= (last p) 0) (poly-trim (butlast p)))
        (t p)))
(define (poly-degree p) (- (length (poly-trim p)) 1))  ; -1 for the zero polynomial
(define (poly-lead p) (last (poly-trim p)))            ; leading coefficient

; long division: returns (quotient remainder) with a = q*b + r and deg r < deg b
(define (poly-divmod a b)
  (labels (((step r q)
             (cond ((< (poly-degree r) (poly-degree b)) (list (poly-trim q) (poly-trim r)))
                   (t (let* ((d (- (poly-degree r) (poly-degree b)))
                             (term (poly-shift (list (gf (* (poly-lead r) (gf-inv (poly-lead b))))) d)))
                        (step (poly-sub r (poly-mul term b)) (poly-add q term)))))))
    (step (poly-trim a) nil)))

(define (poly-monic p)                          ; scale so leading coefficient is 1
  (cond ((poly-zero-p p) nil) (t (poly-scale p (gf-inv (poly-lead p))))))
(define (poly-gcd a b)                          ; Euclid; result is monic
  (cond ((poly-zero-p b) (poly-monic a))
        (t (poly-gcd b (cadr (poly-divmod a b))))))

; --- GF(7) tests ---
(define pa '(1 1 1 1))  ; 1 + x + x^2 + x^3
(define pb '(6 0 1))    ; x^2 + 6 = x^2 - 1 mod 7

(print "a(x) = 1+x+x^2+x^3:") (print pa)
(print "b(x) = 6+x^2 (x^2-1 mod 7):") (print pb)

(print "a+b:") (print (poly-add pa pb))
(print "a*b:") (print (poly-mul pa pb))

; Division a/b, computed by long division.
(define dm (poly-divmod pa pb))
(define quot (car dm))
(define rem (cadr dm))
(print "Division: a/b")
(print "Quotient:") (print quot)
(print "Remainder:") (print rem)
(check "div-verify" (equal (poly-add (poly-mul quot pb) rem) pa))   ; a = q*b + r
(check "quot-is-x+1" (equal quot '(1 1)))
(check "rem-is-2+2x" (equal rem '(2 2)))

; GCD by the Euclidean algorithm (monic result).
(define g (poly-gcd pa pb))
(print "gcd(a,b):") (print g)
(check "gcd-is-x+1" (equal g '(1 1)))
(check "gcd-divides-a" (poly-zero-p (cadr (poly-divmod pa g))))
(check "gcd-divides-b" (poly-zero-p (cadr (poly-divmod pb g))))
; x+1 is the gcd: a and b share the root x = 6 (= -1 mod 7)
(check "common-root-a" (= (poly-eval pa 6) 0))
(check "common-root-b" (= (poly-eval pb 6) 0))

(print "a(0):") (print (poly-eval pa 0))
(print "a(1):") (print (poly-eval pa 1))
(print "a(2):") (print (poly-eval pa 2))
(check "a(0)=1" (= (poly-eval pa 0) 1))
(check "a(1)=4" (= (poly-eval pa 1) 4))

; --- GF(3) squarefree ---
(print "--- GF(3) squarefree decomposition ---")
(define P3 3)
(define (gf3 x) (mod x P3))

(define fp '(1 2 2 2 1)) ; 1+2x+2x^2+2x^3+x^4

; Formal derivative
(define (poly-deriv p)
  (poly-deriv-iter (cdr p) 1))

(define (poly-deriv-iter p n)
  (cond ((equal p nil) nil)
        (t (cons (gf3 (* n (car p))) (poly-deriv-iter (cdr p) (+ n 1))))))

(define fp-prime (poly-deriv fp))
(print "f(x) in GF(3):") (print fp)
(print "f'(x):") (print fp-prime)

(define (poly-eval3 p x)
  (poly-eval3-iter p x 0))

(define (poly-eval3-iter p x acc)
  (cond ((equal p nil) acc)
        (t (poly-eval3-iter (cdr p) x (gf3 (+ (car p) (* x acc)))))))

(print "f(0):") (print (poly-eval3 fp 0))
(print "f(1):") (print (poly-eval3 fp 1))
(print "f(2):") (print (poly-eval3 fp 2))

(check "fp(0)=1" (= (poly-eval3 fp 0) 1))
(check "fp(1)=2" (= (poly-eval3 fp 1) 2))
(check "fp(2)=0" (= (poly-eval3 fp 2) 0))
(check "deriv-correct" (equal fp-prime '(2 1 0 1)))

(print "=== Polynomial Arithmetic Done ===")
