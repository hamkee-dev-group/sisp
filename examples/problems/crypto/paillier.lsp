; Paillier additive homomorphic encryption.
; Keygen p=11, q=17. Enc(m1)*Enc(m2) mod n^2 = Enc(m1+m2).

; Parameters
(define p 11)
(define q 17)
(define n (* p q))
(define n2 (* n n))

(define g (+ n 1))

; Extended GCD: return (g x y) where a*x + b*y = g = gcd(a,b)
(define (xgcd a b)
  (cond ((= b 0) (list a 1 0))
        (t (let* ((r (mod a b))
                  (q (truncate (/ (- a r) b)))
                  (rec (xgcd b r))
                  (g (car rec)) (x1 (cadr rec)) (y1 (caddr rec)))
             (list g y1 (- x1 (* q y1)))))))

; Modular inverse: a^-1 mod m (assumes gcd(a,m)=1)
(define (modinv a m)
  (let* ((res (xgcd a m))
         (g (car res)) (x (cadr res)))
    (mod (+ (mod x m) m) m)))

; Modular exponentiation
(define (modexp base exp modulus)
  (cond ((= exp 0) 1)
        ((= (mod exp 2) 0)
         (modexp (mod (* base base) modulus) (/ exp 2) modulus))
        (t (mod (* base (modexp base (- exp 1) modulus)) modulus))))

(define lambda (lcm (- p 1) (- q 1)))
(define mu (modinv lambda n))

(print "=== Paillier Homomorphic Encryption ===")
(print (cat "n = " (num-to-str n)))
(print (cat "n^2 = " (num-to-str n2)))
(print (cat "lambda = " (num-to-str lambda)))
(print (cat "mu = " (num-to-str mu)))

; Encryption: c = g^m * r^n mod n^2
(define (encrypt m r)
  (mod (* (modexp g m n2) (modexp r n n2)) n2))

; Decryption: m = L(c^lambda mod n^2) * mu mod n
; where L(x) = (x-1)/n
(define (decrypt c)
  (let* ((cl (modexp c lambda n2))
         (lx (/ (- cl 1) n))
         (m (mod (* lx mu) n)))
    m))

; Test basic encrypt/decrypt
(define m1 42)
(define r1 7)
(define c1 (encrypt m1 r1))
(define d1 (decrypt c1))

(print (cat "enc(42) = " (num-to-str c1)))
(print (cat "dec(enc(42)) = " (num-to-str d1)))

(cond ((= d1 (mod m1 n))
       (print "PASS: paillier-enc-dec"))
      (t (print "FAIL: paillier-enc-dec")))

; Test with another message
(define m2 15)
(define r2 5)
(define c2 (encrypt m2 r2))

(cond ((= (decrypt c2) (mod m2 n))
       (print "PASS: paillier-enc-dec-2"))
      (t (print "FAIL: paillier-enc-dec-2")))

; Homomorphic addition: Enc(m1)*Enc(m2) mod n^2 = Enc(m1+m2)
(define c-prod (mod (* c1 c2) n2))
(define d-prod (decrypt c-prod))

(print (cat "Enc(42)*Enc(15) decrypts to: " (num-to-str d-prod)))

(cond ((= d-prod (mod (+ m1 m2) n))
       (print "PASS: paillier-homomorphic-add"))
      (t (print "FAIL: paillier-homomorphic-add")))

; Homomorphic scalar mult: Enc(m)^k = Enc(k*m)
(define k 3)
(define c-pow (modexp c1 k n2))
(define d-pow (decrypt c-pow))

(print (cat "Enc(42)^3 decrypts to: " (num-to-str d-pow)))

(cond ((= d-pow (mod (* k m1) n))
       (print "PASS: paillier-scalar-mult"))
      (t (print "FAIL: paillier-scalar-mult")))

; Probabilistic encryption: different r gives different ciphertext
(define r1b 13)
(define c1b (encrypt m1 r1b))

(cond ((/= c1 c1b)
       (print "PASS: paillier-probabilistic"))
      (t (print "FAIL: paillier-probabilistic")))

(cond ((= (decrypt c1b) (mod m1 n))
       (print "PASS: paillier-probabilistic-dec"))
      (t (print "FAIL: paillier-probabilistic-dec")))

(print "=== PAILLIER DONE ===")
