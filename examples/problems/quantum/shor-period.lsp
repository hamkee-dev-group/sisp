; shor-period.lsp -- Shor's period-finding algorithm for factoring N=15.
; Pick a=7 (coprime to 15). Find period r of f(x)=a^x mod 15.
; 7^1=7, 7^2=49≡4, 7^3=28≡13, 7^4=91≡1 -> r=4 (period).
; r even: a^(r/2)=7^2=49≡4 mod 15.
; gcd(a^(r/2)-1,15)=gcd(3,15)=3, gcd(a^(r/2)+1,15)=gcd(5,15)=5 -> 15=3*5.

(define (sh-modpow b e m)
  (cond ((= e 0) 1)
        ((= (mod e 2) 0)
         (let ((h (sh-modpow b (/ e 2) m)))
           (mod (* h h) m)))
        (t (mod (* b (sh-modpow b (- e 1) m)) m))))

(define (sh-gcd a b)
  (cond ((= b 0) a) (t (sh-gcd b (mod a b)))))

(define (sh-order a N)
  (sh-order-step a N 1 (mod a N)))

(define (sh-order-step a N x cur)
  (cond ((= cur 1) x)
        ((> x N) 0)
        (t (sh-order-step a N (+ x 1) (mod (* cur a) N)))))

(define (sh-factor N a)
  (let ((r (sh-order a N)))
    (cond ((= r 0) (list (quote no-period) a))
          ((= (mod r 2) 1) (list (quote odd-period) r a))
          (t (let ((h (sh-modpow a (/ r 2) N)))
               (let ((p (sh-gcd (+ h 1) N))
                     (q (sh-gcd (- h 1) N)))
                 (list (quote a) a
                       (quote period) r
                       (quote factor1) p
                       (quote factor2) q
                       (quote product) (* p q))))))))

; --- factoring 15 ---
(list
  (list (quote a=7) (sh-factor 15 7))
  (list (quote a=2) (sh-factor 15 2))
  (list (quote a=4) (sh-factor 15 4))
  (list (quote a=8) (sh-factor 15 8))
  (list (quote a=13) (sh-factor 15 13)))
