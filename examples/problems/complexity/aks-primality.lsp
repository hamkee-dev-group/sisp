; aks-primality.lsp -- AKS deterministic primality test for small n.
; Key insight: n is prime iff C(n,k) ≡ 0 mod n for all 0<k<n.
; Compute C(n,k) iteratively. Check perfect power. Verify n=31,15.

(define (aks-gcd a b)
  (cond ((= b 0) a) (t (aks-gcd b (mod a b)))))

(define (aks-isqrt n)
  (cond ((<= n 1) n) (t (aks-isqrt-step n (floor (/ (+ n 1) 2))))))

(define (aks-isqrt-step n g)
  (let ((ng (floor (/ (+ g (floor (/ n g))) 2))))
    (cond ((< ng g) (aks-isqrt-step n ng)) (t g))))

(define (aks-perfect-power-p n)
  (cond ((<= n 3) nil) (t (aks-pp-try n 2 (aks-isqrt n)))))

(define (aks-pp-try n b lim)
  (cond ((> b lim) nil)
        ((aks-pp-pow n b (* b b)) t)
        (t (aks-pp-try n (+ b 1) lim))))

(define (aks-pp-pow n b p)
  (cond ((> p n) nil) ((= p n) t) (t (aks-pp-pow n b (* p b)))))

(define (aks-bin-iter n k res i)
  (cond ((= i k) res)
        (t (aks-bin-iter n k (/ (* res (- n i)) (+ i 1)) (+ i 1)))))

(define (aks-binom n k)
  (cond ((or (< k 0) (> k n)) 0)
        ((or (= k 0) (= k n)) 1)
        ((> k (/ n 2)) (aks-binom n (- n k)))
        (t (aks-bin-iter n k 1 0))))

(define (aks-divisible-p n k)
  (cond ((>= k n) t)
        ((< k 2) (aks-divisible-p n (+ k 1)))
        ((= (mod (aks-binom n k) n) 0)
         (aks-divisible-p n (+ k 1)))
        (t nil)))

(define (aks-prime-p n)
  (cond ((<= n 1) nil)
        ((= n 2) t)
        ((= (mod n 2) 0) nil)
        ((aks-perfect-power-p n) nil)
        (t (aks-divisible-p n 1))))

(define (aks-trial-step n d)
  (cond ((> (* d d) n) t)
        ((= (mod n d) 0) nil)
        (t (aks-trial-step n (+ d 2)))))

(define (aks-oracle n)
  (cond ((<= n 1) nil) ((= n 2) t)
        ((= (mod n 2) 0) nil)
        (t (aks-trial-step n 3))))

(list
  (list (quote n31) (aks-prime-p 31) (aks-oracle 31))
  (list (quote n15) (aks-prime-p 15) (aks-oracle 15))
  (list (quote n17) (aks-prime-p 17) (aks-oracle 17))
  (list (quote n9) (aks-prime-p 9) (aks-oracle 9))
  (list (quote n7) (aks-prime-p 7) (aks-oracle 7))
  (list (quote n2) (aks-prime-p 2) (aks-oracle 2)))
