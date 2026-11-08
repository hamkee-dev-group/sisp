; ============================================================
; sBTC — elliptic curve group  y^2 = x^3 + 7  (secp256k1 form)
;
; Real curve arithmetic at a shrunk field size so all products
; stay under 2^63.  Field prime P and generator G were chosen
; so the group order N is prime; every non-identity point then
; generates the whole group.  Verified: G on curve, N*G = O.
;
; A point is (x y); the identity (point at infinity) is nil.
; ============================================================

(define ec-p 100003)      ; field prime, p = 3 mod 4
(define ec-n 99667)       ; group order (prime)
(define ec-b 7)           ; curve constant
(define ec-gx 2)          ; generator x
(define ec-gy 86328)      ; generator y
(define ec-g (list ec-gx ec-gy))

; ---- modular arithmetic -------------------------------------
(define (xgcd a b)
  (cond ((= b 0) (list a 1 0))
        (t (let* ((r (mod a b))
                  (q (truncate (/ (- a r) b)))
                  (rec (xgcd b r))
                  (g (car rec)) (x1 (cadr rec)) (y1 (caddr rec)))
             (list g y1 (- x1 (* q y1)))))))

(define (modinv a m)
  (let ((res (xgcd (mod a m) m)))
    (mod (+ (mod (cadr res) m) m) m)))

(define (modp x) (mod (+ (mod x ec-p) ec-p) ec-p))

; modular exponentiation (square-and-multiply)
(define (modexp b e m)
  (cond ((= e 0) 1)
        ((= (mod e 2) 0) (modexp (mod (* b b) m) (truncate (/ e 2)) m))
        (t (mod (* b (modexp b (- e 1) m)) m))))

; Legendre symbol as 0 / 1 / p-1
(define (legendre a) (modexp (modp a) (truncate (/ (- ec-p 1) 2)) ec-p))
(define (qr-p a) (= (legendre a) 1))

; square root mod p (valid because p = 3 mod 4)
(define (modsqrt a) (modexp (modp a) (truncate (/ (+ ec-p 1) 4)) ec-p))

; ---- curve operations ---------------------------------------
(define (rhs x) (modp (+ (* x (* x x)) ec-b)))

(define (on-curve-p pt)
  (cond ((eq pt nil) t)
        (t (= (modp (* (cadr pt) (cadr pt))) (rhs (car pt))))))

(define (pt-neg pt)
  (cond ((eq pt nil) nil) (t (list (car pt) (modp (- 0 (cadr pt)))))))

(define (pt-add a b)
  (cond ((eq a nil) b)
        ((eq b nil) a)
        ((and (= (car a) (car b)) (= (modp (+ (cadr a) (cadr b))) 0)) nil)
        (t (let* ((s (cond ((and (= (car a) (car b)) (= (cadr a) (cadr b)))
                            (modp (* (* 3 (* (car a) (car a)))
                                     (modinv (modp (* 2 (cadr a))) ec-p))))
                           (t (modp (* (modp (- (cadr b) (cadr a)))
                                       (modinv (modp (- (car b) (car a))) ec-p))))))
                  (x3 (modp (- (- (* s s) (car a)) (car b))))
                  (y3 (modp (- (* s (- (car a) x3)) (cadr a)))))
             (list x3 y3)))))

; scalar multiplication (double-and-add)
(define (pt-mul k pt)
  (cond ((eq pt nil) nil)
        ((= k 0) nil)
        ((= (mod k 2) 0) (pt-mul (truncate (/ k 2)) (pt-add pt pt)))
        (t (pt-add pt (pt-mul (- k 1) pt)))))

(define (pt-eq a b)
  (cond ((and (eq a nil) (eq b nil)) t)
        ((or (eq a nil) (eq b nil)) nil)
        (t (and (= (car a) (car b)) (= (cadr a) (cadr b))))))
