; Feldman Verifiable Secret Sharing (VSS).
; Share secret S over polynomial of degree t. Publish commitments g^a_i. Verify shares.
;
; FIELD NOTE: Feldman's scheme puts the polynomial in the field whose
; size is the ORDER of g, not mod p.  Here p = 23 is a safe prime
; (23 = 2*11+1) and g = 2 has order q = 11 mod 23.  Coefficients and
; shares are computed mod q = 11, commitments and verification mod p = 23.

(define p 23)
(define q 11)
(define g 2)

(define (modexp base exp mod)
  (cond ((= exp 0) 1)
        ((= (mod exp 2) 0)
         (modexp (mod (* base base) mod) (/ exp 2) mod))
        (t (mod (* base (modexp base (- exp 1) mod)) mod))))

(define S 7)
(define tt 2)
(define a0 (mod S q))
(define a1 3)
(define a2 4)
(define n 5)

(define (poly-eval x)
  (mod (+ a0 (* a1 x) (* a2 x x)) q))

(define (make-shares i)
  (cond ((> i n) nil)
        (t (cons (list i (poly-eval i)) (make-shares (+ i 1))))))

(define shares (make-shares 1))

(print "=== Feldman VSS ===")
(print (cat "Secret = " (num-to-str S)))

(define C0 (modexp g a0 p))
(define C1 (modexp g a1 p))
(define C2 (modexp g a2 p))

(cond ((and (numberp C0) (numberp C1) (numberp C2))
       (print "PASS: vss-commitments-generated"))
      (t (print "FAIL: vss-commitments-generated")))

; Feldman verification uses only the PUBLIC commitments:
;   g^si == C0 * C1^i * C2^(i^2)  (mod p).
; This holds because si = a0 + a1*i + a2*i^2 (mod q) and g has order q,
; so exponents may be reduced mod q.
(define (verify-one share)
  (let* ((i (car share)) (si (cadr share))
         (left (modexp g si p))
         (right (mod (* C0
                        (mod (* (modexp C1 i p)
                                (modexp C2 (mod (* i i) q) p))
                             p))
                     p)))
    (= left right)))

(define (verify-all lst)
  (cond ((eq lst nil) t)
        ((verify-one (car lst)) (verify-all (cdr lst)))
        (t nil)))

(cond ((verify-all shares)
       (print "PASS: vss-all-shares-verified"))
      (t (print "FAIL: vss-all-shares-verified")))

; negative control: a tampered share must fail verification
(cond ((verify-one (list 1 (+ (cadr (car shares)) 1)))
       (print "FAIL: vss-tampered-share-rejected"))
      (t (print "PASS: vss-tampered-share-rejected")))

; Reconstruction via Lagrange interpolation over the order-q field
(define (reconstruct-from-three s0 s1 s2)
  (let* ((x0 (car s0)) (y0 (cadr s0))
         (x1 (car s1)) (y1 (cadr s1))
         (x2 (car s2)) (y2 (cadr s2))
         (l0 (* y0 (mod (* (- 0 x1) (- 0 x2)) q)
                (modexp (* (- x0 x1) (- x0 x2)) (- q 2) q)))
         (l1 (* y1 (mod (* (- 0 x0) (- 0 x2)) q)
                (modexp (* (- x1 x0) (- x1 x2)) (- q 2) q)))
         (l2 (* y2 (mod (* (- 0 x0) (- 0 x1)) q)
                (modexp (* (- x2 x0) (- x2 x1)) (- q 2) q))))
    (mod (+ (mod l0 q) (mod l1 q) (mod l2 q)) q)))

; Dot product of 3-vectors (a helper, not used below)
(define (dot a b)
  (+ (* (nth 0 a) (nth 0 b)) (* (nth 1 a) (nth 1 b)) (* (nth 2 a) (nth 2 b))))

; Check that shares lie on polynomial
(define (on-poly x y)
  (= y (poly-eval x)))

(cond ((on-poly 1 (cadr (nth 0 shares)))
       (print "PASS: vss-share1-on-poly"))
      (t (print "FAIL: vss-share1-on-poly")))

(cond ((on-poly 3 (cadr (nth 2 shares)))
       (print "PASS: vss-share3-on-poly"))
      (t (print "FAIL: vss-share3-on-poly")))

; Reconstruct using 3 shares: solve the linear system
; P(x) = a0 + a1*x + a2*x^2, with yi = a0 + a1*xi + a2*xi^2 for each share.
; reconstruct-secret below keeps only the first Lagrange term and works mod p
; instead of mod q, so it is not a correct reconstruction and is not used;
; lagrange-3 further down is the working version.

(define (reconstruct-secret shares-3)
  (let* ((x0 (caar shares-3)) (y0 (cadar shares-3))
         (x1 (caadr shares-3)) (y1 (cadadr shares-3))
         (x2 (car (caddr shares-3))) (y2 (cadr (caddr shares-3))))
    (let ((inv (modexp (* (- x0 x1) (- x0 x2)) (- p 2) p)))
      (mod (* y0 (- x1) (- x2) inv) p))))

; Dealer-side sanity check: the polynomial at 0 is the secret
(define result (poly-eval 0))

(cond ((= result S)
       (print "PASS: vss-secret-equals-poly-at-0"))
      (t (print "FAIL: vss-secret-equals-poly-at-0")))

; Lagrange formula for secret = sum_i yi * prod_{j!=i} (-xj)/(xi-xj)
(define (lagrange-3 s0 s1 s2)
  (let* ((x0 (car s0)) (y0 (cadr s0))
         (x1 (car s1)) (y1 (cadr s1))
         (x2 (car s2)) (y2 (cadr s2))
         (d0 (mod (- x0 x1) q))
         (d1 (mod (- x0 x2) q))
         (d2 (mod (- x1 x0) q))
         (d3 (mod (- x1 x2) q))
         (d4 (mod (- x2 x0) q))
         (d5 (mod (- x2 x1) q))
         (inv01 (modexp (* d0 d1) (- q 2) q))
         (inv12 (modexp (* d2 d3) (- q 2) q))
         (inv24 (modexp (* d4 d5) (- q 2) q))
         (t0 (mod (* y0 (mod (- 0 x1) q) (mod (- 0 x2) q) inv01) q))
         (t1 (mod (* y1 (mod (- 0 x0) q) (mod (- 0 x2) q) inv12) q))
         (t2 (mod (* y2 (mod (- 0 x0) q) (mod (- 0 x1) q) inv24) q)))
    (mod (+ t0 t1 t2) q)))

(define s0 (nth 0 shares))
(define s1 (nth 1 shares))
(define s2 (nth 2 shares))
(define reconstructed (lagrange-3 s0 s1 s2))

(print (cat "Reconstructed: " (num-to-str reconstructed)))

(cond ((= reconstructed S)
       (print "PASS: vss-reconstruction"))
      (t (print "FAIL: vss-reconstruction")))

(print "=== FELDMAN VSS DONE ===")
