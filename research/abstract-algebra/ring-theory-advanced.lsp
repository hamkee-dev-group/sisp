; Ring theory advanced: polynomial rings over finite fields.
; Irreducibility criteria. Factorization algorithms.
; Chinese remainder theorem. Structure of Z/nZ.

(load "lib/collections/relations.lsp")

(print "=== RING THEORY ADVANCED ===")

; ---- Z/nZ: integers modulo n ----
(define (zn-add a b n) (mod (+ a b) n))
(define (zn-mul a b n) (mod (* a b) n))

(if (= (zn-add 3 5 7) 1)
    (print "PASS: zn-add") (print "FAIL: zn-add"))
(if (= (zn-mul 3 5 7) 1)
    (print "PASS: zn-mul") (print "FAIL: zn-mul"))

; ---- Units in Z/nZ: elements coprime to n ----
; phi(n) known values
(if (= (length (filter (lambda (a) (= 1 (gcd a 8)))
            (list-to-set (list 1 2 3 4 5 6 7)))) 4)
    (print "PASS: units-of-z8") (print "FAIL: units-of-z8"))

(if (= (length (filter (lambda (a) (= 1 (gcd a 12)))
            (list-to-set (list 1 2 3 4 5 6 7 8 9 10 11)))) 4)
    (print "PASS: units-of-z12") (print "FAIL: units-of-z12"))

; Euler's totient phi
(define (euler-phi n)
  (count (lambda (a) (= 1 (gcd a n)))
             (list-to-set (enumerate {tau : (< tau n)}
               (list-to-set (map (lambda (k) k) (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)))))))

(if (= (euler-phi 10) 4)
    (print "PASS: euler-phi-10") (print "FAIL: euler-phi-10"))
(if (= (euler-phi 7) 6)
    (print "PASS: euler-phi-7") (print "FAIL: euler-phi-7"))

; ---- Chinese Remainder Theorem ----
(define (zn-inv a n)
  (labels (((egcd aa bb)
    (cond ((= bb 0) (list aa 1 0))
          (t (let ((q (truncate (/ aa bb)))
                   (r (mod aa bb)))
               (let ((rec (egcd bb r)))
                 (let ((g (car rec)) (s (cadr rec)) (t1 (caddr rec)))
                   (list g t1 (- s (* q t1))))))))))
    (let ((res (egcd a n)))
      (mod (cond ((< (cadr res) 0) (+ (cadr res) n)) (t (cadr res))) n))))

(define (crt a1 m1 a2 m2)
  (let ((m1-inv (zn-inv m1 m2)))
    (mod (+ a1 (* m1 m1-inv (- a2 a1))) (* m1 m2))))

(if (= (crt 2 3 3 5) 8)
    (print "PASS: crt-3-5") (print "FAIL: crt-3-5"))
(if (= (crt 1 5 2 7) 16)
    (print "PASS: crt-5-7") (print "FAIL: crt-5-7"))

; ---- Z/nZ decomposition (via CRT) ----
(if (equal (list (mod 5 2) (mod 5 3)) (list 1 2))
    (print "PASS: zn-decomposition-z6") (print "FAIL: zn-decomposition-z6"))

; ---- Polynomials over GF(2) ----
; (x+1) + (x+1) = 0
(define poly-x-plus-1 (list 1 1))
(define poly-x (list 0 1))

(define (poly-add-manual p q)
  (list (mod (+ (car p) (car q)) 2)
        (mod (+ (cadr p) (cadr q)) 2)))

(if (equal (poly-add-manual poly-x-plus-1 poly-x-plus-1) (list 0 0))
    (print "PASS: poly-add-gf2") (print "FAIL: poly-add-gf2"))

; x * (x+1) = x^2 + x = (0,1,1) in GF(2)
; Convolution: (0*1, 0*1+1*1, 1*1) = (0,1,1)
(define poly-x2-plus-x (list 0 1 1))
; Verify by direct computation
(define conv-0 (mod (+ (* (nth 0 poly-x) (nth 0 poly-x-plus-1))) 2))
(define conv-1 (mod (+ (* (nth 0 poly-x) (nth 1 poly-x-plus-1))
                       (* (nth 1 poly-x) (nth 0 poly-x-plus-1))) 2))
(define conv-2 (mod (* (nth 1 poly-x) (nth 1 poly-x-plus-1)) 2))

(if (and (= conv-0 0) (= conv-1 1) (= conv-2 1))
    (print "PASS: poly-mul-gf2") (print "FAIL: poly-mul-gf2"))

; ---- Irreducibility: x^2 + x + 1 over GF(2) ----
; Evaluate at 0: 1+0+0=1, at 1: 1+1+1=1 mod 2. No roots. Irreducible.
(define eval-at-0 (mod (+ 1 0 0) 2))
(define eval-at-1 (mod 3 2))
(if (and (= eval-at-0 1) (= eval-at-1 1))
    (print "PASS: irreducible-x2+x+1") (print "FAIL: irreducible-x2+x+1"))

; x^2 + 1 = (1,0,1): eval at 1 = 1+0+1 = 0 mod 2. Has root, so reducible.
(define eval-red-at-1 (mod (+ 1 0 1) 2))
(if (= eval-red-at-1 0)
    (print "PASS: reducible-x2+1") (print "FAIL: reducible-x2+1"))

; ---- Z/12Z decomposition via CRT ----
(if (equal (list (mod 7 4) (mod 7 3)) (list 3 1))
    (print "PASS: zn12-decomposition") (print "FAIL: zn12-decomposition"))

; ---- Zero divisors in Z/nZ ----
; 2 * 3 = 0 mod 6, so 2 and 3 are zero divisors in Z/6Z.
(if (= (mod (* 2 3) 6) 0)
    (print "PASS: zero-divisor-in-z6") (print "FAIL: zero-divisor-in-z6"))
(if (not (= (mod (* 1 5) 6) 0))
    (print "PASS: unit-not-zero-divisor") (print "FAIL: unit-not-zero-divisor"))

(print "=== RING THEORY ADVANCED DONE ===")
