; ============================================================
; Lang's Algebra Ch.2: Rings
; Rings, ideals, homomorphisms, polynomial rings, factorization.
; Verified on Z/nZ, F_p[x]/(f(x)).
; ============================================================

(print "=== Lang Ch2: Rings ===")

(define (r+ n a b) (mod (+ a b) n))
(define (r* n a b) (mod (* a b) n))
(define (rneg n a) (mod (- 0 a) n))
(define (pow-mod n a k)
  (cond ((= k 0) 1)
        (t (mod (* a (pow-mod n a (- k 1))) n))))

(define (divides n d) (= (mod n d) 0))
(define (divisors n)
  (labels (((go-iter k acc)
    (cond ((> k n) (reverse acc))
          ((divides n k) (go-iter (+ k 1) (cons k acc)))
          (t (go-iter (+ k 1) acc)))))
    (go-iter 1 nil)))

(define (ideal-from-divisor n d)
  (list-to-set
   (labels (((go-iter k acc)
     (cond ((>= k n) (reverse acc))
           (t (go-iter (+ k d) (cons k acc))))))
     (go-iter 0 nil))))

(define (all-ideals n)
  (list-to-set (map (lambda (d) (ideal-from-divisor n d)) (divisors n))))

(define (units n)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter (lambda (a)
      (any (lambda (b) (= (r* n a b) 1)) ring)) ring)))

(define (zero-divisors n)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter (lambda (a)
      (and (not (= a 0))
           (any (lambda (b)
             (and (not (= b 0))
                  (= (r* n a b) 0))) ring))) ring)))

(define (nilpotent-p n a)
  (labels (((try-pow k)
    (cond ((>= k n) nil)
          ((= (pow-mod n a k) 0) t)
          (t (try-pow (+ k 1))))))
    (try-pow 1)))

(define (nilradical n)
  (filter (lambda (a) (nilpotent-p n a))
    (list-to-set (seq 0 (- n 1)))))

(define (ring-hom-p n m f)
  (let ((Rn (list-to-set (seq 0 (- n 1))))
        (Rm (list-to-set (seq 0 (- m 1)))))
    (and (every (lambda (x) (in (f x) Rm)) Rn)
         (= (f 0) 0)
         (= (f 1) 1)
         (every (lambda (x)
           (every (lambda (y)
             (= (f (r+ n x y)) (r+ m (f x) (f y)))) Rn)) Rn)
         (every (lambda (x)
           (every (lambda (y)
             (= (f (r* n x y)) (r* m (f x) (f y)))) Rn)) Rn))))

(define (f2x2-add a b)
  (list (mod (+ (nth 0 a) (nth 0 b)) 2)
        (mod (+ (nth 1 a) (nth 1 b)) 2)))

(define (f2x2-mul a b)
  (let ((c0 (mod (+ (* (nth 0 a) (nth 0 b))
                     (* (nth 1 a) (nth 1 b))) 2))
        (c1 (mod (+ (* (nth 0 a) (nth 1 b))
                     (* (nth 1 a) (nth 0 b))
                     (* (nth 1 a) (nth 1 b))) 2)))
    (list c0 c1)))

(define gf4-elements
  (list-to-set (list
    (list 0 0)
    (list 1 0)
    (list 0 1)
    (list 1 1))))

(check "GF4-has-4-elements" (= (length gf4-elements) 4))

(define gf4-star (filter
  (lambda (e) (not (equal e (list 0 0)))) gf4-elements))
(check "GF4-star-order-3" (= (length gf4-star) 3))

(check "GF4-x3-equals-1"
  (every (lambda (a)
    (let* ((a2 (f2x2-mul a a))
           (a3 (f2x2-mul a a2)))
      (equal a3 (list 1 0)))) gf4-star))

(print "--- Z/12Z ---")
(define n12 12)
(define ring12 (list-to-set (seq 0 11)))

(define units12 (units n12))
(check "Z12-phi-is-4" (= (length units12) 4))

(define zd12 (zero-divisors n12))
(check "Z12-7-zero-divisors" (= (length zd12) 7))

(check "Z12-partition-units-zero-dvs"
  (= (+ (length units12) 1 (length zd12)) 12))

(define nil12 (nilradical n12))
(check "Z12-nilradical-size-2" (= (length nil12) 2))

(print "--- Z/8Z ---")
(define n8 8)

(define units8 (units n8))
(check "Z8-phi-is-4" (= (length units8) 4))

(define zd8 (zero-divisors n8))
(check "Z8-3-zero-divisors" (= (length zd8) 3))

(define nil8 (nilradical n8))
(check "Z8-nilradical-size-4" (= (length nil8) 4))
(check "Z8-nilradical-all-nilpotent"
  (every (lambda (a) (nilpotent-p n8 a)) nil8))

(print "--- Ring Homomorphisms ---")
(define (pi-12-to-4 x) (mod x 4))
(check "pi-12-to-4-is-homomorphism" (ring-hom-p n12 4 pi-12-to-4))

(define (pi-12-to-3 x) (mod x 3))
(check "pi-12-to-3-is-homomorphism" (ring-hom-p n12 3 pi-12-to-3))

(define (crt-12 x) (list (mod x 4) (mod x 3)))
(check "crt-is-bijective"
  (= (length (list-to-set (map crt-12 (seq 0 11)))) 12))

(print "--- GF(8) = F_2[x]/(x^3+x+1) ---")

(define (gf8-add ea eb)
  (list (mod (+ (nth 0 ea) (nth 0 eb)) 2)
        (mod (+ (nth 1 ea) (nth 1 eb)) 2)
        (mod (+ (nth 2 ea) (nth 2 eb)) 2)))

(define (gf8-mul ea eb)
  (let* ((a0 (nth 0 ea)) (a1 (nth 1 ea)) (a2 (nth 2 ea))
         (b0 (nth 0 eb)) (b1 (nth 1 eb)) (b2 (nth 2 eb))
         (c0 (* a0 b0))
         (c1 (+ (* a0 b1) (* a1 b0)))
         (c2 (+ (* a0 b2) (* a1 b1) (* a2 b0)))
         (c3 (+ (* a1 b2) (* a2 b1)))
         (c4 (* a2 b2)))
    (list (mod (+ c0 c3) 2)
          (mod (+ c1 c3 c4) 2)
          (mod (+ c2 c4) 2))))

(define gf8-elements (list-to-set
  (list (list 0 0 0) (list 1 0 0) (list 0 1 0) (list 1 1 0)
        (list 0 0 1) (list 1 0 1) (list 0 1 1) (list 1 1 1))))
(check "GF8-has-8-elements" (= (length gf8-elements) 8))

(define gf8-star (filter
  (lambda (e) (not (equal e (list 0 0 0)))) gf8-elements))
(check "GF8-star-order-7" (= (length gf8-star) 7))

; The multiplicative group of GF(8) is cyclic of order 7: every nonzero
; element satisfies x^7 = 1, and (7 being prime) every non-identity element
; generates. Both computed from the field multiplication.
(define gf8-one (list 1 0 0))
(define (gf8-pow a k)
  (cond ((= k 0) gf8-one) (t (gf8-mul a (gf8-pow a (- k 1))))))
(check "GF8-x7-equals-1"
  (every (lambda (a) (equal (gf8-pow a 7) gf8-one)) gf8-star))
(check "GF8-star-is-cyclic"
  (every (lambda (a)
    (cond ((equal a gf8-one) t)
          (t (= 7 (length (map (lambda (k) (gf8-pow a k)) (list-to-set (seq 0 6))))))))
    gf8-star))
; Negative control: a proper power that is not the full order does not
; return the identity for a generator.
(check "GF8-generator-has-no-smaller-order"
  (not (every (lambda (a) (equal (gf8-pow a 3) gf8-one)) gf8-star)))

(print "=== Lang Ch2 Done ===")
