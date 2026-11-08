; ============================================================
; Bourbaki — Commutative Ring Theory for Finite Rings
; Z/nZ: ideals, primes, maximals, quotients, sums, products.
; Verified on Z/12Z.
; ============================================================

(print "=== Bourbaki: Commutative Ring Theory ===")

; ---------- ring Z/nZ ----------
(define (r+ n a b) (mod (+ a b) n))
(define (r* n a b) (mod (* a b) n))
(define (rneg n a) (mod (- 0 a) n))

; ---------- divisors of n ----------
(define (divides n d) (= (mod n d) 0))
(define (divisors n)
  (labels (((go k acc)
    (cond ((> k n) (reverse acc))
          ((divides n k) (go (+ k 1) (cons k acc)))
          (t (go (+ k 1) acc)))))
    (go 1 nil)))

; ---------- find the generator d of an ideal of Z/nZ (gcd of any nonzero elt with n) ----------
(define (ideal-generator n I)
  (labels (((find els)
    (cond ((eq els nil) n)
          ((not (= (car els) 0)) (car els))
          (t (find (cdr els))))))
    (let ((g (find (set-to-list I))))
      (if (= g 0) n (let ((d (gcd n g))) d)))))

; ---------- ideal dZ/nZ from divisor d ----------
(define (ideal-from-divisor n d)
  (list-to-set
   (labels (((go k acc)
     (cond ((>= k n) (reverse acc))
           (t (go (+ k d) (cons k acc))))))
     (go 0 nil))))

; ---------- all proper ideals of Z/nZ ----------
(define (all-ideals n)
  (list-to-set (map (lambda (d) (ideal-from-divisor n d)) (divisors n))))

; ---------- brute-force ideal predicate ----------
(define (ideal-p ring add mul neg zero s)
  (and (in zero s)
    (every (lambda (a)
      (every (lambda (b) (in (add a b) s)) s)) s)
    (every (lambda (a) (in (neg a) s)) s)
    (every (lambda (a)
      (every (lambda (r) (in (mul r a) s)) ring)) s)))

; ---------- principal ideal (a) = gcd(a,n)Z/nZ ----------
(define (principal-ideal n a)
  (let ((d (gcd n a)))
    (ideal-from-divisor n d)))

; ---------- ideal sum I+J = (dI, dJ) Z/nZ ----------
(define (ideal-sum n I J)
  (let ((dI (ideal-generator n I))
        (dJ (ideal-generator n J)))
    (ideal-from-divisor n (gcd dI dJ))))

; ---------- ideal product I*J = (dI * dJ) Z/nZ ----------
(define (ideal-product n I J)
  (let ((dI (ideal-generator n I))
        (dJ (ideal-generator n J)))
    (ideal-from-divisor n (gcd n (* dI dJ)))))

; ---------- prime ideal: ab in P => a in P or b in P ----------
(define (prime-ideal-p ring mul zero p)
  (and (not (equal p ring))
       (every (lambda (a)
         (every (lambda (b)
           (or (notin (mul a b) p) (in a p) (in b p))) ring)) ring)))

; ---------- maximal ideal: no proper superset ----------
(define (maximal-ideal-p ring p ideals)
  (and (not (equal p ring))
       (not (any (lambda (q)
         (and (not (equal q p))
              (not (equal q ring))
              (subset p q))) ideals))))

; ========== Z/12Z VERIFICATION ==========
(print "--- Z/12Z ---")
(define n12 12)
(define ring12 (list-to-set (seq 0 11)))
(define (add12 a b) (mod (+ a b) 12))
(define (mul12 a b) (mod (* a b) 12))
(define (neg12 a) (mod (- 0 a) 12))

(define divs12 (divisors 12))
(check "z12-divisors" (equal divs12 (list 1 2 3 4 6 12)))

(define ideals12 (all-ideals n12))
(check "z12-6-ideals" (= (length ideals12) 6))

(define I1 (ideal-from-divisor n12 1))
(define I2 (ideal-from-divisor n12 2))
(define I3 (ideal-from-divisor n12 3))
(define I4 (ideal-from-divisor n12 4))
(define I6 (ideal-from-divisor n12 6))
(define I0 (ideal-from-divisor n12 12))

(check "ideal-2Z12" (equal I2 {0 2 4 6 8 10}))
(check "ideal-3Z12" (equal I3 {0 3 6 9}))
(check "ideal-4Z12" (equal I4 {0 4 8}))
(check "ideal-6Z12" (equal I6 {0 6}))
(check "ideal-zero"  (equal I0 {0}))
(check "ideal-all"   (equal I1 ring12))

; --- verify ideal predicate ---
(check "I2-is-ideal" (ideal-p ring12 add12 mul12 neg12 0 I2))
(check "I3-is-ideal" (ideal-p ring12 add12 mul12 neg12 0 I3))
(check "I0-is-ideal" (ideal-p ring12 add12 mul12 neg12 0 I0))

; --- principal ideals ---
(check "princ-3-is-I3" (equal (principal-ideal n12 3) I3))
(check "princ-8-is-I4" (equal (principal-ideal n12 8) I4))
(check "princ-6-is-I6" (equal (principal-ideal n12 6) I6))
(check "princ-9-is-I3" (equal (principal-ideal n12 9) I3))

; --- ideal sum and product ---
(check "sum-I2+I3=I1" (equal (ideal-sum n12 I2 I3) I1))
(check "sum-I4+I6=I2" (equal (ideal-sum n12 I4 I6) I2))
(check "prod-I2*I3=I6" (equal (ideal-product n12 I2 I3) I6))
(check "prod-I3*I4=I0" (equal (ideal-product n12 I3 I4) I0))

; --- prime ideals ---
(define primes12 (filter
  (lambda (p) (prime-ideal-p ring12 mul12 0 p)) ideals12))
(check "primes-are-2-and-3" (equal primes12 (list-to-set (list I2 I3))))
(check "I2-is-prime" (in I2 primes12))
(check "I3-is-prime" (in I3 primes12))
(check "I4-not-prime" (notin I4 primes12))
(check "I6-not-prime" (notin I6 primes12))
(check "I0-not-prime-here" (notin I0 primes12))

; --- maximal ideals ---
(define maximals12 (filter
  (lambda (p) (maximal-ideal-p ring12 p ideals12)) ideals12))
(check "maximals-are-2-and-3" (equal maximals12 (list-to-set (list I2 I3))))

; --- maximal => prime ---
(check "maximal-implies-prime" (subset maximals12 primes12))

; --- quotient ring R/I: coset ops ---
(define (qadd a b) (mod (+ a b) 12))
(define (qmul a b) (mod (* a b) 12))

(check "quotient-I2-closed-add"
  (every (lambda (a)
    (every (lambda (b) (in (qadd a b) I2)) I2)) I2))
(check "quotient-I2-closed-mul"
  (every (lambda (a)
    (every (lambda (b) (in (qmul a b) I2)) I2)) I2))

(print "=== Commutative Ring Theory Done ===")
