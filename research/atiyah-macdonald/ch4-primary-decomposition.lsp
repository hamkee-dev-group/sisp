; ============================================================
; Atiyah-Macdonald Ch4: Primary Decomposition
; Primary decomposition in Noetherian rings (Z/nZ).
; Associated primes, isolated vs embedded components.
; ============================================================

(print "=== AM Ch4: Primary Decomposition ===")

(define (r* n a b) (mod (* a b) n))
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

(define (prime-ideal-p ring mul p)
  (and (not (equal p ring))
       (every (lambda (a)
         (every (lambda (b)
           (or (notin (mul a b) p) (in a p) (in b p))) ring)) ring)))

(define (nilpotent-element-p n a)
  (labels (((try k)
    (cond ((>= k n) nil)
          ((= (pow-mod n a k) 0) t)
          (t (try (+ k 1))))))
    (try 1)))

; I is primary if: ab ∈ I, a ∉ I ⇒ b^n ∈ I for some n
(define (primary-ideal-p n ring I)
  (and (not (equal I ring))
       (every (lambda (a)
         (every (lambda (b)
           (cond ((in (r* n a b) I)
                  (cond ((in a I) t)
                        (t (labels (((try k)
                               (cond ((>= k n) nil)
                                     ((in (pow-mod n b k) I) t)
                                     (t (try (+ k 1))))))
                             (try 1)))))
                 (t t)))
           ring))
         ring)))

(define (radical-of n I)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter (lambda (a)
      (labels (((try k)
        (cond ((>= k n) nil)
              ((in (pow-mod n a k) I) t)
              (t (try (+ k 1))))))
        (try 1))) ring)))

(define (associated-primes n I)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter
     (lambda (p) (and (prime-ideal-p ring (lambda (a b) (mod (* a b) n)) p)
                      (subset I p)))
     (all-ideals n))))

(define (isolated-primes n I)
  (let ((assoc (associated-primes n I)))
    (filter
     (lambda (p)
       (not (any (lambda (q)
               (and (not (equal q p)) (subset q p)))
             assoc)))
     assoc)))

(define (embedded-primes n I)
  (let ((assoc (associated-primes n I))
        (iso (isolated-primes n I)))
    (filter (lambda (p) (notin p iso)) assoc)))

(print "--- Z/12Z Primary Decomposition ---")
(define n12 12)
(define ring12 (list-to-set (seq 0 11)))

(define I2-12 (ideal-from-divisor n12 2))
(define I3-12 (ideal-from-divisor n12 3))
(define I4-12 (ideal-from-divisor n12 4))
(define I6-12 (ideal-from-divisor n12 6))

(check "I4-primary" (primary-ideal-p n12 ring12 I4-12))
(check "I6-not-primary" (not (primary-ideal-p n12 ring12 I6-12)))
(check "I2-primary" (primary-ideal-p n12 ring12 I2-12))
(check "I3-primary" (primary-ideal-p n12 ring12 I3-12))

(define assoc6 (associated-primes n12 I6-12))
(check "assoc-primes-of-6-size-2" (= (length assoc6) 2))
(check "iso-primes-of-6-size-2" (= (length (isolated-primes n12 I6-12)) 2))
(check "embedded-primes-of-6-empty" (= (length (embedded-primes n12 I6-12)) 0))

(check "radical-I4-is-I2" (equal (radical-of n12 I4-12) I2-12))
(check "I6-equals-cap-I2-I3" (equal I6-12 (cap I2-12 I3-12)))

(print "--- Z/72Z Primary Decomposition ---")
(define n72 72)
(define ring72 (list-to-set (seq 0 71)))

(define I4-72 (ideal-from-divisor n72 4))
(define I2-72 (ideal-from-divisor n72 2))
(define I9-72 (ideal-from-divisor n72 9))
(define I3-72 (ideal-from-divisor n72 3))
(define I12-72 (ideal-from-divisor n72 12))
(define I6-72 (ideal-from-divisor n72 6))

(check "I4-72-primary" (primary-ideal-p n72 ring72 I4-72))
(check "I9-72-primary" (primary-ideal-p n72 ring72 I9-72))
(check "radical-I4-72-is-I2" (equal (radical-of n72 I4-72) I2-72))
(check "radical-I9-72-is-I3" (equal (radical-of n72 I9-72) I3-72))

(define assoc12 (associated-primes n72 I12-72))
(check "assoc-primes-of-12-size-2" (= (length assoc12) 2))
(check "radical-12-is-6" (equal (radical-of n72 I12-72) I6-72))

(define cap-23 (cap I2-72 I3-72))
(check "cap-2-3-is-6" (equal cap-23 I6-72))
(check "radical-equals-intersection-containing-primes"
  (equal (radical-of n72 I12-72) cap-23))

(print "=== AM Ch4 Done ===")
