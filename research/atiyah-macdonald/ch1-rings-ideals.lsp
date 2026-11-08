; ============================================================
; Atiyah-Macdonald Ch1: Rings and Ideals (finite computable)
; nilradical, Jacobson radical, operations on ideals.
; nilradical = intersection of all primes. Verified on Z/nZ.
; ============================================================

(print "=== AM Ch1: Rings and Ideals ===")

; ---------- ring operations on Z/nZ ----------
(define (r+ n a b) (mod (+ a b) n))
(define (r* n a b) (mod (* a b) n))
(define (rneg n a) (mod (- 0 a) n))
(define (pow-mod n a k)
  (cond ((= k 0) 1)
        (t (mod (* a (pow-mod n a (- k 1))) n))))

; ---------- divisors ----------
(define (divides n d) (= (mod n d) 0))
(define (divisors n)
  (labels (((go k acc)
    (cond ((> k n) (reverse acc))
          ((divides n k) (go (+ k 1) (cons k acc)))
          (t (go (+ k 1) acc)))))
    (go 1 nil)))

; ---------- ideal dZ/nZ from divisor d ----------
(define (ideal-from-divisor n d)
  (list-to-set
   (labels (((go k acc)
     (cond ((>= k n) (reverse acc))
           (t (go (+ k d) (cons k acc))))))
     (go 0 nil))))

; ---------- all ideals ----------
(define (all-ideals n)
  (list-to-set (map (lambda (d) (ideal-from-divisor n d)) (divisors n))))

; ---------- ideal predicate ----------
(define (ideal-p ring add mul neg zero s)
  (and (in zero s)
    (every (lambda (a)
      (every (lambda (b) (in (add a b) s)) s)) s)
    (every (lambda (a) (in (neg a) s)) s)
    (every (lambda (a)
      (every (lambda (r) (in (mul r a) s)) ring)) s)))

; ---------- prime ideal test ----------
(define (prime-ideal-p ring mul zero p)
  (and (not (equal p ring))
       (every (lambda (a)
         (every (lambda (b)
           (or (notin (mul a b) p) (in a p) (in b p))) ring)) ring)))

; ---------- maximal ideal test ----------
(define (maximal-ideal-p ring p ideals)
  (and (not (equal p ring))
       (not (any (lambda (q)
         (and (not (equal q p))
              (not (equal q ring))
              (subset p q))) ideals))))

; ---------- nilradical = {x : x^k=0 for some k} ----------
(define (nilpotent-p n a)
  (labels (((try-pow k)
    (cond ((>= k n) nil)
          ((= (pow-mod n a k) 0) t)
          (t (try-pow (+ k 1))))))
    (try-pow 1)))

(define (nilradical n)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter (lambda (a) (nilpotent-p n a)) ring)))

; ---------- Jacobson radical = intersection of all maximal ideals ----------
(define (jacobson-radical n)
  (let ((ideals (all-ideals n))
        (ring (list-to-set (seq 0 (- n 1)))))
    (let ((maximals (filter
          (lambda (p) (maximal-ideal-p ring p ideals)) ideals)))
      (foldl (lambda (acc m) (cap acc m))
        ring (set-to-list maximals)))))

; ---------- intersection of all prime ideals ----------
(define (intersection-of-primes n)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (let ((primes (filter
          (lambda (p) (prime-ideal-p ring (lambda (a b) (mod (* a b) n)) 0 p))
          (all-ideals n))))
      (foldl (lambda (acc p) (cap acc p))
        ring (set-to-list primes)))))

; ---------- ideal sum I+J ----------
(define (ideal-sum n I J)
  (list-to-set
   (foldl (lambda (acc x)
     (foldl (lambda (acc2 y) (cons (mod (+ x y) n) acc2))
       acc (set-to-list J)))
     nil (set-to-list I))))

; ---------- ideal product I*J ----------
(define (ideal-product n I J)
  (list-to-set
   (foldl (lambda (acc x)
     (foldl (lambda (acc2 y) (cons (mod (* x y) n) acc2))
       acc (set-to-list J)))
     nil (set-to-list I))))

; ---------- colon ideal (I:J) = {r : rJ ⊆ I} ----------
(define (colon-ideal n I J)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter (lambda (r)
      (every (lambda (x) (in (mod (* r x) n) I)) J)) ring)))

; ---------- radical of an ideal r(I) = {x : x^k ∈ I for some k} ----------
(define (radical-of n I)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter (lambda (a)
      (labels (((try-pow k)
        (cond ((>= k n) nil)
              ((in (pow-mod n a k) I) t)
              (t (try-pow (+ k 1))))))
        (try-pow 1))) ring)))

; ========== Z/30Z VERIFICATION ==========
(print "--- Z/30Z ---")
(define n30 30)
(define ring30 (list-to-set (seq 0 29)))

(define I30-1 (ideal-from-divisor n30 1))
(define I30-2 (ideal-from-divisor n30 2))
(define I30-3 (ideal-from-divisor n30 3))
(define I30-5 (ideal-from-divisor n30 5))
(define I30-6 (ideal-from-divisor n30 6))
(define I30-10 (ideal-from-divisor n30 10))
(define I30-15 (ideal-from-divisor n30 15))
(define I30-0 (ideal-from-divisor n30 30))

(check "Z30-8-divisors" (= (length (all-ideals n30)) 8))

; --- ideal predicate ---
(check "I30-2-is-ideal" (ideal-p ring30 (lambda (a b) (mod (+ a b) 30))
  (lambda (a b) (mod (* a b) 30)) (lambda (a) (mod (- 0 a) 30)) 0 I30-2))

; --- prime ideals: (2),(3),(5) ---
(define primes30 (filter
  (lambda (p) (prime-ideal-p ring30 (lambda (a b) (mod (* a b) 30)) 0 p))
  (all-ideals n30)))
(check "Z30-3-primes" (= (length primes30) 3))
(check "primes-are-2-3-5"
  (equal primes30 (list-to-set (list I30-2 I30-3 I30-5))))

; --- nilradical of Z/30Z: {0} (30 is squarefree) ---
(define nil30 (nilradical n30))
(check "nilradical-is-zero" (equal nil30 {0}))

; --- Jacobson radical = intersection of maximals ---
(define jac30 (jacobson-radical n30))
(check "jacobson-is-zero" (equal jac30 {0}))

; --- nilradical = intersection of all primes ---
(define inter-primes30 (intersection-of-primes n30))
(check "nilradical-equals-intersection-of-primes" (equal nil30 inter-primes30))

; --- ideal sum ---
(check "sum-2+3-equals-1" (equal (ideal-sum n30 I30-2 I30-3) I30-1))
(check "sum-6+10-equals-2" (equal (ideal-sum n30 I30-6 I30-10) I30-2))

; --- ideal product ---
(check "product-2*3-equals-6" (equal (ideal-product n30 I30-2 I30-3) I30-6))
(check "product-3*5-equals-15" (equal (ideal-product n30 I30-3 I30-5) I30-15))

; --- radical r(I) = intersection of the primes containing I ---
; In Z/30Z, r(6) = (2) ∩ (3) = (6) and r(10) = (2) ∩ (5) = (10): both ideals
; are already radical, since 30 is squarefree.
(check "radical-6-is-6" (equal (radical-of n30 I30-6) I30-6))
(check "radical-10-is-10" (equal (radical-of n30 I30-10) I30-10))

; ========== Z/12Z VERIFICATION ==========
(print "--- Z/12Z ---")
(define n12 12)
(define ring12 (list-to-set (seq 0 11)))

; nilradical = (6) = {0,6}
(define nil12 (nilradical n12))
(check "nilradical-z12-is-0-6" (equal nil12 (ideal-from-divisor n12 6)))

; Jacobson = nilradical = {0,6}
(define jac12 (jacobson-radical n12))
(check "jacobson-equals-nilradical" (equal jac12 nil12))

; nilradical = intersection of all primes = (2) ∩ (3) = (6)
(define inter-primes12 (intersection-of-primes n12))
(check "nilradical-equals-intersection-of-primes-z12" (equal nil12 inter-primes12))

; Colon ideal: (4):(2) = (2) in Z/12Z
(define colon-4-2 (colon-ideal n12 (ideal-from-divisor n12 4) (ideal-from-divisor n12 2)))
(check "colon-4-by-2-is-2" (equal colon-4-2 (ideal-from-divisor n12 2)))

(print "=== AM Ch1 Done ===")
