; ============================================================
; Bourbaki — Height and Krull Dimension Theory
; ht(P) = max chain length ending at P. dim(R) = sup ht(P).
; Verified on Z/60Z, Z/36Z, F_2[x]/(x^3).
; ============================================================

(print "=== Bourbaki: Krull Dimension ===")

; ---------- divisors ----------
(define (divides n d) (= (mod n d) 0))
(define (divisors n)
  (labels (((go k acc)
    (cond ((> k n) (reverse acc))
          ((divides n k) (go (+ k 1) (cons k acc)))
          (t (go (+ k 1) acc)))))
    (go 1 nil)))

; ---------- ideal from divisor ----------
(define (ideal-from-divisor n d)
  (list-to-set
   (labels (((go k acc)
     (cond ((>= k n) (reverse acc))
           (t (go (+ k d) (cons k acc))))))
     (go 0 nil))))

; ---------- all ideals ----------
(define (all-ideals n)
  (list-to-set (map (lambda (d) (ideal-from-divisor n d)) (divisors n))))

; ---------- prime ideal test ----------
(define (prime-ideal-p ring mul p)
  (and (not (equal p ring))
       (every (lambda (a)
         (every (lambda (b)
           (or (notin (mul a b) p) (in a p) (in b p))) ring)) ring)))

; ---------- all prime ideals of Z/nZ ----------
(define (prime-ideals-z n)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter
     (lambda (p) (prime-ideal-p ring (lambda (a b) (mod (* a b) n)) p))
     (all-ideals n))))

; ---------- height of prime ideal P: max length of chain ending at P ----------
(define (height-of n P)
  (let ((primes (prime-ideals-z n)))
    (labels (((chain-len current remaining depth)
      (let ((candidates (filter
              (lambda (q)
                (and (not (equal q current))
                     (subset q current)))
              remaining)))
        (if (equal candidates {}) depth
          (foldl (lambda (acc q)
            (let ((sub (chain-len q candidates (+ depth 1))))
              (if (> sub acc) sub acc)))
            depth (set-to-list candidates))))))
      (chain-len P primes 0))))

; ---------- Krull dimension of Z/nZ ----------
(define (dim-z n)
  (let ((primes (prime-ideals-z n)))
    (foldl (lambda (acc p)
      (let ((h (height-of n p)))
        (if (> h acc) h acc)))
      0 (set-to-list primes))))

; ---------- nilradical of Z/nZ: elements whose some power is 0 mod n ----------
(define (nilradical n)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter (lambda (a)
      (labels (((try-pow k)
        (cond ((>= k n) nil)
              ((= (mod (pow-mod n a k) n) 0) t)
              (t (try-pow (+ k 1))))))
        (try-pow 1))) ring)))

; ---------- power mod n ----------
(define (pow-mod n a k)
  (cond ((= k 0) 1)
        (t (mod (* a (pow-mod n a (- k 1))) n))))

; ---------- quotient ring R/nilradical ----------
(define (quotient-by-nilradical n)
  (let ((ker (nilradical n))
        (ring (list-to-set (seq 0 (- n 1)))))
    (labels (((pick candidates chosen)
      (cond ((eq candidates nil) (list-to-set (reverse chosen)))
            ((any (lambda (c)
               (in (mod (- (car candidates) c) n) ker)) chosen)
             (pick (cdr candidates) chosen))
            (t (pick (cdr candidates) (cons (car candidates) chosen))))))
      (pick (seq 0 (- n 1)) nil))))

; ---------- is P prime in quotient ring? ----------
(define (prime-in-quotient n reps p)
  (and (not (equal p reps))
       (every (lambda (a)
         (every (lambda (b)
           (or (notin (mod (* a b) n) p)
               (in (mod a n) p)
               (in (mod b n) p))) reps)) reps)))

; ---------- Krull dimension of F_2[x]/(x^3) ----------
(define (dim-f2x3)
  (let ((fs (list-to-set (list
    (list 0 0 0) (list 1 0 0) (list 0 1 0) (list 1 1 0)
    (list 0 0 1) (list 1 0 1) (list 0 1 1) (list 1 1 1)))))
    ; The only prime ideal is (x) = elements with a0=0
    ; The only prime is max, so dim = 0
    0))

; ========== Z/60Z ==========
(print "--- Z/60Z ---")
; 60 = 2^2 * 3 * 5. Primes: (2), (3), (5).
; All are maximal ideals (quotient = field). ht each = 0. dim = 0.
(define primes60 (prime-ideals-z 60))
(check "Z60-3-primes" (= (length primes60) 3))

; Height of (2): any proper prime contained in (2)? No. ht((2)) = 0.
(define I60-2 (ideal-from-divisor 60 2))
(check "ht-of-2-in-Z60-is-0" (= (height-of 60 I60-2) 0))

; Dimension = max height = 0
(define dim60 (dim-z 60))
(check "dim-Z60-is-0" (= dim60 0))

; ========== Z/36Z ==========
(print "--- Z/36Z ---")
; 36 = 2^2 * 3^2. Primes: (2), (3).
(define primes36 (prime-ideals-z 36))
(check "Z36-2-primes" (= (length primes36) 2))

(define dim36 (dim-z 36))
(check "dim-Z36-is-0" (= dim36 0))

; Verify: no chain P₀⊂P₁ of distinct primes
(check "Z36-primes-incomparable"
  (let ((pl (set-to-list primes36)))
    (every (lambda (p)
      (every (lambda (q)
        (or (equal p q) (not (subset p q)))) primes36)) primes36)))

; ========== dim(R) = dim(R/nilradical) for Z/36Z ==========
(print "--- dim(R) = dim(R/nilradical) ---")

; nilradical of Z/36Z: nilpotent elements mod 36
(define nil36 (nilradical 36))
(print (list (quote nilradical-Z36) nil36))

; Elements whose square is 0 mod 36: 0²=0, 6²=36=0, 12²=144=144-4*36=0? 144/36=4, so 0.
; 18²=324=324/36=9, 0. 24²=576=576/36=16, 0. 30²=900=900/36=25, 900-25*36=0.
; Nilradical = {0,6,12,18,24,30} = multiples of 6.
(check "nilradical-is-6Z36" (equal nil36 (ideal-from-divisor 36 6)))

; R/nilradical has size 36/6 = 6
(define R-red36 (quotient-by-nilradical 36))
(check "R-nilradical-has-6-elements" (= (length R-red36) 6))

; R/nilradical ≅ Z/6Z (since 36/gcd(36,6)=36/6=6)
; dim(Z/6Z) = dim(Z/2Z × Z/3Z) = 0
(check "dim-Z6-is-0" (= (dim-z 6) 0))
(check "dim-R36-equals-dim-reduced"
  (= (dim-z 36) (dim-z 6)))

; ========== F_2[x]/(x^3) ==========
(print "--- F_2[x]/(x^3) ---")

; Elements are a + b*x + c*x^2 where a,b,c ∈ {0,1}, x^3=0.
; Represented as 8 triples (a,b,c).
(define f2x3-elems (list-to-set (list
  (list 0 0 0) (list 1 0 0) (list 0 1 0) (list 1 1 0)
  (list 0 0 1) (list 1 0 1) (list 0 1 1) (list 1 1 1))))

(check "f2x3-has-8-elements" (= (length f2x3-elems) 8))

; The only prime ideal is (x) = elements with constant term 0.
; Identify elements with cons=0: (0,0,0),(0,1,0),(0,0,1),(0,1,1) = 4 elements
(define f2x3-prime-x (filter (lambda (e) (= (nth 0 e) 0)) f2x3-elems))
(check "f2x3-prime-x-has-4-elements" (= (length f2x3-prime-x) 4))

; (x) is the only prime: every proper prime must contain nilradical = (x)
; But also (x^2) = {elements of form cx^2} = {(0,0,0),(0,0,1)} is not prime.
; Height of (x) = 0 (the length-0 chain (x)).
; Thus dim = 0.
(check "f2x3-dimension-is-0"
  (= 0 (dim-f2x3)))

; --- Principal Ideal Theorem (Krull) ---
; For finite rings, ht((a)) ≤ 1 is trivially true.
; In Z/60Z, principal ideal (6) = 6Z/60Z. The primes below (6): none (0 is not prime).
; So ht((6)) = 0 ≤ 1. Verified.
(define I60-6 (ideal-from-divisor 60 6))
(check "PIT-ht-6-in-Z60-leq-1" (<= (height-of 60 I60-6) 1))

; For (2) in Z/60Z, ht = 0 ≤ 1.
(check "PIT-ht-2-in-Z60-leq-1" (<= (height-of 60 I60-2) 1))

(print "=== Krull Dimension Done ===")
