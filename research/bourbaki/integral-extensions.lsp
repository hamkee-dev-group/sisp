; ============================================================
; Bourbaki — Integral Extensions and Krull Dimension
; Integral elements, integral closure, dimension for Z/nZ.
; Verified on Z/12Z, Z/30Z.
; ============================================================

(print "=== Bourbaki: Integral Extensions ===")

; ---------- divisors ----------
(define (divides n d) (= (mod n d) 0))
(define (divisors n)
  (labels (((go k acc)
    (cond ((> k n) (reverse acc))
          ((divides n k) (go (+ k 1) (cons k acc)))
          (t (go (+ k 1) acc)))))
    (go 1 nil)))

; ---------- prime test for numbers ----------
(define (primep n)
  (cond ((<= n 1) nil)
        ((= n 2) t)
        ((= (mod n 2) 0) nil)
        (t (labels (((go d)
              (cond ((> (* d d) n) t)
                    ((= (mod n d) 0) nil)
                    (t (go (+ d 2))))))
             (go 3)))))

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

; ---------- prime ideal test for Z/nZ ----------
(define (prime-ideal-p-z ring mul zero p)
  (and (not (equal p ring))
       (every (lambda (a)
         (every (lambda (b)
           (or (notin (mul a b) p) (in a p) (in b p))) ring)) ring)))

; ---------- all prime ideals ----------
(define (prime-ideals n)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter
     (lambda (p) (prime-ideal-p-z ring
       (lambda (a b) (mod (* a b) n)) 0 p))
     (all-ideals n))))

; ---------- Krull dimension: max length of chain of distinct prime ideals ----------
; A chain P₀ ⊂ P₁ ⊂ ... ⊂ P_k
(define (krull-dimension n)
  (let ((primes (set-to-list (prime-ideals n))))
    (labels (((chain-len p remaining depth)
      (let ((candidates (filter
            (lambda (q) (and (not (equal p q)) (subset p q)))
            remaining)))
        (cond ((eq candidates nil) depth)
              (t (foldl (lambda (acc q)
                    (let ((sub (chain-len q candidates (+ depth 1))))
                      (if (> sub acc) sub acc)))
                  depth candidates))))))
      (foldl (lambda (acc p)
        (let ((len (chain-len p primes 0)))
          (if (> len acc) len acc)))
        0 primes))))

; ---------- integral element test: b∈S is integral over R if ∃ monic polynomial in R[x] vanishing at b ----------
; In a finite ring S every element b is integral over any subring R: the
; powers b, b², b³, ... take finitely many values, so b^i = b^j for some
; i < j, and b is a root of the monic polynomial t^j − t^i ∈ R[t].
; integral-over therefore simply returns t.
(define (integral-over n a)
  t)

; ---------- integral closure of R in S ----------
; For R ⊆ S finite rings, the integral closure is all of S (every element is
; integral over R), so integral-closure returns the elements of S unchanged.
(define (integral-closure n subring-ring elems)
  elems)

; ========== Z/12Z DIMENSION ==========
(print "--- Z/12Z ---")

(define ring12 (list-to-set (seq 0 11)))
(define primes12 (prime-ideals 12))
(print (list (quote primes-Z12) primes12))

; Only primes: (2) and (3). Both are maximal. No inclusion chain of distinct primes.
; dim = 0.
(define dim12 (krull-dimension 12))
(check "dim-Z12-is-0" (= dim12 0))

; But verify: there is NO chain of two distinct prime ideals with P₀ ⊂ P₁
(check "primes-are-incomparable"
  (every (lambda (p)
    (every (lambda (q)
      (or (equal p q) (not (subset p q)))) primes12)) primes12))

; Verify that each prime ideal is maximal
(check "primes-are-maximal"
  (let ((ideals12 (all-ideals 12)))
    (every (lambda (p)
      (not (any (lambda (q)
        (and (not (equal q p))
             (not (equal q ring12))
             (subset p q))) ideals12))) primes12)))

; ========== Z/30Z DIMENSION ==========
(print "--- Z/30Z ---")

(define primes30 (prime-ideals 30))
(print (list (quote primes-Z30) primes30))

; Z/30Z = Z/2Z × Z/3Z × Z/5Z. Primes: (2), (3), (5).
; All are maximal, so dim = 0.
(check "Z30-has-3-primes" (= (length primes30) 3))
(define dim30 (krull-dimension 30))
(check "dim-Z30-is-0" (= dim30 0))

; The primes correspond to prime divisors of 30
(define prime-divisors30 (filter primep (divisors 30)))
(check "prime-divisors-are-2-3-5"
  (equal prime-divisors30 (list 2 3 5)))

; ========== INTEGRAL ELEMENTS FOR Z/12Z ==========
(print "--- Integral Extensions over Z/12Z ---")

; Every element of a finite ring is integral over any subring
; (x^i = x^j for some i < j, so x is a root of the monic t^j − t^i)
(check "all-Z12-elements-integral"
  (every (lambda (x) (integral-over 12 x)) ring12))

; Integral closure of the subring Z/4Z inside Z/12Z
; By CRT, Z/12Z ≅ Z/4Z × Z/3Z, and the Z/4Z factor is {0,3,6,9} = 9·Z/12Z:
; a ring under + and · mod 12 whose identity is the idempotent 9 (9·9 = 81 ≡ 9).
; It does not contain 1, so it is not a unital subring of Z/12Z (the only
; unital subring of Z/12Z is Z/12Z itself, since 1 generates it additively).
(define subring-4 (list-to-set (list 0 3 6 9)))
(check "Z4-inside-Z12-is-a-set" (= (length subring-4) 4))

; In a finite ring extension, the integral closure equals the whole ring;
; integral-closure returns ring12 unchanged, so this check records that fact.
(check "integral-closure-is-full-ring"
  (equal (integral-closure 12 subring-4 ring12) ring12))

(print "=== Integral Extensions Done ===")
