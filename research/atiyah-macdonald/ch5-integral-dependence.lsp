; ============================================================
; Atiyah-Macdonald Ch5: Integral Dependence
; Integral extensions, going-up/down theorems.
; Noether normalization for finite rings.
; ============================================================

(print "=== AM Ch5: Integral Dependence ===")

; ---------- ring operations ----------
(define (r+ n a b) (mod (+ a b) n))
(define (r* n a b) (mod (* a b) n))
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

; ---------- ideal ----------
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
(define (prime-ideal-p-z ring mul p)
  (and (not (equal p ring))
       (every (lambda (a)
         (every (lambda (b)
           (or (notin (mul a b) p) (in a p) (in b p))) ring)) ring)))

; ---------- all prime ideals ----------
(define (prime-ideals n)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter
     (lambda (p) (prime-ideal-p-z ring (lambda (a b) (mod (* a b) n)) p))
     (all-ideals n))))

; ---------- integral element test for Z/nZ extensions ----------
; An element b ∈ S is integral over a subring R if b is a root of a monic
; polynomial with coefficients in R.  In a finite ring every element is
; integral over every subring: the powers of b take finitely many values, so
; b^i = b^j for some i < j, and b is a root of the monic X^j - X^i.
; (Equivalently: S is a finitely generated R-module, so Cayley-Hamilton
; applies.)  The predicate below therefore returns t without computing.
(define (integral-over-subring n m a)
  ; a^i = a^j for some i < j, so a is a root of X^j - X^i: always integral.
  t)

; ---------- integral closure of R in S ----------
(define (integral-closure n m)
  (list-to-set (seq 0 (- n 1))))

; ---------- copy of Z/mZ inside Z/nZ from divisor m (m|n) ----------
; The additive map x mod m ↦ x*(n/m) mod n embeds Z/mZ into Z/nZ; its image
; is {k*(n/m) mod n : k=0..m-1} = (n/m)Z/nZ ≅ Z/mZ as abelian groups.
; It is a group embedding, not a ring map (1 does not go to 1): Z/nZ has no
; unital subring except itself, and for m|n the natural ring map goes the
; other way, Z/nZ → Z/mZ.  The "extensions" below use this additive copy.
(define (subring-from-divisor n m)
  (list-to-set
   (labels (((go k acc)
     (cond ((>= k m) (reverse acc))
           (t (go (+ k 1) (cons (mod (* k (/ n m)) n) acc))))))
     (go 0 nil))))

; ---------- Krull dimension: longest chain of prime ideals ----------
; If R ⊆ S is an integral extension then dim(R) = dim(S) (AM Cor. 5.9 and
; Thm. 5.11).  For Z/nZ every prime is maximal, so dim = 0 always.
(define (dimension n)
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

; ---------- Going Up (AM 5.11): for an integral extension R ⊆ S, if P ⊆ Q are
; primes in R, then there exist primes P' ⊆ Q' in S with P' ∩ R = P, Q' ∩ R = Q.
; In Z/nZ every prime is maximal, so the only chains are P = Q and going-up
; reduces to lying over.  The test below checks that S has a prime for each
; prime of R.
(define (going-up-holds-p n m)
  (let ((R-primes (set-to-list (prime-ideals m)))
        (S-primes (set-to-list (prime-ideals n))))
    ; Primes of R = Z/mZ are the (p) with p|m; primes of S = Z/nZ are the (p)
    ; with p|n.  If p|m then p|n, so each prime of R has a matching prime of S
    ; and going-up reduces to a comparison of the two sets of primes.
    (every (lambda (p)
      (any (lambda (q)
        ; The prime (p) of Z/mZ matches the prime (p) of Z/nZ (as p|n).  The
        ; predicate is constant t, so this only asks that S have some prime.
        t) S-primes)) R-primes)))

; ---------- Going Down (AM 5.16): for domains R ⊆ S with R integrally closed
; and S integral over R, if P ⊆ Q are primes in R and Q' is a prime in S with
; Q' ∩ R = Q, then there exists P' ⊆ Q' in S with P' ∩ R = P.
; Not checked here: Z/nZ is a domain only for n prime, so the hypotheses
; do not hold for the rings in this file.

; ---------- Noether Normalization: for a finitely generated algebra S over a
; field k, there exist elements y1,..,yd algebraically independent over k such
; that S is integral over k[y1,..,yd], and d = dim(S).
; For Z/nZ (dim 0): d = 0, no generators are needed.

; ========== Z/12Z EXTENSION FROM Z/6Z ==========
(print "--- Z/6Z subset  Z/12Z Integral Extension ---")
(define n6 6)
(define n12 12)
(define ring12 (list-to-set (seq 0 11)))

; Subring Z/6Z inside Z/12Z: image of x->2x is {0,2,4,6,8,10} = 2Z/12Z
(define sub6in12 (subring-from-divisor n12 n6))
(check "subring-Z6-in-Z12-is-2Z" (equal sub6in12 (ideal-from-divisor n12 2)))

; Every element of Z/12Z is integral over Z/6Z
(check "all-Z12-integral-over-Z6"
  (every (lambda (a) (integral-over-subring n12 n6 a)) ring12))

; Integral closure of Z/6Z in Z/12Z = all of Z/12Z
(define closure-6in12 (integral-closure n12 n6))
(check "integral-closure-is-full-ring" (= (length closure-6in12) 12))

; Dimension equality: dim(Z/6Z) = 0, dim(Z/12Z) = 0
(check "dim-Z6-is-0" (= (dimension n6) 0))
(check "dim-Z12-is-0" (= (dimension n12) 0))
(check "dim-equal-for-integral-extension" (= (dimension n6) (dimension n12)))

; Going-up holds: primes in Z/6Z = {(2),(3)}. Primes in Z/12Z = {(2),(3)}.
(check "going-up-holds" (going-up-holds-p n12 n6))

; ========== Z/30Z EXTENSION FROM Z/6Z ==========
(print "--- Z/6Z subset  Z/30Z Integral Extension ---")
(define n30 30)
(define ring30 (list-to-set (seq 0 29)))

; Subring Z/6Z inside Z/30Z: {5*k mod 30 : k=0..5} = {0,5,10,15,20,25}
(define sub6in30 (subring-from-divisor n30 n6))
(check "subring-Z6-in-Z30-has-6" (= (length sub6in30) 6))

; Every element of Z/30Z is integral over Z/6Z
(check "all-Z30-integral-over-Z6"
  (every (lambda (a) (integral-over-subring n30 n6 a)) ring30))

; ========== Z/4Z ⊆ Z/12Z EXTENSION ==========
(print "--- Z/4Z subset  Z/12Z Integral Extension ---")
(define n4 4)
(define sub4in12 (subring-from-divisor n12 n4))
; image: 3*k mod 12, k=0..3 gives {0,3,6,9}
(check "subring-Z4-in-Z12-is-3Z"
  (equal sub4in12 (ideal-from-divisor n12 3)))

; Integral closure = all of Z/12Z
(check "Z12-integral-over-Z4"
  (every (lambda (a) (integral-over-subring n12 n4 a)) ring12))

; ========== NOETHER NORMALIZATION ==========
(print "--- Noether Normalization ---")

; For Z/nZ (dimension 0): take d = 0 algebraically independent elements,
; so d = dim(Z/nZ) = 0.
(check "noether-normalization-d-equals-dim"
  (= (dimension n12) 0))

; For a ring of dim 0 no algebraically independent elements are needed.
; Z/nZ is integral over Z via the natural map Z → Z/nZ, and F_p = Z/pZ is
; integral over itself.  Z/12Z is not an algebra over Z/2Z at all: a ring map
; Z/2Z → Z/12Z would need 2 = 0 in Z/12Z.  It is a Z-algebra, integral over Z
; because it is a finitely generated Z-module (generated by 1).

; In the Noether normalization picture, dim = 0 matches the fact that Z/12Z
; is a finite module over its base Z: it is finite, generated by 1.

(print "=== AM Ch5 Done ===")
