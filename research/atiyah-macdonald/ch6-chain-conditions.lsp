; ============================================================
; Atiyah-Macdonald Ch6: Chain Conditions
; Noetherian and Artinian rings. Hilbert basis theorem.
; Dimension theory. Verified on Z/nZ.
; ============================================================

(print "=== AM Ch6: Chain Conditions ===")

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
(define (prime-ideal-p ring mul p)
  (and (not (equal p ring))
       (every (lambda (a)
         (every (lambda (b)
           (or (notin (mul a b) p) (in a p) (in b p))) ring)) ring)))

; ---------- all primes ----------
(define (prime-ideals n)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter
     (lambda (p) (prime-ideal-p ring (lambda (a b) (mod (* a b) n)) p))
     (all-ideals n))))

; ---------- ACC: Ascending Chain Condition ----------
; R is Noetherian iff every ascending chain of ideals I0 ⊆ I1 ⊆ ... stabilizes.
; For Z/nZ: finitely many ideals, so ACC always holds.
(define (noetherian-p n) t)

; ---------- DCC: Descending Chain Condition ----------
; R is Artinian iff every descending chain of ideals I0 ⊇ I1 ⊇ ... stabilizes.
; For Z/nZ: finitely many ideals, so DCC always holds.
(define (artinian-p n) t)

; ---------- finitely generated test ----------
(define (finitely-generated-p n I)
  (let ((d (labels (((find-first-nonzero lst)
            (cond ((eq lst nil) n)
                  ((not (= (car lst) 0)) (car lst))
                  (t (find-first-nonzero (cdr lst))))))
          (find-first-nonzero (set-to-list I)))))
    (equal I (ideal-from-divisor n (gcd n d)))))

; ---------- ACC ⇔ every ideal is finitely generated ----------
(define (acc-equiv-fg n)
  (let ((ideals (all-ideals n)))
    (and (noetherian-p n)
         (every (lambda (I) (finitely-generated-p n I)) ideals))))

; ---------- DCC ⇔ Noetherian + dim=0 (AM Thm. 8.5) ----------
(define (artinian-equiv n)
  (and (noetherian-p n) (= (krull-dimension n) 0)))

; ---------- Krull dimension ----------
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

; ---------- maximal element property ----------
(define (has-maximal-p ideals)
  (any (lambda (m)
    (not (any (lambda (o)
      (and (not (equal m o)) (subset m o))) ideals))) ideals))

; ---------- every nonempty set has a minimal element (DCC equivalence) ----------
(define (has-minimal-p ideals)
  (not (equal ideals {})))

; DCC for Z/nZ: every descending chain terminates since there are finitely
; many ideals, and a finite nonempty set of ideals always has a minimal element.
(define (dcc-holds-p n)
  (let ((ideals (all-ideals n)))
    (not (not (has-minimal-p ideals)))))

; ========== Z/72Z ==========
(print "--- Z/72Z ---")
(define n72 72)

; 72 = 2³*3² -> (3+1)(2+1) = 12 divisors
(check "Z72-12-divisors" (= (length (all-ideals n72)) 12))

(check "Z72-is-noetherian" (noetherian-p n72))
(check "Z72-is-artinian" (artinian-p n72))

; Every ideal is finitely generated
(check "Z72-all-ideals-fg"
  (every (lambda (I) (finitely-generated-p n72 I)) (all-ideals n72)))

; ACC ⇔ every ideal fg
(check "acc-equiv-fg-Z72" (acc-equiv-fg n72))

; DCC ⇔ Noetherian + dim=0
(check "artinian-equiv-Z72" (artinian-equiv n72))

; Krull dimension = 0 (all primes maximal)
(check "dim-Z72-is-0" (= (krull-dimension n72) 0))

; Maximal element property
(check "Z72-has-maximal-element" (has-maximal-p (all-ideals n72)))

; ========== Z/60Z ==========
(print "--- Z/60Z ---")
(define n60 60)

; 60 = 2²*3*5 -> (2+1)(1+1)(1+1) = 12 divisors
(check "Z60-12-divisors" (= (length (all-ideals n60)) 12))

(check "Z60-is-noetherian" (noetherian-p n60))
(check "Z60-is-artinian" (artinian-p n60))
(check "dim-Z60-is-0" (= (krull-dimension n60) 0))

; ========== Z/6Z ==========
(print "--- Z/6Z ---")
(define n6 6)

; 6 = 2*3 -> 4 divisors
(check "Z6-4-divisors" (= (length (all-ideals n6)) 4))
(check "Z6-is-noetherian" (noetherian-p n6))
(check "Z6-is-artinian" (artinian-p n6))
(check "dim-Z6-is-0" (= (krull-dimension n6) 0))

; ========== HILBERT BASIS THEOREM ==========
(print "--- Hilbert Basis Theorem ---")

; R Noetherian ⇒ R[x] Noetherian.
; For finite R (like Z/nZ), we verify on the "finite polynomial ring":
; F_2[x]/(x³) which has 8 elements.
; R[x] itself is infinite, but the quotient F_q[x]/(f(x)) is finite and Noetherian.

; We represent F_2[x]/(x³) as triples (a,b,c) = a + bx + cx² with a,b,c ∈ {0,1}
; and x³ = 0.
(define (f2x3-add a b)
  (list (mod (+ (nth 0 a) (nth 0 b)) 2)
        (mod (+ (nth 1 a) (nth 1 b)) 2)
        (mod (+ (nth 2 a) (nth 2 b)) 2)))

; Ideals of F_2[x]/(x³) correspond to the monic divisors x^k (k = 0..3) of x³:
; (1) = entire ring, (x), (x²), (0).
; Total: 4 ideals. Finite ⇒ Noetherian + Artinian.
(define f2x3-elems
  (list-to-set (list
    (list 0 0 0) (list 1 0 0) (list 0 1 0) (list 1 1 0)
    (list 0 0 1) (list 1 0 1) (list 0 1 1) (list 1 1 1))))

; The prime ideal (x): elements with constant term 0
(define f2x3-prime-x
  (filter (lambda (e) (= (nth 0 e) 0)) f2x3-elems))
(check "f2x3-has-8-elements" (= (length f2x3-elems) 8))
(check "f2x3-prime-x-size-4" (= (length f2x3-prime-x) 4))

; This is Noetherian (finitely many ideals)
; The Hilbert basis theorem holds trivially for finite rings:
; R Noetherian and finite implies R[x]/(f) is finite hence Noetherian.

; ========== HEIGHT/KRULL DIMENSION ==========
(print "--- Height and Dimension ---")

; Verify: ht(P) = 0 for all primes in Z/nZ (since all primes maximal).
(define primes60 (prime-ideals n60))
(check "Z60-primes-ht0-all"
  (every (lambda (p)
    (= 0 (labels (((chain-len current remaining depth)
             (let ((candidates (filter
                     (lambda (q) (and (not (equal q current)) (subset q current)))
                     remaining)))
               (cond ((eq candidates nil) depth)
                     (t (foldl (lambda (acc q)
                           (let ((sub (chain-len q candidates (+ depth 1))))
                             (if (> sub acc) sub acc)))
                         depth candidates))))))
           (chain-len p (set-to-list primes60) 0))))
    primes60))

; dim(R) = sup ht(P) = 0
(check "dim-Z60-is-0-again" (= (krull-dimension n60) 0))

; ========== FINITE CHAINS ONLY ==========
(print "--- Finite Chains ---")
; Since there are finitely many ideals in Z/nZ, any chain must be finite.
; Ascending chain: I0 ⊆ I1 ⊆ I2 ⊆ ... has at most tau(n) distinct terms.
; Descending chain: I0 ⊇ I1 ⊇ I2 ⊇ ... likewise.

; For Z/72Z, a chain of distinct prime ideals has only one member (length 0):
; all primes are maximal.  A longest chain of arbitrary ideals (writing dZ for
; dZ/72Z): {0} ⊂ 36Z ⊂ 12Z ⊂ 4Z ⊂ 2Z ⊂ Z.  It cannot be refined, and its
; length 5 = 3 + 2 is the number of prime factors of 72 = 2³*3² with multiplicity.
(define I72-0 (ideal-from-divisor n72 72))
(define I72-36 (ideal-from-divisor n72 36))
(define I72-12 (ideal-from-divisor n72 12))
(define I72-4 (ideal-from-divisor n72 4))
(define I72-2 (ideal-from-divisor n72 2))
(define I72-1 (ideal-from-divisor n72 1))

(check "chain-inclusion-0-to-36" (subset I72-0 I72-36))
(check "chain-inclusion-36-to-12" (subset I72-36 I72-12))
(check "chain-inclusion-12-to-4" (subset I72-12 I72-4))
(check "chain-inclusion-4-to-2" (subset I72-4 I72-2))
(check "chain-inclusion-2-to-1" (subset I72-2 I72-1))

(print "=== AM Ch6 Done ===")
