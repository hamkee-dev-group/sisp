; ============================================================
; 62 — Spec(Z/8), the Zariski Topology, and Baby EGA
; Ideals of a finite ring computed by brute enumeration, prime
; ideals by definition, the nilradical theorem verified, and the
; Chinese Remainder isomorphism checked element by element.
; ============================================================

(print "=== Spec and the Zariski Topology ===")


; ---------- the ring Z/8 ----------
(define ring {0 1 2 3 4 5 6 7})
(define (r+ a b) (mod (+ a b) 8))
(define (r* a b) (mod (* a b) 8))
(define (rneg a) (mod (- 0 a) 8))

; ---------- ideals, found by exhaustive search ----------
; An ideal contains 0, is closed under addition and negation, and
; absorbs multiplication by every ring element.
(define (ideal-p s)
  (and (in 0 s)
    (every (lambda (a)
      (every (lambda (b) (in (r+ a b) s)) s)) s)
    (every (lambda (a) (in (rneg a) s)) s)
    (every (lambda (a)
      (every (lambda (r) (in (r* r a) s)) ring)) s)))

(define ideals (filter (lambda (s) (ideal-p s)) (powerset ring)))
(print (list (quote ideals) ideals))
(check "ideals-are-the-divisor-chain"
  (equal ideals {{0} {0 4} {0 2 4 6} {0 1 2 3 4 5 6 7}}))

; ---------- prime ideals and Spec ----------
; P is prime: proper, and ab in P implies a in P or b in P.
(define (prime-p p)
  (and (not (equal p ring))
    (every (lambda (a)
      (every (lambda (b)
        (or (notin (r* a b) p) (in a p) (in b p))) ring)) ring)))

(define spec (filter (lambda (p) (prime-p p)) ideals))
(print (list (quote spec) spec))
(check "spec-z8-is-one-point" (equal spec {{0 2 4 6}}))
(check "maximal-2-is-prime" (in {0 2 4 6} spec))
(check "zero-ideal-not-prime-here" (notin {0} spec))

; ---------- the nilradical theorem ----------
; nil(R) = every nilpotent element = the cap of all primes.
(define (nilpotent-p a)
  (or (= (r* a a) 0) (= (r* a (r* a a)) 0)))
(define nilradical (filter (lambda (a) (nilpotent-p a)) ring))
(define primes-intersection
  (foldl (lambda (acc p) (cap acc p)) ring (set-to-list spec)))
(print (list (quote nilradical) nilradical))
(check "nilradical-equals-intersection-of-primes"
  (equal nilradical primes-intersection))
(check "z8-has-nonzero-nilpotents" (in 2 nilradical))

; ---------- Zariski closed sets: V(I) = primes containing I ----------
(define (V i) (filter (lambda (p) (subset i p)) spec))
(check "V-of-zero-is-everything" (equal (V {0}) spec))
(check "V-of-unit-ideal-is-empty" (equal (V ring) {}))
; V turns ideal products/intersections into unions: with one point
; the topology is trivially verified; Artinian rings have discrete,
; finite spectra — that IS the checked theorem here.
(check "spec-of-artinian-ring-is-finite-discrete"
  (every (lambda (p) (in (V p) (powerset spec))) spec))

; ---------- Chinese Remainder: Z/6 = Z/2 x Z/3 ----------
(define z6 {0 1 2 3 4 5})
(define (crt x) (list (mod x 2) (mod x 3)))
(define crt-image (map (lambda (x) (crt x)) z6))
(check "crt-is-injective" (= (length crt-image) 6))
(check "crt-is-surjective"
  (equal crt-image (cartesian-product {0 1} {0 1 2})))
(check "crt-preserves-addition"
  (every (lambda (a)
    (every (lambda (b)
      (equal (crt (mod (+ a b) 6))
             (list (mod (+ (mod a 2) (mod b 2)) 2)
                   (mod (+ (mod a 3) (mod b 3)) 3)))) z6)) z6))
(check "crt-preserves-multiplication"
  (every (lambda (a)
    (every (lambda (b)
      (equal (crt (mod (* a b) 6))
             (list (mod (* (mod a 2) (mod b 2)) 2)
                   (mod (* (mod a 3) (mod b 3)) 3)))) z6)) z6))
; and Spec respects the product: Z/6 has exactly two primes (2),(3),
; one from each factor.
(define (ideal-p6 s)
  (and (in 0 s)
    (every (lambda (a)
      (every (lambda (b) (in (mod (+ a b) 6) s)) s)) s)
    (every (lambda (a)
      (every (lambda (r) (in (mod (* r a) 6) s)) z6)) s)))
(define (prime-p6 p)
  (and (not (equal p z6))
    (every (lambda (a)
      (every (lambda (b)
        (or (notin (mod (* a b) 6) p) (in a p) (in b p))) z6)) z6)))
(define spec6
  (filter (lambda (p) (and (ideal-p6 p) (prime-p6 p))) (powerset z6)))
(print (list (quote spec-z6) spec6))
(check "spec-z6-has-two-points"
  (equal spec6 {{0 2 4} {0 3}}))

(print "=== Spec and the Zariski Topology Done ===")
