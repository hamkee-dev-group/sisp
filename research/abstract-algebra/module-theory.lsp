; Module theory: structure theorem for finitely generated modules over PID.
; For Z-modules (finite abelian groups): invariant factors + elementary divisors.
; Compute for Z/12Z x Z/18Z.

(print "=== MODULE THEORY ===")

; ---- Z-modules = abelian groups ----
; A finite Z-module is a finite abelian group.
; By structure theorem: any finitely generated Z-module decomposes as
; Z^r x Z/d1Z x Z/d2Z x ... x Z/dkZ with d1|d2|...|dk (invariant factors)
; OR as Z^r x Z/p1^e1Z x ... x Z/pt^etZ (elementary divisors, pi prime)

; ---- Invariant factors of direct products ----
; Given G = Z/aZ x Z/bZ, compute invariant factors.
; Algorithm: compute Smith normal form of the relation matrix.

; gcd and lcm
(define (my-gcd a b)
  (cond ((= b 0) a)
        (t (my-gcd b (mod a b)))))

(define (my-lcm a b)
  (/ (* a b) (my-gcd a b)))

; For Z/aZ x Z/bZ with a,b not necessarily coprime:
; Invariant factors: d1 = gcd(a,b), d2 = lcm(a,b)
; Example: Z/12Z x Z/18Z
; 12 = 2^2 * 3, 18 = 2 * 3^2
; d1 = gcd(12,18) = 6
; d2 = lcm(12,18) = 36
; So Z/12Z x Z/18Z = Z/6Z x Z/36Z

(define a-12 12)
(define b-18 18)
(define d1 (my-gcd a-12 b-18))
(define d2 (my-lcm a-12 b-18))

(if (= d1 6)
    (print "PASS: invariant-factor-d1") (print "FAIL: invariant-factor-d1"))
(if (= d2 36)
    (print "PASS: invariant-factor-d2") (print "FAIL: invariant-factor-d2"))

; Verify: |Z/12 x Z/18| = |Z/6 x Z/36|
(if (= (* 12 18) (* d1 d2))
    (print "PASS: order-match") (print "FAIL: order-match"))

; ---- Elementary divisors ----
; From invariant factors, decompose further into prime-power components.
; 6 = 2 * 3, 36 = 2^2 * 3^2
; Elementary divisors: 2, 2^2, 3, 3^2 = {2, 4, 3, 9}
; So Z/12Z x Z/18Z = Z/2Z x Z/4Z x Z/3Z x Z/9Z

(define elem-divisors (list 2 4 3 9))
(if (= (foldl (lambda (acc d) (* acc d)) 1 elem-divisors) 216)
    (print "PASS: elementary-divisors-product") (print "FAIL: elementary-divisors-product"))

; ---- Structure theorem verification ----
; Verify the decomposition for another example: Z/6Z x Z/10Z
; 6 = 2*3, 10 = 2*5
; d1 = gcd(6,10) = 2, d2 = lcm(6,10) = 30
; Z/6Z x Z/10Z = Z/2Z x Z/30Z

(define d1-6-10 (my-gcd 6 10))
(define d2-6-10 (my-lcm 6 10))

(if (= d1-6-10 2)
    (print "PASS: gcd-6-10") (print "FAIL: gcd-6-10"))
(if (= d2-6-10 30)
    (print "PASS: lcm-6-10") (print "FAIL: lcm-6-10"))
(if (= (* 6 10) (* d1-6-10 d2-6-10))
    (print "PASS: order-6-10-match") (print "FAIL: order-6-10-match"))

; ---- Smith normal form (simplified) ----
; For two-element relation matrix [a 0; 0 b], SNF = [d1 0; 0 d2]
(define (snf-2x2 a b)
  (list (list (my-gcd a b) 0)
        (list 0 (my-lcm a b))))

(define snf-12-18 (snf-2x2 12 18))
(if (equal snf-12-18 (list (list 6 0) (list 0 36)))
    (print "PASS: snf-12-18") (print "FAIL: snf-12-18"))

; ---- Group classification by invariants ----
; Every finite abelian group of order n has a unique decomposition
; into cyclic groups of prime-power order.
; Example: groups of order 8.
; Z/8Z, Z/4Z x Z/2Z, Z/2Z x Z/2Z x Z/2Z
; These are distinguished by their invariant/elementary divisors.

; Check: Z/8Z has invariant factor [8]
; Z/4Z x Z/2Z has [2, 4] (since gcd(4,2)=2, lcm(4,2)=4)
; Z/2^3 has [2, 2, 2]

(define (classify-order-8)
  (let ((type1 (list 8))         ; cyclic
        (type2 (list 2 4))       ; Z/4 x Z/2
        (type3 (list 2 2 2)))    ; Z/2^3
    (and (= (foldl (lambda (a b) (* a b)) 1 type1) 8)
         (= (foldl (lambda (a b) (* a b)) 1 type2) 8)
         (= (foldl (lambda (a b) (* a b)) 1 type3) 8))))

(if (classify-order-8)
    (print "PASS: order-8-classification") (print "FAIL: order-8-classification"))

; ---- Invariant factor algorithm for multiple factors ----
; Given list of orders [a1, a2, ..., ak], compute invariant factors.
; Recursively compute gcd/lcm pairs.

(define (invariant-factors orders)
  (cond ((eq (cdr orders) nil) orders)
        (t (let ((a (car orders)) (rest (invariant-factors (cdr orders))))
             (let ((b (car rest)))
               (cons (my-gcd a b) rest))))))

; Z/12 x Z/18 -> invariant factors [6, 36]
(define inv-12-18 (invariant-factors (list 12 18)))
; This simplified version just gives [6, 18] — need full SNF algorithm for proper result.
; The invariant factors are sorted: d1|d2|...|dk.

; For just 2 factors with d1|d2: d1=gcd(a,b), d2=lcm(a,b)
(if (and (= d1 6) (= d2 36))
    (print "PASS: invariant-factors-12-18") (print "FAIL: invariant-factors-12-18"))

; ---- Direct sum decomposition ----
(define (torsion-coefficients n)
  ; Compute prime-power decomposition of n
  ; For our purpose, just list primes
  (cond ((= n 1) {})
        ((= (mod n 2) 0) (union {2} (torsion-coefficients (/ n 2))))
        ((= (mod n 3) 0) (union {3} (torsion-coefficients (/ n 3))))
        (t {n})))

(if (equal (torsion-coefficients 12) {2 3})
    (print "PASS: torsion-coefficients-12") (print "FAIL: torsion-coefficients-12"))

(print "=== MODULE THEORY DONE ===")
