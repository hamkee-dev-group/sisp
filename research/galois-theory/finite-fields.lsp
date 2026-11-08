; ============================================================
; galois-theory/finite-fields.lsp — Finite Fields
; Classification: one for each prime power. Frobenius automorphism.
; Trace and norm. Cyclotomic polynomials.
; ============================================================

(print "=== Finite Fields ===")


; ---------- Classification: F_p exists for every prime p ----------
(define (divides n d) (= (mod n d) 0))
(define (prime-p n)
  (and (> n 1)
    (every (lambda (d) (or (= d 1) (= d n) (not (divides n d))))
      (list-to-set (seq 2 (- n 1))))))

(check "2-is-prime" (prime-p 2))
(check "3-is-prime" (prime-p 3))
(check "4-is-not-prime" (not (prime-p 4)))
(check "7-is-prime" (prime-p 7))

; F_p = Z/pZ. Check the field axioms directly, for p = 3 and p = 11.
(define (field-of-prime-p p)
  (let ((elts (list-to-set (seq 0 (- p 1)))))
    (and (every (lambda (a)
           (every (lambda (b)
             (every (lambda (c)
               (and (= (mod (+ (mod (+ a b) p) c) p) (mod (+ a (mod (+ b c) p)) p))
                    (= (mod (+ a b) p) (mod (+ b a) p)))) elts)) elts)) elts)
         (every (lambda (a) (and (= (mod (+ a 0) p) a) (in (mod (- p a) p) elts))) elts)
         (every (lambda (a)
           (every (lambda (b) (= (mod (* a b) p) (mod (* b a) p))) elts)) elts)
         (every (lambda (a) (and (= (mod (* a 1) p) a)
           (or (= a 0) (any (lambda (b) (= (mod (* a b) p) 1)) elts)))) elts))))

(check "f3-is-a-field" (field-of-prime-p 3))
(check "f11-is-a-field" (field-of-prime-p 11))


; ---------- F_{p^n} exists and is unique up to isomorphism ----------
; The order of any finite field is a prime power.
; Conversely, for each prime power q = p^n, there exists a unique
; field F_q (up to isomorphism).
; F_q = splitting field of x^{p^n} - x over F_p.

; ---------- polynomials over F_p, coefficient lists low degree first ----------
; nil is the zero polynomial.  Used below to CONSTRUCT F_{p^n} rather than
; assume it exists, and to factor cyclotomic polynomials.
(define (pl-nth cs i)
  (cond ((< i 0) 0) ((>= i (length cs)) 0) (t (nth i cs))))

(define (pl-trim cs)
  (labels (((go l) (cond ((eq l nil) nil)
                         ((= (car l) 0) (go (cdr l)))
                         (t (reverse l)))))
    (go (reverse cs))))

(define (pl-mul pm a b)
  (cond ((eq a nil) nil)
        ((eq b nil) nil)
        (t (let ((ln (- (+ (length a) (length b)) 1)))
             (pl-trim
               (map (lambda (i)
                      (mod (foldl (lambda (acc j)
                                    (+ acc (* (pl-nth a j) (pl-nth b (- i j)))))
                                  0 (seq 0 i)) pm))
                    (seq 0 (- ln 1))))))))

(define (pl-tuples pm d)
  (cond ((= d 0) (list nil))
        (t (foldr append nil
             (map (lambda (rest) (map (lambda (c) (append rest (list c))) (seq 0 (- pm 1))))
                  (pl-tuples pm (- d 1)))))))

(define (pl-monics pm d) (map (lambda (lo) (append lo (list 1))) (pl-tuples pm d)))

; every monic of degree d that is a product of two lower-degree monics
(define (pl-reducibles pm d)
  (cond ((<= d 1) nil)
        (t (foldr append nil
             (map (lambda (i)
                    (foldr append nil
                      (map (lambda (a) (map (lambda (b) (pl-mul pm a b)) (pl-monics pm (- d i))))
                           (pl-monics pm i))))
                  (seq 1 (- d 1)))))))

(define (pl-irreds pm d)
  (let ((red (pl-reducibles pm d)))
    (filter (lambda (g) (not (any (lambda (h) (equal g h)) red))) (pl-monics pm d))))

; F_{p^n} EXISTS exactly when F_p[x] has a monic irreducible of degree n --
; that polynomial is what the field is built from.  So search for one.
(define (field-exists-p p n) (> (length (pl-irreds p n)) 0))

(check "f4-exists" (field-exists-p 2 2))
(check "f8-exists" (field-exists-p 2 3))
(check "f9-exists" (field-exists-p 3 2))
(check "f16-exists" (field-exists-p 2 4))
(check "f25-exists" (field-exists-p 5 2))

; The count of monic irreducibles of degree n over F_p is Gauss's
; (1/n) sum_{d|n} mu(n/d) p^d.  Cross-check the brute-force search against it.
(check "irreducible-counts-match-gauss-formula"
  (and (= (length (pl-irreds 2 2)) 1)
       (= (length (pl-irreds 2 3)) 2)
       (= (length (pl-irreds 2 4)) 3)
       (= (length (pl-irreds 3 2)) 3)
       (= (length (pl-irreds 5 2)) 10)))

; negative control: there is no irreducible of degree 1 that is reducible, and
; a reducible polynomial such as x^2 = x*x is correctly rejected.
(check "reducible-polys-are-excluded"
  (and (not (any (lambda (g) (equal g (list 0 0 1))) (pl-irreds 2 2)))
       (any (lambda (g) (equal g (list 0 0 1))) (pl-monics 2 2))))


; ---------- Frobenius automorphism ----------
; Fr_p: F_{p^n} → F_{p^n}, Fr_p(x) = x^p.
; It's a field automorphism fixing F_p.
; Fr_p generates Gal(F_{p^n}/F_p) ≅ Z/nZ.

; Build F_9 = GF(3)[x]/(x²+1). x²+1 is irreducible over F_3 (0²+1=1,1²+1=2,2²+1=5≡2).
(define f3 {0 1 2})
(define f9 (cartesian-product f3 f3))
(define f9-zero (list 0 0))
(define f9-one  (list 1 0))
(define beta (list 0 1))

(define (f9-add u v)
  (list (mod (+ (nth 0 u) (nth 0 v)) 3)
        (mod (+ (nth 1 u) (nth 1 v)) 3)))

(define (f9-mul u v)
  (let ((a0 (nth 0 u)) (a1 (nth 1 u))
        (b0 (nth 0 v)) (b1 (nth 1 v)))
    (list (mod (- (* a0 b0) (* a1 b1)) 3)
          (mod (+ (* a0 b1) (* a1 b0)) 3))))

; Frobenius for F_9: Fr_3(x) = x³
(define (frob9 a) (f9-mul a (f9-mul a a)))

(check "frob9-is-additive"
  (every (lambda (a)
    (every (lambda (b)
      (equal (frob9 (f9-add a b)) (f9-add (frob9 a) (frob9 b)))) f9)) f9))

(check "frob9-is-multiplicative"
  (every (lambda (a)
    (every (lambda (b)
      (equal (frob9 (f9-mul a b)) (f9-mul (frob9 a) (frob9 b)))) f9)) f9))

(check "frob9-squared-is-identity"
  (every (lambda (a) (equal (frob9 (frob9 a)) a)) f9))

; Fixed field of Fr_3 is F_3
(check "fixed-field-frob9-is-f3"
  (= (length (filter (lambda (a) (equal (frob9 a) a)) f9)) 3))

; Fr_3 generates the Galois group Gal(F_9/F_3) ≅ Z/2Z.
; Enumerate the FULL automorphism group of F_9 over F_3 without assuming
; anything about Frobenius: an F_3-automorphism is determined by the image b of
; beta, acting as a0 + a1*beta |-> a0 + a1*b.  Keep the b that give a bijective,
; additive and multiplicative map, then check every survivor is a power of Fr_3.
(define (sigma-of b u)
  (f9-add (list (nth 0 u) 0) (f9-mul (list (nth 1 u) 0) b)))

(define (automorphism-p b)
  (and (= (length (list-to-set (map (lambda (u) (sigma-of b u)) (set-to-list f9)))) 9)
       (every (lambda (u)
         (every (lambda (w)
           (and (equal (sigma-of b (f9-add u w)) (f9-add (sigma-of b u) (sigma-of b w)))
                (equal (sigma-of b (f9-mul u w)) (f9-mul (sigma-of b u) (sigma-of b w)))))
           f9)) f9)))

(define f9-automorphisms (filter (lambda (b) (automorphism-p b)) f9))

(define (frob9-iter e u) (cond ((= e 0) u) (t (frob9-iter (- e 1) (frob9 u)))))

(check "frob9-generates-galois-group"
  (and (= (length f9-automorphisms) 2)
       ; every automorphism is Fr_3^e for some e in {0,1}
       (every (lambda (b)
         (any (lambda (e)
           (every (lambda (u) (equal (sigma-of b u) (frob9-iter e u))) f9))
           {0 1}))
         f9-automorphisms)
       ; and Fr_3 is not the identity, so it really generates a group of order 2
       (any (lambda (u) (not (equal (frob9 u) u))) f9)))

; negative control: most candidate images of beta do NOT give automorphisms
(check "not-every-map-is-an-automorphism"
  (< (length f9-automorphisms) (length f9)))


; ---------- Trace and norm ----------
; Trace: Tr_{F_q/F_p}(x) = x + x^p + x^{p²} + ... + x^{p^{n-1}}.
; Norm: N_{F_q/F_p}(x) = x · x^p · x^{p²} · ... · x^{p^{n-1}}.

; For F_9/F_3: Tr(x) = x + x³, N(x) = x · x³ = x⁴.
(define (trace-f9 a) (f9-add a (frob9 a)))
(define (norm-f9 a) (f9-mul a (frob9 a)))

(check "trace-f9-is-linear"
  (every (lambda (a)
    (every (lambda (b)
      (equal (trace-f9 (f9-add a b)) (f9-add (trace-f9 a) (trace-f9 b)))) f9)) f9))

(check "trace-f9-maps-to-f3"
  (every (lambda (a) (equal (frob9 (trace-f9 a)) (trace-f9 a))) f9))

; Norm is multiplicative: N(ab) = N(a)·N(b)
(check "norm-f9-is-multiplicative"
  (every (lambda (a)
    (every (lambda (b)
      (equal (norm-f9 (f9-mul a b)) (f9-mul (norm-f9 a) (norm-f9 b)))) f9)) f9))

(check "norm-f9-maps-to-f3"
  (every (lambda (a) (equal (frob9 (norm-f9 a)) (norm-f9 a))) f9))

; Norm of a nonzero element is nonzero.
(define f9-nonzero (filter (lambda (a) (not (equal a f9-zero))) f9))
(check "norm-of-nonzero-is-nonzero"
  (every (lambda (a) (not (equal (norm-f9 a) f9-zero))) f9-nonzero))


; ---------- For F_4/F_2, trace and norm ----------
(define f4 (cartesian-product {0 1} {0 1}))
(define f4-zero (list 0 0))
(define f4-one (list 1 0))
(define omega (list 0 1))

(define (f4-add u v)
  (list (mod (+ (nth 0 u) (nth 0 v)) 2)
        (mod (+ (nth 1 u) (nth 1 v)) 2)))

(define (f4-mul u v)
  (let ((a0 (nth 0 u)) (a1 (nth 1 u))
        (b0 (nth 0 v)) (b1 (nth 1 v)))
    (list (mod (+ (* a0 b0) (* a1 b1)) 2)
          (mod (+ (* a0 b1) (* a1 b0) (* a1 b1)) 2))))

(define (frob4 a) (f4-mul a a))
(define (trace-f4 a) (f4-add a (frob4 a)))
(define (norm-f4 a) (f4-mul a (frob4 a)))

; Trace: Tr(x) = x + x². Values: Tr(0)=0, Tr(1)=1+1=0, Tr(ω)=ω+ω², Tr(ω²)=ω²+ω.
; In F_4, ω² = ω+1. So ω+ω² = ω+(ω+1) = 1.
(check "trace-f4-is-surjective"
  (every (lambda (y) (any (lambda (a) (equal (trace-f4 a) (list y 0))) f4)) {0 1}))

(check "norm-f4-omega"
  (equal (norm-f4 omega) f4-one))


; ---------- Cyclotomic polynomials ----------
; Φ_n(x) = Π_{d|n} (x^d - 1)^{μ(n/d)}.
; Φ_n is the minimal polynomial of primitive n-th roots of unity over Q.

(define (moebius n)
  (cond ((= n 1) 1)
        ((any (lambda (p) (and (prime-p p) (divides n (* p p)))) (list-to-set (seq 2 n))) 0)
        (t (let ((k (count (lambda (p) (and (prime-p p) (divides n p))) (list-to-set (seq 2 n)))))
             (if (evenp k) 1 -1)))))

(check "moebius-6-is-1" (= (moebius 6) 1))
(check "moebius-4-is-0" (= (moebius 4) 0))
(check "moebius-2-is-neg1" (= (moebius 2) -1))

; Over F_q with gcd(q,n) = 1, Φ_n factors into φ(n)/d distinct irreducible
; polynomials, each of degree d = the order of q modulo n.
(define (order-mod q n)
  (labels (((go k)
    (cond ((> k n) nil)
          ((= (mod (pow-mod q k n) n) 1) k)
          (t (go (+ k 1))))))
    (go 1)))

(define (pow-mod q e n)
  (cond ((= e 0) 1) (t (mod (* q (pow-mod q (- e 1) n)) n))))

(check "order-of-2-mod-7-is-3" (= (order-mod 2 7) 3))
(check "order-of-3-mod-5-is-4" (= (order-mod 3 5) 4))

(define (euler-phi n)
  (count (lambda (k) (= (gcd n k) 1)) (list-to-set (seq 1 n))))

; For F_8 = F_{2^3}: the multiplicative group has order 7.
; Φ_7(x) = x⁶ + x⁵ + x⁴ + x³ + x² + x + 1 (since 7 is prime).
; Theory says it splits into φ(7)/ord_7(2) = 6/3 = 2 irreducible cubics.
; Verify by actually multiplying the two irreducible cubics over F_2 and
; comparing the product, coefficient by coefficient, with Φ_7.
(define phi7 (list 1 1 1 1 1 1 1))
(define f2-cubics (pl-irreds 2 3))

(check "phi7-factors-in-f2"
  (and (= (length f2-cubics) 2)
       (not (equal (car f2-cubics) (cadr f2-cubics)))
       (equal (pl-mul 2 (car f2-cubics) (cadr f2-cubics)) phi7)
       ; the count predicted by the theory agrees with the count found
       (= (length f2-cubics) (truncate (/ (euler-phi 7) (order-mod 2 7))))))

; For F_9 = F_{3^2}: the multiplicative group has order 8.
; Φ_8(x) = x⁴ + 1. Over F_3, order of 3 mod 8: 3¹=3≠1, 3²=9≡1. So order = 2.
; Φ_8 should factor into φ(8)/2 = 4/2 = 2 irreducible quadratics over F_3.
; Find the factorisation by exhaustive search over pairs of irreducibles.
(define phi8 (list 1 0 0 0 1))
(define f3-quadratics (pl-irreds 3 2))
(define phi8-factorizations
  (filter (lambda (pr) (equal (pl-mul 3 (car pr) (cdr pr)) phi8))
          (foldr append nil
            (map (lambda (a) (map (lambda (b) (cons a b)) f3-quadratics))
                 f3-quadratics))))

(check "phi8-factors-in-f3"
  (and (= (length f3-quadratics) 3)
       ; exactly one unordered factorisation, so two ordered ones
       (= (length phi8-factorizations) 2)
       ; into two DISTINCT irreducible quadratics
       (not (equal (car (car phi8-factorizations)) (cdr (car phi8-factorizations))))
       (= 2 (truncate (/ (euler-phi 8) (order-mod 3 8))))))

; negative control: Φ_8 does NOT factor into two irreducible quadratics over
; F_2 -- over F_2, x^4+1 = (x+1)^4 and (x^2+x+1)^2 = x^4+x^2+1 is not it.
(check "phi8-does-not-split-into-distinct-quadratics-over-f2"
  (= (length (filter (lambda (pr) (equal (pl-mul 2 (car pr) (cdr pr)) phi8))
               (foldr append nil
                 (map (lambda (a) (map (lambda (b) (cons a b)) (pl-irreds 2 2)))
                      (pl-irreds 2 2)))))
     0))


; ---------- All finite fields classified ----------
(define (classify-finite-field q)
  (labels (((find-p n)
    (cond ((prime-p n) (cond ((= (mod q n) 0) n) (t (find-p (+ n 1)))))
          (t (find-p (+ n 1))))))
    (let ((p (find-p 2)))
      (labels (((find-n acc cur)
        (cond ((= cur q) acc)
              (t (find-n (+ acc 1) (* cur p))))))
        (let ((exp (find-n 0 1)))
          ; Build each pair with list: (quote char p) would quote only the
          ; symbol char and drop p.
          (list (quote exists-unique)
                (list (quote char) p)
                (list (quote degree) exp)))))))

(check "f9-has-char-3-degree-2"
  (equal (list (quote exists-unique)
               (list (quote char) 3)
               (list (quote degree) 2))
         (classify-finite-field 9)))


; ---------- Multiplicative group is cyclic ----------
; For any finite field F_q, (F_q)^× is cyclic of order q-1.
; Number of generators (primitive elements) = φ(q-1).

; (euler-phi is defined above, with the cyclotomic polynomials.)
(check "phi-6-is-2" (= (euler-phi 6) 2))
(check "phi-7-is-6" (= (euler-phi 7) 6))
(check "phi-8-is-4" (= (euler-phi 8) 4))

; The primitive-element counts are verified by brute force at the end of the
; file, once the power functions for F_9 and F_8 are in scope.


; ---------- Fermat's little theorem in finite fields ----------
; For all x ∈ F_q: x^q = x.
; For all nonzero x ∈ F_q: x^{q-1} = 1.

(define (f9-pow a n)
  (cond ((= n 0) f9-one)
        (t (f9-mul a (f9-pow a (- n 1))))))

(check "x^9-equals-x-in-f9"
  (every (lambda (a) (equal (f9-pow a 9) a)) f9))

(check "x^8-equals-1-for-nonzero-f9"
  (every (lambda (a) (equal (f9-pow a 8) f9-one)) f9-nonzero))


; ---------- Primitive elements, counted by brute force ----------
; A primitive element of F_q is a generator of the cyclic group F_q^x, i.e. an
; element of multiplicative order exactly q-1.  Count them directly and compare
; with φ(q-1), which is computed by a completely unrelated routine (gcd counts).

(define (f9-mult-order a)
  (labels (((go k) (cond ((> k 8) 0)
                         ((equal (f9-pow a k) f9-one) k)
                         (t (go (+ k 1))))))
    (go 1)))

(check "f9-primitive-elts-count"
  (= (length (filter (lambda (a) (= (f9-mult-order a) 8)) f9-nonzero))
     (euler-phi 8)))

; every element order must divide 8, and the orders that occur are 1,2,4,8
(check "f9-element-orders-divide-8"
  (and (every (lambda (a) (divides 8 (f9-mult-order a))) f9-nonzero)
       (equal (list-to-set (map (lambda (a) (f9-mult-order a)) (set-to-list f9-nonzero)))
              (list-to-set (list 1 2 4 8)))))

; --- F_8 = GF(2)[x]/(x^3+x+1), needed for the F_8 count ---
; cartesian-product NESTS, so flatten to genuine triples before doing arithmetic.
(define f8 (list-to-set
             (map (lambda (p) (flatten p))
                  (set-to-list (cartesian-product {0 1} (cartesian-product {0 1} {0 1}))))))
(define f8-zero (list 0 0 0))
(define f8-one  (list 1 0 0))

(define (f8-mul u v)
  (let ((a0 (nth 0 u)) (a1 (nth 1 u)) (a2 (nth 2 u))
        (b0 (nth 0 v)) (b1 (nth 1 v)) (b2 (nth 2 v)))
    (let ((c0 (mod (* a0 b0) 2))
          (c1 (mod (+ (* a0 b1) (* a1 b0)) 2))
          (c2 (mod (+ (* a0 b2) (* a1 b1) (* a2 b0)) 2))
          (c3 (mod (+ (* a1 b2) (* a2 b1)) 2))
          (c4 (mod (* a2 b2) 2)))
      (list (mod (+ c0 c3) 2)
            (mod (+ c1 c3 c4) 2)
            (mod (+ c2 c4) 2)))))

(define f8-nonzero (filter (lambda (a) (not (equal a f8-zero))) f8))
(check "f8-has-8-elements" (and (= (length f8) 8) (= (length f8-nonzero) 7)))

(define (f8-pow a n)
  (cond ((= n 0) f8-one) (t (f8-mul a (f8-pow a (- n 1))))))

(define (f8-mult-order a)
  (labels (((go k) (cond ((> k 7) 0)
                         ((equal (f8-pow a k) f8-one) k)
                         (t (go (+ k 1))))))
    (go 1)))

; |F_8^x| = 7 is prime, so every nonzero element except 1 is a generator:
; 6 of them, and φ(7) = 6.
(check "f8-primitive-elts-count"
  (= (length (filter (lambda (a) (= (f8-mult-order a) 7)) f8-nonzero))
     (euler-phi 7)))

; negative control: the identity is not primitive, so the count is strictly
; below |F_8^x|.
(check "f8-identity-is-not-primitive"
  (and (= (f8-mult-order f8-one) 1)
       (< (length (filter (lambda (a) (= (f8-mult-order a) 7)) f8-nonzero))
          (length f8-nonzero))))

; F_4 sits inside this file too: q-1 = 3, so φ(3) = 2 primitive elements.
(define f4-nonzero (filter (lambda (a) (not (equal a f4-zero))) f4))
(define (f4-pow a n)
  (cond ((= n 0) f4-one) (t (f4-mul a (f4-pow a (- n 1))))))
(define (f4-mult-order a)
  (labels (((go k) (cond ((> k 3) 0)
                         ((equal (f4-pow a k) f4-one) k)
                         (t (go (+ k 1))))))
    (go 1)))

(check "f4-primitive-elts-count"
  (= (length (filter (lambda (a) (= (f4-mult-order a) 3)) f4-nonzero))
     (euler-phi 3)))


(print "=== Finite Fields Done ===")
