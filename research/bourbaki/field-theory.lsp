; ============================================================
; Bourbaki -- Field Theory: Field Extensions, Algebraic Closure
; for Finite Fields. Separable/Inseparable Extensions. Perfect Fields.
; ============================================================

(print "=== Field Theory ===")

; ---------- pow ----------
(define (pow b e)
  (cond ((= e 0) 1)
        (t (* b (pow b (- e 1))))))

; ---------- polynomial evaluation over F_q ----------
(define (poly-eval coeff-lst x modq)
  (labels (((go cs pwr acc)
    (cond ((eq cs nil) (mod acc modq))
          (t (go (cdr cs) (* pwr x) (mod (+ acc (* (car cs) pwr)) modq))))))
    (go coeff-lst 1 0)))

; ---------- minimal polynomial: find irreducible m(x) of degree d over F_p ----------
(define (irred-poly-p poly-coeffs p-val domain)
  (and (not (every (lambda (x) (not (= (poly-eval poly-coeffs x p-val) 0))) domain))
       (labels (((has-root-p coeffs)
          (any (lambda (xr) (= (poly-eval coeffs xr p-val) 0)) domain)))
         (not (has-root-p poly-coeffs)))))

; ---------- F_2 field: polynomials ----------
(print "--- F_2 field extensions ---")

; x^2 + x + 1 is irreducible over F_2: no roots in {0,1}
(define poly-x2+x+1 (list 1 1 1))
(define f2 {0 1})

; Test: poly(0)=1, poly(1)=3≡1 mod 2 => no roots
(check "x2+x+1-irred-over-F2"
  (not (any (lambda (xr) (= (poly-eval poly-x2+x+1 xr 2) 0))
            (set-to-list f2))))

; F_4 ≅ F_2[x]/(x^2+x+1). Elements: {0, 1, a, a+1} where a^2=a+1.
; F_4 = F_2^2, the unique degree-2 extension of F_2.
(print "F_4 = F_2[x]/(x^2+x+1) = {0, 1, a, a+1}")

; Concrete model of F_4: an element c0 + c1*a is encoded as the integer
; u = c0 + 2*c1, so F_4 = {0,1,2,3} with 2 = a and 3 = a+1.
(define (f4-c0 u) (mod u 2))
(define (f4-c1 u) (truncate (/ u 2)))
(define (f4-add u v)
  (+ (mod (+ (f4-c0 u) (f4-c0 v)) 2)
     (* 2 (mod (+ (f4-c1 u) (f4-c1 v)) 2))))
; (c0+c1 a)(d0+d1 a) = c0d0 + (c0d1+c1d0) a + c1d1 a^2, and a^2 = a+1.
(define (f4-mul u v)
  (+ (mod (+ (* (f4-c0 u) (f4-c0 v)) (* (f4-c1 u) (f4-c1 v))) 2)
     (* 2 (mod (+ (* (f4-c0 u) (f4-c1 v))
                  (* (f4-c1 u) (f4-c0 v))
                  (* (f4-c1 u) (f4-c1 v))) 2))))
(define f4-elts (list 0 1 2 3))
(define f4-units (list 1 2 3))

; F_4 really is a degree-2 extension field of F_2:
;  - the defining relation a^2 = a+1 holds in the model,
;  - every nonzero element is invertible (so it is a field, not just a ring),
;  - it has 4 = 2^2 elements, i.e. dimension 2 as an F_2-vector space, and
;    a generates it: 1 and a are F_2-independent, so {c0 + c1 a} has 4 members.
(check "F4-is-degree2-extension"
  (and (= (f4-mul 2 2) 3)
       (every (lambda (u) (any (lambda (v) (= (f4-mul u v) 1)) f4-units)) f4-units)
       (= 0 (f4-add 1 1))
       (= (length (dedup (map (lambda (k)
             (f4-add (f4-mul (mod k 2) 1)
                     (f4-mul (truncate (/ k 2)) 2))) f4-elts))) (pow 2 2))))

; x^3 + x + 1 irreducible over F_2 => F_8
(define poly-x3+x+1 (list 1 1 0 1))
(check "x3+x+1-irred-over-F2"
  (not (any (lambda (xr) (= (poly-eval poly-x3+x+1 xr 2) 0))
            (set-to-list f2))))

(print "F_8 = F_2[x]/(x^3+x+1), degree 3")

; ---------- F_3 field extensions ----------
(print "--- F_3 field extensions ---")

; x^2 + 1: evaluate at 0 => 1, 1 => 2, 2 => 5≡2. All nonzero => irreducible.
(define poly-x2+1-f3 (list 1 0 1))
(check "x2+1-irred-over-F3"
  (not (any (lambda (xr) (= (poly-eval poly-x2+1-f3 xr 3) 0))
            (set-to-list (seq 0 2)))))

(print "F_9 = F_3[x]/(x^2+1), degree 2 over F_3")

; ---------- F_5 field extensions ----------
(print "--- F_5 field extensions ---")

; Find a quadratic irreducible over F_5.
; x^2 + 2: eval 0=>2, 1=>3, 2=>6≡1, 3=>11≡1, 4=>18≡3. No roots => irreducible.
(define poly-x2+2-f5 (list 2 0 1))
(check "x2+2-irred-over-F5"
  (not (any (lambda (xr) (= (poly-eval poly-x2+2-f5 xr 5) 0))
            (set-to-list (seq 0 4)))))

(print "F_25 = F_5[x]/(x^2+2), degree 2 over F_5")

; ---------- Algebraic closure of finite fields ----------
; The algebraic closure of F_p is the union of all F_{p^n}.
; Any finite extension F_{p^n}/F_p is Galois with Galois group cyclic Z/nZ.
; F_{q^n} is the splitting field of x^{q^n} - x over F_q.
(print "--- Algebraic closure of F_p ---")
(print "F_p-bar = union of all F_{p^n}")
(print "Every finite extension of F_p is Galois (normal + separable)")

; ============================================================
; Generic F_p[x] and F_p[x]/(f) arithmetic, so the uniqueness and
; Galois-degree claims below can be COMPUTED rather than asserted.
; Polynomials are coefficient lists, low degree first; nil is the zero poly.
; ============================================================
(define (pl-nth cs i)
  (cond ((< i 0) 0) ((>= i (length cs)) 0) (t (nth i cs))))

(define (pl-trim cs)
  (labels (((go l) (cond ((eq l nil) nil)
                         ((= (car l) 0) (go (cdr l)))
                         (t (reverse l)))))
    (go (reverse cs))))

(define (pl-deg cs) (- (length (pl-trim cs)) 1))

(define (pl-add pm a b)
  (let ((ln (cond ((> (length a) (length b)) (length a)) (t (length b)))))
    (pl-trim (map (lambda (i) (mod (+ (pl-nth a i) (pl-nth b i)) pm)) (seq 0 (- ln 1))))))

(define (pl-scale pm cs s) (pl-trim (map (lambda (c) (mod (* c s) pm)) cs)))
(define (pl-sub pm a b) (pl-add pm a (pl-scale pm b (- pm 1))))

(define (pl-shift cs k)
  (cond ((= k 0) cs)
        ((eq cs nil) nil)
        (t (append (map (lambda (i) 0) (seq 1 k)) cs))))

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

; remainder of a modulo the MONIC polynomial fpoly
(define (pl-mod pm a fpoly)
  (let ((d (pl-deg fpoly)))
    (labels (((go cs)
      (let ((k (pl-deg cs)))
        (cond ((< k d) cs)
              (t (go (pl-sub pm cs
                       (pl-shift (pl-scale pm fpoly (pl-nth cs k)) (- k d)))))))))
      (go (pl-trim a)))))

; every coefficient list of length d over F_p
(define (pl-tuples pm d)
  (cond ((= d 0) (list nil))
        (t (foldr append nil
             (map (lambda (rest) (map (lambda (c) (append rest (list c))) (seq 0 (- pm 1))))
                  (pl-tuples pm (- d 1)))))))

(define (pl-monics pm d) (map (lambda (lo) (append lo (list 1))) (pl-tuples pm d)))

; every monic degree-d poly that factors as (monic of degree i)*(monic of degree d-i)
(define (pl-reducibles pm d)
  (cond ((<= d 1) nil)
        (t (foldr append nil
             (map (lambda (i)
                    (foldr append nil
                      (map (lambda (a) (map (lambda (b) (pl-mul pm a b)) (pl-monics pm (- d i))))
                           (pl-monics pm i))))
                  (seq 1 (- d 1)))))))

; irreducible = monic of degree d that is not any such product
(define (pl-irreds pm d)
  (let ((red (pl-reducibles pm d)))
    (filter (lambda (g) (not (any (lambda (h) (equal g h)) red))) (pl-monics pm d))))

; --- the field K = F_p[x]/(fpoly) ---
(define (kf-mul pm fpoly a b) (pl-mod pm (pl-mul pm a b) fpoly))
(define (kf-elts pm fpoly) (dedup (map (lambda (cs) (pl-trim cs)) (pl-tuples pm (pl-deg fpoly)))))

; Horner evaluation of an F_p-polynomial g at an element a of K
(define (kf-eval pm fpoly g a)
  (foldr (lambda (c acc) (pl-add pm (list c) (kf-mul pm fpoly a acc))) nil g))

(define (kf-has-root pm fpoly g)
  (any (lambda (a) (eq (kf-eval pm fpoly g a) nil)) (kf-elts pm fpoly)))

; Number of degree-n extensions of F_p UP TO ISOMORPHISM.  Build every field
; F_p[x]/(f) for f irreducible of degree n, then merge two of them whenever
; one's defining polynomial has a root in the other (an explicit isomorphism).
(define (ext-deg-count p-val n-deg)
  (length
    (foldl (lambda (reps g)
             (cond ((any (lambda (h) (kf-has-root p-val h g)) reps) reps)
                   (t (append reps (list g)))))
           nil (pl-irreds p-val n-deg))))

; Sanity on the search space: the number of monic irreducibles of degree n over
; F_p is (1/n) sum_{d|n} mu(n/d) p^d -- 1, 2, 3, 3 and 10 for these cases.
; If that count were zero the uniqueness checks below would be vacuous.
(check "irreducible-poly-counts-match-gauss-formula"
  (and (= (length (pl-irreds 2 2)) 1)
       (= (length (pl-irreds 2 3)) 2)
       (= (length (pl-irreds 2 4)) 3)
       (= (length (pl-irreds 3 2)) 3)
       (= (length (pl-irreds 5 2)) 10)))

(check "unique-degree2-of-F2" (= (ext-deg-count 2 2) 1))
(check "unique-degree3-of-F2" (= (ext-deg-count 2 3) 1))
(check "unique-degree2-of-F3" (= (ext-deg-count 3 2) 1))
(check "unique-degree1-is-F2-itself" (= (ext-deg-count 2 1) 1))

; The two distinct irreducible cubics over F_2 really are DIFFERENT polynomials
; that nonetheless define isomorphic fields -- that is the whole content of the
; uniqueness claim, and it is what makes ext-deg-count 2 3 collapse to 1.
(check "the-two-cubics-over-F2-are-distinct-but-isomorphic"
  (let ((cs (pl-irreds 2 3)))
    (and (= (length cs) 2)
         (not (equal (car cs) (cadr cs)))
         (kf-has-root 2 (car cs) (cadr cs))
         (kf-has-root 2 (cadr cs) (car cs)))))

; ---------- Separable and inseparable extensions ----------
; Over a finite field (which is perfect), all algebraic extensions are separable.
; A perfect field has char 0 OR char p > 0 with Frobenius surjective.
; Finite fields are perfect: Frobenius F(x) = x^p is an automorphism.
(print "--- Separable and perfect fields ---")
(print "F_q is perfect: Frobenius x -> x^p is an automorphism")
(print "All algebraic extensions of F_q are separable")
(print "No inseparable extensions for finite fields")

; For a non-perfect field like F_p(t): extension by t^{1/p} is inseparable.
; But finite fields are always perfect.

; A field of characteristic p is perfect iff Frobenius x -> x^p is SURJECTIVE.
; Verify it directly, as a bijection of the finite set onto itself:
;  - on F_4 (p=2): x -> x^2 hits all 4 elements, and it is not the identity
;    (it swaps a and a+1), so it is a nontrivial automorphism;
;  - on F_p for p = 2,3,5,7: x -> x^p mod p hits all p residues.
(check "finite-fields-are-perfect"
  (and (= (length (dedup (map (lambda (u) (f4-mul u u)) f4-elts))) 4)
       (not (equal (map (lambda (u) (f4-mul u u)) f4-elts) f4-elts))
       (every (lambda (p-val)
                (= (length (dedup (map (lambda (xr) (mod (pow xr p-val) p-val))
                                       (set-to-list (seq 0 (- p-val 1))))))
                   p-val))
              (list 2 3 5 7))))

; ---------- Galois group of F_{q^n}/F_q ----------
; Gal(F_{q^n}/F_q) ≅ Z/nZ, generated by Frobenius Fr(x) = x^q.
(print "--- Galois group ---")
(print "Gal(F_{q^n}/F_q) = Z/nZ, generated by Frobenius")
(print "|Gal(F_{q^n}/F_q)| = n = [F_{q^n} : F_q]")

; Compute |Gal(K/F_q)| as the multiplicative ORDER OF FROBENIUS acting on K --
; the least e >= 1 with x^(q^e) = x for every x -- and compare it against
; [K : F_q], which comes independently from the CARDINALITY of K.  The two are
; derived by unrelated routes, so a wrong field model breaks the equality.
(define (kf-pow pm fpoly a e)
  (cond ((= e 0) (list 1)) (t (kf-mul pm fpoly a (kf-pow pm fpoly a (- e 1))))))

(define (kf-frob pm fpoly a) (kf-pow pm fpoly a pm))

(define (kf-frob-iter pm fpoly a e)
  (cond ((= e 0) a) (t (kf-frob-iter pm fpoly (kf-frob pm fpoly a) (- e 1)))))

(define (kf-frob-order pm fpoly)
  (let ((elts (kf-elts pm fpoly)))
    (labels (((go e)
      (cond ((> e 8) 0)
            ((every (lambda (a) (equal (kf-frob-iter pm fpoly a e) a)) elts) e)
            (t (go (+ e 1))))))
      (go 1))))

; the fixed field of Frobenius is the prime field F_p
(define (kf-fixed-size pm fpoly)
  (length (filter (lambda (a) (equal (kf-frob pm fpoly a) a)) (kf-elts pm fpoly))))

; |Gal| = order of Frobenius; [K:F_p] read off from |K| = p^n; Fix(Fr) = F_p.
(define (gal-degree-ok pm fpoly n-deg)
  (and (= (kf-frob-order pm fpoly) n-deg)
       (= (length (kf-elts pm fpoly)) (pow pm n-deg))
       (= (kf-fixed-size pm fpoly) pm)))

(check "Gal-F4-over-F2-degree-2"  (gal-degree-ok 2 poly-x2+x+1 2))
(check "Gal-F8-over-F2-degree-3"  (gal-degree-ok 2 poly-x3+x+1 3))
(check "Gal-F9-over-F3-degree-2"  (gal-degree-ok 3 poly-x2+1-f3 2))
(check "Gal-F25-over-F5-degree-2" (gal-degree-ok 5 poly-x2+2-f5 2))

; negative controls: the Frobenius order is genuinely n, not merely "at most n"
(check "Frobenius-is-not-the-identity"
  (and (not (= (kf-frob-order 2 poly-x3+x+1) 1))
       (not (= (kf-frob-order 2 poly-x3+x+1) 2))
       (not (= (kf-frob-order 5 poly-x2+2-f5) 1))))

; ---------- Subfield structure ----------
; F_{q^n} contains F_{q^d} as subfield iff d | n.
; The Galois correspondence: subfields ↔ subgroups of Z/nZ.
(print "--- Subfield structure ---")
(print "F_{16} contains F_4 and F_2 (since 2|4 and 1|4)")
(print "F_{12} contains F_4, F_2, F_3 (but F_4 and F_3 not comparable)")

; F_{64} = F_{2^6}: subfields for divisors 1,2,3,6 => F_2, F_4, F_8, F_64
(define (divides-n d n) (= (mod n d) 0))

; Count subfields of F_{2^6}: divisors of 6 = {1,2,3,6} => 4 subfields
(check "subfields-F64-count"
  (= (length (filter (lambda (d) (divides-n d 6))
          (list-to-set (list 1 2 3 6)))) 4))

; ========== F_q EXTENSIONS SYSTEMATICALLY ==========
(print "--- Systematic F_q extensions ---")

; For each q = p^n, extensions F_{q^d} for d = 1, 2, 3, ...
; The degree-d extension has q^d elements.

(define (field-size p-val n-deg)
  (pow p-val n-deg))

(check "F4-size-4" (= (field-size 2 2) 4))
(check "F8-size-8" (= (field-size 2 3) 8))
(check "F9-size-9" (= (field-size 3 2) 9))
(check "F25-size-25" (= (field-size 5 2) 25))
(check "F16-size-16" (= (field-size 2 4) 16))
(check "F27-size-27" (= (field-size 3 3) 27))

; ---------- Norm and Trace ----------
; For F_{q^n}/F_q, N(a) = a * a^q * ... * a^{q^{n-1}} = a^{(q^n-1)/(q-1)}
; Tr(a) = a + a^q + ... + a^{q^{n-1}}
(print "--- Norm and Trace ---")
(print "N_{F_{q^n}/F_q}(a) = a^{(q^n-1)/(q-1)}")
(print "Tr_{F_{q^n}/F_q}(a) = sum_{i=0}^{n-1} a^{q^i}")

; For F_4/F_2: N_{F_4/F_2}(a) = a^3 (since (4-1)/(2-1)=3)
; Elements: 0->0, 1->1, a->a^3=a(a^2)=a(a+1)=a^2+a=(a+1)+a=1, a+1->(a+1)^3
; (a+1)^3 = (a+1)(a+1)^2 = (a+1)(a^2+1) = (a+1)(a+1+1) = (a+1)a = a^2+a = a+1+a = 1
; So Norm maps: 0->0, 1->1, a->1, a+1->1. N(x)=1 for all nonzero x.

; Compute N(u) = u * u^2 in the F_4 model and compare against the two claims
; made above, each derived independently:
;  - N(u) = 1 for every nonzero u (N is onto F_2^x = {1}, kernel = all units);
;  - N(0) = 0;
;  - N is multiplicative: N(uv) = N(u)N(v).
; Tr(u) = u + u^2 lands in F_2 = {0,1} and is onto (Tr(a) = a + a+1 = 1).
(define (f4-norm u) (f4-mul u (f4-mul u u)))
(define (f4-trace u) (f4-add u (f4-mul u u)))

(check "norm-F4-over-F2-is-1-on-units"
  (and (every (lambda (u) (= (f4-norm u) 1)) f4-units)
       (= (f4-norm 0) 0)
       (every (lambda (u)
                (every (lambda (v) (= (f4-norm (f4-mul u v))
                                      (f4-mul (f4-norm u) (f4-norm v))))
                       f4-elts))
              f4-elts)))

(check "trace-F4-over-F2-is-onto-F2"
  (and (every (lambda (u) (in (f4-trace u) {0 1})) f4-elts)
       (= (f4-trace 2) 1)
       (= (f4-trace 0) 0)
       (every (lambda (u)
                (every (lambda (v) (= (f4-trace (f4-add u v))
                                      (f4-add (f4-trace u) (f4-trace v))))
                       f4-elts))
              f4-elts)))

(print "=== Field Theory Done ===")
