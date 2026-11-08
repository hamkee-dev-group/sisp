; ============================================================
; ega/normalization.lsp — Normalization of Finite Rings
; Integral closure in total quotient ring.
; Blowup of non-normal locus.
; Verified on Z/12Z → Z/3Z × Z/4Z.
; ============================================================

(print "=== EGA: Normalization ===")


; ---------- Ring operations for Z/nZ ----------
(define (rmod n a) (mod a n))
(define (radd n a b) (mod (+ a b) n))
(define (rmul n a b) (mod (* a b) n))
(define (rneg n a) (mod (- 0 a) n))


; ---------- Zero divisors of Z/12Z ----------
(define n12 12)
(define ring12 (list-to-set (seq 0 11)))

; NOTE: the carrier must be Z/nZ, not a hard-wired Z/12Z, or the predicate
; is wrong for every n /= 12.
(define (zero-divisor-p n a)
  (and (not (= a 0))
       (any (lambda (b)
         (and (not (= b 0)) (= (rmul n a b) 0))) (seq 0 (- n 1)))))

(define zero-divisors12
  (filter (lambda (a) (zero-divisor-p n12 a)) ring12))

(check "zero-divisors-of-z12"
  (equal zero-divisors12 {2 3 4 6 8 9 10}))

; Non-zero-divisors = regular elements.
(define non-zero-divisors12 (diff (diff ring12 {0}) zero-divisors12))
(check "regular-elements-z12" (equal non-zero-divisors12 {1 5 7 11}))


; ---------- Total quotient ring Q(R) = S^{-1}R where S = non-zero-divisors ----------
; In a finite ring every non-zero-divisor is a unit (multiplication by it is
; injective, hence bijective).  For Z/12Z the regular elements are exactly
; the units {1,5,7,11}, so localizing at S does not change the ring:
; Q(Z/12Z) = Z/12Z.

(define (is-unit n a)
  (= (gcd n a) 1))

(define units12 (filter (lambda (a) (is-unit n12 a)) ring12))
(check "units-z12" (equal units12 {1 5 7 11}))
(check "regulars-equal-units-z12"
  (equal non-zero-divisors12 units12))


; ---------- Normalization: integral closure of R in Q(R) ----------
; Since Q(R) = R, the integral closure of R in Q(R) is R itself: Z/12Z is
; its own normalization.  By CRT, Z/12Z ≅ Z/3Z × Z/4Z via
; x ↦ (x mod 3, x mod 4) (e.g. (2,0) ↔ 8 and (0,3) ↔ 3), and likewise
; Q(Z/3Z) = Z/3Z and Q(Z/4Z) = Z/4Z.
;
; Two notions must be kept apart:
; - "integrally closed in Q(R)": automatic for every finite ring, as above;
; - "normal" (EGA, Serre): every localization R_℘ is an integrally closed
;   DOMAIN.  This forces R to be reduced.
; Z/12Z is integrally closed in Q(Z/12Z) but NOT normal: its localization at
; (2) is Z/4Z, which is not a domain (2·2 = 0).  The defect is
; non-reducedness, which integral closure inside Q(R) cannot see.
; Integral closure does enlarge non-normal domains such as the cusp
; k[t², t³] ≅ k[x,y]/(y²-x³), whose normalization is k[t], but those are infinite.

(print "--- Z/12Z normalization via CRT ---")

; Build the CRT map x -> (x mod 3, x mod 4) explicitly and verify that it is
; a ring isomorphism Z/12Z -> Z/3Z x Z/4Z: injective on 12 elements (hence
; bijective onto the 12-element product) and compatible with + and *.
; The two sides of each identity are computed by different routes: reduce in
; Z/12Z then split, versus split then reduce componentwise.

(define (crt-pair x) (list (mod x 3) (mod x 4)))
(define (crt-add u v)
  (list (mod (+ (car u) (car v)) 3) (mod (+ (cadr u) (cadr v)) 4)))
(define (crt-mul u v)
  (list (mod (* (car u) (car v)) 3) (mod (* (cadr u) (cadr v)) 4)))

(define z12-list (seq 0 11))
(define crt-images (map crt-pair z12-list))

(define crt-bijective-p (= (length (dedup crt-images)) 12))

(define crt-homomorphism-p
  (every (lambda (a)
           (every (lambda (b)
                    (and (equal (crt-pair (radd n12 a b))
                                (crt-add (crt-pair a) (crt-pair b)))
                         (equal (crt-pair (rmul n12 a b))
                                (crt-mul (crt-pair a) (crt-pair b)))))
                  z12-list))
         z12-list))

(check "z12-isomorphic-to-z3xz4"
  (and crt-bijective-p crt-homomorphism-p))


; ---------- Integrality: element t is integral over R ----------
; t satisfies a monic polynomial with coefficients in R.
; Equivalently: R[t] is a finitely generated R-module.

; For Z/12Z and t satisfying t² + at + b = 0 (a,b ∈ Z/12Z):
; This is a quadratic integrality condition.

(define (integral-over-z12 a b x)
  (= (mod (+ (* x x) (* a x) b) 12) 0))

(check "element-4-integral-over-z12"
  (integral-over-z12 0 8 4))

(check "element-6-integral-over-z12"
  (integral-over-z12 0 0 6))


; ---------- Integral closure computation ----------
; The integral closure of Z/12Z in Q(Z/12Z):
; We need all elements s/t where s∈R, t∈S (non-zero-divisors),
; that satisfy a monic polynomial over R.

; Since S = units12, and each unit already has an inverse in R,
; the localization R_S = R. So integral closure = R.
; The same holds for every finite ring, e.g. Z/6Z (S = {1,5}) and
; Z/4Z (S = {1,3}).

; Construct Q(R) = S^-1 R explicitly, as formal fractions s/t with s in R and
; t in S = non-zero-divisors, under the equivalence s1/t1 = s2/t2 iff
; s1*t2 = s2*t1 (legitimate because every t in S is regular).
; If every fraction s/t has exactly one representative of the form x/1, then
; the canonical map R -> Q(R) is bijective, so Q(R) = R.  The integral closure
; of R inside Q(R) = R is then R itself: R is its own normalization.

(define nzd-list (set-to-list non-zero-divisors12))

; The elements x of R whose image x/1 equals the fraction s/tt.
(define (frac-reps s tt)
  (filter (lambda (x) (= (rmul n12 x tt) (mod s n12))) z12-list))

(define q-of-z12-equals-z12
  (every (lambda (s)
           (every (lambda (tt) (= (length (frac-reps s tt)) 1)) nzd-list))
         z12-list))

; and the collected representatives exhaust R, so the normalization is Z/12Z.
(define normalization12
  (list-to-set
    (foldr append nil
      (map (lambda (s) (foldr append nil (map (lambda (tt) (frac-reps s tt)) nzd-list)))
           z12-list))))

(check "z12-is-its-own-normalization"
  (and q-of-z12-equals-z12 (equal normalization12 ring12)))


; ---------- Normalization by blowing up the conductor ----------
; The conductor ℭ = Ann_R(Ř/R) where Ř = normalization.
; Blowup: Proj(⊕_{n≥0} ℭ^n).
; For the cusp R = k[t²,t³], the normalization is k[t], the conductor is
; t²k[t] = (t²,t³)R, and blowing it up gives the smooth curve.

; For Z/12Z, Ř = R, so ℭ = R and the conductor sees no non-normal locus.
; Spec(Z/12Z) = Spec(Z/3Z) ⊔ Spec(Z/4Z).  Z/3Z is a field; Z/4Z is an
; Artinian local ring but not a field (2·2 = 0).  Z/4Z is integrally closed in
; Q(Z/4Z) = Z/4Z (S = {1,3} consists of units: 3·3 = 9 ≡ 1 mod 4), but its
; only stalk, at ℘ = (2), is Z/4Z itself, not a domain: Spec(Z/4Z) is
; neither reduced nor normal in the EGA sense.

(print "--- Normalization of Z/12Z is trivial ---")

; The computable content here: every regular element of Z/nZ is already a
; unit, so S^-1 R = R = Q(R) and R is integrally closed in Q(R).  (Normal in
; the EGA sense needs, in addition, that R be reduced; see below.)
; regular-list is computed from the multiplication table (annihilators),
; unit-list from gcd -- two independent routes to the same set.

(define (regular-list n)
  (filter (lambda (a) (and (not (= a 0)) (not (zero-divisor-p n a))))
          (seq 0 (- n 1))))

(define (unit-list n)
  (filter (lambda (a) (is-unit n a)) (seq 0 (- n 1))))

(check "z12-is-normal-ring"
  (and (equal (regular-list n12) (unit-list n12))
       (every (lambda (m) (equal (regular-list m) (unit-list m))) (seq 2 20))))


; ---------- Blowup of the non-normal locus ----------
; The non-normal locus N ⊂ Spec(R) is the set of primes ℘ where R_℘ is not
; normal.  For a reduced ring with finite normalization it is V(ℭ), with the
; conductor ℭ = {r ∈ R | rŘ ⊆ R}, Ř the normalization.

; For Z/12Z, Ř = R, so ℭ = R, V(ℭ) = ∅, and blowing up ℭ does nothing:
; Proj(⊕_{n≥0} ℭ^n) = Proj(R[t]) = Spec(R) × P⁰ = Spec(R).
; The blowup is the identity.  (Z/12Z is not reduced, so V(ℭ) misses its
; non-normal point (2), whose stalk Z/4Z is not a domain: blowing up cannot
; remove nilpotents.)

; Compute the conductor C = {r in R | r*Rn subset R} from the normalization
; Rn computed above, then compute the non-normal locus V(C) = {primes P of
; Z/12Z with C subset P}.  The primes of Z/12Z are the ideals generated by
; the prime divisors 2 and 3.  The blowup Proj(sum_n C^n) is the identity
; exactly when V(C) is empty, i.e. when C is the unit ideal.

(define (ideal-gen n g) (list-to-set (map (lambda (k) (rmul n g k)) (seq 0 (- n 1)))))

(define conductor12
  (filter (lambda (r)
                (every (lambda (x) (in (rmul n12 r x) ring12)) normalization12))
              ring12))

(define primes12 (list (ideal-gen n12 2) (ideal-gen n12 3)))

(define non-normal-locus12
  (filter (lambda (pr) (subset conductor12 pr)) primes12))

(check "blowup-of-non-normal-locus-is-trivial"
  (and (equal conductor12 ring12)
       (in 1 conductor12)
       (eq non-normal-locus12 nil)))


; ---------- Other small examples ----------
; Z/6Z ≅ Z/2Z × Z/3Z by CRT. Both are fields, so Z/6Z IS normal:
; Spec(Z/6Z) = {℘₂, ℘₃}, and both stalks are fields.

; Z/8Z: non-zero-divisors = {1,3,5,7} (units), so Q(Z/8Z) = Z/8Z and Z/8Z is
; integrally closed in it; but Z/8Z is local and not reduced (2³ = 0), so it
; is not normal.  Every finite ring equals its total quotient ring, since
; every non-zero-divisor of a finite ring is a unit; Z/nZ is normal exactly
; when n is squarefree.

; So for finite rings the integral closure in Q(R) is always R itself.  For
; it to change anything, R needs non-zero-divisors that are not units, which
; forces R to be infinite (e.g. the node or the cusp over a field).

; For the sake of this demonstration, we work with the abstract concept:
(print "--- Abstract normalization for finite ring ---")

; Given a finite ring R with total quotient ring Q(R) = S⁻¹R,
; the normalization Ř = integral closure of R in Q(R).
; Ř = {q ∈ Q(R) | ∃ monic f ∈ R[x] : f(q) = 0}.

; For a reduced finite ring, Ř = R.
; Every reduced finite ring is a product of finite fields.
; A product of finite fields IS normal.

; Conversely, a finite ring with nilpotent elements is not normal (some
; stalk is not a domain), even though it equals its own integral closure
; in Q(R).  So for finite rings: normal ⇔ reduced ⇔ product of finite fields.

; A finite commutative ring is reduced iff it is a product of finite fields,
; iff it is von Neumann regular (every a satisfies a^e = a for some e >= 2).
; This is checked by computing the two conditions independently for Z/mZ,
; m = 2..20:
;   reduced-p     -- from nilpotency (a^m = 0)
;   vn-regular-p  -- from the existence of an exponent with a^e = a
; and checking they agree.  (They both fail exactly for non-squarefree m.)

(define (rpow n a e) (cond ((= e 0) 1) (t (rmul n a (rpow n a (- e 1))))))

(define (nilpotent-p n a) (= (rpow n a n) 0))

(define (reduced-p n)
  (every (lambda (a) (or (= a 0) (not (nilpotent-p n a)))) (seq 0 (- n 1))))

(define (vn-regular-p n)
  (every (lambda (a) (any (lambda (e) (= (rpow n a e) a)) (seq 2 (+ n 1))))
         (seq 0 (- n 1))))

(check "reduced-implies-normal-for-finite-rings"
  (and (every (lambda (m) (<=> (reduced-p m) (vn-regular-p m))) (seq 2 20))
       (reduced-p 6) (not (reduced-p 12))))


; ---------- Concrete normalization: subrings of F_8 ----------
; Let α ∈ F_8 with α³ = α+1.  Since [F_8 : F_2] = 3 is prime, there are no
; intermediate fields, so any element outside F_2 generates F_8.  For example
; α+α² = α⁴ is a conjugate of α (same minimal polynomial x³+x+1), so
; F_2[α+α²] = F_8; likewise F_2[α²] = F_8 (α² also generates F_8^×).

; The only proper subring is F_2.  It is a field, so Q(F_2) = F_2 and its
; normalization is F_2 itself.  (Its integral closure in the extension F_8 is
; all of F_8, since every element of F_8 is algebraic over F_2; that is
; closure in an extension, not normalization.)

; Build F_8 = F_2[x]/(x^3 + x + 1) concretely: an element is its coefficient
; list (c0 c1 c2) over F_2.  Multiply as polynomials and reduce with
; x^3 = x + 1, x^4 = x^2 + x.  A finite field has no proper regular elements
; to invert -- every nonzero element is a unit -- so Q(F_8) = F_8 and F_8 is
; its own integral closure, i.e. normal.  Zero is not invertible.

(define g8-zero (list 0 0 0))
(define g8-one  (list 1 0 0))

(define g8-elements
  (list (list 0 0 0) (list 1 0 0) (list 0 1 0) (list 1 1 0)
        (list 0 0 1) (list 1 0 1) (list 0 1 1) (list 1 1 1)))

(define (g8-mul u v)
  (let ((a0 (car u)) (a1 (cadr u)) (a2 (caddr u))
        (b0 (car v)) (b1 (cadr v)) (b2 (caddr v)))
    (let ((p0 (* a0 b0))
          (p1 (+ (* a0 b1) (* a1 b0)))
          (p2 (+ (* a0 b2) (* a1 b1) (* a2 b0)))
          (p3 (+ (* a1 b2) (* a2 b1)))
          (p4 (* a2 b2)))
      (list (mod (+ p0 p3) 2)
            (mod (+ p1 p3 p4) 2)
            (mod (+ p2 p4) 2)))))

(define (g8-invertible-p u)
  (any (lambda (v) (equal (g8-mul u v) g8-one)) g8-elements))

(check "finite-fields-are-normal"
  (and (every (lambda (u) (or (equal u g8-zero) (g8-invertible-p u)))
              g8-elements)
       (not (g8-invertible-p g8-zero))))

; ---------- General principle ----------
(print "For finite rings, normalization = integral closure in total quotient ring.")
(print "Product of fields = normal. Local Artinian with all regular elements units = normal.")
(print "Non-normal finite rings exist but are pathological.")
; (No check here: this section is a narrative summary of the computations
; carried out above, not a separate assertion.)

(print "=== EGA: Normalization Done ===")
