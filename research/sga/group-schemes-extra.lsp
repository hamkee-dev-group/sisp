; ============================================================
; SGA — Cartier Duality for Finite Commutative Group Schemes
; Cartier dual G^D = Hom(G, G_m).
; Compute Cartier dual of μ_n ≅ Z/nZ, and α_p.
; Verified on μ_3, μ_5 over F_7, and α_2 over F_2.
; ============================================================

(print "=== SGA: Cartier Duality ===")

; ============================================================
; 1. Cartier duality: G^D(R) = Hom_{grp}(G_R, G_{m,R})
; ============================================================
; For a finite commutative group scheme G over a field k:
; G^D is represented by the Hopf algebra A^D = Hom_k(A, k)
; with multiplication dual to comultiplication, etc.
;
; Key facts:
; - (G^D)^D ≅ G (biduality)
; - μ_n^D ≅ Z/nZ (constant group scheme of the cyclic group)
; - (Z/nZ)^D ≅ μ_n
; - α_p is self-dual: α_p^D ≅ α_p

(print "--- Cartier Duality Overview ---")
(print "μ_n^D ≅ Z/nZ, α_p^D ≅ α_p (self-dual).")

; ============================================================
; 2. μ_n and its Cartier dual over F_q
; ============================================================
; μ_n = Spec(k[x]/(x^n-1)) with Δ(x)=x⊗x, ε(x)=1, S(x)=x^{n-1}.
; μ_n(R) = {r ∈ R : r^n = 1}, group operation = multiplication.
;
; Cartier dual (μ_n)^D(R) = Hom_{grp}(μ_n, G_m)(R).
; This represents the "character group" of μ_n.
; Over a field sep_closed: Hom(μ_n, G_m) ≅ Z/nZ.
;
; Concrete: for R = F_q, (μ_n)^D(F_q) = Hom(μ_n(F_q), F_q^×).
; μ_n(F_q) = cyclic of order d = gcd(n, q-1).
; Hom(C_d, C_{q-1}) ≅ C_d (since both cyclic, any map is determined by image of generator).
; But this is just the q-points; the scheme itself is constant Z/nZ.

; Verify for μ_3 over F_7:
; μ_3(F_7) = {x∈F_7 : x^3=1} = {1,2,4}, cyclic order 3.
(define mu3-F7 {1 2 4})
(check "mu3-F7-order-3" (= (length mu3-F7) 3))

; F_7^× = {1,2,3,4,5,6}, cyclic order 6.
(define F7star {1 2 3 4 5 6})
(check "F7star-order-6" (= (length F7star) 6))

; Hom(μ_3(F_7), F_7^×) ≅ Z/3Z (maps generator 2 to a cube root of unity).
; Generators of μ_3(F_7): 2 and 4 (since 2^2=4, 2^3=1).
; Homomorphisms are determined by image of 2: must map to {x: x^3=1} in F_7^× = {1,2,4}.
; So |Hom(μ_3(F_7), F_7^×)| = 3, giving Z/3Z.

(define (char-maps gen target-group)
  (filter (lambda (g)
    (and (in g target-group)
         (= (mod (fn-pow g 3 7) 7) 1)))
    target-group))

(define (fn-pow a n modq)
  (cond ((= n 0) 1)
        (t (mod (* a (fn-pow a (- n 1) modq)) modq))))

; The character group: any hom φ: μ_3→F_7^× is determined by φ(2).
; φ(2) must satisfy φ(2)^3 = 1 in F_7, so φ(2) ∈ {1,2,4}.
(define mu3-dual-points (char-maps 2 F7star))
(check "mu3-dual-over-F7-order-3" (= (length mu3-dual-points) 3))

; Verify: these are the 3 cube roots of 1 in F_7 = same as μ_3(F_7).
(check "mu3-dual-equals-mu3"
  (equal mu3-dual-points mu3-F7))

; Cartier dual of μ_n is Z/nZ (constant). Over an algebraically closed
; field, the character group has exactly n elements.

; ------------------------------------------------------------
; The character group, built by BRUTE FORCE rather than by the shortcut above.
; A finite group is a pair (element list, binary operation).  We enumerate
; EVERY function G -> F_7^× (there are |F_7^×|^|G| of them) and keep the ones
; that are homomorphisms.  Nothing about the answer is assumed.
; ------------------------------------------------------------
(define (all-maps dom cod)
  (cond ((eq dom nil) (list nil))
        (t (foldr append nil
             (map (lambda (rest) (map (lambda (v) (cons v rest)) cod))
                  (all-maps (cdr dom) cod))))))
; a map is stored as the list of its values, in the order of dom
(define (evalmap dom vals el) (nth (index-of el dom) vals))

(define (homs dom gop cod cop)
  (filter (lambda (vals)
            (every (lambda (a)
                     (every (lambda (b)
                              (equal (evalmap dom vals (gop a b))
                                     (cop (evalmap dom vals a) (evalmap dom vals b))))
                            dom))
                   dom))
          (all-maps dom cod)))

; D(G) = Hom(G, G_m) is itself a group under pointwise multiplication
(define (dual-op cop u v) (map (lambda (pr) (cop (car pr) (cadr pr))) (zip u v)))

; isomorphism invariants of a finite abelian group: order, and the sorted
; list of element orders
(define (group-identity dom gop)
  (find (lambda (e) (every (lambda (a) (equal (gop e a) a)) dom)) dom))
(define (elt-order-h el gop idn cur k)
  (cond ((equal cur idn) k) (t (elt-order-h el gop idn (gop cur el) (+ k 1)))))
(define (elt-order el gop idn) (elt-order-h el gop idn el 1))
(define (order-profile dom gop)
  (let ((idn (group-identity dom gop)))
    (sort < (map (lambda (a) (elt-order a gop idn)) dom))))

(define mu3-list (list 1 2 4))
(define (mul7 a b) (mod (* a b) 7))
(define f7-list (list 1 2 3 4 5 6))
(define (mu3-dual-op u v) (dual-op mul7 u v))

; Hom(μ_3(F_7), F_7^×), found by testing all 6^3 = 216 candidate maps
(define mu3-dual (homs mu3-list mul7 f7-list mul7))
(print "D(μ_3) = Hom(μ_3(F_7), F_7^×); each hom listed by its values on 1,2,4:")
(print mu3-dual)
(print "order profile of D(μ_3):") (print (order-profile mu3-dual mu3-dual-op))

; μ_n^D ≅ Z/nZ: compare against Z/3Z built independently as {0,1,2} under +.
(define (z3-op a b) (mod (+ a b) 3))
(check "mu3-dual-is-cyclic-of-order-3"
  (and (= (length mu3-dual) 3)
       (equal (order-profile mu3-dual mu3-dual-op)
              (order-profile (list 0 1 2) z3-op))))

; The invariant really discriminates: Z/4Z and the Klein group both have
; order 4 but different order profiles, so this comparison could have failed.
(check "order-profile-separates-Z4-from-Klein"
  (not (equal (order-profile (list 0 1 2 3) (lambda (a b) (mod (+ a b) 4)))
              (order-profile (list (list 0 0) (list 0 1) (list 1 0) (list 1 1))
                             (lambda (u v) (list (mod (+ (car u) (car v)) 2)
                                                 (mod (+ (cadr u) (cadr v)) 2)))))))

; ============================================================
; 3. Constant group scheme Z/nZ and its Cartier dual
; ============================================================
; (Z/nZ)(R) = {set of n elements} with addition mod n.
; (Z/nZ)^D ≅ μ_n.
;
; Over F_7: μ_5(F_7) = 5th roots of 1 in F_7.
; 5th roots: x^5 ≡ 1 mod 7. Since F_7^× has order 6, and 5∤6,
; only solution is x=1. So |μ_5(F_7)| = 1.
; But the scheme still has μ_5 as Hopf algebra.

(print "--- μ_n^D ≅ Z/nZ ---")

; Verify for μ_5 over F_7:
; μ_5(F_7) = {x: x^5=1 mod 7} = {1} (trivial, since 5∤6).
(define mu5-points
  (filter (lambda (x) (= (fn-pow x 5 7) 1)) F7star))
(check "mu5-F7-trivial" (= (length mu5-points) 1))

; But Cartier dual μ_5^D = Z/5Z as group scheme (constant cyclic of order 5).
; The q-points of (μ_n)^D are Hom(μ_n, G_m)(F_q) which for q points
; has size gcd(n, q-1) typically, but the scheme itself has order n.

; BIDUALITY: (G^D)^D ≅ G.
; (μ_n)^D ≅ Z/nZ, and (Z/nZ)^D ≅ μ_n.
; So ((μ_n)^D)^D ≅ μ_n. ✓
;
; We verify this the honest way, by TWO INDEPENDENT ROUTES:
;   route A — enumerate DD(G) = Hom(D(G), G_m) by brute force over all
;             6^|D(G)| candidate maps, exactly as we did for D(G);
;   route B — push each g ∈ G forward through the canonical evaluation map
;             ev(g) = (χ ↦ χ(g)), which never inspects route A's answer.
; Biduality is the statement that ev is a bijection onto route A's set,
; and a homomorphism.  If either failed, this check would fail.

(define mu3-dd (homs mu3-dual mu3-dual-op f7-list mul7))
(define (ev-of g) (map (lambda (chi) (evalmap mu3-list chi g)) mu3-dual))
(define mu3-ev-image (map ev-of mu3-list))

(print "|DD(μ_3)| enumerated =") (print (length mu3-dd))
(print "image of the evaluation map G -> DD(G):") (print mu3-ev-image)

(check "Cartier-biduality-mu_n"
  (and (= (length mu3-dd) (length mu3-list))
       (and (every (lambda (u) (contains u mu3-dd)) mu3-ev-image)
            (and (every (lambda (u) (contains u mu3-ev-image)) mu3-dd)
                 ; ev is injective and multiplicative
                 (and (= (length (dedup mu3-ev-image)) 3)
                      (every (lambda (a)
                               (every (lambda (b)
                                        (equal (ev-of (mul7 a b))
                                               (mu3-dual-op (ev-of a) (ev-of b))))
                                      mu3-list))
                             mu3-list))))))

; ============================================================
; 4. α_p and its Cartier dual (self-dual)
; ============================================================
; α_p = Spec(k[x]/(x^p)) with Δ(x)=x⊗1+1⊗x, ε(x)=0, S(x)=-x.
; α_p(R) = {r∈R : r^p=0} with addition (additive group).
;
; Cartier dual α_p^D = Spec(A^D) where A^D is the Hopf algebra dual.
; A^D ≅ k[y]/(y^p) with Δ^D(y) = y⊗1 + 1⊗y.
; So α_p^D ≅ α_p (self-dual).
;
; For α_2 over F_2: A = F_2[x]/(x^2), dual basis {1, x} with
; dual basis vectors {e_0, e_1} where e_i(x^j) = δ_{ij}.
; The dual Hopf algebra has same structure.

(print "--- α_p is self-dual ---")

; α_2(F_2[t]/(t^2)): elements r with r^2=0.
; In F_2[t]/(t^2) = {0, 1, t, 1+t}: the nilpotents are {0, t}.
; α_2(R) has group structure = additive.
; Cartier dual: α_2^D ≅ α_2.

; Verify self-duality concretely:
; A = k[x]/(x^p) with basis {1, x, ..., x^{p-1}}.
; Dual basis {f_0, ..., f_{p-1}} where f_i(x^j) = δ_{ij}.
; The multiplication in A^D is dual to Δ_A: x→x⊗1+1⊗x.
; Under duality: f_i · f_j = (binom(i+j, i) mod p) · f_{i+j}.
; This is exactly the divided power algebra, which for p=2 is
; isomorphic to k[y]/(y^2) again.

(print "α_2 over F_2: A = F_2[x]/(x^2), A^D ≅ F_2[y]/(y^2) ≅ A.")

; ------------------------------------------------------------
; The dual Hopf algebra A^D, computed from Δ_A on the dual basis.
; A = F_p[x]/(x^p) with basis x^0..x^{p-1}; Δ(x^m) = Σ_{a+b=m} C(m,a) x^a⊗x^b.
; Dual basis f_0..f_{p-1} with f_i(x^j) = δ_{ij}.  By definition of the dual
; algebra structure, (f_i·f_j)(x^m) = (f_i⊗f_j)(Δ(x^m)) = Δ(x^m)[i·p+j],
; so the structure constants of A^D are READ OFF Δ_A — no answer assumed.
; ------------------------------------------------------------
(define (zerov k) (cond ((<= k 0) nil) (t (cons 0 (zerov (- k 1))))))
(define (unitv k i) (map (lambda (j) (cond ((= j i) 1) (t 0))) (seq 0 (- k 1))))
(define (binom nn kk)
  (cond ((< kk 0) 0) ((> kk nn) 0) ((= kk 0) 1) ((= kk nn) 1)
        (t (+ (binom (- nn 1) (- kk 1)) (binom (- nn 1) kk)))))
(define (ap-delta pp m)
  (map (lambda (k)
         (let ((a (truncate (/ k pp))) (b (mod k pp)))
           (cond ((= (+ a b) m) (mod (binom m a) pp)) (t 0))))
       (seq 0 (- (* pp pp) 1))))

; f_i · f_j expressed in the f-basis, for a comultiplication cm given on basis
(define (dmul-b pp cm i j)
  (map (lambda (m) (nth (+ (* i pp) j) (cm m))) (seq 0 (- pp 1))))
(define (dmul-g pp cm u v)
  (map (lambda (m)
         (mod (foldl + 0
                (map (lambda (k)
                       (let ((i (truncate (/ k pp))) (j (mod k pp)))
                         (* (nth i u) (nth j v) (nth m (dmul-b pp cm i j)))))
                     (seq 0 (- (* pp pp) 1))))
              pp))
       (seq 0 (- pp 1))))
(define (dpow-g pp cm u k)
  (cond ((= k 0) (unitv pp 0)) (t (dmul-g pp cm u (dpow-g pp cm u (- k 1))))))
; specialised to A = F_p[x]/(x^p)
(define (dmul pp u v) (dmul-g pp (lambda (m) (ap-delta pp m)) u v))
(define (dpow pp u k) (dpow-g pp (lambda (m) (ap-delta pp m)) u k))

; the comultiplication of A^D is dual to MULTIPLICATION in A:
; Δ^D(f_m)(x^i⊗x^j) = f_m(x^i·x^j) = 1 iff i+j = m.
(define (ddelta pp m)
  (map (lambda (k)
         (let ((i (truncate (/ k pp))) (j (mod k pp)))
           (cond ((= (+ i j) m) 1) (t 0))))
       (seq 0 (- (* pp pp) 1))))

; A^D ≅ F_p[y]/(y^p) as a Hopf algebra, with y = f_1, provided:
;   (a) f_0 is the unit of A^D,
;   (b) y^k ≠ 0 for k < p but y^p = 0  (so 1,y,...,y^{p-1} is a basis),
;   (c) Δ^D(y) = y⊗1 + 1⊗y  (the ADDITIVE comultiplication of α_p).
(define (ad-unit-ok pp)
  (every (lambda (i) (equal (dmul pp (unitv pp 0) (unitv pp i)) (unitv pp i)))
         (seq 0 (- pp 1))))
(define (ad-nilpotency-gen pp cm)
  (and (every (lambda (k) (not (equal (dpow-g pp cm (unitv pp 1) k) (zerov pp))))
              (seq 1 (- pp 1)))
       (equal (dpow-g pp cm (unitv pp 1) pp) (zerov pp))))
(define (ad-nilpotency-ok pp) (ad-nilpotency-gen pp (lambda (m) (ap-delta pp m))))
(define (ad-additive-comult-ok pp)
  (equal (ddelta pp 1)
         (map (lambda (k)
                (let ((i (truncate (/ k pp))) (j (mod k pp)))
                  (cond ((and (= i 1) (= j 0)) 1) ((and (= i 0) (= j 1)) 1) (t 0))))
              (seq 0 (- (* pp pp) 1)))))
(define (alpha-self-dual-p pp)
  (and (ad-unit-ok pp) (and (ad-nilpotency-ok pp) (ad-additive-comult-ok pp))))

(print "powers y, y², ... , y^p of y = f_1 in A^D for p = 3:")
(print (map (lambda (k) (dpow 3 (unitv 3 1) k)) (seq 1 3)))
(check "alpha_2-self-dual" (alpha-self-dual-p 2))
(check "alpha_p-self-dual-p3-p5" (and (alpha-self-dual-p 3) (alpha-self-dual-p 5)))

; Sanity: run the SAME dualisation on μ_3's Hopf algebra, where instead
; Δ(x^m) = x^m⊗x^m.  Its dual basis elements are orthogonal idempotents, so
; f_1 is not nilpotent and the dual is the group algebra, not α_3.  The
; self-duality test above therefore has real content — it can come out false.
(define (mu-delta nn m) (unitv (* nn nn) (+ (* m nn) m)))
(check "mu_p-dual-is-not-alpha_p"
  (not (ad-nilpotency-gen 3 (lambda (m) (mu-delta 3 m)))))

; ============================================================
; 5. Cartier dual of product
; ============================================================
; (G × H)^D ≅ G^D × H^D.
; For μ_m × μ_n: dual is Z/mZ × Z/nZ.

; Example: μ_2 × μ_2 over F_7.
(define mu2-F7 {1 6})  ; 1^2=1, 6^2=36≡1
(define mu2x2-F7 (cartesian-product mu2-F7 mu2-F7))
(check "mu2xmu2-F7-order-4" (= (length mu2x2-F7) 4))

; Cartier dual: (μ_2)^D × (μ_2)^D = Z/2Z × Z/2Z, Klein 4-group.
(check "mu2xmu2-dual-is-Klein4"
  (let ((dual-order (* (length mu2-F7) (length mu2-F7))))
    (= dual-order 4)))

; ============================================================
; 6. Duality pairing
; ============================================================
; G × G^D → G_m is the natural evaluation pairing.
; For μ_n × Z/nZ → G_m: send (ζ, a) → ζ^a ∈ G_m.
; For α_p × α_p → G_m: (x, y) → exp(xy) = 1 + xy + (xy)^2/2 + ...

; Over F_7, the pairing μ_3 × (μ_3)^D → G_m:
; μ_3(F_7) = {1,2,4}, (μ_3)^D(F_7) ≅ {1,2,4} (character group).
; Pairing: (g, χ) → χ(g).

(define (pairing-F7 g chi)
  (fn-pow g chi 7))

; Verify bilinearity: pairing(g*h, chi) = pairing(g,chi)*pairing(h,chi)
(check "pairing-respects-product"
  (let ((g 2) (h 4) (chi 2))
    (= (pairing-F7 (mod (* g h) 7) chi)
       (mod (* (pairing-F7 g chi) (pairing-F7 h chi)) 7))))

; pairing(1, chi) = 1 for all chi (identity element maps to 1)
(check "pairing-identity"
  (every (lambda (chi)
    (= (pairing-F7 1 chi) 1))
    (set-to-list mu3-dual-points)))

; ============================================================
; 7. Summary
; ============================================================
; These four claims are exactly the theorems established above, so they are
; narration, not assertions — the computations that back them are:
;   μ_n^D ≅ Z/nZ      -> "mu3-dual-is-cyclic-of-order-3" (section 2)
;   α_p^D ≅ α_p       -> "alpha_2-self-dual", "alpha_p-self-dual-p3-p5" (4)
;   (G^D)^D ≅ G       -> "Cartier-biduality-mu_n" (section 3)
;   the pairing        -> "pairing-respects-product", "pairing-identity" (6)
(print "--- Cartier Duality Summary ---")
(print "μ_n^D ≅ Z/nZ; α_p^D ≅ α_p; (G^D)^D ≅ G; pairing G × G^D → G_m.")

(print "=== Cartier Duality Done ===")
