; ============================================================
; SGA — Affine Group Schemes over Finite Fields
; G = Spec(A) with comultiplication Δ:A→A⊗A, counit ε:A→F_q, antipode S:A→A.
; Examples: μ_n, α_p, GL_n. Verify group axioms for μ_3 over F_7.
; ============================================================

(print "=== Affine Group Schemes ===")

; ------------------------------------------------------------
; HOPF ALGEBRA MACHINERY (used to verify the axioms as real computations)
;
; A is a finite-dimensional k-algebra of dimension nn over k = F_pp, with a
; distinguished basis b_0,...,b_{nn-1}.  We represent
;   an element of A       as a coefficient list of length nn,
;   an element of A⊗A     as a list of length nn² (index i·nn+j <-> b_i⊗b_j),
;   an element of A⊗A⊗A   as a list of length nn³ (index (u·nn+v)·nn+w).
; A Hopf structure is given by four functions ON BASIS ELEMENTS:
;   dl : i -> Δ(b_i)      in A⊗A
;   ep : i -> ε(b_i)      in k
;   sb : i -> S(b_i)      in A
;   mb : i j -> b_i·b_j   in A
; Everything else is extended k-linearly below, and the three Hopf axioms are
; then evaluated as two INDEPENDENTLY COMPUTED vectors that are compared.
; ------------------------------------------------------------

(define (zerov k) (cond ((<= k 0) nil) (t (cons 0 (zerov (- k 1))))))
(define (unitv k i) (map (lambda (j) (cond ((= j i) 1) (t 0))) (seq 0 (- k 1))))
(define (vadd a b pp) (map (lambda (pr) (mod (+ (car pr) (cadr pr)) pp)) (zip a b)))
(define (vscale c a pp) (map (lambda (u) (mod (* c u) pp)) a))
(define (vsum vs pp len) (foldl (lambda (acc v) (vadd acc v pp)) (zerov len) vs))

; dtab = the table of Δ(b_a) for every basis element, computed once
(define (dtab-of nn dl) (map dl (seq 0 (- nn 1))))

; (Δ⊗id)∘Δ applied to the basis element b_i, as a vector in A⊗A⊗A.
; Writing Δ(b_i) = Σ_{a,b} c[a·nn+b] b_a⊗b_b, the coefficient of
; b_u⊗b_v⊗b_b (index (u·nn+v)·nn + b) is Σ_a c[a·nn+b]·Δ(b_a)[u·nn+v]:
; the FIRST tensor leg gets expanded.
(define (coas-l nn pp dtab i)
  (let ((c (nth i dtab)))
    (map (lambda (k3)
           (let ((b (mod k3 nn)) (m (truncate (/ k3 nn))))
             (mod (foldl + 0 (map (lambda (a)
                                    (* (nth (+ (* a nn) b) c) (nth m (nth a dtab))))
                                  (seq 0 (- nn 1))))
                  pp)))
         (seq 0 (- (* nn nn nn) 1)))))

; (id⊗Δ)∘Δ applied to b_i — the SECOND tensor leg gets expanded, so the
; coefficient of b_a⊗b_u⊗b_v (index a·nn² + (u·nn+v)) is
; Σ_b c[a·nn+b]·Δ(b_b)[u·nn+v].  Different sum, different index arithmetic.
(define (coas-r nn pp dtab i)
  (let ((c (nth i dtab)) (n2 (* nn nn)))
    (map (lambda (k3)
           (let ((a (truncate (/ k3 n2))) (m (mod k3 n2)))
             (mod (foldl + 0 (map (lambda (b)
                                    (* (nth (+ (* a nn) b) c) (nth m (nth b dtab))))
                                  (seq 0 (- nn 1))))
                  pp)))
         (seq 0 (- (* nn nn nn) 1)))))

(define (coassoc-p nn pp dl)
  (let ((dtab (dtab-of nn dl)))
    (every (lambda (i) (equal (coas-l nn pp dtab i) (coas-r nn pp dtab i)))
           (seq 0 (- nn 1)))))

; (ε⊗id)∘Δ contracts the LEFT leg against ε; (id⊗ε)∘Δ contracts the RIGHT one.
(define (counit-l nn pp dl ep i)
  (let ((c (dl i)))
    (map (lambda (b)
           (mod (foldl + 0 (map (lambda (a) (* (nth (+ (* a nn) b) c) (ep a)))
                                (seq 0 (- nn 1))))
                pp))
         (seq 0 (- nn 1)))))
(define (counit-r nn pp dl ep i)
  (let ((c (dl i)))
    (map (lambda (a)
           (mod (foldl + 0 (map (lambda (b) (* (nth (+ (* a nn) b) c) (ep b)))
                                (seq 0 (- nn 1))))
                pp))
         (seq 0 (- nn 1)))))
; the counit axiom says both composites are the IDENTITY of A
(define (counit-p nn pp dl ep)
  (every (lambda (i) (and (equal (counit-l nn pp dl ep i) (unitv nn i))
                          (equal (counit-r nn pp dl ep i) (unitv nn i))))
         (seq 0 (- nn 1))))

; multiply an arbitrary element u of A by a basis element, via mb
(define (vmul-b nn pp mb u b)
  (vsum (map (lambda (w) (vscale (nth w u) (mb w b) pp)) (seq 0 (- nn 1))) pp nn))
(define (vmul-b2 nn pp mb a u)
  (vsum (map (lambda (w) (vscale (nth w u) (mb a w) pp)) (seq 0 (- nn 1))) pp nn))

; m∘(S⊗id)∘Δ and m∘(id⊗S)∘Δ
(define (anti-l nn pp dl sb mb i)
  (vsum (map (lambda (k)
               (let ((a (truncate (/ k nn))) (b (mod k nn)))
                 (vscale (nth k (dl i)) (vmul-b nn pp mb (sb a) b) pp)))
             (seq 0 (- (* nn nn) 1)))
        pp nn))
(define (anti-r nn pp dl sb mb i)
  (vsum (map (lambda (k)
               (let ((a (truncate (/ k nn))) (b (mod k nn)))
                 (vscale (nth k (dl i)) (vmul-b2 nn pp mb a (sb b)) pp)))
             (seq 0 (- (* nn nn) 1)))
        pp nn))
; the antipode axiom says both composites equal η∘ε, i.e. ε(b_i)·1_A
(define (antipode-p nn pp dl ep sb mb)
  (every (lambda (i)
           (let ((rhs (vscale (ep i) (unitv nn 0) pp)))
             (and (equal (anti-l nn pp dl sb mb i) rhs)
                  (equal (anti-r nn pp dl sb mb i) rhs))))
         (seq 0 (- nn 1))))

; ============================================================
; 1. μ_n = Spec(F_q[x]/(x^n-1))
; ============================================================
; A = F_q[x]/(x^n-1), with Hopf algebra structure:
; Comultiplication: Δ(x) = x ⊗ x
; Counit: ε(x) = 1
; Antipode: S(x) = x^{n-1} = x^{-1} (since x^n=1)
;
; Properties:
; Δ is coassociative: (Δ⊗id)∘Δ = (id⊗Δ)∘Δ
; (Δ⊗id)(Δ(x)) = (Δ⊗id)(x⊗x) = x⊗x⊗x
; (id⊗Δ)(Δ(x)) = (id⊗Δ)(x⊗x) = x⊗x⊗x  ✓
;
; Counit axiom: (ε⊗id)∘Δ = id = (id⊗ε)∘Δ
; (ε⊗id)(x⊗x) = 1·x = x ✓
;
; Antipode axiom: μ∘(S⊗id)∘Δ = η∘ε = μ∘(id⊗S)∘Δ
; μ(S(x)⊗x) = x^{n-1}·x = x^n = 1 = ε(x)·1 ✓
;
; Group scheme μ_n represents the functor R -> {r∈R | r^n=1}
; Over F_q: μ_n(F_q) = {x∈F_q | x^n=1} = cyclic group of order gcd(n, q-1).

(print "--- 1. μ_n group scheme ---")
(print "A = F_q[x]/(x^n-1), Δ(x)=x⊗x, ε(x)=1, S(x)=x^{n-1}.")
(print "μ_n(R) = {r∈R | r^n=1} = n-th roots of unity.")

; The Hopf structure of A = F_p[x]/(x^n-1) on the basis 1,x,...,x^{n-1}:
;   Δ(x^i) = x^i⊗x^i,  ε(x^i) = 1,  S(x^i) = x^{-i} = x^{n-i mod n},
;   x^i·x^j = x^{i+j mod n}.
(define (mu-dl nn i) (unitv (* nn nn) (+ (* i nn) i)))
(define (mu-ep i) 1)
(define (mu-sb nn i) (unitv nn (mod (- nn i) nn)))
(define (mu-mb nn i j) (unitv nn (mod (+ i j) nn)))

(define (mu-coassoc nn pp) (coassoc-p nn pp (lambda (i) (mu-dl nn i))))
(define (mu-counit nn pp) (counit-p nn pp (lambda (i) (mu-dl nn i)) mu-ep))
(define (mu-antipode nn pp)
  (antipode-p nn pp (lambda (i) (mu-dl nn i)) mu-ep
              (lambda (i) (mu-sb nn i)) (lambda (i j) (mu-mb nn i j))))

(print "(Δ⊗id)Δ = (id⊗Δ)Δ on every basis element of F_7[x]/(x^3-1)?")
(print (mu-coassoc 3 7))
(check "mu_n-coassociative" (and (mu-coassoc 3 7) (and (mu-coassoc 4 7) (mu-coassoc 6 7))))
(check "mu_n-counit-axiom"  (and (mu-counit 3 7) (and (mu-counit 4 7) (mu-counit 6 7))))
(check "mu_n-antipode-axiom" (and (mu-antipode 3 7) (and (mu-antipode 4 7) (mu-antipode 6 7))))
(check "mu_n-group-scheme-axioms"
       (and (mu-coassoc 5 11) (and (mu-counit 5 11) (mu-antipode 5 11))))

; The axiom checks are DISCRIMINATING: replacing the antipode S(x)=x^{-1} by
; the identity map breaks m(S⊗id)Δ = ηε, and replacing ε by the zero map
; breaks (ε⊗id)Δ = id.
(check "mu_n-wrong-antipode-is-rejected"
       (not (antipode-p 3 7 (lambda (i) (mu-dl 3 i)) mu-ep
                        (lambda (i) (unitv 3 i)) (lambda (i j) (mu-mb 3 i j)))))
(check "mu_n-wrong-counit-is-rejected"
       (not (counit-p 3 7 (lambda (i) (mu-dl 3 i)) (lambda (i) 0))))

; ============================================================
; 2. Verify group axioms for μ_3 over F_7
; ============================================================
; F_7^× = {1,2,3,4,5,6}, order 6.
; μ_3(F_7) = {x∈F_7 | x³=1}.
; x=1: 1³=1. ✓
; x=2: 2³=8≡1. ✓
; x=3: 3³=27≡6. ✗
; x=4: 4³=64≡1. ✓
; x=5: 5³=125≡6. ✗
; x=6: 6³=216≡6. ✗
; μ_3(F_7) = {1,2,4}, cyclic of order 3.

; Check group axioms on μ_3(F_7):
; 1) Closure: 2*2=4∈μ₃, 2*4=8≡1∈μ₃, 4*4=16≡2∈μ₃. ✓
; 2) Identity: 1∈μ₃. ✓
; 3) Inverses: 1^{-1}=1, 2^{-1}=4 (since 2*4=1), 4^{-1}=2. ✓
; 4) Associativity: multiplication in F_7 is associative. ✓

(print "--- 2. μ_3 over F_7 ---")
(print "F_7^× = {1,2,3,4,5,6}, order 6.")
(print "μ_3(F_7) = {x∈F_7: x³=1} = {1,2,4}.")

(define mu3-F7 {1 2 4})
(define (check-mu3-order) (= (length mu3-F7) 3))
(print "|μ_3(F_7)| = 3:") (print (check-mu3-order))
(check "mu3-F7-order-3" (check-mu3-order))

; Verify closure: for all a,b∈μ₃(F₇), a*b mod 7 ∈ μ₃(F₇)
(define mu3-products (map (lambda (p) (car p)) (cartesian-product mu3-F7 mu3-F7)))
; Closure: every product a*b mod 7 must lie in μ₃(F₇)
(define (mu3-closed)
  (every (lambda (a)
    (every (lambda (b)
      (in (mod (* a b) 7) mu3-F7))
      mu3-F7))
    mu3-F7))
(print "μ_3(F_7) is closed under multiplication?") (print (mu3-closed))
(check "mu3-F7-closed" (mu3-closed))

; Verify associativity
(define (mu3-assoc)
  (every (lambda (a)
    (every (lambda (b)
      (every (lambda (c)
        (= (mod (* (mod (* a b) 7) c) 7) (mod (* a (mod (* b c) 7)) 7)))
        mu3-F7))
      mu3-F7))
    mu3-F7))
(print "Multiplication in μ_3(F_7) is associative?") (print (mu3-assoc))
(check "mu3-F7-assoc" (mu3-assoc))

; Verify inverses
(define (mu3-inverses)
  (every (lambda (x)
    (any (lambda (y) (= (mod (* x y) 7) 1)) mu3-F7))
    mu3-F7))
(print "Every element of μ_3(F_7) has an inverse?") (print (mu3-inverses))
(check "mu3-F7-inverses" (mu3-inverses))

; ============================================================
; 3. α_p = Spec(F_p[x]/(x^p))
; ============================================================
; A = F_p[x]/(x^p), Hopf algebra:
; Δ(x) = x⊗1 + 1⊗x
; ε(x) = 0
; S(x) = -x
;
; Coassociativity: (Δ⊗id)Δ(x) = x⊗1⊗1 + 1⊗x⊗1 + 1⊗1⊗x = (id⊗Δ)Δ(x). ✓
; Counit: (ε⊗id)Δ(x) = 0·1 + 1·x = x. ✓
; Antipode: μ(S⊗id)Δ(x) = -x + x = 0 = ε(x)·1. ✓
;
; Group scheme α_p represents R -> {r∈R | r^p=0} with additive group structure.
; α_p(F_q) = {0} (only zero, since F_q is a field).
; But α_p(F_q[t]/(t^p)) has many nilpotents.

(print "--- 3. α_p group scheme ---")
(print "A = F_p[x]/(x^p), Δ(x)=x⊗1+1⊗x, ε(x)=0, S(x)=-x.")
(print "α_p(R) = {r∈R | r^p=0} with addition.")

; Hopf structure of A = F_p[x]/(x^p) on the basis 1,x,...,x^{p-1}.
; Δ(x^i) = (x⊗1 + 1⊗x)^i = Σ_{a+b=i} C(i,a) x^a⊗x^b   (binomial theorem),
; ε(x^i) = δ_{i,0}, S(x^i) = (-1)^i x^i, x^i·x^j = x^{i+j} (0 if i+j ≥ p).
(define (binom nn kk)
  (cond ((< kk 0) 0) ((> kk nn) 0) ((= kk 0) 1) ((= kk nn) 1)
        (t (+ (binom (- nn 1) (- kk 1)) (binom (- nn 1) kk)))))
(define (ap-dl pp i)
  (map (lambda (k)
         (let ((a (truncate (/ k pp))) (b (mod k pp)))
           (cond ((= (+ a b) i) (mod (binom i a) pp)) (t 0))))
       (seq 0 (- (* pp pp) 1))))
(define (ap-ep i) (cond ((= i 0) 1) (t 0)))
(define (ap-sb pp i) (vscale (mod (^ (- pp 1) i) pp) (unitv pp i) pp))
(define (ap-mb pp i j) (cond ((< (+ i j) pp) (unitv pp (+ i j))) (t (zerov pp))))

(define (ap-coassoc pp) (coassoc-p pp pp (lambda (i) (ap-dl pp i))))
(define (ap-counit pp) (counit-p pp pp (lambda (i) (ap-dl pp i)) ap-ep))
(define (ap-antipode pp)
  (antipode-p pp pp (lambda (i) (ap-dl pp i)) ap-ep
              (lambda (i) (ap-sb pp i)) (lambda (i j) (ap-mb pp i j))))

(print "Δ(x²) in F_3[x]/(x³), as a vector over the basis x^a⊗x^b:")
(print (ap-dl 3 2))
(check "alpha_p-coassociative" (and (ap-coassoc 2) (and (ap-coassoc 3) (ap-coassoc 5))))
(check "alpha_p-counit-axiom"  (and (ap-counit 2)  (and (ap-counit 3)  (ap-counit 5))))
(check "alpha_p-antipode-axiom" (and (ap-antipode 2) (and (ap-antipode 3) (ap-antipode 5))))
(check "alpha_p-axioms" (and (ap-coassoc 7) (and (ap-counit 7) (ap-antipode 7))))

; Δ must be the ADDITIVE comultiplication, not the multiplicative one:
; if we tried Δ(x^i)=x^i⊗x^i on F_3[x]/(x³) with ε(x^i)=δ_{i,0}, the counit
; axiom collapses.  And S(x)= x instead of -x breaks the antipode axiom.
(check "alpha_p-needs-additive-comultiplication"
       (not (counit-p 3 3 (lambda (i) (unitv 9 (+ (* i 3) i))) ap-ep)))
(check "alpha_p-wrong-antipode-is-rejected"
       (not (antipode-p 3 3 (lambda (i) (ap-dl 3 i)) ap-ep
                        (lambda (i) (unitv 3 i)) (lambda (i j) (ap-mb 3 i j)))))

; ============================================================
; 4. GL_n = Spec(F_q[x_{ij}, det^{-1}])
; ============================================================
; A = F_q[x_{11},...,x_{nn}, t]/(t·det(X) - 1)
; where det(X) = Σ_{σ∈S_n} sgn(σ) x_{1σ(1)}·...·x_{nσ(n)}.
;
; For n=1: GL_1 = G_m = Spec(F_q[x, x^{-1}]).
; A = F_q[x, y]/(xy-1), Δ(x) = x⊗x, Δ(y) = y⊗y.
; G_m(R) = R^× (units).
; Over F_q: G_m(F_q) = F_q^×, cyclic of order q-1.

(print "--- 4. GL_n group scheme ---")
(print "GL_n = Spec(F_q[x_{ij}, det^{-1}]). For n=1: GL_1 = G_m = F_q^×.")
(print "G_m(F_q) = F_q^× cyclic of order q-1.")

; The functor of points, made concrete: GL_2(F_q) is the set of 2×2 matrices
; over F_q whose determinant is a UNIT.  We enumerate all q^4 matrices, keep
; the invertible ones, and compare the count with the closed formula
; |GL_2(F_q)| = (q²-1)(q²-q) derived independently (choose a nonzero first
; row in q²-1 ways, then a second row outside its span in q²-q ways).
(define (m2mul u v qq)
  (let ((a (nth 0 u)) (b (nth 1 u)) (c (nth 2 u)) (d (nth 3 u))
        (e (nth 0 v)) (f (nth 1 v)) (g (nth 2 v)) (h (nth 3 v)))
    (list (mod (+ (* a e) (* b g)) qq) (mod (+ (* a f) (* b h)) qq)
          (mod (+ (* c e) (* d g)) qq) (mod (+ (* c f) (* d h)) qq))))
(define (m2det u qq) (mod (- (* (nth 0 u) (nth 3 u)) (* (nth 1 u) (nth 2 u))) qq))
(define (all-mats qq)
  (let ((rr (seq 0 (- qq 1))))
    (foldr append nil (map (lambda (a)
      (foldr append nil (map (lambda (b)
        (foldr append nil (map (lambda (c)
          (map (lambda (d) (list a b c d)) rr)) rr))) rr))) rr))))
(define (gl2 qq) (filter (lambda (u) (/= (m2det u qq) 0)) (all-mats qq)))
(define (gl2-formula qq) (* (- (* qq qq) 1) (- (* qq qq) qq)))

(print "|GL_2(F_2)| enumerated / by formula:")
(print (list (length (gl2 2)) (gl2-formula 2)))
(print "|GL_2(F_3)| enumerated / by formula:")
(print (list (length (gl2 3)) (gl2-formula 3)))
(check "GL_n-definition"
       (and (= (length (gl2 2)) (gl2-formula 2))
            (= (length (gl2 3)) (gl2-formula 3))))

; It really is a group: closed under multiplication, and every element has an
; inverse INSIDE the enumerated set.  (Both verified by brute force.)
(define (gl2-closed qq)
  (let ((gg (gl2 qq)))
    (every (lambda (u) (every (lambda (v) (contains (m2mul u v qq) gg)) gg)) gg)))
(define (gl2-inverses qq)
  (let ((gg (gl2 qq)))
    (every (lambda (u) (any (lambda (v) (equal (m2mul u v qq) (list 1 0 0 1))) gg)) gg)))
(check "GL_2-F3-closed" (gl2-closed 3))
(check "GL_2-F3-inverses" (gl2-inverses 3))

; Contrast: the SINGULAR matrices (det = 0) are closed under multiplication
; but contain no invertible elements — that is exactly why det^{-1} has to be
; adjoined to the coordinate ring.
(check "singular-matrices-have-no-inverses"
       (let ((sing (filter (lambda (u) (= (m2det u 3) 0)) (all-mats 3))))
         (not (any (lambda (u)
                     (any (lambda (v) (equal (m2mul u v 3) (list 1 0 0 1))) sing))
                   sing))))

; ============================================================
; 5. Product group scheme μ_2 × μ_2 over F_7
; ============================================================
; μ_2(F_7) = {x: x²=1} = {1,6} (cyclic of order 2).
; μ_2 × μ_2(F_7) = {(a,b): a²=b²=1} of order 4.
; This is isomorphic to Z/2Z × Z/2Z.
; The product Hopf algebra is (F_q[x]/(x²-1)) ⊗ (F_q[y]/(y²-1))
; ≅ F_q[x,y]/(x²-1, y²-1).

(print "--- 5. μ_2 × μ_2 over F_7 ---")
(define mu2-F7 {1 6})
(define mu2x2-F7 (cartesian-product mu2-F7 mu2-F7))
(print "μ_2(F_7) = {1,6} (order 2).")
(print "|μ_2 × μ_2(F_7)| =") (print (length mu2x2-F7))
(check "mu2xmu2-F7-order-4" (= (length mu2x2-F7) 4))

(print "=== Affine Group Schemes Done ===")
