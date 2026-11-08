; ============================================================
; SGA — Tannakian Reconstruction
; Neutral Tannakian category: rigid abelian tensor category with
; fiber functor ω: C -> Vect_K.
; Tannaka duality: C ≅ Rep_K(G) for affine group scheme G = Aut^⊗(ω).
; For Rep(G) with finite G: recover G from the representation category.
; ============================================================

(print "=== Tannakian Reconstruction ===")

; ============================================================
; 1. The Tannakian formalism: Rep(G) -> Vect_K
; ============================================================
; Let G be a finite group, K a field.
; Rep_K(G): finite-dimensional K-representations of G.
; Fiber functor ω: Rep_K(G) -> Vect_K is the forgetful functor
; (V, ρ) -> V, forgetting the G-action.
;
; ω is exact (preserves exact sequences), faithful, and
; is a tensor functor: ω(V⊗W) = ω(V) ⊗_K ω(W).
;
; Aut^⊗(ω) = group of natural automorphisms of ω respecting tensor structure.
; Tannaka: G ≅ Aut^⊗(ω).
;
; Concretely: G acts on each V via ρ_V(g): V -> V.
; This collection {ρ_V(g): V∈Rep(G)} is a tensor automorphism of ω.
; Conversely, any tensor automorphism of ω arises from a unique g∈G.

(print "--- 1. Tannakian formalism ---")
(print "C = Rep_K(G), ω = forgetful functor.")
(print "Aut^⊗(ω) ≅ G: recover group from representations.")

; ------------------------------------------------------------
; SISP machinery: Tannakian reconstruction for G = Z/nZ.
; The simple objects of Rep(Z/nZ) are the characters V_0..V_{n-1},
; with V_a ⊗ V_b = V_{a+b mod n}.  A tensor automorphism of the fibre
; functor ω assigns to each V_a a scalar; writing the scalar as a power
; of the primitive n-th root of unity, sigma is a LIST s of length n with
; s[a] in Z/nZ, and the tensor condition reads
;     s[(a+b) mod n] = (s[a] + s[b]) mod n,   s[0] = 0.
; Aut^⊗(ω) is then found by EXHAUSTIVE SEARCH over all n^n candidate
; assignments — nothing about the answer is assumed.
; ------------------------------------------------------------

(define (all-funcs vals k)
  (cond ((= k 0) (list nil))
        (t (foldr append nil
             (map (lambda (v) (map (lambda (e) (cons e v)) vals))
                  (all-funcs vals (- k 1)))))))

(define (tensor-auto-p s nn)
  (and (= (nth 0 s) 0)
       (every (lambda (pr)
                (= (nth (mod (+ (car pr) (cadr pr)) nn) s)
                   (mod (+ (nth (car pr) s) (nth (cadr pr) s)) nn)))
              (foldr append nil
                (map (lambda (a) (map (lambda (b) (list a b)) (seq 0 (- nn 1))))
                     (seq 0 (- nn 1)))))))

(define (tensor-autos nn)
  (filter (lambda (s) (tensor-auto-p s nn)) (all-funcs (seq 0 (- nn 1)) nn)))

; the reconstruction map G -> Aut^⊗(ω): g^k acts on V_a by zeta^{ak}
(define (recon nn k) (map (lambda (a) (mod (* a k) nn)) (seq 0 (- nn 1))))

(print "n, #candidate assignments, |Aut^tensor(omega)|, |G|:")
(foreach (lambda (nn)
           (print (list nn (length (all-funcs (seq 0 (- nn 1)) nn))
                        (length (tensor-autos nn)) nn)))
         (list 2 3 4))

; Tannaka duality: |Aut^⊗(ω)| = |G|, and every tensor automorphism is
; realised by a group element (so reconstruction is onto).
(check "tannakian-formalism"
  (every (lambda (nn)
           (and (= (length (tensor-autos nn)) nn)
                (every (lambda (k) (contains (recon nn k) (tensor-autos nn)))
                       (seq 0 (- nn 1)))))
         (list 2 3 4)))

; ============================================================
; 2. Reconstruction for G = Z/3Z
; ============================================================
; G = Z/3Z = {1, g, g²} cyclic of order 3.
; Simple representations over C (or char ≠ 3):
; Trivial rep: V_0 = C with g acting by 1.
; χ_1: V_1 = C with g acting by ω = e^{2πi/3} (primitive cube root of unity).
; χ_2: V_2 = C with g acting by ω² = ω^{-1}.
;
; Tensor product: V_a ⊗ V_b ≅ V_{a+b mod 3}.
; This is the character group \hat{G} ≅ Z/3Z.
;
; Reconstruct G as automorphisms of the forgetful functor:
; Each g^k acts on V_a by ω^{ak}.
; The collection of these actions for all V_a determines k mod 3.

(print "--- 2. Reconstruct G = Z/3Z from Rep(G) ---")
(print "Rep(Z/3Z) has 3 simple objects: V_0, V_1, V_2.")
(print "Aut^⊗(ω) ≅ Z/3Z via action on V_1: g ↦ ω.")

; SISP: for n=3, exhibit Aut^⊗(ω) explicitly and check that
; k -> sigma_k is not merely a bijection but a GROUP ISOMORPHISM:
;   sigma_{k+l} = sigma_k * sigma_l   (pointwise sum mod 3),
; and that sigma_1 generates the whole group (so it is cyclic of order 3).
(define (sig-mul s1 s2 nn) (map (lambda (pr) (mod (+ (car pr) (cadr pr)) nn)) (zip s1 s2)))
(define (sig-powers s nn)
  (map (lambda (k) (foldl (lambda (acc i) (sig-mul acc s nn)) (recon nn 0) (seq 1 k)))
       (seq 0 (- nn 1))))

(print "Aut^tensor(omega) for Z/3Z (each row is (sigma(V_0) sigma(V_1) sigma(V_2))):")
(print (tensor-autos 3))
(print "powers of sigma_1:") (print (sig-powers (recon 3 1) 3))

(check "reconstruct-Z3"
  (and (= (length (tensor-autos 3)) 3)
       ; homomorphism: sigma_{k+l} = sigma_k . sigma_l
       (and (every (lambda (pr)
                     (equal (recon 3 (mod (+ (car pr) (cadr pr)) 3))
                            (sig-mul (recon 3 (car pr)) (recon 3 (cadr pr)) 3)))
                   (foldr append nil
                     (map (lambda (a) (map (lambda (b) (list a b)) (seq 0 2))) (seq 0 2))))
            ; injective: the three sigma_k are distinct
            (and (= 3 (length (dedup (map (lambda (k) (recon 3 k)) (seq 0 2)))))
                 ; sigma_1 generates: its powers exhaust Aut^tensor(omega)
                 (= 3 (length (dedup (sig-powers (recon 3 1) 3))))))))

; ============================================================
; 3. The representation ring and character table
; ============================================================
; For G = S_3 (symmetric group on 3 letters, order 6):
; Conjugacy classes: {1}, {transpositions=3}, {3-cycles=2}.
; Irreducible representations over C:
;   χ_0 (trivial):  (1, 1, 1)
;   χ_sign (sign):  (1, -1, 1)
;   χ_std (standard, dim 2): (2, 0, -1)
;
; Tensor products: sign ⊗ std = std, std ⊗ std = triv ⊕ sign ⊕ std.
; The representation ring R(G) has basis {[triv], [sign], [std]}.
; Recover G from the tensor category structure.

(print "--- 3. S_3 character table ---")
(print "Triv:   (1, 1, 1)")
(print "Sign:   (1, -1, 1)")
(print "Std(2): (2, 0, -1)")
; Compute tensor product std ⊗ std:
; χ_std⊗std(g) = χ_std(g)² = (4, 0, 1).
; Decomposition: χ_std² = χ_triv + χ_sign + χ_std,
; since (1+1+2, 1-1+0, 1+1-1) = (4, 0, 1). ✓

; SISP: real character theory of S_3 on the three conjugacy classes
; {1}, {transpositions}, {3-cycles} with sizes 1, 3, 2.  All characters
; are rational, so the inner product
;   <chi,psi> = (1/|G|) * sum_i |C_i| chi(C_i) psi(C_i)
; is exact.  The tensor product of representations is the POINTWISE
; PRODUCT of characters; decomposing it means computing the multiplicities
; <chi_V chi_W, chi_i> and reading them off.
(define s3-classes (list 1 3 2))              ; class sizes
(define s3-triv (list 1 1 1))
(define s3-sign (list 1 -1 1))
(define s3-std  (list 2 0 -1))
(define s3-irrs (list s3-triv s3-sign s3-std))
(define (chi-inner a b)
  (/ (foldl + 0 (map (lambda (tr) (* (car tr) (* (car (cadr tr)) (cadr (cadr tr)))))
                     (zip s3-classes (zip a b))))
     6))
(define (chi-prod a b) (map (lambda (pr) (* (car pr) (cadr pr))) (zip a b)))
(define (decompose ch) (map (lambda (ir) (chi-inner ch ir)) s3-irrs))

(print "std x std character:") (print (chi-prod s3-std s3-std))
(print "multiplicities in (triv sign std):") (print (decompose (chi-prod s3-std s3-std)))
(print "sign x std multiplicities:") (print (decompose (chi-prod s3-sign s3-std)))
(print "sum of squares of dimensions:")
(print (foldl + 0 (map (lambda (ir) (* (nth 0 ir) (nth 0 ir))) s3-irrs)))
(print "std⊗std ≅ triv ⊕ sign ⊕ std (verified).")

(check "S3-tensor-decomp"
  (and (equal (chi-prod s3-std s3-std) (list 4 0 1))
       (and (equal (decompose (chi-prod s3-std s3-std)) (list 1 1 1))
            (and (equal (decompose (chi-prod s3-sign s3-std)) (list 0 0 1))
                 (and ; orthonormality of the character table
                      (every (lambda (pr)
                               (= (chi-inner (nth (car pr) s3-irrs) (nth (cadr pr) s3-irrs))
                                  (cond ((= (car pr) (cadr pr)) 1) (t 0))))
                             (foldr append nil
                               (map (lambda (a) (map (lambda (b) (list a b)) (seq 0 2)))
                                    (seq 0 2))))
                      ; sum of squares of dimensions = |S_3| = 6
                      (= 6 (foldl + 0 (map (lambda (ir) (* (nth 0 ir) (nth 0 ir))) s3-irrs))))))))

; ============================================================
; 4. Fiber functor on Rep(Z/2Z)
; ============================================================
; G = Z/2Z = {1, -1}.
; Reps: V_0 (trivial, dim 1): -1 acts as 1.
;        V_1 (sign, dim 1):    -1 acts as -1.
;
; The tensor category has two simple objects.
; Aut^⊗(ω) = {id, σ}: a tensor automorphism acts on V_0 as 1 (V_0 is the
; unit object, so tensor compatibility forces σ(V_0) = 1) and on V_1 by a
; scalar σ(V_1).
;
; Tensor constraint: σ(V_1⊗V_1) = σ(V_1)⊗σ(V_1) = σ(V_0) = 1.
; So (σ(V_1))² = 1 => σ(V_1) = ±1. Both satisfy: two choices, giving Z/2Z.
; The choice -1 corresponds to g=-1∈G, the choice +1 to g=1.
; So Aut^⊗(ω) = {id, g} ≅ Z/2Z.

(print "--- 4. Reconstruct Z/2Z ---")
(print "Rep(Z/2Z): V_0 (trivial), V_1 (sign).")
(print "Aut^⊗(ω) = {id, flip on V_1} ≅ Z/2Z.")

; SISP: n=2.  Exhaustive search over the 4 candidate assignments
; (s[V_0] s[V_1]) with entries in Z/2Z must return exactly two:
; (0 0) = id and (0 1) = the sign flip.  The other two candidates,
; (1 0) and (1 1), are killed by the unit / tensor conditions — so the
; tensor constraint really does cut the search space down.
(print "all 4 candidate assignments:") (print (all-funcs (list 0 1) 2))
(print "those that are tensor automorphisms:") (print (tensor-autos 2))
(check "reconstruct-Z2"
  (and (equal (tensor-autos 2) (list (list 0 0) (list 0 1)))
       (and (= 4 (length (all-funcs (list 0 1) 2)))       ; 2 of 4 survive: real constraint
            (and (not (tensor-auto-p (list 1 0) 2))
                 (and (not (tensor-auto-p (list 1 1) 2))
                      ; the surviving flip squares to the identity: sigma(V_1)^2 = 1
                      (equal (sig-mul (list 0 1) (list 0 1) 2) (list 0 0)))))))

; ============================================================
; 5. SISP computation: tensor product structure for Z/2Z
; ============================================================
; Model the representation category as SISP sets.
; Objects: {V_0, V_1}
; Tensor product: V_0⊗X = X, V_1⊗V_1 = V_0.
; This is the group Z/2Z: V_0↔0, V_1↔1 with addition mod 2.

(define objs {0 1})  ; 0=V_0(triv), 1=V_1(sign)
(define (tensor a b) (mod (+ a b) 2))

(print "--- 5. Tensor category as SISP ---")
(print "Objects: {0=triv, 1=sign}")
(print "Tensor: a⊗b = (a+b) mod 2")
(print "0⊗0 =") (print (tensor 0 0))
(print "0⊗1 =") (print (tensor 0 1))
(print "1⊗0 =") (print (tensor 1 0))
(print "1⊗1 =") (print (tensor 1 1))
(check "tensor-Z2-1x1=0" (= (tensor 1 1) 0))
(check "tensor-Z2-1x0=1" (= (tensor 1 0) 1))

; Verify associativity
(define (tensor-assoc)
  (every (lambda (a)
    (every (lambda (b)
      (every (lambda (c)
        (= (tensor (tensor a b) c) (tensor a (tensor b c))))
        objs))
      objs))
    objs))
(print "Tensor product is associative:") (print (tensor-assoc))
(check "tensor-assoc-Z2" (tensor-assoc))

(print "=== Tannakian Reconstruction Done ===")
