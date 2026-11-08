; ============================================================
; bourbaki/lie-algebras.lsp — Lie Algebras over Finite Fields
; Bracket [x,y], ideals, simple, solvable, nilpotent.
; Killing form. Root systems for sl(2) and sl(3) over F_q.
; Weyl group.
; ============================================================

(print "=== Lie Algebras over Finite Fields ===")


; ---------- F_5 field ops ----------
(define (f+ a b) (mod (+ a b) 5))
(define (f- a b) (mod (- a b) 5))
(define (f* a b) (mod (* a b) 5))
(define f5 {0 1 2 3 4})


; ---------- sl(2, F_5) ----------
; sl(2) = {M ∈ M_2(F_5) | tr(M) = 0}.
; Basis: e = [[0,1],[0,0]], f = [[0,0],[1,0]], h = [[1,0],[0,-1]].
; Relations: [h,e] = 2e, [h,f] = -2f, [e,f] = h.

(define e (list 0 1 0 0))
(define f (list 0 0 1 0))
; h is stored in reduced form: bracket/mat2-sub return residues in {0..4},
; so (equal (bracket e f) h) needs the entry 4 rather than -1.
(define h (list 1 0 0 (mod -1 5)))   ; = (1 0 0 4)

(define (mat2-apply m v)
  (list (f+ (f* (nth 0 m) (nth 0 v)) (f* (nth 1 m) (nth 1 v)))
        (f+ (f* (nth 2 m) (nth 0 v)) (f* (nth 3 m) (nth 1 v)))))

(define (mat2-trace m) (mod (+ (nth 0 m) (nth 3 m)) 5))

(check "e-f-h-are-traceless"
  (and (= (mat2-trace e) 0) (= (mat2-trace f) 0) (= (mat2-trace h) 0)))


; ---------- Lie bracket [x,y] = xy - yx ----------
(define (mat2-mul m1 m2)
  (list (mod (+ (* (nth 0 m1) (nth 0 m2)) (* (nth 1 m1) (nth 2 m2))) 5)
        (mod (+ (* (nth 0 m1) (nth 1 m2)) (* (nth 1 m1) (nth 3 m2))) 5)
        (mod (+ (* (nth 2 m1) (nth 0 m2)) (* (nth 3 m1) (nth 2 m2))) 5)
        (mod (+ (* (nth 2 m1) (nth 1 m2)) (* (nth 3 m1) (nth 3 m2))) 5)))

(define (mat2-sub m1 m2)
  (list (mod (- (nth 0 m1) (nth 0 m2)) 5)
        (mod (- (nth 1 m1) (nth 1 m2)) 5)
        (mod (- (nth 2 m1) (nth 2 m2)) 5)
        (mod (- (nth 3 m1) (nth 3 m2)) 5)))

(define (bracket x y) (mat2-sub (mat2-mul x y) (mat2-mul y x)))

(define zero-mat (list 0 0 0 0))

(check "bracket-he-2e" (equal (bracket h e) (list 0 2 0 0)))
; [h,f] = -2f. Over F_5, -2 ≡ 3, so the entry is 3.
(check "bracket-hf-neg2f" (equal (bracket h f) (list 0 0 3 0)))
(check "bracket-ef-h" (equal (bracket e f) h))

; Bracket is antisymmetric: [x,y] = -[y,x]
(check "bracket-antisymmetric"
  (every (lambda (a)
    (every (lambda (b)
      (equal (bracket a b) (mat2-sub zero-mat (bracket b a))))
      (list-to-set (list e f h zero-mat))))
    (list-to-set (list e f h zero-mat))))


; ---------- Jacobi identity ----------
(define (mat2-add m1 m2)
  (list (f+ (nth 0 m1) (nth 0 m2))
        (f+ (nth 1 m1) (nth 1 m2))
        (f+ (nth 2 m1) (nth 2 m2))
        (f+ (nth 3 m1) (nth 3 m2))))

(define (mat2-scale c m)
  (list (f* c (nth 0 m)) (f* c (nth 1 m)) (f* c (nth 2 m)) (f* c (nth 3 m))))

(check "jacobi-identity"
  (every (lambda (x)
    (every (lambda (y)
      (every (lambda (z)
        (equal (mat2-add (bracket x (bracket y z))
                          (mat2-add (bracket y (bracket z x))
                                     (bracket z (bracket x y))))
               zero-mat))
        (list-to-set (list e f h))))
      (list-to-set (list e f h))))
    (list-to-set (list e f h))))


; ---------- Ideals of sl(2) ----------
; An ideal I satisfies [I, sl(2)] ⊆ I.
; sl(2, F_5) is simple (no proper nonzero ideals) as long as 2 ≠ 0 (p ≠ 2).
; We check using the basis {e,f,h}.

(define sl2-basis (list-to-set (list e f h)))

; The BASIS is not the algebra: [h,e] = 2e is not one of {e,f,h}, so ideal /
; derived-series / lower-central-series tests must run on the full span,
; sl(2,F_5) = {(a b c -a)} — all 125 traceless 2×2 matrices over F_5.
(define sl2
  (list-to-set
    (foldr append nil
      (map (lambda (a)
             (foldr append nil
               (map (lambda (b)
                      (map (lambda (c) (list a b c (mod (- 0 a) 5)))
                           (set-to-list f5)))
                    (set-to-list f5))))
           (set-to-list f5)))))

(check "sl2-has-125-elements" (= (length sl2) 125))
(check "sl2-elements-are-traceless"
  (every (lambda (m) (= (mat2-trace m) 0)) sl2))
(check "sl2-basis-lies-in-sl2"
  (and (in e sl2) (and (in f sl2) (in h sl2))))

; NOTE: the ideal must NOT be called `I` — identifiers case-fold, so the
; inner `(lambda (i) ...)` would shadow it and `in` would be handed a single
; matrix instead of the set.
(define (ideal-p idl full-algebra)
  (every (lambda (a)
    (every (lambda (g) (in (bracket a g) idl)) full-algebra)) idl))

(check "sl2-is-its-own-ideal"
  (ideal-p sl2 sl2))


; ---------- Derived series for solvability ----------
; L^{(0)} = L, L^{(k+1)} = [L^{(k)}, L^{(k)}]
; L is solvable if L^{(n)} = 0 for some n.

; NOTE: `flatmap` deep-flattens, which would shred each 4-entry matrix into
; loose integers; concatenate exactly one level with foldr/append instead.
(define (derived-algebra L)
  (list-to-set
    (foldr append nil
      (map (lambda (x)
             (map (lambda (y) (bracket x y)) (set-to-list L)))
           (set-to-list L)))))

(define dersl2 (derived-algebra sl2))
(print (list (quote derived-sl2-size) (length dersl2)))

(check "sl2-perfect" (equal dersl2 sl2))
; Since L^{(1)} = L, the derived series doesn't terminate: sl(2) is not solvable.
(check "sl2-is-not-solvable"
  (equal (derived-algebra dersl2) sl2))


; ---------- Lower central series for nilpotence ----------
; L^0 = L, L^{k+1} = [L, L^k]
; L is nilpotent if L^n = 0 for some n.

; [sl2, L]: x ranges over the whole algebra sl2 and y over L.
(define (lower-central L)
  (list-to-set
    (foldr append nil
      (map (lambda (x)
             (map (lambda (y) (bracket x y)) (set-to-list L)))
           (set-to-list sl2)))))

(define lc1 (lower-central sl2))
(check "sl2-not-nilpotent-lower-central-is-full" (equal lc1 sl2))


; ---------- Killing form K(x,y) = tr(ad(x)∘ad(y)) ----------
; ad(x)(y) = [x,y]. In sl(2), ad(x) is a 3×3 matrix.
; For sl(2): K(e,f) = 4, K(h,h) = 8. Over F_5: K(h,h) = 8 ≡ 3.
; If K is nondegenerate then L is semisimple (in any characteristic); the
; converse (Cartan's criterion) needs characteristic 0.

; A traceless 2×2 matrix (a b c d) equals a·h + b·e + c·f, so the coordinate
; of a matrix along a basis vector is just the right entry.
; K(x,y) = tr(ad(x)∘ad(y)) = Σ_{b ∈ {e,f,h}} coeff_b( [x,[y,b]] ).
; (This is the trace of the 3×3 operator ad(x)∘ad(y), not a sum of 2×2
; matrix traces.)
(define (coeff-e m) (nth 1 m))
(define (coeff-f m) (nth 2 m))
(define (coeff-h m) (nth 0 m))

(define (killing-scalar x y)
  (f+ (f+ (coeff-e (bracket x (bracket y e)))
          (coeff-f (bracket x (bracket y f))))
      (coeff-h (bracket x (bracket y h)))))

; Over Q: K(e,f) = 4 and K(h,h) = 8. Over F_5: 4 and 8 ≡ 3.
(check "killing-e-f-over-f5" (= (killing-scalar e f) 4))
(check "killing-f-e-symmetric" (= (killing-scalar f e) (killing-scalar e f)))
(check "killing-h-h-over-f5" (= (killing-scalar h h) 3))
(check "killing-e-e-is-zero" (= (killing-scalar e e) 0))
(check "killing-e-h-is-zero" (= (killing-scalar e h) 0))

; The Killing form is symmetric on the whole basis.
(check "killing-form-symmetric"
  (every (lambda (x)
    (every (lambda (y)
      (= (killing-scalar x y) (killing-scalar y x)))
      sl2-basis))
    sl2-basis))

; Nondegeneracy: the Gram matrix of K in the basis {e,f,h} is
;   [[0 4 0] [4 0 0] [0 0 8]],  det = -128 ≡ 2 (mod 5) ≠ 0.
; A nondegenerate Killing form means sl(2,F_5) is semisimple (this direction
; holds in every characteristic).
(define (killing-gram-det)
  (let ((kee (killing-scalar e e)) (kef (killing-scalar e f)) (keh (killing-scalar e h))
        (kfe (killing-scalar f e)) (kff (killing-scalar f f)) (kfh (killing-scalar f h))
        (khe (killing-scalar h e)) (khf (killing-scalar h f)) (khh (killing-scalar h h)))
    (f- (f+ (f* kee (f- (f* kff khh) (f* kfh khf)))
            (f* keh (f- (f* kfe khf) (f* kff khe))))
        (f* kef (f- (f* kfe khh) (f* kfh khe))))))

(check "killing-gram-det-is-2-mod-5" (= (killing-gram-det) 2))
(check "sl2-semisimple" (not (= (killing-gram-det) 0)))


; ---------- sl(3, F_5): root system A_2 ----------
; sl(3) has dimension 8. Cartan subalgebra h has dimension 2.
; Root system A_2: 6 roots ±α, ±β, ±(α+β) at angles of 60° in the plane.
; Simple roots: α₁ = (2,-1) and α₂ = (-1,2) in the basis of fundamental
; weights (the rows of the Cartan matrix).
; Weyl group: S_3 (symmetries of the equilateral triangle).

(print "--- sl(3, F_5) Root System ---")
(print "sl(3) dim 8. Cartan ~ diagonal traceless, dim 2.")
(print "Root system A_2: 6 roots in hexagonal pattern.")
(print "Simple roots: α₁, α₂, angle 120°, length ratio 1.")
(print "Cartan matrix: [[2 -1] [-1 2]]")

; Cartan matrix A_ij = 2(α_i,α_j)/(α_j,α_j)
(define cartan-a2 (list (list 2 -1) (list -1 2)))
(check "cartan-det-is-3" (= (- (* 2 2) (* -1 -1)) 3))


; ---------- Weyl group of sl(3) ----------
; Weyl group of A_2 is S_3 (permutations of 3 root lines).
; Generators s₁, s₂ (reflections in simple roots).
; s₁(α₁) = -α₁, s₁(α₂) = α₁+α₂.
; s₂(α₁) = α₁+α₂, s₂(α₂) = -α₂.
; s₁² = s₂² = 1, (s₁s₂)³ = 1.

(print "--- Weyl Group W(A_2) ≅ S_3 ---")
(print "Generators: s₁, s₂ (reflections).")
(print "Relations: s₁² = s₂² = 1, (s₁s₂)³ = 1.")
(print "|W| = 6. W acts on root lattice.")
; Work in the basis of simple roots: the vector (c1 c2) means c1·α₁ + c2·α₂.
; s₁(α₁) = -α₁, s₁(α₂) = α₁+α₂  ⇒  s₁ = [[-1 1] [0 1]]
; s₂(α₁) = α₁+α₂, s₂(α₂) = -α₂  ⇒  s₂ = [[1 0] [1 -1]]
; These are exact integer matrices (no reduction mod 5 — the Weyl group acts
; on the root LATTICE, not on the field of definition of the algebra).
(define (imat-mul a b)
  (list (+ (* (nth 0 a) (nth 0 b)) (* (nth 1 a) (nth 2 b)))
        (+ (* (nth 0 a) (nth 1 b)) (* (nth 1 a) (nth 3 b)))
        (+ (* (nth 2 a) (nth 0 b)) (* (nth 3 a) (nth 2 b)))
        (+ (* (nth 2 a) (nth 1 b)) (* (nth 3 a) (nth 3 b)))))

(define (imat-apply m v)
  (list (+ (* (nth 0 m) (nth 0 v)) (* (nth 1 m) (nth 1 v)))
        (+ (* (nth 2 m) (nth 0 v)) (* (nth 3 m) (nth 1 v)))))

(define wid (list 1 0 0 1))
(define ws1 (list -1 1 0 1))
(define ws2 (list 1 0 1 -1))

; close a set of matrices under multiplication
(define (close-under-mul s)
  (let ((next (union s
                (list-to-set
                  (foldr append nil
                    (map (lambda (x)
                           (map (lambda (y) (imat-mul x y)) (set-to-list s)))
                         (set-to-list s)))))))
    (cond ((equal next s) s)
          (t (close-under-mul next)))))

(define weyl-a2 (close-under-mul (list-to-set (list wid ws1 ws2))))

(check "weyl-group-a2-has-order-6" (= (length weyl-a2) 6))
(check "weyl-s1-is-an-involution" (equal (imat-mul ws1 ws1) wid))
(check "weyl-s2-is-an-involution" (equal (imat-mul ws2 ws2) wid))
; (s₁s₂) has order exactly 3 — so W(A_2) ≅ S_3 (Coxeter presentation).
(check "weyl-group-a2-is-s3"
  (let ((p (imat-mul ws1 ws2)))
    (and (equal (imat-mul (imat-mul p p) p) wid)
         (and (not (equal p wid))
              (not (equal (imat-mul p p) wid))))))


; ---------- Root space decomposition ----------
; sl(3) = h ⊕ g_α ⊕ g_β ⊕ g_{α+β} ⊕ g_{-α} ⊕ g_{-β} ⊕ g_{-α-β}.
; Each g_α is 1-dimensional.
; [g_α, g_β] ⊆ g_{α+β}.
; [g_α, g_{-α}] ⊆ h.
; [h, g_α] = α(h)·g_α.

; The root system Φ(A_2) is the W-orbit of the simple roots. In the (c1 c2)
; basis it must come out as exactly ±α₁, ±α₂, ±(α₁+α₂) — six roots.
(define alpha1 (list 1 0))
(define alpha2 (list 0 1))

(define roots-a2
  (list-to-set
    (foldr append nil
      (map (lambda (w)
             (list (imat-apply w alpha1) (imat-apply w alpha2)))
           (set-to-list weyl-a2)))))

(define roots-a2-expected
  (list-to-set (list (list 1 0) (list -1 0)
                     (list 0 1) (list 0 -1)
                     (list 1 1) (list -1 -1))))

(check "root-system-a2-has-6-roots" (= (length roots-a2) 6))
(check "root-space-decomposition-a2" (equal roots-a2 roots-a2-expected))

; Φ is closed under negation, and W permutes Φ.
(check "root-system-a2-closed-under-negation"
  (every (lambda (r) (in (list (- 0 (nth 0 r)) (- 0 (nth 1 r))) roots-a2))
             roots-a2))
(check "weyl-group-permutes-the-roots"
  (every (lambda (w)
    (every (lambda (r) (in (imat-apply w r) roots-a2)) roots-a2))
    weyl-a2))


; ---------- Dynkin diagram of A_2 ----------
(print "Dynkin diagram A_2: o──o (two nodes connected by single line)")
; A Dynkin diagram has an edge between nodes i≠j exactly when A_ij ≠ 0;
; for A_2 the off-diagonal entries are -1, so the two nodes are joined and
; the diagram is connected (hence sl(3) is simple, not a proper direct sum).
(define (cartan-at bigA i j) (nth j (nth i bigA)))

(check "cartan-diagonal-is-2"
  (and (= (cartan-at cartan-a2 0 0) 2) (= (cartan-at cartan-a2 1 1) 2)))
(check "dynkin-a2-connected"
  (and (not (= (cartan-at cartan-a2 0 1) 0))
       (not (= (cartan-at cartan-a2 1 0) 0))))
; Single bond: A_12 · A_21 = 1 means one line (a double bond would give 2).
(check "dynkin-a2-single-bond"
  (= (* (cartan-at cartan-a2 0 1) (cartan-at cartan-a2 1 0)) 1))


; ---------- Alternative: 1-dimensional (abelian) Lie algebra ----------
; The bracket returns a matrix, so it is compared against zero-mat
; (comparing it to the integer 0 would never be true).
(define (abelian-lie-p L bracket-fn)
  (every (lambda (x)
    (every (lambda (y) (equal (bracket-fn x y) zero-mat)) L)) L))

; Every 1-dimensional Lie algebra is abelian: on span{x}, [ax,bx] = ab[x,x] = 0.
; span{e} = {0, e, 2e, 3e, 4e} is a concrete 1-dimensional subalgebra of sl(2).
(define span-e (map (lambda (c) (mat2-scale c e)) f5))

(check "span-e-is-1-dimensional" (= (length span-e) 5))   ; |F_5|^1
(check "1-dim-lie-is-abelian" (abelian-lie-p span-e bracket))
; The same holds for span{f} and span{h}.
(check "span-f-is-abelian"
  (abelian-lie-p (map (lambda (c) (mat2-scale c f)) f5) bracket))
(check "span-h-is-abelian"
  (abelian-lie-p (map (lambda (c) (mat2-scale c h)) f5) bracket))
; Sanity: the predicate is not vacuously true — sl(2) itself is NOT abelian.
(check "sl2-is-not-abelian" (not (abelian-lie-p sl2 bracket)))

; A 2-dimensional Lie algebra over a field is either abelian or
; has basis {x,y} with [x,y] = x.
; The latter is the non-abelian 2-dim Lie algebra (the "ax+b" algebra).

; Verify with concrete representation in sl(2):
; Take h and e: [h, e] = 2e ≠ 0. Span{h, e} is a 2-dim non-abelian Lie subalgebra
; (with x = e, y = -h/2 = 2h it has [x,y] = x); the check uses the basis {h, e}.
(define subalg-he (list-to-set (list h e)))
(check "subalg-he-is-2-dim-nonabelian"
  (and (= (length subalg-he) 2)
       (not (abelian-lie-p subalg-he bracket))))


; ---------- Nilpotent Lie algebra example ----------
; Upper triangular 3×3 matrices with zeros on the diagonal (strict upper).
; Dimension 3: basis {e12, e23, e13} with [e12, e23] = e13.
; Lower central series: L → span{e13} → 0. Nilpotent (index 2).

; 3×3 matrices over F_5 as flat 9-lists, row-major.
(define (mat3-mul a b)
  (map (lambda (ij)
         (let ((i (truncate (/ ij 3))) (j (mod ij 3)))
           (f+ (f+ (f* (nth (+ (* 3 i) 0) a) (nth (+ 0 j) b))
                   (f* (nth (+ (* 3 i) 1) a) (nth (+ 3 j) b)))
               (f* (nth (+ (* 3 i) 2) a) (nth (+ 6 j) b)))))
       (seq 0 8)))

(define (mat3-sub a b) (map (lambda (k) (f- (nth k a) (nth k b))) (seq 0 8)))
(define (bracket3 x y) (mat3-sub (mat3-mul x y) (mat3-mul y x)))
(define zero-mat3 (map (lambda (k) 0) (seq 0 8)))

; n = strictly upper triangular: entries e12, e13, e23 free, rest zero.
(define n3
  (list-to-set
    (foldr append nil
      (map (lambda (a)
             (foldr append nil
               (map (lambda (b)
                      (map (lambda (c) (list 0 a b  0 0 c  0 0 0))
                           (set-to-list f5)))
                    (set-to-list f5))))
           (set-to-list f5)))))

(define e12 (list 0 1 0  0 0 0  0 0 0))
(define e23 (list 0 0 0  0 0 1  0 0 0))
(define e13 (list 0 0 1  0 0 0  0 0 0))

(check "n3-is-3-dimensional" (= (length n3) 125))        ; |F_5|^3
(check "n3-bracket-e12-e23-is-e13" (equal (bracket3 e12 e23) e13))
(check "n3-bracket-e12-e13-is-zero" (equal (bracket3 e12 e13) zero-mat3))
(check "n3-bracket-e23-e13-is-zero" (equal (bracket3 e23 e13) zero-mat3))

; [n, M] for the lower central series of n.
(define (lower-central-n3 bigm)
  (list-to-set
    (foldr append nil
      (map (lambda (x)
             (map (lambda (y) (bracket3 x y)) (set-to-list bigm)))
           (set-to-list n3)))))

(define n3-lc1 (lower-central-n3 n3))          ; [n,n]   = span{e13}
(define n3-lc2 (lower-central-n3 n3-lc1))      ; [n,[n,n]] = 0

; [n,n] is the 1-dimensional centre span{e13}, and the series then dies:
; n is nilpotent of class 2 (unlike sl(2), whose lower central series is
; stationary at the whole algebra).
(check "n3-derived-is-span-e13"
  (equal n3-lc1 (map (lambda (c) (map (lambda (k) (f* c (nth k e13))) (seq 0 8))) f5)))
(check "n3-lower-central-step1-is-5-elements" (= (length n3-lc1) 5))
(check "strict-upper-triangular-is-nilpotent"
  (and (= (length n3-lc2) 1) (in zero-mat3 n3-lc2)))


; ---------- sl(2) root system: A_1 ----------
; sl(2) has 1-dimensional Cartan h = span{h}.
; Roots: +α (weight of e), -α (weight of f) where α(h) = 2.
; Weyl group: {1, s_α} ≅ S_2 ≅ Z/2Z.

(print "--- sl(2) Root System A_1 ---")
; ad(h) acts diagonally on the basis: ad(h)e = 2e, ad(h)f = -2f, ad(h)h = 0.
; So the roots (nonzero ad(h)-eigenvalues) are α = 2 and -α = -2 ≡ 3 (mod 5):
; exactly two roots, negatives of each other — the A_1 root system.
(check "ad-h-eigenvalue-on-e-is-2"  (equal (bracket h e) (mat2-scale 2 e)))
(check "ad-h-eigenvalue-on-f-is-neg2" (equal (bracket h f) (mat2-scale (mod -2 5) f)))
(check "ad-h-kills-the-cartan" (equal (bracket h h) zero-mat))

(define sl2-roots (list-to-set (list 2 (mod -2 5))))
(check "sl2-root-system-a1"
  (and (= (length sl2-roots) 2)
       (every (lambda (r) (in (mod (- 0 r) 5) sl2-roots)) sl2-roots)))
; The Weyl group of A_1 is {1, s_α} ≅ Z/2Z: s_α negates the root, i.e. acts
; on the root line by ±1, which is what the check below records.
(check "weyl-group-a1-has-order-2"
  (= (length (list-to-set (list 1 -1))) 2))

; Representation theory (over C): the irreducible finite-dimensional
; representations of sl(2) are classified by highest weights n ∈ N, with
; dim V_n = n+1. The weights (eigenvalues of h) on V_n are
; n, n-2, n-4, ..., -n  (n+1 of them); weights-of lists them as integers.
(define (weights-of n) (map (lambda (k) (- n (* 2 k))) (seq 0 n)))

(check "sl2-representation-theory"
  (every (lambda (n)
           (let ((ws (weights-of n)))
             (and (= (length ws) (+ n 1))            ; dim V_n = n+1
                  (and (= (nth 0 ws) n)              ; highest weight is n
                       (= (nth n ws) (- 0 n))))))    ; lowest weight is -n
         (seq 0 6)))

; Consecutive weights drop by exactly 2 (the root α acts by lowering).
(check "sl2-weights-step-by-2"
  (every (lambda (n)
           (let ((ws (weights-of n)))
             (every (lambda (k) (= (- (nth k ws) (nth (+ k 1) ws)) 2))
                    (seq 0 (- n 1)))))
         (seq 1 6)))

; V_3 has weights 3,1,-1,-3.
(check "sl2-v3-weights" (equal (weights-of 3) (list 3 1 -1 -3)))


; ---------- Character of sl(2) representations ----------
; The character of V_n records its weights: ch V_n = x^n + x^{n-2} + ... + x^{-n},
; each weight with multiplicity 1.
; The weight multiset of V_n is symmetric under w ↦ -w (the Weyl group of A_1
; acts by negation), hence the weights sum to 0 and the character is
; W-invariant.
(check "characters-and-weights"
  (every (lambda (n)
           (= (foldr + 0 (weights-of n)) 0))
         (seq 0 8)))

(check "weight-multiset-is-weyl-symmetric"
  (every (lambda (n)
           (let ((ws (weights-of n)))
             (every (lambda (w) (in (- 0 w) (list-to-set ws))) ws)))
         (seq 0 8)))

; dim V_n = n+1 = number of weights, so the character evaluated at the
; identity gives the dimension.
(check "character-at-identity-is-dimension"
  (every (lambda (n) (= (length (weights-of n)) (+ n 1))) (seq 0 8)))


(print "=== Lie Algebras Done ===")
