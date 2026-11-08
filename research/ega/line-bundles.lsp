; ============================================================
; ega/line-bundles -- Invertible sheaves = line bundles.
; Picard group Pic(X) = H¹(X, O_X^×).
; Pic(Spec(R)) = 0 for local R.
; Cover by D(f_i) and transition functions.
; ============================================================

(print "=== EGA: Line Bundles ===")

; ---------- Picard group for affine schemes ----------
; Pic(Spec(R)) ≅ group of isomorphism classes of rank-1 projective
; R-modules, under ⊗_R.  It is not zero for every affine scheme: for a
; Dedekind domain it is the ideal class group, e.g. Pic(Z[√-5]) ≅ Z/2Z.
;
; Theorem: Pic(Spec(R)) = 0 for any local ring R, since finitely generated
; projective modules over a local ring are free (Kaplansky: even without
; finite generation).  Also Pic(R_1 × R_2) = Pic(R_1) × Pic(R_2).
;
; Z/6Z is NOT local, but Z/6Z ≅ Z/2Z × Z/3Z (CRT), so
; Pic(Z/6Z) = Pic(Z/2Z) × Pic(Z/3Z) = 0 × 0 = 0 (fields are local).
; So every line bundle on Spec(Z/6Z) is trivial.
; Likewise Z/12Z ≅ Z/3Z × Z/4Z, where Z/4Z is local (its unique maximal
; ideal is (2)) though not a field; hence Pic(Z/12Z) = Pic(Z/3Z) × Pic(Z/4Z) = 0.
;
; In general a finite ring is a finite product of local rings, so
; Pic(Spec(R)) = 0 for every finite ring R: ALL line bundles on Spec of a
; finite ring are trivial.  A non-trivial Picard group needs an infinite
; ring such as Z[√-5].

; ---------- Spec(Z/6Z) --- trivial Picard group ----------
(define z6 {0 1 2 3 4 5})
(define (add6 a b) (mod (+ a b) 6))
(define (mul6 a b) (mod (* a b) 6))
(define (neg6 a) (mod (- 0 a) 6))

(define (ideal-p6 s)
  (and (in 0 s)
    (every (lambda (a)
      (every (lambda (b) (in (add6 a b) s)) s)) s)
    (every (lambda (a) (in (neg6 a) s)) s)
    (every (lambda (a)
      (every (lambda (r) (in (mul6 r a) s)) z6)) s)))

(define ideals6 (filter (lambda (s) (ideal-p6 s)) (powerset z6)))
(define (prime-p6 p)
  (and (not (equal p z6))
    (every (lambda (a)
      (every (lambda (b)
        (or (notin (mul6 a b) p) (in a p) (in b p))) z6)) z6)))
(define spec6 (filter (lambda (p) (prime-p6 p)) ideals6))

; ---------- Line bundle = rank-1 locally free sheaf ----------
; A sheaf L is a line bundle if there exists an open cover {U_i}
; such that L|_{U_i} ≅ O|_{U_i} (free of rank 1).
; Equivalently: L is invertible = L ⊗ L^∨ ≅ O.

; For an affine scheme X = Spec(R), a line bundle corresponds to
; a rank-1 projective R-module P. Since a finite ring R is a product of local
; rings, and projective modules over local rings are free, P ≅ R as R-modules.
; So every line bundle on X = Spec(R) is trivial.

; The trivial line bundle is R viewed as a module over itself.  For that to be
; FREE OF RANK 1 the generator 1 must span R and have zero annihilator.
(check "trivial-line-bundle-module-is-R"
  (and (equal (map (lambda (r) (mul6 r 1)) z6) z6)
       (= (length (filter (lambda (r) (= (mul6 r 1) 0)) z6)) 1)
       (= (length z6) 6)))

; ---------- Cover by D(f_i) and transition functions ----------
; A line bundle can be described by gluing data:
; - An open cover {U_i} (e.g., {D(2), D(3)} for Spec(Z/6Z)).
; - On each U_i, L(U_i) ≅ O(U_i) (free of rank 1).
; - On overlaps U_i ∩ U_j, identify the two trivializations via
;   an element of O(U_i ∩ U_j)^× (a unit in the ring of regular functions).
;
; For Spec(Z/6Z):
; U₀ = D(2) = {(3)}, O(U₀) = R_2 ≅ Z/3Z. Units: {1, 2} (since Z/3Z* = Z/2Z).
; U₁ = D(3) = {(2)}, O(U₁) = R_3 ≅ Z/2Z. Units: {1} (only 1).
; U₀∩U₁ = ∅, O(U₀∩U₁) = {0}, the zero ring, whose only unit is 0 (= 1 there).
;
; A line bundle is given by transition functions g_{ij} ∈ O(U_i ∩ U_j)^×
; satisfying the cocycle condition: g_{ij}·g_{jk} = g_{ik} on triple intersections.
; Here the only transition function is the unique unit of the zero ring, and
; there are no triple intersections, so only the trivial bundle arises.

; ---------- Pic as Cech H¹ with O_X^× ----------
; Ȟ¹(X, O_X^×) classifies line bundles up to isomorphism: Pic(X) ≅ H¹(X, O_X^×).
; O_X^× is the sheaf of units: a sheaf of abelian groups, but NOT a
; quasi-coherent O_X-module, so Serre's vanishing theorem for affine schemes
; does not apply to it, and H¹ can be non-zero on an affine scheme (Z[√-5]).
; For finite rings it vanishes because every local factor has Pic = 0.

; Pic(R) = {invertible ideals} / {principal ideals}.  Compute both sides over
; the ACTUAL ideal lattice of Z/6Z rather than asserting the theorem.
(define (ideal-mul6 i j)
  (list-to-set
    (foldr append nil
      (map (lambda (a) (map (lambda (b) (mul6 a b)) (set-to-list j)))
           (set-to-list i)))))

; I is invertible when some ideal J satisfies I*J = R.
(define invertible6
  (filter (lambda (i) (any (lambda (j) (equal (ideal-mul6 i j) z6)) ideals6))
              ideals6))

; the principal ideals (a) = aR
(define principals6
  (list-to-set (map (lambda (a) (map (lambda (r) (mul6 r a)) z6))
                    (set-to-list z6))))

(define (principal-p i) (any (lambda (pi-id) (equal i pi-id)) principals6))

; Pic = 0 means: every invertible ideal is principal, so the quotient
; {invertible}/{principal} has a single class.
(check "Pic-spec-Z6-is-zero"
  (and (every (lambda (i) (principal-p i)) invertible6)
       (= (length (filter (lambda (i) (not (principal-p i))) invertible6)) 0)
       (= (length invertible6) 1)
       (equal (car (set-to-list invertible6)) z6)))

; Sanity: the lattice being searched is not degenerate -- Z/6Z really does have
; four ideals ((0),(2),(3),(1)) and most of them are NOT invertible.
(check "Z6-ideal-lattice-is-nondegenerate"
  (and (= (length ideals6) 4)
       (= (length (filter (lambda (i) (not (principal-p i))) ideals6)) 0)
       (< (length invertible6) (length ideals6))))

; The structural reason Pic vanishes: Z/6Z splits by CRT into local rings.
; e=3 and e=4 are complementary nontrivial idempotents, and each factor eR
; is a field (every nonzero element invertible), hence local.
(define idem6 (filter (lambda (e) (= (mul6 e e) e)) z6))
(define (factor-of e) (map (lambda (r) (mul6 r e)) z6))
(define (field-factor-p e)
  (let ((s (factor-of e)))
    (every (lambda (x) (or (= x 0) (any (lambda (y) (= (mul6 x y) e)) s))) s)))

(check "Z6-splits-into-local-factors"
  (and (equal idem6 (list-to-set (list 0 1 3 4)))
       (= (add6 3 4) 1)
       (= (mul6 3 4) 0)
       (= (length (factor-of 3)) 2)
       (= (length (factor-of 4)) 3)
       (= (* (length (factor-of 3)) (length (factor-of 4))) (length z6))
       (field-factor-p 3)
       (field-factor-p 4)))

; ---------- Explicit: O(D) line bundle from a divisor ----------
; For a prime divisor P (point), the line bundle O(P) has
; sections: meromorphic functions with at most a simple pole at P.
; On an affine curve (Spec of Dedekind domain), every line bundle
; is of the form O(D) for some divisor D.
;
; But for affine Spec(R) where R is a finite ring, Spec has only
; finitely many points (atoms in the Zariski topology).
; The Picard group is trivial, so all O(D) are trivial.

; ---------- For a local ring, Pic = 0 ----------
; Example: R = Z/9Z (local, maximal ideal (3)).
; Z/9Z is local → every projective module is free → Pic=0.
; Units: Z/9Z^× = {1,2,4,5,7,8} (order φ(9)=6).

(define z9 {0 1 2 3 4 5 6 7 8})
(define (mul9 a b) (mod (* a b) 9))

(define units9 (filter (lambda (x)
  (and (not (= x 0))
    (any (lambda (y) (= (mul9 x y) 1)) z9)))
  z9))

(check "Z9-units-has-6-elements" (= (length units9) 6))

; Z/9Z is LOCAL: verify that directly by enumerating its ideals and finding
; exactly one maximal one, namely (3) = {0,3,6}.  Then verify Pic = 0 the same
; way as for Z/6Z: the only invertible ideal is R, and R is principal.
(define (add9 a b) (mod (+ a b) 9))
(define (neg9 a) (mod (- 0 a) 9))

(define (ideal-p9 s)
  (and (in 0 s)
    (every (lambda (a) (every (lambda (b) (in (add9 a b) s)) s)) s)
    (every (lambda (a) (in (neg9 a) s)) s)
    (every (lambda (a) (every (lambda (r) (in (mul9 r a) s)) z9)) s)))

(define ideals9 (filter (lambda (s) (ideal-p9 s)) (powerset z9)))

; maximal = proper, and not strictly contained in any other proper ideal
(define proper9 (filter (lambda (i) (not (equal i z9))) ideals9))
(define maximal9
  (filter (lambda (i)
    (not (any (lambda (j) (and (not (equal i j)) (subset i j))) proper9)))
    proper9))

(define (ideal-mul9 i j)
  (list-to-set
    (foldr append nil
      (map (lambda (a) (map (lambda (b) (mul9 a b)) (set-to-list j)))
           (set-to-list i)))))

(define invertible9
  (filter (lambda (i) (any (lambda (j) (equal (ideal-mul9 i j) z9)) ideals9))
              ideals9))

(check "Z9-is-a-local-ring"
  (and (= (length ideals9) 3)
       (= (length maximal9) 1)
       (equal (car (set-to-list maximal9)) (list-to-set (list 0 3 6)))))

(check "Pic-local-is-zero"
  (and (= (length invertible9) 1)
       (equal (car (set-to-list invertible9)) z9)
       ; R is principal, generated by 1, so the single class is trivial
       (equal (map (lambda (r) (mul9 r 1)) z9) z9)))

; ---------- Projective modules that are not line bundles ----------
; Over a non-local finite ring R ≅ ∏ R_i (R_i local), a finitely generated
; projective module is free over each factor R_i, but its ranks on the
; different factors may differ.  Such a module is projective but not free.
;
; Example: R = F_2 × F_3 ≅ Z/6Z. The module M = F_2 × 0 is projective
; (it's a direct summand of R), but not free (its rank is not constant:
; 1 at one point of Spec(R), 0 at the other), so it is not a line bundle.
;
; A line bundle must be rank 1 everywhere on Spec. For R = F_2 × F_3,
; Spec(R) = {P₁, P₂} (two points), and a line bundle L is free of rank 1 on
; each factor, so L ≅ F_2 × F_3 = R.
; So every line bundle on a product of fields is isomorphic to R itself.
; Pic = 0.

; ---------- Pic = Ȟ¹({D(2),D(3)}, O^×), computed ----------
; Localize Z/6Z at f: over a finite ring R_f is the stable image of
; multiplication by f (the part of R on which f acts invertibly).
(define (loc6 f)
  (let ((nxt (map (lambda (a) (mul6 f a)) z6)))
    (labels (((go s)
      (let ((nx (map (lambda (a) (mul6 f a)) s)))
        (cond ((= (length nx) (length s)) s)
              (t (go nx))))))
      (go nxt))))

; the multiplicative identity of a subring, found by search
(define (ring-one s)
  (car (set-to-list
    (filter (lambda (e)
      (every (lambda (a) (= (mul6 e a) a)) s)) s))))

(define (units-of s)
  (let ((e (ring-one s)))
    (filter (lambda (x)
      (any (lambda (y) (= (mul6 x y) e)) s)) s)))

(define (unit-inv x s)
  (let ((e (ring-one s)))
    (car (set-to-list (filter (lambda (y) (= (mul6 x y) e)) s)))))

(define o-u0 (loc6 2))               ; O(D(2)) = R_2
(define o-u1 (loc6 3))               ; O(D(3)) = R_3
(define o-u01 (loc6 (mul6 2 3)))     ; O(D(2)∩D(3)) = R_6 = R_0 = zero ring

(print (list (quote rings) o-u0 o-u1 o-u01
             (quote units) (units-of o-u0) (units-of o-u1) (units-of o-u01)))

; restriction to the overlap: in a one-element target ring every section
; lands on that ring's single element.
(define (restrict-to-u01 x)
  (cond ((= (length o-u01) 1) (car (set-to-list o-u01)))
        (t (mod x 6))))

; 1-cocycles: units of O(U0 ∩ U1) (no triple overlaps, so no cocycle condition).
(define cocycles (units-of o-u01))

; 1-coboundaries: g = h0|_{U01} · (h1|_{U01})^{-1}
(define coboundaries
  (map (lambda (pr)
    (let ((h0 (restrict-to-u01 (car pr)))
          (h1 (restrict-to-u01 (cadr pr))))
      (mul6 h0 (unit-inv h1 o-u01))))
    (cartesian-product (units-of o-u0) (units-of o-u1))))

; Pic = Z¹/B¹ is trivial exactly when every cocycle is a coboundary.
(check "line-bundles-onfinite-rings-are-trivial"
  (and (equal cocycles coboundaries) (= (length cocycles) 1)))

(print "=== EGA: Line Bundles Done ===")
