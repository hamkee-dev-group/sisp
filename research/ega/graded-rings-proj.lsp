; ============================================================
; ega/graded-rings-proj -- Proj of graded ring.
; Proj(F_q[x,y,z]/(f)) for homogeneous f.
; Proj(F_2[x,y]/(x²+xy+y²)) ≅ Spec(F_4); P¹(F_2) and a plane cubic
; over F_2 with 3 rational points each.
; ============================================================

(print "=== EGA: Graded Rings and Proj ===")

; ---------- Proj(F_2[x,y]/(x²+xy+y²)) ----------
; The homogeneous equation x² + xy + y² = 0 in P¹(F_2).
; Over F_2: compute all solutions in P¹.
;
; P¹(F_2) has 3 points: [1:0], [0:1], [1:1].
; Evaluate f(x,y) = x² + xy + y² mod 2 at each:
; [1:0]: f(1,0) = 1 + 0 + 0 = 1 ≠ 0 → not a zero.
; [0:1]: f(0,1) = 0 + 0 + 1 = 1 ≠ 0 → not a zero.
; [1:1]: f(1,1) = 1 + 1 + 1 = 3 ≡ 1 mod 2 ≠ 0 → not a zero.
;
; So x²+xy+y² = 0 has NO solutions in P¹(F_2): Proj(F_2[x,y]/(x²+xy+y²))
; has no F_2-rational points.  It is not empty as a scheme, though.
;
; f = x²+xy+y² is irreducible over F_2: it is the norm form of F_4/F_2, and
; it splits only over F_4.  So S = F_2[x,y]/(f) is a graded domain of
; dimension 1, with irrelevant ideal S_+ = (x,y).
; Proj(S) = {homogeneous prime P ⊂ S : P ⊉ S_+}.
; The homogeneous primes of S are (0) and S_+, and (0) ⊉ S_+ since S_+ ≠ 0,
; so Proj(S) = {(0)} has exactly 1 point.
;
; That point is the closed point of P¹ cut out by f, with residue field F_4:
; on D₊(x), S_(x) = F_2[t]/(1 + t + t²) ≅ F_4 (t = y/x).  So
; Proj(S) ≅ Spec(F_4): it has no F_2-points and 2 F_4-points, the two roots
; ω, ω² of t² + t + 1 = 0.

; ---------- Proj(F_2[x,y,z]/(z)) = P¹(F_2) ----------
; S = F_2[x,y,z]/(z) ≅ F_2[x,y] (graded, all variables of degree 1).
; Proj(S) = Proj(F_2[x,y]) = P¹ over F_2.
; F_2-points: [0:1], [1:0], [1:1]. Total 3.

; ---------- Proj(F_2[x,y,z]/(x²+y²+z²)) ----------
; Over F_2: x²+y²+z² = (x+y+z)². So the equation is (x+y+z)²=0, which
; defines a double line! The reduced subscheme is the line x+y+z=0 in P²(F_2).
; P²(F_2) has 7 points. Line x+y+z=0: solutions in F_2³:
; (0,0,0) — excluded (projective).
; (1,1,0): 1+1+0=0 ✓
; (1,0,1): 1+0+1=0 ✓
; (0,1,1): 0+1+1=0 ✓
; So 3 points on the line.

; ---------- The points of Proj(F_2[x,y]) ----------
; S = F_2[x,y] (polynomial ring in 2 vars, graded by total degree).
; The homogeneous prime ideals of S are:
; - (0), the generic point;
; - (f) for each irreducible homogeneous f (height 1: these are closed points);
; - (x,y) = S_+ (the irrelevant ideal, excluded from Proj).
; The degree-1 irreducibles give the 3 F_2-rational points, which correspond
; to the 3 lines through the origin in F_2²:
;   (y) = {f : f(x,0)=0} ↔ [1:0],  (x) ↔ [0:1],  (x+y) ↔ [1:1].
; An irreducible of degree d gives a closed point with residue field F_{2^d};
; e.g. (x²+xy+y²) is the F_4-point of the first section.
;
; So as a scheme Proj(F_2[x,y]) has infinitely many points: the generic
; point (0), which is dense but not open, and infinitely many closed points,
; exactly 3 of which are F_2-rational.

; ---------- Working with F_2² lines = P¹(F_2) points ----------
; We can enumerate the 3 lines in F_2² \ {0}:
; NB: {a b} is a SET literal, so {0 0} would collapse to the set {0} and
; {1 0} to {0 1} -- neither is an ordered vector. Use (list ...) for pairs
; so they match the (list x y) pairs produced by cartesian-product below.
(define v00 (list 0 0))
(define v10 (list 1 0))
(define v01 (list 0 1))
(define v11 (list 1 1))

(define f2-v2 (cartesian-product {0 1} {0 1}))
(define f2-v2-nonzero
  (filter (lambda (v) (not (equal v v00))) f2-v2))

; 3 nonzero vectors in F_2² (excluding (0,0))
(check "three-nonzero-vectors" (= (length f2-v2-nonzero) 3))

; Lines through origin = equivalence classes under scaling by F_2^* = {1}.
; Since F_2^* = {1}, each nonzero vector is its own equivalence class.
; So P¹(F_2) has exactly 3 points.
(define p1-f2 f2-v2-nonzero)
(check "P1-F2-has-3-points" (= (length p1-f2) 3))

; Verifying they are [(1,0)], [(0,1)], [(1,1)]:
(check "point-10" (in v10 p1-f2))
(check "point-01" (in v01 p1-f2))
(check "point-11" (in v11 p1-f2))

; ---------- The homogeneous coordinate ring and its Proj ----------
; For P¹, the graded ring is S = F_2[x,y].
; Proj(S) = {homogeneous primes P | P ⊉ S_+}.
; The closed points of degree 1 (homogeneous primes maximal among those
; not containing S_+):
; (y) = {f(x,y)∈F_2[x,y] : f(x,0)=0} ↔ [1:0]
; (x) = {f(x,y)∈F_2[x,y] : f(0,y)=0} ↔ [0:1]
; (x+y) = {f : f(t,t)=0} ↔ [1:1]

; Proj(S) also contains the generic point (0) (since 0 ⊉ S_+) and the
; closed points of higher degree.  Our finite computation captures the
; 3 F_2-rational points.

; ---------- Proj with a homogeneous polynomial ----------
; For Proj(F_2[x,y,z]/(x²y + y²z + z²x)):
; This is a smooth plane cubic over F_2 (in characteristic 2 its partials
; are z², x², y², which vanish together only at 0), i.e. an elliptic curve.
; Points with z=0: the equation becomes x²y = 0.
; In P², points where z=0: [1:0:0], [0:1:0], [1:1:0].
; Check x²y=0: at [1:0:0]: 1*0=0 ✓. at [0:1:0]: 0*1=0 ✓. at [1:1:0]: 1*1=1≠0 ✗.
; Points where z=1: need x²y + y² + x = 0 mod 2.
; On F_2-points y² = y and x² = x, so this is y + x + xy = 0, i.e.
; y(1+x) + x = 0.
; If x=0: y = 0. So [0:0:1].
; If x=1: 0 + 1 = 1 ≠ 0 → no point with x=1, z=1.
; So P²(F_2) points satisfying equation: [1:0:0], [0:1:0], [0:0:1] = 3 points.

; ---------- Compute the 3 points: ----------
; Build P²(F_2) points (7 points) and check the equation.
(define p2-points
  (list-to-set
    (list (list 0 0 1) (list 0 1 0) (list 0 1 1)
          (list 1 0 0) (list 1 0 1) (list 1 1 0) (list 1 1 1))))

(define (eqn-satisfied x y z)
  (let ((x2y (mod (* x x y) 2))
        (y2z (mod (* y y z) 2))
        (z2x (mod (* z z x) 2)))
    (= (mod (+ (+ x2y y2z) z2x) 2) 0)))

(define curve-points
  (filter (lambda (v)
    (eqn-satisfied (car v) (cadr v) (caddr v)))
    p2-points))

(print (list (quote curve-F2-points) curve-points))
(check "three-points-on-curve" (= (length curve-points) 3))

; ---------- Verifying these are the three we computed ----------
(check "point-100" (in (list 1 0 0) curve-points))
(check "point-010" (in (list 0 1 0) curve-points))
(check "point-001" (in (list 0 0 1) curve-points))

; ---------- Point not on the curve ----------
(check "point-111-not-on-curve" (notin (list 1 1 1) curve-points))

; This is Proj(F_2[x,y,z]/(x²y + y²z + z²x))(F_2) — 3 F_2-rational points.
; Each point corresponds to a homogeneous prime of the graded ring that is
; maximal among those not containing (x,y,z).

(print "=== EGA: Graded Rings and Proj Done ===")
