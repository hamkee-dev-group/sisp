; ============================================================
; EGA -- Intersection Theory: Intersection Multiplicities
; for Curves on Surfaces Over Finite Fields.
; Serre's Tor Formula. Compute Intersection Numbers.
; ============================================================

(print "=== EGA: Intersection Theory ===")

; ============================================================
; 1. INTERSECTION MULTIPLICITY FOR CURVES
; ============================================================
; For two curves C and D on a smooth surface S over a field k,
; the intersection number C·D = Σ_{P∈C∩D} i(C,D;P)
; where i(C,D;P) is the intersection multiplicity at P.

; Serre's Tor formula:
; i(C,D;P) = Σ_{i=0}^{∞} (-1)^i length_{O_{S,P}} Tor_i^{O_{S,P}}(O_{C,P}, O_{D,P})
; where length is the length of the module.

; For a smooth surface S over an algebraically closed field and curves
; C, D with no common component through P (then Tor_i = 0 for i ≥ 1):
; i(C,D;P) = dim_k O_{S,P}/(I_C + I_D)_P = dim_k k[[x,y]]/(f,g)
; where f=0 defines C, g=0 defines D locally.

(print "--- 1. Intersection multiplicity via Tor ---")
(print "i(C,D;P) = Σ (-1)^i length Tor_i(O_{C,P}, O_{D,P})")
(print "For transverse intersection: i = dim_k O_P/(f,g)")

; ============================================================
; 1b. MACHINERY: UNIVARIATE POLYNOMIALS AND LOCAL MULTIPLICITY
; ============================================================
; A polynomial in x is a coefficient list in ASCENDING order:
;   (a0 a1 a2 ...)  represents  a0 + a1*x + a2*x^2 + ...
; A plane curve g(x,y) is a list of x-polynomials indexed by the
; power of y:  (g0 g1 g2 ...)  represents  g0(x) + g1(x)*y + ...

(define (poly-trim cs)
  (cond ((eq cs nil) (list 0))
        ((eq (cdr cs) nil) cs)
        ((= (last cs) 0) (poly-trim (butlast cs)))
        (t cs)))

(define (poly-add pa pb)
  (cond ((eq pa nil) pb)
        ((eq pb nil) pa)
        (t (cons (+ (car pa) (car pb)) (poly-add (cdr pa) (cdr pb))))))

(define (poly-scale kk pa) (map (lambda (cc) (* kk cc)) pa))

(define (poly-mul pa pb)
  (cond ((eq pa nil) nil)
        (t (poly-add (poly-scale (car pa) pb)
                     (cons 0 (poly-mul (cdr pa) pb))))))

(define (poly-monomial kk)
  (cond ((= kk 0) (list 1)) (t (cons 0 (poly-monomial (- kk 1))))))

; degree of a polynomial (after dropping trailing zeros)
(define (poly-deg pa) (- (length (poly-trim pa)) 1))

; order of vanishing at x = 0 = index of the first nonzero coefficient
(define (poly-ord pa)
  (cond ((eq pa nil) 0)
        ((/= (car pa) 0) 0)
        (t (+ 1 (poly-ord (cdr pa))))))

; p(x + a), by Horner:  a0 + (x+a)*q(x+a)  where p = a0 + x*q
(define (poly-shift pa aval)
  (cond ((eq pa nil) (list 0))
        (t (poly-add (list (car pa))
                     (poly-mul (list aval 1) (poly-shift (cdr pa) aval))))))

(define (poly-ord-at pa aval) (poly-ord (poly-shift pa aval)))

(define (poly-reduce-mod pa mm) (map (lambda (cc) (mod cc mm)) pa))

(define (poly-ord-at-mod pa aval mm)
  (poly-ord (poly-reduce-mod (poly-shift pa aval) mm)))

; Substitute y = px(x) into the curve g:  g0 + px*(g1 + px*(g2 + ...))
(define (curve-subst gcoeffs px)
  (cond ((eq gcoeffs nil) (list 0))
        (t (poly-add (car gcoeffs)
                     (poly-mul px (curve-subst (cdr gcoeffs) px))))))

; Local intersection multiplicity at x = a of the graph curve y = px(x)
; with the curve g(x,y) = 0.  Since y - px(x) generates the local ideal of
; the graph, O_P/(y-px, g) = k[[x]]/(g(x,px(x))), whose length is the
; order of vanishing of g(x,px(x)) at x = a.
(define (int-mult px gcoeffs aval)
  (poly-ord-at (curve-subst gcoeffs px) aval))

; Total (affine) intersection number = degree of the eliminated polynomial,
; i.e. the sum of all local multiplicities over the algebraic closure.
(define (int-total px gcoeffs) (poly-deg (curve-subst gcoeffs px)))

; ============================================================
; 2. COMPUTATION FOR SIMPLE CASES OVER FINITE FIELDS
; ============================================================

; --- Case A: Two lines on P² over F_q ---
; C: y=0, D: x=0, meeting at P=(0,0).
; O_{P²,P} ≅ k[[x,y]], O_{C,P} ≅ k[[x,y]]/(y) ≅ k[[x]], O_{D,P} ≅ k[[x,y]]/(x) ≅ k[[y]].
; Tor_0 = k[[x]]⊗_{k[[x,y]]} k[[y]] = k[[x,y]]/(x,y) ≅ k.
; Tor_1: resolve O_{C,P} = R/(y) by 0 → R →^y R → R/(y) → 0 and tensor
;   with R/(x) ≅ k[[y]]: multiplication by y is injective there, so
;   Tor_1 = 0 (equivalently, x, y is a regular sequence in R).
; Tor_i = 0 for i ≥ 1.
; i(C,D;P) = length(k) = 1.

(print "--- 2. Two lines on P² ---")
(print "C: y=0, D: x=0. Transverse at (0,0).")
(print "i(C,D; (0,0)) = dim_k k = 1.")

; C: y = 0 is the graph of px = 0.  D: x = 0 has g0(x) = x, no y terms.
(define crv-line-y0 (list 0))
(define crv-line-x0 (list (list 0 1)))
(check "two-lines-intersect-1"
  (= (int-mult crv-line-y0 crv-line-x0 0) 1))

; --- Case B: Line and conic, tangent ---
; C: y=0, D: y=x^2 over k.
; Meet at (0,0). Not transverse (both have tangent line y = 0 there).
; O_P/(y, y-x^2) ≅ k[[x,y]]/(y, y-x^2) ≅ k[[x]]/(x^2), has dimension 2.
; Here f=y, g=y-x^2, and the ideal (y, y-x^2) = (y, x^2).
; So Tor_0 = R/(y, x^2) ≅ k[[x]]/(x^2), length 2; Tor_1 = 0 (section 3).
; i = 2.

(print "--- Line and conic, tangent ---")
(print "C: y=0, D: y=x^2. At (0,0): i = 2.")

; D: y - x^2 = 0  ->  g0 = -x^2, g1 = 1.
(define crv-parabola (list (list 0 0 -1) (list 1)))
(check "line-conic-tangent-i=2"
  (= (int-mult crv-line-y0 crv-parabola 0) 2))

; --- Case C: Two transversal conics ---
; C: y=x^2, D: x=y^2. Meet at (0,0) transversely.
; O_P/(y-x^2, x-y^2) ≅ k[[x,y]]/(y-x^2, x-x^4) ≅ k[[x]]/(x-x^4) ≅ k[[x]]/(x(1-x^3)).
; Since 1-x^3 is a unit in k[[x]], we get k[[x]]/(x) ≅ k. length = 1.
; i = 1.

(print "--- Two transversal cubics ---")
(print "C: y=x^2, D: x=y^2. Transverse at (0,0), i=1.")

; C: y = x^2 is the graph of px = x^2.  D: x - y^2 = 0 -> g0 = x, g1 = 0, g2 = -1.
; Eliminating gives x - x^4 = x*(1 - x^3), order 1 at x=0 since 1-x^3 is a unit.
(define crv-graph-x2 (list 0 0 1))
(define crv-x-eq-y2 (list (list 0 1) (list 0) (list -1)))
(check "transversal-cubics-i=1"
  (= (int-mult crv-graph-x2 crv-x-eq-y2 0) 1))
; the eliminated polynomial really is x - x^4
(check "transversal-cubics-elimination"
  (equal (poly-trim (curve-subst crv-x-eq-y2 crv-graph-x2))
         (list 0 1 0 0 -1)))

; ============================================================
; 3. SERRE'S TOR FORMULA (computed)
; ============================================================
; For the line-conic tangent case:
; R = k[[x,y]], M = R/(y) ≅ k[[x]], N = R/(y-x^2).
; Compute Tor_i^R(M,N):
;   Resolution of M = R/(y): 0 → R →^y R → M → 0.
;   Tensor with N: 0 → R/(y-x^2) →^y R/(y-x^2) → 0.
;   y acts on R/(y-x^2) by multiplication by y = x^2.
;   So Tor_0 = N/yN = R/(y-x^2, y) = R/(y, x^2) ≅ k[x]/(x^2), length 2.
;   Tor_1 = ker(y on N).  N = R/(y-x^2) ≅ k[[x]] (with y = x^2), so
;   multiplication by y is multiplication by x^2 on k[[x]], which is
;   injective (k[[x]] is an integral domain).  So Tor_1 = 0, length = 0.
; Alternating sum: 2 - 0 = 2. Matches direct computation.

(print "--- 3. Serre Tor formula verified ---")
(print "line-conic: Tor_0 length=2, Tor_1 length=0 => i=2")

; Tor_0 = R/(y, y-x^2) = R/(y, x^2) = k[[x]]/(x^2): length 2.
(define tor0-line-conic (int-mult crv-line-y0 crv-parabola 0))
(check "serre-tor0-length-2" (= tor0-line-conic 2))

; Tor_1 = ker(mult by y on N = R/(y-x^2) = k[[x]]), and y acts as x^2 there.
; Multiplication by x^2 sends the basis monomial x^k to x^(k+2); these images
; have pairwise distinct orders, so the map is injective and the kernel is 0.
(define mult-by-x2-image-orders
  (map (lambda (kk) (poly-ord (poly-mul (list 0 0 1) (poly-monomial kk))))
       (seq 0 7)))
(define tor1-line-conic
  (cond ((= (length (dedup mult-by-x2-image-orders)) 8) 0) (t 1)))
(check "serre-tor1-vanishes" (= tor1-line-conic 0))
(check "serre-mult-by-x2-injective"
  (equal mult-by-x2-image-orders (seq 2 9)))

; Serre's alternating sum reproduces the direct dimension count.
(check "serre-tor-line-conic"
  (= (- tor0-line-conic tor1-line-conic)
     (int-mult crv-line-y0 crv-parabola 0)))

; ============================================================
; 4. INTERSECTION NUMBERS FOR COMPLETE CURVES
; ============================================================
; For two curves on P² over F_q:
; C: degree d, D: degree e. By Bezout's theorem: C·D = d*e.
; This counts intersection points with multiplicity over algebraic closure.

; But over F_q: the F_q-rational intersection points may be fewer.
; The total intersection number over F_q-bar is still d*e.

(print "--- 4. Bezout's theorem ---")
(print "On P²: C (deg d) ∩ D (deg e) = d*e (counted with multiplicity)")

; Verify: line (deg 1) and conic (deg 2) meet in 2 points.
; L: y=0 against the conic y = x^2 - 1, i.e. g0 = 1 - x^2, g1 = 1.
; Eliminating y gives 1 - x^2, degree 2: roots x = 1 and x = -1, each simple.
(define crv-conic-x2-1 (list (list 1 0 -1) (list 1)))
(check "line-conic-bezout-2"
  (= (int-total crv-line-y0 crv-conic-x2-1) (* 1 2)))
(check "line-conic-bezout-2-simple-roots"
  (and (= (int-mult crv-line-y0 crv-conic-x2-1 1) 1)
       (= (int-mult crv-line-y0 crv-conic-x2-1 -1) 1)))
; and the local multiplicities sum to the Bezout number
(check "line-conic-bezout-2-local-sum"
  (= (+ (int-mult crv-line-y0 crv-conic-x2-1 1)
        (int-mult crv-line-y0 crv-conic-x2-1 -1))
     (int-total crv-line-y0 crv-conic-x2-1)))

; Bezout still holds when the two curves are tangent: y=0 and y=x^2 meet at
; the single point (0,0), but with local multiplicity 2 = 1*2.
(check "line-conic-bezout-tangent-2"
  (= (int-mult crv-line-y0 crv-parabola 0)
     (int-total crv-line-y0 crv-parabola)))

; Two lines: deg 1 * deg 1 = 1 intersection point.
; L: y=0 against M: y = x + 1, i.e. g0 = -1 - x, g1 = 1.
(define crv-line-slanted (list (list -1 -1) (list 1)))
(check "two-lines-bezout-1"
  (= (int-total crv-line-y0 crv-line-slanted) (* 1 1)))

; Line and cubic: 1 * 3 = 3 intersection points.
; L: y=0 against y = x^3 - x, i.e. g0 = x - x^3, g1 = 1.
(define crv-cubic (list (list 0 1 0 -1) (list 1)))
(check "line-cubic-bezout-3"
  (= (int-total crv-line-y0 crv-cubic) (* 1 3)))
(check "line-cubic-three-simple-points"
  (and (= (int-mult crv-line-y0 crv-cubic 0) 1)
       (and (= (int-mult crv-line-y0 crv-cubic 1) 1)
            (= (int-mult crv-line-y0 crv-cubic -1) 1))))

; ============================================================
; 5. COMPUTATION OVER FINITE FIELD (F_3)
; ============================================================
; P² over F_3. Line L: x+y+z=0 (deg 1), conic C: x^2+y^2+z^2=0 (deg 2).
; Intersection: solve x+y+z=0 => z = -(x+y). Substitute into conic:
; x^2 + y^2 + (x+y)^2 = 2x^2 + 2y^2 + 2xy = 2(x^2 + xy + y^2) = 0,
; so mod 3: x^2 + xy + y^2 ≡ 0.
; Over F_3 the binary form x^2 + xy + y^2 has discriminant 1 - 4 = -3 ≡ 0,
; so it is a perfect square: x^2 + xy + y^2 = (x + 2y)^2 in F_3[x,y].
; Its only root is x = y, a DOUBLE root: (1,1) gives (1,1,-2) ≡ (1,1,1), and
; (2,2) gives (2,2,2), the same projective point [1:1:1].  So there is
; exactly 1 intersection point, already rational over F_3.
; Geometrically: the tangent line to C at P = [1:1:1] is
;   (∂C/∂x)x + (∂C/∂y)y + (∂C/∂z)z = 2x + 2y + 2z = 2(x+y+z),
; which is exactly L.  So L is tangent to C at [1:1:1] and
;   i(L,C;[1:1:1]) = 2 = deg(L)*deg(C),
; and Bezout is satisfied by that single rational point alone.

(print "--- 5. Line and conic over F_3 ---")
(print "L: x+y+z=0, C: x^2+y^2+z^2=0 => intersect at [1:1:1]")
(print "Bezout=2, met by ONE point: L is tangent to C at [1:1:1], i=2")

; Enumerate P^2(F_3): normalised representatives [1:y:z], [0:1:z], [0:0:1].
(define f3 (list 0 1 2))
(define (concat-map fn l) (foldr append nil (map fn l)))
(define p2-f3
  (append
    (concat-map (lambda (yv) (map (lambda (zv) (list 1 yv zv)) f3)) f3)
    (append (map (lambda (zv) (list 0 1 zv)) f3)
            (list (list 0 0 1)))))
(check "p2-f3-has-13-points" (= (length p2-f3) 13))

(define (on-line-f3 pt)
  (= (mod (+ (car pt) (cadr pt) (caddr pt)) 3) 0))
(define (on-conic-f3 pt)
  (= (mod (+ (* (car pt) (car pt))
             (+ (* (cadr pt) (cadr pt)) (* (caddr pt) (caddr pt)))) 3) 0))

; A line in P^2(F_q) has q+1 points; a smooth conic is isomorphic to P^1,
; so it also has q+1 points.  For q=3 both counts are 4.
(check "line-f3-has-4-points" (= (count on-line-f3 p2-f3) 4))
(check "conic-f3-smooth-4-points" (= (count on-conic-f3 p2-f3) 4))

(define lc-f3-points
  (filter (lambda (pt) (and (on-line-f3 pt) (on-conic-f3 pt))) p2-f3))
(check "line-conic-F3-1-rational-point" (= (length lc-f3-points) 1))
(check "line-conic-F3-point-is-111" (equal lc-f3-points (list (list 1 1 1))))

; Local multiplicity at [1:1:1], computed in the affine chart z = 1.
; L: x + y + 1 = 0  =>  y = -x - 1 ≡ 2x + 2  (mod 3), a graph.
; C: x^2 + y^2 + 1 = 0  =>  g0 = 1 + x^2, g1 = 0, g2 = 1.
; Eliminating: (1 + x^2) + (2x+2)^2 = 5x^2 + 8x + 5 ≡ 2x^2 + 2x + 2
;            = 2(x + 2)^2  (mod 3), a double root at x = 1.
(define crv-line-f3 (list 2 2))
(define crv-conic-f3 (list (list 1 0 1) (list 0) (list 1)))
(define lc-f3-elim (curve-subst crv-conic-f3 crv-line-f3))
(check "line-conic-F3-elim-poly"
  (equal (poly-reduce-mod (poly-trim lc-f3-elim) 3) (list 2 2 2)))
(check "line-conic-F3-tangent-mult-2"
  (= (poly-ord-at-mod lc-f3-elim 1 3) 2))
; Bezout over F_3: the one rational point carries the full multiplicity 1*2.
(check "line-conic-F3-bezout"
  (= (* (length lc-f3-points) (poly-ord-at-mod lc-f3-elim 1 3)) (* 1 2)))

; ============================================================
; 6. INTERSECTION NUMBER ON ELLIPTIC CURVE × LINE
; ============================================================
; For E: y^2 = x^3 + x over F_5, line L: y = 0.
; Intersection points: y=0 => x^3 + x = x(x^2+1) = 0.
; Over F_5: x=0, x^2+1=0 => x^2=4 => x=2,3.
; So 3 points: (0,0), (2,0), (3,0).
; Projectively the line is Y = 0, which does not pass through the point at
; infinity O = [0:1:0]; by Bezout (1*3 = 3) these 3 finite points are the
; whole intersection.

(print "--- 6. Line y=0 ∩ E: y^2=x^3+x / F_5 ---")
(print "Intersection: (0,0), (2,0), (3,0) => 3 points")
(print "Bezout: deg(line)*deg(E) = 1*3 = 3 in P²-bar")

(define f5 {0 1 2 3 4})
(define line-ec-pts
  (filter (lambda (xr)
    (= (mod (+ (* xr xr xr) xr) 5) 0)) f5))
(check "line-y=0-meets-E-at-3-pts" (= (length line-ec-pts) 3))
; x^3 + x = x(x^2+1) and over F_5 we have x^2 = -1 for x = 2 and x = 3.
(check "line-y=0-meets-E-at-x-0-2-3"
  (equal line-ec-pts (list-to-set (list 0 2 3))))
; Bezout: deg(line)*deg(E) = 1*3 = 3, and each of the three roots is simple,
; so all three intersections are already rational over F_5.
(check "line-ec-bezout-3" (= (length line-ec-pts) (* 1 3)))
; each root simple: x^3 + x has no repeated root mod 5 (gcd with 3x^2+1 = 1)
(check "line-ec-roots-simple"
  (every (lambda (xr) (/= (mod (+ (* 3 (* xr xr)) 1) 5) 0)) line-ec-pts))

; ============================================================
; 7. SELF-INTERSECTION NUMBER
; ============================================================
; For a curve C on a surface S: self-intersection C·C.
; Can be computed via the adjunction formula: C·C = deg(N_{C/S}) = 2g(C) - 2 - K_S·C.
; Alternatively: on P², a line L has L·L = 1 (the degree).
; In general, for a curve on P²: C·C = d^2 where d = deg(C).
; Intersection numbers do not change under extension of the base field,
; so the same values hold over F_q.

(print "--- 7. Self-intersection ---")
(print "Line L on P²: L·L = 1")
(print "Conic C on P²: C·C = 4")

; Pic(P²) = Z·H with H·H = 1, and a degree-d curve is linearly equivalent
; to dH, so C·C = d^2.
(define (self-int-degree dd) (* dd dd))

; Independent route, via adjunction:  C·C = 2g(C) - 2 - K_S·C.
; On P²: K = -3H, so K·C = -3d, and g(C) = (d-1)(d-2)/2 for a smooth
; plane curve.  Hence C·C = (d^2 - 3d) + 3d = d^2, matching the above.
(define (genus-plane-curve dd) (truncate (/ (* (- dd 1) (- dd 2)) 2)))
(define (self-int-adjunction dd)
  (- (- (* 2 (genus-plane-curve dd)) 2) (* -3 dd)))

(check "self-intersection-line-1" (= (self-int-degree 1) 1))
(check "self-intersection-conic-4" (= (self-int-degree 2) 4))
(check "adjunction-matches-degree-formula"
  (every (lambda (dd) (= (self-int-adjunction dd) (self-int-degree dd)))
         (seq 1 8)))
; sanity on the genus side: line and conic are rational, cubic is elliptic,
; quartic has genus 3.
(check "plane-curve-genera"
  (equal (map genus-plane-curve (seq 1 5)) (list 0 0 1 3 6)))

(print "=== Intersection Theory Done ===")
