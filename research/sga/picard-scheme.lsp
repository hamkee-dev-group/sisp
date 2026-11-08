; ============================================================
; SGA — Picard Scheme Pic_{X/k}
; For smooth projective curve C: Pic^0(C) = Jacobian.
; Over finite fields: #Pic^0(C)(F_q) = |J(C)(F_q)|.
; For elliptic curve: #J(F_q) = #C(F_q).
; ============================================================

(print "=== Picard Scheme ===")

; ============================================================
; 0. Machinery: F_q arithmetic, the group law on E, divisors on P^1
; ============================================================
; Nothing below is asserted: point counts are obtained by brute force,
; divisor classes by actually adding divisors, and the principality of
; degree-0 divisors on P^1 by expanding an explicit rational function and
; re-extracting its orders of vanishing by synthetic division.

(define (fmod a mm) (mod (+ (mod a mm) mm) mm))
(define (fadd a b mm) (fmod (+ a b) mm))
(define (fsub a b mm) (fmod (- a b) mm))
(define (fmul a b mm) (fmod (* a b) mm))
(define (finv a mm) (find (lambda (u) (= (fmul a u mm) 1)) (seq 1 (- mm 1))))
(define (cube a mm) (fmul a (fmul a a mm) mm))

(define (pairs-over qq)
  (foldr append nil
         (map (lambda (xx) (map (lambda (yy) (list xx yy)) (seq 0 (- qq 1))))
              (seq 0 (- qq 1)))))
(define (all-pairs l)
  (foldr append nil (map (lambda (u) (map (lambda (v) (list u v)) l)) l)))
(define (all-triples l)
  (foldr append nil
         (map (lambda (pr) (map (lambda (w) (list (car pr) (cadr pr) w)) l))
              (all-pairs l))))

; ---- E: y^2 = x^3 + a x + b over F_q; the point at infinity is the symbol inf
(define (ec-affine-points aa bb qq)
  (filter (lambda (pt)
            (= (fmul (cadr pt) (cadr pt) qq)
               (fadd (cube (car pt) qq) (fadd (fmul aa (car pt) qq) bb qq) qq)))
          (pairs-over qq)))
(define (ec-points aa bb qq) (cons (quote inf) (ec-affine-points aa bb qq)))

; ROUTE 1 to #E(F_q): enumerate every (x,y) in F_q x F_q.
(define (ec-count-brute aa bb qq) (length (ec-points aa bb qq)))

; ROUTE 2 to #E(F_q): the character sum q+1+sum_x chi(x^3+ax+b), with the
; quadratic character evaluated by Euler's criterion.  No y is ever
; enumerated, so this is a genuinely independent computation.
(define (fpow base ee mm)
  (cond ((= ee 0) 1) (t (fmul base (fpow base (- ee 1) mm) mm))))
(define (chi a qq)
  (cond ((= (fmod a qq) 0) 0)
        ((= (fpow (fmod a qq) (truncate (/ (- qq 1) 2)) qq) 1) 1)
        (t -1)))
(define (ec-count-chi aa bb qq)
  (+ (+ qq 1)
     (foldl + 0
            (map (lambda (xx) (chi (+ (cube xx qq) (+ (* aa xx) bb)) qq))
                 (seq 0 (- qq 1))))))

; ---- the chord-and-tangent group law
(define (ec-neg pt qq)
  (cond ((eq pt (quote inf)) pt) (t (list (car pt) (fmod (- 0 (cadr pt)) qq)))))
(define (ec-add pa pb aa qq)
  (cond ((eq pa (quote inf)) pb)
        ((eq pb (quote inf)) pa)
        ((equal pb (ec-neg pa qq)) (quote inf))
        (t (let* ((x1 (car pa)) (y1 (cadr pa)) (x2 (car pb)) (y2 (cadr pb))
                  (lam (cond ((equal pa pb)
                              (fmul (fadd (fmul 3 (fmul x1 x1 qq) qq) aa qq)
                                    (finv (fmul 2 y1 qq) qq) qq))
                             (t (fmul (fsub y2 y1 qq)
                                      (finv (fsub x2 x1 qq) qq) qq))))
                  (x3 (fsub (fsub (fmul lam lam qq) x1 qq) x2 qq)))
             (list x3 (fsub (fmul lam (fsub x1 x3 qq) qq) y1 qq))))))
(define (ec-mul kk pt aa qq)
  (cond ((= kk 0) (quote inf))
        ((< kk 0) (ec-mul (- 0 kk) (ec-neg pt qq) aa qq))
        (t (ec-add pt (ec-mul (- kk 1) pt aa qq) aa qq))))

; ---- divisors: coefficient vectors over a fixed support
(define (coeff-vectors kk lo hi)
  (cond ((= kk 0) (list nil))
        (t (foldr append nil
                  (map (lambda (v) (map (lambda (c) (cons c v)) (seq lo hi)))
                       (coeff-vectors (- kk 1) lo hi))))))
(define (deg0-divisors kk lo hi)
  (filter (lambda (v) (= (foldl + 0 v) 0)) (coeff-vectors kk lo hi)))

; Abel-Jacobi: the class of sum n_P (P) on an elliptic curve is the
; group-law sum sum n_P . P.
(define (divisor-class coeffs pts aa qq)
  (foldl (lambda (acc pr) (ec-add acc (ec-mul (car pr) (cadr pr) aa qq) aa qq))
         (quote inf) (zip coeffs pts)))
(define (ec-deg0-classes aa bb qq lo hi)
  (let ((pts (ec-points aa bb qq)))
    (dedup (map (lambda (v) (divisor-class v pts aa qq))
                (deg0-divisors (length pts) lo hi)))))

; ---- polynomials over F_q, low-to-high coefficients
(define (poly-coef p i)
  (cond ((< i 0) 0) ((>= i (length p)) 0) (t (nth i p))))
(define (poly-mul a b qq)
  (map (lambda (k)
         (fmod (foldl + 0 (map (lambda (i) (* (poly-coef a i) (poly-coef b (- k i))))
                               (seq 0 k)))
               qq))
       (seq 0 (- (+ (length a) (length b)) 2))))
(define (poly-eval p a qq)
  (fmod (foldl + 0 (map (lambda (i) (* (poly-coef p i) (^ a i)))
                        (seq 0 (- (length p) 1))))
        qq))
; synthetic division of p by (x - a)
(define (poly-div-root p a qq)
  (let* ((hicoef (reverse p))
         (bs (foldl (lambda (acc c) (cons (fmod (+ c (* a (car acc))) qq) acc))
                    (list 0) hicoef)))
    (reverse (butlast (cdr (reverse bs))))))
; order of vanishing of p at a, recovered from the EXPANDED polynomial
(define (root-mult p a qq)
  (cond ((eq p nil) 0)
        ((= (poly-eval p a qq) 0) (+ 1 (root-mult (poly-div-root p a qq) a qq)))
        (t 0)))
(define (linear-factor a qq) (list (fmod (- 0 a) qq) 1))
(define (poly-power p kk qq)
  (cond ((= kk 0) (list 1)) (t (poly-mul p (poly-power p (- kk 1) qq) qq))))

; ---- divisors on P^1(F_q): ns = coefficients at the affine points 0..q-1,
; the coefficient at infinity being forced to -(sum ns) so that deg = 0.
; f = prod_a (x-a)^{n_a}; num and den are expanded by polynomial multiplication.
(define (poly-from ns qq wantpos)
  (foldl (lambda (acc pr)
           (let ((ee (cond (wantpos (max (cadr pr) 0)) (t (max (- 0 (cadr pr)) 0)))))
             (poly-mul acc (poly-power (linear-factor (car pr) qq) ee qq) qq)))
         (list 1) (zip (seq 0 (- qq 1)) ns)))
(define (div-of ns qq)
  (let* ((num (poly-from ns qq t))
         (den (poly-from ns qq nil))
         (ords (map (lambda (a) (- (root-mult num a qq) (root-mult den a qq)))
                    (seq 0 (- qq 1))))
         (oinf (- (- (length den) 1) (- (length num) 1))))
    (list ords oinf)))
; the class of D is D - div(f); it is 0 exactly when D is principal
(define (p1-class ns qq)
  (let* ((dd (div-of ns qq))
         (ninf (- 0 (foldl + 0 ns))))
    (list (map (lambda (pr) (- (car pr) (cadr pr))) (zip ns (car dd)))
          (- ninf (cadr dd)))))
(define (p1-deg0-class-count qq lo hi)
  (length (dedup (map (lambda (ns) (p1-class ns qq)) (coeff-vectors qq lo hi)))))

; ============================================================
; 1. Picard group and Picard scheme
; ============================================================
; For a variety X over a field k:
; Pic(X) = {invertible sheaves on X} / isomorphism.
; Pic_{X/k} = the relative Picard functor (representable under nice conditions).
;
; Pic^0(X) = {invertible sheaves of degree 0} (connected component).
; For smooth projective curve C of genus g:
; Pic^0(C) = J(C) (Jacobian variety), an abelian variety of dimension g.
;
; For an elliptic curve E (g=1): Pic^0(E) ≅ E itself!
; The map E -> Pic^0(E): P -> [(P) - (O)], i.e. O(P) ⊗ O(O)^{-1}, is an isomorphism.

(print "--- 1. Picard scheme basics ---")
(print "Pic^0(C) = J(C) for curves.")
(print "For elliptic curves: Pic^0(E) ≅ E (self-duality).")

; SISP: the statement Pic^0(E) ≅ E only makes sense because E(F_q) IS a group.
; Verify that from scratch: the chord-and-tangent law on the brute-forced
; point set is closed, unital, has inverses, is commutative and associative.
; Associativity is checked over every triple — nothing here is asserted.
; y^2=x^3+x over F_5 is all 2-torsion, so it would never exercise the
; tangent (doubling) formula; y^2=x^3+x+1 over F_5 has points of odd order
; and does.  Both are tested.
(define (ec-is-a-group aa bb qq)
  (let ((pts (ec-points aa bb qq)))
    (and (every (lambda (pr) (contains (ec-add (car pr) (cadr pr) aa qq) pts))
                (all-pairs pts))
         (every (lambda (pt) (equal (ec-add pt (quote inf) aa qq) pt)) pts)
         (every (lambda (pt) (eq (ec-add pt (ec-neg pt qq) aa qq) (quote inf))) pts)
         (every (lambda (pr) (equal (ec-add (car pr) (cadr pr) aa qq)
                                    (ec-add (cadr pr) (car pr) aa qq)))
                (all-pairs pts))
         (every (lambda (tr)
                  (equal (ec-add (ec-add (car tr) (cadr tr) aa qq) (caddr tr) aa qq)
                         (ec-add (car tr) (ec-add (cadr tr) (caddr tr) aa qq) aa qq)))
                (all-triples pts)))))
(check "picard-basics"
  (and (ec-is-a-group 1 0 5)
       (ec-is-a-group 1 1 5)
       (ec-is-a-group 0 1 5)
       ; a point of order 3 exists on y^2=x^3+1 over F_5, so doubling is
       ; genuinely exercised above and not short-circuited by 2-torsion
       (any (lambda (pt) (and (not (eq pt (quote inf)))
                              (not (eq (ec-mul 2 pt 0 5) (quote inf)))))
            (ec-points 0 1 5))))

; ============================================================
; 2. Over finite fields: point counting on Jacobian
; ============================================================
; #J(C)(F_q) = #Pic^0(C)(F_q).
; For elliptic curve E/F_q: #J(E)(F_q) = #E(F_q).
; (Since E is isomorphic to its Jacobian.)
;
; Verification: E: y^2 = x^3 + x over F_5.
; #E(F_5) = 4. So #J(E)(F_5) = 4.
;
; E: y^2 = x^3 + x + 1 over F_5.
; #E(F_5) = 9. So #J(E)(F_5) = 9.
;
; More generally for genus g curve: #J(C)(F_q) = P_C(1)
; where P_C(T) is the numerator of the zeta function (degree 2g polynomial).
; For elliptic curves (g=1): P_C(T) = 1 - a_q T + q T^2.
; #J(C)(F_q) = P_C(1) = 1 - a_q + q = #C(F_q). ✓

(print "--- 2. Point counting on Jacobian ---")
(print "For elliptic E: #J(F_q) = P_E(1) = 1 - a_q + q = #E(F_q).")

; SISP computation: verify for two elliptic curves over F_5.
; #J(F_q) is computed from the zeta numerator, P(1) = 1 - a_q + q, while
; #E(F_q) is computed by brute force on the curve equation.  Two routes.
(define (jacobian-count aq qq) (+ (- 1 aq) qq))

(define ec1-count (ec-count-brute 1 0 5))   ; E: y^2=x^3+x over F_5, a=2
(define ec2-count (ec-count-brute 1 1 5))   ; E: y^2=x^3+x+1 over F_5, a=-3

(print "#E_1(F_5) counted, P(1) =") (print (list ec1-count (jacobian-count 2 5)))
(print "#E_2(F_5) counted, P(1) =") (print (list ec2-count (jacobian-count -3 5)))
(check "jacobian-E1-F5" (= (jacobian-count 2 5) ec1-count))
(check "jacobian-E2-F5" (= (jacobian-count -3 5) ec2-count))

; ============================================================
; 3. Divisor classes and Picard group
; ============================================================
; For a curve C/F_q, Div(C) = formal sums of closed points.
; Pic(C) = Div(C) / principal divisors.
; Pic^0(C) = {divisors of degree 0} / principal divisors.
;
; For P^1(F_q): Pic(P^1) = Z (degree map). Pic^0(P^1) = {0}.
; For elliptic curve E: Pic^0(E) ≅ E. Each divisor class [D]
; with deg(D) = 0 corresponds to a unique point P ∈ E such that
; D ~ (P) - (O) (linear equivalence).
;
; This is why #Pic^0(E)(F_q) = #E(F_q).

(print "--- 3. Divisor classes ---")
(print "Deg-0 divisors on E modulo principal: each class = [P - O].")
(print "Map P -> [P - O] is bijection E -> Pic^0(E).")

; SISP: build Pic^0(E)(F_q) BY HAND.  Enumerate every divisor sum n_P (P)
; of degree 0 with coefficients in {-1,0,1} over the whole point set, send
; each to its class by the Abel-Jacobi sum sum n_P . P (group law), and
; collect the distinct classes.  The resulting count is compared against
; the point count obtained from the character sum — a different route.
(print "Pic^0(E_1/F_5) classes:") (print (ec-deg0-classes 1 0 5 -1 1))
(check "divisor-classes-E"
  (and (= (length (ec-deg0-classes 1 0 5 -1 1)) (ec-count-chi 1 0 5))
       (= (length (ec-deg0-classes 2 0 3 -1 1)) (ec-count-chi 2 0 3))
       ; y^2=x^3+1 over F_5 is not 2-torsion, so its class group really
       ; depends on the tangent case of the group law
       (= (length (ec-deg0-classes 0 1 5 -1 1)) (ec-count-chi 0 1 5))
       (= (ec-count-brute 1 0 5) (ec-count-chi 1 0 5))
       (= (ec-count-brute 0 1 5) (ec-count-chi 0 1 5))
       (= (ec-count-brute 2 0 3) (ec-count-chi 2 0 3))
       ; every class really is of the form [P - O] for a point P of the curve
       (every (lambda (cl) (contains cl (ec-points 0 1 5)))
              (ec-deg0-classes 0 1 5 -1 1))))

; ============================================================
; 4. The Picard scheme of P^1
; ============================================================
; Pic(P^1) ≅ Z: every invertible sheaf is O(n) for some n.
; Pic^0(P^1) = {O} (trivial).
; #Pic^0(P^1)(F_q) = 1 regardless of q.
; The Jacobian of P^1 is a point.

(print "--- 4. Picard scheme of P^1 ---")
(print "Pic^0(P^1) = {0}. #J(P^1)(F_q) = 1 for all q.")

; SISP: for EVERY degree-0 divisor D on P^1(F_q) with coefficients in
; {-1,0,1}, build the rational function f = prod_a (x-a)^{n_a} as an
; expanded numerator/denominator pair, recover div(f) by extracting orders
; of vanishing with synthetic division (plus deg(den)-deg(num) at infinity),
; and reduce D by it.  Every class collapses to 0, so there is exactly ONE
; degree-0 class.  The CONTRAST with the elliptic curve, where the same
; style of enumeration produces q+1-a_q distinct classes, is what makes
; this check discriminating rather than vacuous.
(print "#Pic^0(P^1) over F_3, F_5:")
(print (list (p1-deg0-class-count 3 -1 1) (p1-deg0-class-count 5 -1 1)))
(check "picard-P1-is-trivial"
  (and (= (p1-deg0-class-count 3 -1 1) 1)
       (= (p1-deg0-class-count 5 -1 1) 1)
       (> (length (ec-deg0-classes 1 0 5 -1 1)) 1)
       (= (length (ec-deg0-classes 1 0 5 -1 1)) (- (+ 5 1) 2))))

; ============================================================
; 5. SISP: Pic^0 vs #C(F_q) equality check
; ============================================================
; Verify the identity #J(F_q) = P(1) for various elliptic curves.
; P(T) = 1 - a_q T + q T^2.
; P(1) = 1 - a_q + q = q + 1 - a_q = #E(F_q).

(define (P-value-at aq q val) (+ 1 (* -1 aq val) (* q (^ val 2))))

; Test: E over F_5 with a=-3: P(1) = 1+3+5=9 = #E(F_5).
(define p1val (P-value-at -3 5 1))
(print "P(1) for E_2/F_5 =") (print p1val)
(check "P1-equals-EC2-count" (and (= p1val 9) (= p1val (ec-count-brute 1 1 5))))

; Test: E over F_7: y^2=x^3+1.  Since 7 ≡ 1 mod 3 the cubic x^3 = -1 has
; three roots in F_7 and the count is #E(F_7) = 12, so a_7 = 8-12 = -4.
; P(1) = 1+4+7 = 12. ✓  (The brute-force count below confirms #E(F_7) = 12.)
(define p2val (P-value-at -4 7 1))
(print "P(1) for E/F_7 y^2=x^3+1 =") (print p2val)
(check "P1-equals-EC-F7" (and (= p2val 12) (= p2val (ec-count-brute 0 1 7))))

; Test: E over F_3: y^2=x^3+2x, #E(F_3)=4, a_3=0.
; P(1) = 1-0+3 = 4. ✓
(define p3val (P-value-at 0 3 1))
(check "P1-equals-EC-F3" (and (= p3val 4) (= p3val (ec-count-brute 2 0 3))))

(print "=== Picard Scheme Done ===")
