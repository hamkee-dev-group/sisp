; ============================================================
; bourbaki/quadratic-forms.lsp — Quadratic Forms over Finite Fields
; Witt decomposition, classification over F_q (two types),
; orthogonal group.
; ============================================================

(print "=== Quadratic Forms over Finite Fields ===")


; ---------- F_5 field ops ----------
(define (f+ a b) (mod (+ a b) 5))
(define (f- a b) (mod (- a b) 5))
(define (f* a b) (mod (* a b) 5))
(define f5 {0 1 2 3 4})


; ---------- Quadratic forms in 2 variables over F_5 ----------
; Q(x,y) = ax² + bxy + cy².

(define (qform2 a b c x y)
  (mod (+ (* a (* x x)) (* b (* x y)) (* c (* y y))) 5))

(define (representable-p a b c n)
  (any (lambda (v)
    (= (qform2 a b c (nth 0 v) (nth 1 v)) n))
    (cartesian-product f5 f5)))

; The set of nontrivial zeros of Q over F_5 (the zero vector is excluded).
; Its cardinality is an invariant of the equivalence class of Q.
(define (zeros-f5 a b c)
  (filter (lambda (v)
    (and (not (and (= (nth 0 v) 0) (= (nth 1 v) 0)))
         (= (qform2 a b c (nth 0 v) (nth 1 v)) 0)))
    (cartesian-product f5 f5)))

; Discriminant D = b^2 - 4ac of ax^2+bxy+cy^2 over F_5.
(define (disc-qform a b c)
  (mod (- (* b b) (* 4 (* a c))) 5))

; Base-p digit extraction, used to enumerate F_p^n by an integer index.
(define (base-digit v i p-val) (mod (truncate (/ v (^ p-val i))) p-val))

; Check which values the quadratic form Q(x,y) = x² + y² takes on F_5:
; Squares in F_5: 0²=0, 1²=1, 2²=4, 3²=4, 4²=1.
; Values of x²+y² with x², y² ∈ {0,1,4}: 0, 1, 4, 2 (1+1), 0 (1+4), 3 (4+4),
; so every element of F_5 occurs.
(check "x^2+y^2-represents-all-values"
  (every (lambda (n) (representable-p 1 0 1 n)) f5))

; Q(x,y) = x² + 2y². 2 is not a square in F_5 (squares: 0,1,4).
; This is a genuinely different (inequivalent) quadratic form: the number of
; nontrivial zeros is an equivalence invariant and the two counts differ.
(check "x^2+2y^2-distinct-from-x^2+y^2"
  (not (= (length (zeros-f5 1 0 1)) (length (zeros-f5 1 0 2)))))


; ---------- Witt decomposition ----------
; Over a finite field, every quadratic form is the orthogonal sum of:
; (1) Hyperbolic planes H = ⟨x,y⟩ with Q(x)=0, Q(y)=0, B(x,y)=1.
; (2) An anisotropic core of dimension 0, 1, or 2.

; For dimension 1: Q(x) = ax². Two equivalence classes:
;   a is a square (type +) or a is a nonsquare (type -) in F_q^×.
; For F_5: squares = {1,4}, nonsquares = {2,3}.

(define (is-square5 a)
  (any (lambda (x) (= (mod (* x x) 5) (mod a 5))) f5))

(define squares5 (filter (lambda (a) (is-square5 a)) f5))
(define nonsquares5 (diff (diff f5 {0}) squares5))

(check "f5-squares" (equal squares5 {0 1 4}))
(check "f5-nonsquares" (equal nonsquares5 {2 3}))


; ---------- Classification of quadratic forms over F_q ----------
; Every nondegenerate quadratic form over F_q (q odd) in n variables is
; equivalent to:
; - Either Q ≅ x₁² + ... + x_{n-1}² + a x_n²   (type 1)
; - Or Q ≅ x₁² + ... + x_{n-1}² + a' x_n²       (type 2)
; where a and a' represent the two square classes in F_q^×; the discriminant
; tells the two types apart.

; In odd dimension the two types differ only by scaling (multiplying Q by a
; nonsquare swaps them) and have the same Witt index: one type up to scaling.
; In even dimension they are genuinely different: the Witt index differs by 1.

; For n=2 over F_5:
; Type 1: x² + y² (Witt index 1 — represents 0 nontrivially).
; Type 2: x² + 2y² (Witt index 0 — does not represent 0 nontrivially).

(check "x^2+y^2-isotropic"
  (any (lambda (v)
    (and (not (and (= (nth 0 v) 0) (= (nth 1 v) 0)))
         (= (qform2 1 0 1 (nth 0 v) (nth 1 v)) 0)))
    (cartesian-product f5 f5)))

; x² + y² represents 0 nontrivially (e.g., x=1,y=2: 1+4=5≡0).
; So x²+y² is isotropic => Witt index ≥ 1, i.e. it IS a hyperbolic plane
; (a nondegenerate isotropic plane is hyperbolic).
; A hyperbolic plane over F_q has exactly 2(q-1) nontrivial isotropic vectors.
; LHS is enumerated, RHS is the closed formula — computed independently.
(check "x^2+y^2-is-type-1"
  (= (length (zeros-f5 1 0 1)) (* 2 (- 5 1))))

; x² + 2y² = 0 ⇒ x² = -2y² = 3y² (mod 5).
; If y = 0 then x = 0 — trivial.
; If y ≠ 0 then (x/y)² = 3, but 3 is not a square mod 5.
; No nontrivial zero => anisotropic.

(check "x^2+2y^2-anisotropic"
  (not (any (lambda (v)
    (and (not (and (= (nth 0 v) 0) (= (nth 1 v) 0)))
         (= (qform2 1 0 2 (nth 0 v) (nth 1 v)) 0)))
    (cartesian-product f5 f5))))


; ---------- Classify forms over F_3 (three elements) ----------
; F_3: {0,1,2}. Squares: 0²=0, 1²=1, 2²=4=1. So squares = {0,1}, nonsquares = {2}.
; Q(x,y) = x² + y²: x² and y² are 0 or 1, so for (x,y) ≠ 0 the value is
; 1 (= 1+0) or 2 (= 1+1), never 0. So x²+y² is anisotropic over F_3.
; (In three variables, x²+y²+z² is isotropic: 1+1+1 = 3 ≡ 0.)

; For n=2: x²+2y² over F_3. Q(1,1) = 1+2=3≡0. Isotropic.
; So over F_3, x²+y² is anisotropic but x²+2y² is isotropic: the two types
; swap roles compared with F_5, because -1 is a square in F_5 (5 ≡ 1 mod 4)
; but not in F_3 (3 ≡ 3 mod 4).

(define f3 {0 1 2})
(define (qform2-f3 a b c x y)
  (mod (+ (* a (* x x)) (* b (* x y)) (* c (* y y))) 3))

(define (zeros-f3 a b c)
  (filter (lambda (v)
    (and (not (and (= (nth 0 v) 0) (= (nth 1 v) 0)))
         (= (qform2-f3 a b c (nth 0 v) (nth 1 v)) 0)))
    (cartesian-product f3 f3)))

; Enumerate all 8 nonzero vectors of F_3^2: none is a zero of x^2+y^2.
(check "x^2+y^2-anisotropic-over-f3"
  (= (length (zeros-f3 1 0 1)) 0))

; x^2+2y^2 over F_3 is a hyperbolic plane: it has the full 2(q-1) = 4
; nontrivial zeros, and (1,1) is an explicit witness.
(check "x^2+2y^2-isotropic-over-f3"
  (and (= (length (zeros-f3 1 0 2)) (* 2 (- 3 1)))
       (= (qform2-f3 1 0 2 1 1) 0)))


; ---------- Witt index ----------
; The Witt index ν(Q) is the maximal dimension of a totally isotropic subspace.
; For n=2 over F_5:
;   ν(x²+y²) = 1 (hyperbolic plane).
;   ν(x²+2y²) = 0 (anisotropic plane).

; Over a finite field every quadratic form in ≥ 3 variables is isotropic
; (Chevalley-Warning: more variables than the degree 2). The bound 5 is the
; corresponding fact over the p-adic fields Q_p; the check below uses 5
; variables over F_3, comfortably above the finite-field bound.

; Diagonal form a_0 x_0^2 + ... + a_{n-1} x_{n-1}^2 evaluated at the vector
; whose base-p digits are those of the integer v.
(define (diag-value coeffs v p-val)
  (labels (((go cs idx acc)
    (cond ((eq cs nil) (mod acc p-val))
          (t (go (cdr cs) (+ idx 1)
                 (+ acc (* (car cs) (^ (base-digit v idx p-val) 2))))))))
    (go coeffs 0 0)))

; v ranges over 1 .. p^n - 1, i.e. exactly the nonzero vectors of F_p^n.
(define (diag-isotropic-p coeffs p-val nvars)
  (any (lambda (v) (= (diag-value coeffs v p-val) 0))
       (set-to-list (seq 1 (- (^ p-val nvars) 1)))))

; Every diagonal form in 5 variables over F_3 (each coefficient in F_3^x =
; {1,2}, so 32 forms) is searched over all 242 nonzero vectors of F_3^5.
; This is Chevalley-Warning: n = 5 > 2 = deg Q forces a nontrivial zero.
(check "every-form-in-5-variables-is-isotropic"
  (every (lambda (m)
           (diag-isotropic-p
             (map (lambda (i) (+ 1 (base-digit m i 2))) (set-to-list (seq 0 4)))
             3 5))
         (set-to-list (seq 0 31))))


; ---------- Orthogonal group O(Q) ----------
; O(Q) = {T ∈ GL(n,F_q) | Q(Tx) = Q(x) for all x}.
; For the hyperbolic plane H over F_5: Q(x,y) = xy.
; O(H) ≅ F_5^× ⋊ Z/2Z (semidirect product).

; The special orthogonal group SO(Q) = O(Q) ∩ SL(n,F_q).
; PSO(Q) = SO(Q)/{±I} (projective).

(define (hyperbolic-plane x y) (f* x y))

; A 2x2 matrix over F_5 is encoded as an integer m in 0..624; its entries are
; the base-5 digits (m00 m01 m10 m11).
(define (mat-e m i) (base-digit m i 5))

; Does the linear map given by m preserve the hyperbolic form Q(x,y)=xy?
(define (preserves-hyp-p m)
  (every (lambda (v)
    (= (mod (* (+ (* (mat-e m 0) (nth 0 v)) (* (mat-e m 1) (nth 1 v)))
               (+ (* (mat-e m 2) (nth 0 v)) (* (mat-e m 3) (nth 1 v)))) 5)
       (hyperbolic-plane (nth 0 v) (nth 1 v))))
    (cartesian-product f5 f5)))

; Reflection in a non-isotropic vector a: τ_a(x) = x - B(x,a)/Q(a)·a, where
; B(x,y) = Q(x+y) - Q(x) - Q(y) is the polar form (so B(a,a) = 2Q(a)).
; These generate the orthogonal group (Cartan-Dieudonné, char ≠ 2).

; Over F_5, for Q(x,y)=xy, the polar form is B((x₁,y₁),(x₂,y₂)) = x₁y₂+y₁x₂.
; A non-isotropic vector is (1,1) since Q(1,1)=1≠0, and its reflection is
; τ(x,y) = (x,y) - (x+y)·(1,1) = (-y,-x).

; Brute-force O(H): scan all 625 endomorphisms of F_5^2 and keep those with
; Q(Tv) = Q(v) for every v. The count must be |O(H)| = 2(q-1) = 8.
(check "orthogonal-group-of-hyperbolic-plane"
  (= (length (filter (lambda (m) (preserves-hyp-p m)) (seq 0 624)))
     (* 2 (- 5 1))))


; ---------- Discriminant of a quadratic form ----------
; For Q(x,y) = ax² + bxy + cy², the discriminant is D = b² - 4ac.
; In F_q, D is defined up to a square.
; Q is nondegenerate iff D ≠ 0.
; Q is isotropic iff D is a square (completing the square gives the roots).
; Over F_5, -1 = 4 = 2² is itself a square, so D and -D lie in the same
; square class; over F_3 they do not, so the criterion is stated with D.
; (disc-qform is defined near the top of this file.)

(check "disc-x^2+y^2-is-1" (= (disc-qform 1 0 1) 1))
; For x²+y²: D = 0-4 = -4 = 1 mod 5, which is a square. So isotropic. ✓

(check "disc-x^2+2y^2-is-2" (= (disc-qform 1 0 2) 2))
; For x²+2y²: D = -8 = 2 mod 5, which is NOT a square. Anisotropic. ✓

; Criterion (q odd): a nondegenerate binary form over F_q is isotropic iff
; D^((q-1)/2) = 1 (Euler's criterion for D to be a nonzero square).

; For q=5: (q-1)/2 = 2, and the squares are 0,1,4.
; D for x²+y²: 1, which is a square. D for x²+2y²: 2, which is not. ✓

; Verify the criterion against brute force on ALL 125 triples (a,b,c) in F_5^3
; with D != 0: "has a nontrivial zero" (enumeration over F_5^2) must agree
; with "D is a square" (is-square5, a search over the squares of F_5). The two
; sides are computed by independent routes.
(check "isotropy-criterion-works"
  (every (lambda (k)
    (let ((a (base-digit k 0 5)) (b (base-digit k 1 5)) (c (base-digit k 2 5)))
      (cond ((= (disc-qform a b c) 0) t)
            (t (not (xor (not (= (length (zeros-f5 a b c)) 0))
                         (is-square5 (disc-qform a b c))))))))
    (set-to-list (seq 0 124))))


; ---------- Classification for n ≥ 3 ----------
; For dimension 3 over F_5:
; Every nondegenerate quadratic form is equivalent to an orthogonal sum:
; H ⊥ ⟨a⟩  (hyperbolic plane + 1-dim form).
; All such forms in given odd dimension are equivalent up to scaling (one type).

; For dimension 4 over F_5:
; Two types: H ⊥ H (Witt index 2) and H ⊥ A with A an anisotropic plane
; (Witt index 1); there is no anisotropic form in 4 variables over F_q.
; The discriminant (Gram determinant mod squares) distinguishes them:
; H⊥H has discriminant 1 (square), while H ⊥ A has a nonsquare discriminant.

; Odd dimension, one type: enumerate all 64 nondegenerate diagonal ternary
; forms a x² + b y² + c z² over F_5 (a,b,c in F_5^x) and count the nontrivial
; zeros of each over all 124 nonzero vectors of F_5^3. Every one of them must
; give the SAME count, q^{n-1} - 1 = 24: a single class up to scaling.
; (The zero count is invariant under scaling, so it cannot see the two
; isometry classes, which differ by discriminant.)
(define ternary-zero-counts
  (list-to-set
    (map (lambda (k)
      (count (lambda (v)
        (= (diag-value (list (+ 1 (base-digit k 0 4))
                             (+ 1 (base-digit k 1 4))
                             (+ 1 (base-digit k 2 4))) v 5) 0))
        (set-to-list (seq 1 124))))
      (set-to-list (seq 0 63)))))

(check "classification-odd-dim-one-type"
  (equal ternary-zero-counts (list-to-set (list (- (^ 5 2) 1)))))

; Nondegenerate forms of dimension n over F_q (q odd) fall into exactly 2
; isometry classes, told apart by the discriminant:
;  n odd:  H^{⌊n/2⌋} ⊥ ⟨1⟩ and H^{⌊n/2⌋} ⊥ ⟨a⟩ (a a nonsquare); they differ
;          only by scaling and have the same Witt index ⌊n/2⌋.
;  n even: Witt index n/2 (H^{n/2}) and n/2 - 1 (H^{n/2-1} ⊥ anisotropic plane).

; Even dimension, two types: over all nondegenerate binary forms over F_5
; the zero-count invariant takes EXACTLY two values — 2(q-1) = 8 (hyperbolic,
; Witt index 1) and 0 (anisotropic, Witt index 0).
(define binary-zero-counts
  (list-to-set
    (map (lambda (k) (length (zeros-f5 (base-digit k 0 5)
                                    (base-digit k 1 5)
                                    (base-digit k 2 5))))
      (filter (lambda (k) (not (= (disc-qform (base-digit k 0 5)
                                              (base-digit k 1 5)
                                              (base-digit k 2 5)) 0)))
              (set-to-list (seq 0 124))))))

(check "classification-even-dim-two-types"
  (and (= (length binary-zero-counts) 2)
       (equal binary-zero-counts (list-to-set (list 0 (* 2 (- 5 1)))))))


; ---------- Arf invariant ----------
; For quadratic forms over fields of characteristic 2,
; the Arf invariant distinguishes the two types.
; Writing Q = sum_i (a_i x_i² + x_i y_i + b_i y_i²), Arf(Q) = sum_i a_i b_i.
; Over F_2, Q0 = x₁x₂ + x₃x₄ has Arf invariant 0.
; Adding a square of one variable per pair, as in x₁x₂ + x₃x₄ + x₁² + x₃²,
; keeps Arf 0 (each a_i b_i = 1·0 = 0). An Arf-1 form is
; x₁² + x₁x₂ + x₂² + x₃x₄ (a_1 = b_1 = 1).
; Over odd characteristic, the discriminant serves this role.

(print "--- Arf Invariant (char 2) ---")

; Coordinates of F_2^4 as the bits of an integer v in 0..15.
(define (f2bit v i) (base-digit v i 2))
(define (arf0-form v)
  (mod (+ (* (f2bit v 0) (f2bit v 1)) (* (f2bit v 2) (f2bit v 3))) 2))
(define (arf1-form v)
  (mod (+ (* (f2bit v 0) (f2bit v 0))
          (* (f2bit v 0) (f2bit v 1))
          (* (f2bit v 1) (f2bit v 1))
          (* (f2bit v 2) (f2bit v 3))) 2))

; The Arf invariant is detected by the number of zeros: a form of rank 2m over
; F_2 has 2^{2m-1} + 2^{m-1} zeros when Arf = 0 and 2^{2m-1} - 2^{m-1} when
; Arf = 1. Here m = 2, so 10 versus 6 — counted by enumerating all 16 vectors.
(check "arf-invariant-char-2"
  (and (= (count (lambda (v) (= (arf0-form v) 0)) (set-to-list (seq 0 15)))
          (+ (^ 2 3) (^ 2 1)))
       (= (count (lambda (v) (= (arf1-form v) 0)) (set-to-list (seq 0 15)))
          (- (^ 2 3) (^ 2 1)))))


; ---------- Computation: |O(n,F_q)| ----------
; |GL(n,q)| = (q^n - 1)(q^n - q)...(q^n - q^{n-1})
; |O(2,q)| for nondegenerate forms:
;   If Q is the hyperbolic plane: |O(H)| = 2(q-1).
;   If Q is anisotropic: |O(Q)| = 2(q+1).

; For F_5: |O(H)| = 2·4 = 8, |O(Q_anis)| = 2·6 = 12. The checks below record
; these formulas; |O(H)| = 8 is also found by enumeration above.
(check "|O(H)|-over-f5-is-8" (= (* 2 (- 5 1)) 8))
(check "|O(Q_anis)|-over-f5-is-12" (= (* 2 (+ 5 1)) 12))


; ---------- Spinor norm ----------
; The spinor norm θ: O(Q) → F_q^×/(F_q^×)².
; For a product of reflections, θ(Π τ_{a_i}) = Π Q(a_i) mod squares.
; On SO(Q) its kernel is the image of Spin(Q) → SO(Q) (often written Ω(Q)).

; θ(τ_a) = class of Q(a) in F_5^×/(F_5^×)². Collect Q(a) over every nonzero a
; for the anisotropic form Q = x²+2y² (all such a are non-isotropic, so every
; one really does define a reflection) and check both square classes occur —
; i.e. the spinor norm is onto F_5^×/(F_5^×)², so its kernel is a proper
; subgroup of O(Q), of index 2.
(define spinor-values
  (list-to-set
    (map (lambda (v) (qform2 1 0 2 (nth 0 v) (nth 1 v)))
      (filter (lambda (v) (not (and (= (nth 0 v) 0) (= (nth 1 v) 0))))
              (set-to-list (cartesian-product f5 f5))))))

(check "spinor-norm"
  (and (notin 0 spinor-values)
       (not (= (length (cap spinor-values squares5)) 0))
       (not (= (length (cap spinor-values nonsquares5)) 0))))


; ---------- Verify form equivalence ----------
; Two forms Q₁ and Q₂ are equivalent if ∃ T∈GL(n) such that Q₁(Tx) = Q₂(x).
; Over finite fields, the invariants (dim, discriminant, Witt index) classify forms;
; for q odd, dimension and discriminant already suffice.

; Search all 625 endomorphisms T of F_5^2 for one with Q1(Tv) = Q2(v) for all v.
(define (equiv-forms-p a1 b1 c1 a2 b2 c2)
  (any (lambda (m)
    (every (lambda (v)
      (= (qform2 a1 b1 c1
           (mod (+ (* (mat-e m 0) (nth 0 v)) (* (mat-e m 1) (nth 1 v))) 5)
           (mod (+ (* (mat-e m 2) (nth 0 v)) (* (mat-e m 3) (nth 1 v))) 5))
         (qform2 a2 b2 c2 (nth 0 v) (nth 1 v))))
      (cartesian-product f5 f5)))
    (set-to-list (seq 0 624))))

; xy and x²+y² share all invariants (D = 1 in both cases, Witt index 1), so an
; explicit T must exist and the search finds one. x²+2y² has a different
; discriminant class, so the search must come up empty.
(check "form-equivalence-by-invariants"
  (and (equiv-forms-p 0 1 0 1 0 1)
       (equiv-forms-p 1 0 1 0 1 0)
       (not (equiv-forms-p 1 0 1 1 0 2))))


(print "=== Quadratic Forms Done ===")
