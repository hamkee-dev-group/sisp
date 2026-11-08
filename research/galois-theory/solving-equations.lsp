; ============================================================
; galois-theory/solving-equations.lsp — Solvability by Radicals
; Discriminant, cubic formula, quartic reduction,
; insolvability of quintic (conceptual proof via S_5).
; ============================================================

(print "=== Solving Equations ===")


; ---------- Quadratic formula over a field ----------
(define (f+ a b) (mod (+ a b) 7))
(define (f- a b) (mod (- a b) 7))
(define (f* a b) (mod (* a b) 7))
(define (f/ a b) (f* a (f-inv7 b)))
(define (f-inv7 a)
  (cond ((= a 0) (error "division by zero"))
        ((= a 1) 1) ((= a 2) 4) ((= a 3) 5) ((= a 4) 2)
        ((= a 5) 3) (t 6)))

; x² + bx + c = 0 over F_7. Discriminant D = b² - 4c.
(define (has-sqrt7 a)
  (any (lambda (x) (= (mod (* x x) 7) (mod a 7))) {0 1 2 3 4 5 6}))

(define (sqrt7 a)
  (find (lambda (x) (= (mod (* x x) 7) (mod a 7))) {0 1 2 3 4 5 6}))

(define (quadratic-roots b c)
  (let ((D (mod (- (* b b) (* 4 c)) 7)))
    (cond ((not (has-sqrt7 D)) nil)
          (t (let ((rD (sqrt7 D)))
               (list-to-set (list (f/ (mod (- rD b) 7) 2)
                                   (f/ (mod (- 0 (+ rD b)) 7) 2))))))))

(check "x^2+2x+3=0-no-roots" (eq (quadratic-roots 2 3) nil))
(check "x^2-1=0-roots-1-6"
  (let ((roots (quadratic-roots 0 6)))
    (and (in 1 roots) (in 6 roots))))

(check "x^2-2x+1=0-double-root"
  (let ((roots (quadratic-roots 5 1)))
    (= (length roots) 1)))


; ---------- Discriminant ----------
; For cubic x³ + px + q: D = -4p³ - 27q².
(define (discriminant-cubic p q)
  (mod (- 0 (+ (* 4 (* p p p)) (* 27 (* q q)))) 7))

(check "discriminant-example"
  (= (discriminant-cubic 0 0) 0))

; If D ≠ 0 then distinct roots. If D = 0 then multiple root.
(check "x^3-1-over-f7-discriminant"
  (not (= (discriminant-cubic 0 6) 0)))


; ---------- Cubic formula (Cardano's formula) over F_7 ----------
; Solve x³ + ax² + bx + c = 0.
; Step 1: Tschirnhaus transformation x = y - a/3.
; Step 2: Get depressed cubic y³ + py + q = 0.
; Step 3: Cardano: y = u + v where u³+v³ = -q and uv = -p/3.

(define (depressed-cubic a b c)
  (let ((a3 (f/ a 3)))
    (let ((p (mod (- b (f* 3 (f* a3 a3))) 7))
          (q (mod (+ (* 2 (f* a3 (f* a3 a3))) c (f- 0 (f* a3 b))) 7)))
      (list p q))))

(define (cubic-eval a b c x)
  (f+ (f+ (f+ (f* x (f* x x)) (f* a (f* x x))) (f* b x)) c))
(define (depressed-cubic-eval p q y)
  (f+ (f+ (f* y (f* y y)) (f* p y)) q))

; Real assertion (was `(not (eq dp nil))`, which depressed-cubic can never
; violate): the Tschirnhaus substitution x = y - a/3 must make the depressed
; cubic y³+py+q agree with x³+ax²+bx+c identically over F_7.
(check "depressed-cubic-matches-original-under-x-equals-y-minus-a-over-3"
  (every (lambda (a)
    (every (lambda (b)
      (every (lambda (c)
        (let ((dp (depressed-cubic a b c)))
          (every (lambda (y)
            (= (depressed-cubic-eval (nth 0 dp) (nth 1 dp) y)
               (cubic-eval a b c (f- y (f/ a 3)))))
            {0 1 2 3 4 5 6})))
        {0 1 2 3 4 5 6}))
      {0 1 2 3 4 5 6}))
    {0 1 2 3 4 5 6}))

; Concretely for x³ + 2x² + 3x + 4: the depressed pair has no cubic/quadratic
; term left, so p and q must reproduce the original at every point.
(check "depressed-cubic-example"
  (let ((dp (depressed-cubic 2 3 4)))
    (and (= (length dp) 2)
         (every (lambda (y)
           (= (depressed-cubic-eval (nth 0 dp) (nth 1 dp) y)
              (cubic-eval 2 3 4 (f- y (f/ 2 3)))))
           {0 1 2 3 4 5 6}))))

; The resolvent cubic: t² + qt - p³/27 = 0
; Solve for t = u³. Then u = t^{1/3} (cube root in F_7).

(define (cube-roots7 a)
  (filter (lambda (x) (= (mod (* x (* x x)) 7) (mod a 7))) {0 1 2 3 4 5 6}))

(define (solve-cubic a b c)
  (let ((dp (depressed-cubic a b c)))
    (let ((p (nth 0 dp)) (q (nth 1 dp)))
      (cond ((= p 0)
             ; y³ = -q, so y = cube root of -q
             (let ((cuberoots (cube-roots7 (mod (- 0 q) 7))))
               (map (lambda (y) (mod (- y (f/ a 3)) 7)) cuberoots)))
            ((= q 0)
             ; y³ + py = y(y²+p) = 0
             (let ((roots-y {0}))
               (let ((sqp (has-sqrt7 (mod (- 0 p) 7))))
                 (if sqp
                     (let ((r (sqrt7 (mod (- 0 p) 7))))
                       (map (lambda (y) (mod (- y (f/ a 3)) 7))
                                (list-to-set (list 0 r (mod (- 0 r) 7)))))
                     (map (lambda (y) (mod (- y (f/ a 3)) 7)) {0})))))
            (t
             ; Use Cardano: find t such that t² + qt - p³/3³ = 0
             (let ((p3-27 (f/ (f* p (f* p p)) (f* 3 (f* 3 3)))))
               (let ((D (mod (+ (* q q) (* 4 p3-27)) 7)))
                 (cond ((not (has-sqrt7 D)) {})
                       (t (let ((rD (sqrt7 D)))
                            ; `t` is a RESERVED name in SISP and cannot be a
                            ; let binding, so the Cardano resolvent root is `tc`.
                            (let ((tc (f/ (mod (- rD q) 7) 2)))
                              (let ((cu (cube-roots7 tc)))
                                (let ((a3 (f/ a 3)))
                                  (map (lambda (u)
                                    (let ((v (f/ (mod (- 0 p) 7) (f* 3 u))))
                                      (mod (- (mod (+ u v) 7) a3) 7))) cu))))))))))))))

; x³ = 0 over F_7 has the single root 0.
(check "cubic-example-roots"
  (equal (solve-cubic 0 0 0) {0}))

; Soundness of Cardano's method: everything solve-cubic returns is genuinely
; a root of x³+ax²+bx+c, for every cubic over F_7.
(check "cardano-returns-only-genuine-roots"
  (every (lambda (a)
    (every (lambda (b)
      (every (lambda (c)
        (every (lambda (r) (= (cubic-eval a b c r) 0))
                   (solve-cubic a b c)))
        {0 1 2 3 4 5 6}))
      {0 1 2 3 4 5 6}))
    {0 1 2 3 4 5 6}))


; ---------- Quartic reduction to cubic ----------
; Ferrari's method: x⁴ + ax³ + bx² + cx + d = 0.
; Step 1: Reduce to x⁴ + px² + qx + r = 0 (depressed quartic).
; Step 2: Introduce resolvent cubic.
; Step 3: Factor as (x² + ux + v)(x² - ux + w).

; Substituting x = y - s with s = a/4 into x⁴+ax³+bx²+cx+d kills the cubic
; term and yields y⁴ + py² + qy + r with
;   p = b - 6s²,  q = c - 2bs + 8s³,  r = d - cs + bs² - 3s⁴.
; (The previous q and r coefficients here were wrong: q used 3s³ instead of
; 8s³, and r omitted the bs² and -3s⁴ terms entirely.)
(define (depress-quartic a b c d)
  (let ((s (f/ a 4)))
    (let ((s2 (f* s s)))
      (let ((s3 (f* s2 s))
            (s4 (f* s2 s2)))
        (list (f- b (f* 6 s2))
              (f+ (f- c (f* 2 (f* b s))) (f* 8 s3))
              (f+ (f- (f- d (f* c s)) (f* 3 s4)) (f* b s2)))))))

(define (quartic-eval a b c d x)
  (f+ (f+ (f+ (f+ (f* x (f* x (f* x x))) (f* a (f* x (f* x x))))
                  (f* b (f* x x)))
              (f* c x))
      d))

(define (depressed-eval p q r y)
  (f+ (f+ (f+ (f* y (f* y (f* y y))) (f* p (f* y y))) (f* q y)) r))

; The resolvent cubic for x⁴ + px² + qx + r = 0 is:
; y³ + 2py² + (p²-4r)y - q² = 0.
; If y is a root of the resolvent cubic, then:
; u = √(y+2p)  (if square exists)
; and the quartic factors as two quadratics.

; Real assertion (was a vacuous `(check "quartic-reduction-works" t)`):
; the depressed quartic in y must agree with the original quartic at
; x = y - a/4 for every y in F_7, and must have no cubic term by construction.
(check "quartic-reduction-works"
  (every (lambda (a)
    (let ((b 3) (c 5) (d 2))
      (let ((dq (depress-quartic a b c d)))
        (let ((p (nth 0 dq)) (q (nth 1 dq)) (r (nth 2 dq)) (s (f/ a 4)))
          (every (lambda (y)
            (= (depressed-eval p q r y)
               (quartic-eval a b c d (f- y s))))
            {0 1 2 3 4 5 6})))))
    {0 1 2 3 4 5 6}))

; Example: x⁴ + 1 over F_7. This is x⁴ = 6 = -1.
; 1⁴ = 1, 2⁴ = 16 = 2, 3⁴ = 81 = 4, 4⁴ = 256 = 4, 5⁴ = 625 = 2, 6⁴ = 1296 = 1.
; No fourth root of -1 in F_7 (all fourth powers are 1,2,4).
(check "x^4+1-has-no-root-in-f7"
  (every (lambda (x) (not (= (mod (* x (* x (* x x))) 7) 6))) {0 1 2 3 4 5 6}))


; ---------- Insolvability of the quintic ----------
; The general quintic x⁵ + ax⁴ + bx³ + cx² + dx + e = 0.
; The Galois group of the general quintic over Q is S_5.
; S_5 is not solvable (its commutator series is S_5 > A_5 > A_5 > ...).
; A_5 is simple and non-abelian.

; The commutator [σ, τ] = σ⁻¹τ⁻¹στ.
; For S_5, the derived subgroup is A_5 (index 2).
; The derived subgroup of A_5 is A_5 itself (A_5 is perfect).
; Since the derived series doesn't terminate at {1}, S_5 is not solvable.

(define S3 {(1 2 3) (2 3 1) (3 1 2) (1 3 2) (3 2 1) (2 1 3)})
(define A3 {(1 2 3) (2 3 1) (3 1 2)})

; A permutation is one-line notation: a list whose (i-1)-th entry is the
; image of i. SISP's `nth` is (nth INDEX LIST) and is 0-based, so a 1-based
; image x must be looked up at position (- x 1).
(define (perm-mul f g) (map (lambda (x) (nth (- x 1) f)) g))   ; (f∘g)(i) = f(g(i))
(define (perm-inv f)
  (let ((n (length f)))
    (map (lambda (x) (+ (index-of x f) 1)) (seq 1 n))))

; [a,b] = a⁻¹b⁻¹ab
(define (commutator a b)
  (perm-mul (perm-mul (perm-inv a) (perm-inv b)) (perm-mul a b)))

; NOTE: `flatmap` deep-flattens, which would shred each permutation into
; loose integers; concatenate exactly one level with foldr/append instead.
(define (derived-subgroup G)
  (list-to-set
    (foldr append nil
      (map (lambda (a)
             (map (lambda (b) (commutator a b)) (set-to-list G)))
           (set-to-list G)))))

(check "derived-S3-is-A3"
  (equal (derived-subgroup S3) A3))

; A3 is cyclic of order 3, hence abelian, so its derived subgroup is {id}.
(check "derived-A3-is-identity"
  (= (length (derived-subgroup A3)) 1))

; Thus S3 is solvable. The cubic can be solved by radicals.
; S4 is also solvable (derived series: S4 > A4 > V4 > {1}).
; S5 is NOT solvable because A5 is simple and perfect.

(print "--- Insolvability of the Quintic ---")
(print "Gal(f) for general quintic = S_5.")
(print "Derived series: S_5 > A_5 (simple, perfect).")
(print "A_5' = A_5, so series doesn't reach {1}.")
(print "Therefore S_5 is not solvable.")
(print "The general quintic cannot be solved by radicals.")
; Real assertions (was a vacuous `(check "quintic-insolvability-conceptual" t)`).
; We actually build S_5 and A_5 and compute derived subgroups, establishing
; the derived series S_5 ▷ A_5 ▷ A_5 ▷ ... which never reaches {1}.
(define (perms l)
  (cond ((eq l nil) (list nil))
        (t (foldr append nil
             (map (lambda (x)
                    (map (lambda (rest) (cons x rest))
                         (perms (remove (lambda (y) (= y x)) l))))
                  l)))))

; parity via inversion count
(define (inversions f)
  (let ((n (length f)))
    (length (filter (lambda (p) (and (< (car p) (cadr p))
                                     (> (nth (- (car p) 1) f) (nth (- (cadr p) 1) f))))
      (set-to-list (cartesian-product (list-to-set (seq 1 n))
                                      (list-to-set (seq 1 n))))))))
(define (even-perm-p f) (= (mod (inversions f) 2) 0))

(define S5 (list-to-set (perms (seq 1 5))))
(define A5 (list-to-set (filter even-perm-p (perms (seq 1 5)))))

(check "S5-has-order-120" (= (length S5) 120))
(check "A5-has-order-60"  (= (length A5) 60))

; D(S_5) = A_5
(check "derived-S5-is-A5" (equal (derived-subgroup S5) A5))

; A_5 is perfect: D(A_5) = A_5, so the derived series stabilises at A_5 ≠ {1}
; and S_5 is NOT solvable — hence the general quintic is not solvable by radicals.
(check "A5-is-perfect" (equal (derived-subgroup A5) A5))
(check "quintic-insolvability-derived-series-never-reaches-identity"
  (and (not (= (length (derived-subgroup A5)) 1))
       (equal (derived-subgroup (derived-subgroup A5)) A5)))

; Contrast: S_4 IS solvable, so the general quartic IS solvable by radicals.
; Derived series S_4 ▷ A_4 ▷ V_4 ▷ {1}.
(define S4 (list-to-set (perms (seq 1 4))))
(define A4 (list-to-set (filter even-perm-p (perms (seq 1 4)))))
(check "derived-S4-is-A4" (equal (derived-subgroup S4) A4))
(check "derived-A4-is-V4" (= (length (derived-subgroup A4)) 4))
(check "derived-V4-is-trivial"
  (= (length (derived-subgroup (derived-subgroup A4))) 1))


; ---------- Discriminant detects Galois group ----------
; For a quintic, if the discriminant is a perfect square, the Galois group
; is a subgroup of A_5. Otherwise, it's a subgroup of S_5 not in A_5.
; Abel's theorem: the general quintic has no radical solution because
; S_5 and A_5 are not solvable groups.

; Discriminant formula for general polynomial:
; D = Π_{i<j} (r_i - r_j)^2.
; It's in the base field. D is a square iff Gal(f) ≤ A_n.

; Real assertions (was a vacuous `(check "... " t)`).
(define (cubic-roots-f7 p q)
  (filter (lambda (x) (= (f+ (f+ (f* x (f* x x)) (f* p x)) q) 0))
              {0 1 2 3 4 5 6}))

(define (disc-from-roots rs)
  (let ((r1 (nth 0 rs)) (r2 (nth 1 rs)) (r3 (nth 2 rs)))
    (f* (f* (f* (f- r1 r2) (f- r1 r2)) (f* (f- r1 r3) (f- r1 r3)))
        (f* (f- r2 r3) (f- r2 r3)))))

(define f7xf7 (cartesian-product {0 1 2 3 4 5 6} {0 1 2 3 4 5 6}))

; D = Π_{i<j} (r_i - r_j)², checked against the closed form -4p³-27q²
; for every x³+px+q over F_7 that splits into three distinct roots.
(check "discriminant-equals-product-of-squared-root-differences"
  (every (lambda (pq)
    (let ((p (car pq)) (q (cadr pq)))
      (let ((rs (set-to-list (cubic-roots-f7 p q))))
        (or (not (= (length rs) 3))
            (= (discriminant-cubic p q) (disc-from-roots rs))))))
    f7xf7))

; Gal(f) ≤ A_3 iff D is a square. Over F_7 the Galois group of a cubic is
; cyclic: trivial when f splits (3 roots), Z/3 = A_3 when f is irreducible
; (0 roots) — both are inside A_3, so D must be a square in exactly those cases.
(check "discriminant-square-implies-galois-subgroup-of-alternating"
  (every (lambda (pq)
    (let ((p (car pq)) (q (cadr pq)))
      (let ((nroots (length (cubic-roots-f7 p q))))
        (or (not (or (= nroots 3) (= nroots 0)))
            (has-sqrt7 (discriminant-cubic p q))))))
    f7xf7))

; Converse direction: exactly one root means f = (linear)(irreducible quadratic),
; so Gal is generated by a transposition — odd — and D is a NON-square.
(check "one-root-cubic-has-nonsquare-discriminant"
  (every (lambda (pq)
    (let ((p (car pq)) (q (cadr pq)))
      (let ((nroots (length (cubic-roots-f7 p q))))
        (or (not (= nroots 1))
            (or (= (discriminant-cubic p q) 0)
                (not (has-sqrt7 (discriminant-cubic p q))))))))
    f7xf7))


; ---------- Concrete solvable example: cubic x³ + x + 1 over F_7 ----------
; Discriminant: D = -4(1)³ - 27(1)² = -4 - 27 = -31 ≡ 4 mod 7.
; 4 is a square (2² = 4). So Galois group is A_3 (order 3, solvable).
(check "cubic-x^3+x+1-has-square-discriminant" (= (discriminant-cubic 1 1) 4))

; Roots of x³ + x + 1 over F_7:
; Try 0: 1. 1: 3. 2: 8+2+1=11≡4. 3: 27+3+1=31≡3. 4: 64+4+1=69≡6. 5: 125+5+1=131≡5. 6: 216+6+1=223≡6.
; No root in F_7 — irreducible cubic over F_7.
; The splitting field is F_{343} = F_{7^3}.
; The Galois group is cyclic of order 3 (solvable).

(check "cubic-x^3+x+1-irred-over-f7"
  (every (lambda (x) (not (= (mod (+ (* x x x) x 1) 7) 0))) {0 1 2 3 4 5 6}))


; ---------- Quadratic solvability over F_q ----------
; x² + ax + b = 0 has radical solution iff the discriminant a² - 4b is a square.
; This is always solvable by radicals: x = (-a ± √Δ)/2.

(define (solve-quadratic-f7 a b)
  (let ((D (mod (- (* a a) (* 4 b)) 7)))
    (cond ((not (has-sqrt7 D)) {})
          (t (let ((rD (sqrt7 D)))
               (let ((inv2 4)) ; 2⁻¹ = 4 in F_7
                 (list-to-set (list (f* (mod (- rD a) 7) inv2)
                                     (f* (mod (- 0 (+ rD a)) 7) inv2)))))))))

(check "x^2+3x+2=0-roots-5-6"
  (let ((roots (solve-quadratic-f7 3 2)))
    (and (in 5 roots) (in 6 roots))))


(print "=== Solving Equations Done ===")
