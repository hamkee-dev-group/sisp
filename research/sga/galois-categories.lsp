; ============================================================
; SGA — Galois Categories (Grothendieck)
; Category C with fiber functor F:C→FinSet. C ≅ G-Set, G profinite.
; For finite etale covers of Spec(F_q): algebras = products of finite fields.
; Fiber functor: F(A) = Hom_{F_q}(A, \bar{F_q}).
; ============================================================

(print "=== Galois Categories ===")

; ------------------------------------------------------------
; Machinery — finite fields F_{p^d} = Z/pZ[x]/(g) as coefficient lists
; (LOW to HIGH). The fiber functor is computed by ROOT-FINDING: an
; F_q-algebra map F_q[x]/(g) -> K is exactly a choice of root of g in K.
; ------------------------------------------------------------
(define (zn a m) (mod (+ (mod a m) m) m))
(define (zeros k) (cond ((<= k 0) nil) (t (cons 0 (zeros (- k 1))))))
(define (padto p dd) (append p (zeros (- dd (length p)))))
(define (coefat p i) (cond ((< i 0) 0) ((>= i (length p)) 0) (t (nth i p))))
(define (vsub a b m) (map (lambda (pr) (zn (- (car pr) (cadr pr)) m)) (zip a b)))
(define (vadd a b m) (map (lambda (pr) (zn (+ (car pr) (cadr pr)) m)) (zip a b)))
(define (scale-poly c p m) (map (lambda (co) (zn (* c co) m)) p))
(define (poly-mul a b m)
  (map (lambda (k)
         (zn (foldl + 0 (map (lambda (i) (* (coefat a i) (coefat b (- k i)))) (seq 0 k))) m))
       (seq 0 (- (+ (length a) (length b)) 2))))
(define (poly-reduce p f m)
  (cond ((< (length p) (length f)) p)
        (t (let ((cc (last p)) (shift (- (length p) (length f))))
             (poly-reduce (butlast (vsub p (append (zeros shift) (scale-poly cc f m)) m)) f m)))))
(define (fdeg f) (- (length f) 1))
(define (fone f) (padto (list 1) (fdeg f)))
(define (fzero f) (zeros (fdeg f)))
(define (fmul a b f m) (padto (poly-reduce (poly-mul a b m) f m) (fdeg f)))
(define (felts dd m)
  (cond ((= dd 0) (list nil))
        (t (foldr append nil
                  (map (lambda (v) (map (lambda (c) (cons c v)) (seq 0 (- m 1))))
                       (felts (- dd 1) m))))))
(define (fpow a k f m) (cond ((= k 0) (fone f)) (t (fmul a (fpow a (- k 1) f m) f m))))
(define (frob a f m) (fpow a m f m))
(define (iterfrob k a f m) (cond ((= k 0) a) (t (iterfrob (- k 1) (frob a f m) f m))))
(define (feval p a f m)
  (foldl (lambda (acc i)
           (vadd acc (fmul (padto (list (coefat p i)) (fdeg f)) (fpow a i f m) f m) m))
         (fzero f) (seq 0 (- (length p) 1))))
; roots of the Z/pZ-polynomial p inside the field F_p[x]/(f)
(define (froots p f m)
  (filter (lambda (a) (equal (feval p a f m) (fzero f))) (felts (fdeg f) m)))
; |F(A)| for A = F_p[x]/(g): the number of roots of g in its own splitting field
(define (fiber-size g m) (length (froots g g m)))
; Frobenius orbit structure on a fibre
(define (orb-go a f m acc)
  (cond ((contains a acc) acc) (t (orb-go (frob a f m) f m (cons a acc)))))
(define (orbits-acc rts f m acc)
  (cond ((eq rts nil) acc)
        ((any (lambda (o) (contains (car rts) o)) acc) (orbits-acc (cdr rts) f m acc))
        (t (orbits-acc (cdr rts) f m (append acc (list (orb-go (car rts) f m nil)))))))
(define (fiber-orbits g m) (sort < (map length (orbits-acc (froots g g m) g m nil))))

; the polynomials used below, all over F_2
(define g1 (list 1 1))               ; x+1        -> F_2
(define g2 (list 1 1 1))             ; x²+x+1     -> F_4
(define g3 (list 1 1 0 1))           ; x³+x+1     -> F_8
(define g3b (list 1 0 1 1))          ; x³+x²+1    -> F_8 (the other cubic)

; ============================================================
; 1. The Galois category of finite etale F_q-algebras
; ============================================================
; Objects: finite F_q-algebras that are products of finite fields.
; Example: F_4 = F_2[x]/(x^2+x+1): degree-2 extension of F_2.
; Object F_4 x F_4: product, corresponds to 2-point set.
; Object F_2 x F_2 x F_2: trivial cover, 3 fixed points.
; Morphisms: F_q-algebra homomorphisms.
;
; Fiber functor F: A -> Hom_{F_q}(A, \bar{F_q}).
; For A = F_{q^n}: |F(A)| = n (n embeddings into \bar{F_q}).
; For A = product of d_i degree extensions: |F(A)| = sum d_i.

(print "--- 1. Fiber functor on finite etale algebras ---")
(print "F(F_{q^n}) = set of n embeddings F_{q^n} -> F_q-bar.")
(print "F(F_{q^{d1}} x ... x F_{q^{dk}}) = d1 + ... + dk = dim_{F_q}(A).")
; For A = F_8 over F_2: F(A) has size 3.
; For A = F_8 x F_2 over F_2: F(A) has size 3+1 = 4.

; LEFT SIDE: count the actual F_2-algebra maps into \bar{F_2} by finding the
; roots of each factor's defining polynomial in its splitting field.
; RIGHT SIDE: dim_{F_2}(A) = sum of the polynomial degrees. Different routes.
(print "|F(F_2)|, |F(F_4)|, |F(F_8)| by root counting:")
(print (map (lambda (g) (fiber-size g 2)) (list g1 g2 g3)))
(check "fiber-functor-dim"
       (and (equal (map (lambda (g) (fiber-size g 2)) (list g1 g2 g3 g3b))
                   (map fdeg (list g1 g2 g3 g3b)))
            ; A = F_8 x F_2: fibre 3+1 = 4 = dim
            (= (+ (fiber-size g3 2) (fiber-size g1 2)) 4)
            ; A = F_8 x F_4: fibre 3+2 = 5 = dim
            (= (+ (fiber-size g3 2) (fiber-size g2 2)) 5)))

; ============================================================
; 2. Axioms of Galois categories
; ============================================================
; (G1) Finite limits exist.
; (G2) Finite coproducts exist.
; (G3) Every arrow has a factorisation strict epi -> mono, and
;      strict epis are stable under pullback.
; (G4) F preserves finite limits.
; (G5) F preserves finite coproducts.
; (G6) F reflects isomorphisms.
;
; For etale F_q-algebras:
; G1: Products = direct sum of algebras (A x B).
; G2: Coproducts = tensor product over F_q (A ⊗_{F_q} B).
; G3: holds for any finite variety of algebras.
; G4: F(A x B) = F(A) ⨿ F(B) (preserves products and equalizers).
; G5: F(A ⊗ B) = F(A) x F(B) (preserves coproducts).
; G6: If F(f): F(A) -> F(B) is bijection, then f is isomorphism.

(print "--- 2. Galois category axioms ---")
(print "G1-G6 for etale F_q-algebras with F = Hom(-, F_q-bar).")
(print "F preserves both products (x) and coproducts (tensor).")

; G4 (products): |F(A x B)| = |F(A)| + |F(B)|. Both sides by root counting.
; G5 (coproducts = tensor over F_q): |F(A ⊗ B)| = |F(A)| · |F(B)|.
;   F_4 ⊗_{F_2} F_4 = F_4[y]/(y²+y+1). Count the roots of y²+y+1 IN F_4:
;   there are 2, so the tensor SPLITS as F_4 x F_4 — 2 factors of degree 2 —
;   giving dim 2·2 = 4 over F_2, which is |F(F_4)|·|F(F_4)|.
;   F_4 ⊗_{F_2} F_8 = F_8[y]/(y²+y+1). Count the roots of y²+y+1 IN F_8:
;   there are 0, so it stays a FIELD of degree 2 over F_8, dim 2·3 = 6 = 2·3.
(define split-4-over-4 (length (froots g2 g2 2)))   ; roots of x²+x+1 in F_4
(define split-4-over-8 (length (froots g2 g3 2)))   ; roots of x²+x+1 in F_8
(print "roots of x²+x+1 in F_4:") (print split-4-over-4)
(print "roots of x²+x+1 in F_8:") (print split-4-over-8)
; The engine behind all of this: |Hom_{F_2}(F_2[x]/(g), F_{2^b})| = deg(g) when
; deg(g) divides b, and 0 otherwise. Brute-force the whole 3x3 matrix of root
; counts and compare with that formula — this is what makes F computable at all.
(define fields3 (list g1 g2 g3))
(define hom-matrix
  (map (lambda (bb) (map (lambda (ga) (length (froots ga bb 2))) fields3)) fields3))
(define hom-formula
  (map (lambda (bb) (map (lambda (ga)
                           (cond ((= 0 (mod (fdeg bb) (fdeg ga))) (fdeg ga)) (t 0)))
                         fields3))
       fields3))
(print "root-count matrix (rows: target field F_2,F_4,F_8):") (print hom-matrix)
(print "formula deg(g) if deg(g)|b else 0:") (print hom-formula)
(check "galois-category-axioms"
       (and (equal hom-matrix hom-formula)
            ; G4 (products): |F(F_8 x F_4)| = |F(F_8)| + |F(F_4)| = dim
            (= (+ (fiber-size g3 2) (fiber-size g2 2))
               (+ (fdeg g3) (fdeg g2)))
            ; G5: F_4 ⊗ F_4 splits — y²+y+1 has 2 roots in F_4 — into 2 copies of
            ; F_4, so dim = 2*2 = |F(F_4)|·|F(F_4)|
            (= split-4-over-4 2)
            (= (* split-4-over-4 (fdeg g2)) (* (fiber-size g2 2) (fiber-size g2 2)))
            ; G5: F_4 ⊗ F_8 does NOT split — 0 roots in F_8 — so it is the field
            ; F_8[y]/(y²+y+1), of dim 2*3 = |F(F_4)|·|F(F_8)|
            (= split-4-over-8 0)
            (= (* (fdeg g2) (fdeg g3)) (* (fiber-size g2 2) (fiber-size g3 2)))))

; ============================================================
; 3. Proof: F reflects isomorphisms
; ============================================================
; Let f: A -> B be an F_q-algebra homomorphism between etale algebras.
; Suppose F(f): F(B) -> F(A) is bijective.
; F(A) = Hom(A, F_q-bar), same for B.
; Since each etale algebra is a product of fields, and over algebraically
; closed fields embeddings correspond to factors, |F(A)| = #(factors of A).
; Bijectivity of F(f) forces #(factors of A) = #(factors of B),
; and f must map each factor isomorphically onto a factor of B.
; Hence f is an isomorphism.

(print "--- 3. F reflects isomorphisms ---")
(print "F(f) bijective => |factors(A)| = |factors(B)|, factor-by-factor iso.")

; A concrete instance, and its concrete failure mode.
; A = F_2[x]/(x³+x+1), B = F_2[x]/(x³+x²+1). Both have |F| = 3, so F(f) can be
; a bijection — and indeed A ≅ B: EXHIBIT the isomorphism by finding a root of
; x³+x²+1 inside A. Any such root b gives x |-> b, an F_2-algebra map B -> A,
; which is an iso by dimension count.
; Contrast: F_4 has |F| = 2 ≠ 3, and x²+x+1 has NO root in F_8 — no map at all,
; so nothing to reflect. The functor separates the two situations.
(define g3b-roots-in-f8 (froots g3b g3 2))
(define g2-roots-in-f8 (froots g2 g3 2))
(print "roots of x³+x²+1 inside F_2[x]/(x³+x+1):") (print g3b-roots-in-f8)
(print "roots of x²+x+1 inside F_2[x]/(x³+x+1):") (print g2-roots-in-f8)
(check "f-reflects-isos"
       (and (= (fiber-size g3 2) (fiber-size g3b 2))       ; same fibre size, 3
            (= (length g3b-roots-in-f8) 3)                 ; so B embeds in A: iso
            ; the witness really is a root: evaluate the polynomial at it
            (equal (feval g3b (car g3b-roots-in-f8) g3 2) (fzero g3))
            ; and the non-isomorphic case is detected, not glossed over
            (/= (fiber-size g2 2) (fiber-size g3 2))
            (eq g2-roots-in-f8 nil)))

; ============================================================
; 4. Correspondence: etale F_q-algebra A <=> finite \hat{Z}-set F(A)
; ============================================================
; π₁(Spec(F_q)) = \hat{Z} with generator Fr: x -> x^q.
; For A = F_{q^n}: F(A) = Z/nZ-set (cyclic of size n).
; For A = F_{q^{d1}} x ... x F_{q^{dk}}:
;   F(A) is the sum of Z/d_iZ-sets: one cyclic orbit of size d_i per factor.
;
; Example A = F_8 over F_2: F(A) = Z/3Z-set (one 3-cycle).
; A = F_4 x F_2 over F_2: F(A) = Z/2Z-set + Z/1Z-set (size 3 total).
; The Frobenius action decomposes the fiber.
(print "--- 4. Correspondence with hat{Z}-sets ---")
(print "F(A) classifies the etale algebra. Fiber = Z/d_iZ-sets.")

; Let Frobenius act on the fibre and read off the orbit sizes. The claim
; "A is connected (a field) <=> F(A) is a transitive Ẑ-set" becomes a computation.
(print "Frobenius orbit sizes on F(F_2), F(F_4), F(F_8):")
(print (map (lambda (g) (fiber-orbits g 2)) (list g1 g2 g3)))
(check "correspondence-zhat-sets"
       (and ; each field gives ONE orbit, of size equal to its degree
            (equal (map (lambda (g) (fiber-orbits g 2)) (list g1 g2 g3))
                   (list (list 1) (list 2) (list 3)))
            ; A = F_4 x F_2 : fibre is a 2-orbit plus a fixed point, type 1+2
            (equal (sort < (append (fiber-orbits g2 2) (fiber-orbits g1 2)))
                   (list 1 2))
            ; A = F_2 x F_2 x F_2 : three fixed points, type 1+1+1 — same total
            ; size 3 as F_8, but a DIFFERENT Ẑ-set, so a different cover
            (equal (sort < (foldr append nil (map (lambda (g) (fiber-orbits g 2))
                                                  (list g1 g1 g1))))
                   (list 1 1 1))
            (not (equal (fiber-orbits g3 2) (list 1 1 1)))))

; ============================================================
; 5. Enumerate the Galois category for low-degree covers of F_2
; ============================================================
; Degree <= 3 covers correspond to partitions of degree via:
; Degree-1: F_2 (1 fixed point).
; Degree-2: F_4 (2-cycle) or F_2 x F_2 (2 fixed points).
; Degree-3: F_8 (3-cycle) or F_4 x F_2 (2-cycle + fixed pt) or F_2 x F_2 x F_2 (3 fixed pts).
;
; Total: 1 + 2 + 3 = 6 isomorphism classes.
; These correspond to conjugacy classes of continuous homomorphisms
; \hat{Z} -> S_d for d <= 3, i.e., the number of conjugacy classes
; in S_1, S_2, S_3: 1 + 2 + 3 = 6.

; Count them, do not assert them: a degree-d cover is a multiset of field
; factor-degrees summing to d, i.e. a partition of d. Enumerate partitions.
(define (parts-ge nn lo)
  (cond ((= nn 0) (list nil))
        (t (foldr append nil
                  (map (lambda (k) (map (lambda (rest) (cons k rest)) (parts-ge (- nn k) k)))
                       (seq lo nn))))))
(define (partitions nn) (parts-ge nn 1))
(define num-deg1 (length (partitions 1)))
(define num-deg2 (length (partitions 2)))
(define num-deg3 (length (partitions 3)))
(define total-covers (+ num-deg1 (+ num-deg2 num-deg3)))
(print "partitions of 3 (= the degree-3 covers):") (print (partitions 3))

(print "--- 5. Enumerate low-degree covers of Spec(F_2) ---")
(print "Degree-1 covers: 1 (F_2)")
(print "Degree-2 covers: 2 (F_4, F_2xF_2)")
(print "Degree-3 covers: 3 (F_8, F_4xF_2, F_2^3)")
(print "Total (deg<=3):") (print total-covers)
(check "enum-low-degree-covers" (= total-covers 6))

(print "=== Galois Categories Done ===")
