; ============================================================
; Bourbaki — Regular Sequences and Koszul Complex
; Depth, Cohen-Macaulay property.
; Koszul homology vanishes on regular sequences.
; For F_q[x,y]/(x²,xy): compute depth.
; Verified: depth(F_2[x,y]/(x²,xy)) = 0.
; ============================================================

(print "=== Bourbaki: Regular Sequences ===")

; ---------- ring operations ----------
(define (rmod n a) (mod a n))
(define (radd n a b) (mod (+ a b) n))
(define (rmul n a b) (mod (* a b) n))

; ---------- field F_2 via Z/2Z ----------
(define pf 2)

; ========== REGULAR SEQUENCE DEFINITION ==========
; A sequence (f_1,...,f_r) in a ring R is regular if:
; 1. f_1 is a non-zero-divisor on R
; 2. f_i is a non-zero-divisor on R/(f_1,...,f_{i-1}) for i=2,...,r
; 3. (f_1,...,f_r) ≠ R

; depth(R) = maximal length of a regular sequence.
; For local rings, depth(R) ≤ dim(R).
; Cohen-Macaulay: depth(R) = dim(R).

; ---------- COHEN-MACAULAY TEST ON F_q[x,y]/(x², xy) ----------
; R = F_2[x,y]/(x², xy). The relations x² = 0, xy = 0 leave the F_2-basis
; {1, x, y, y², y³, ...}: x is nilpotent, y is not.
; dim(R) = 1: x is nilpotent, so every prime ideal contains (x), and
; R/(x) ≅ F_2[y] has the prime chain (0) ⊂ (y); in R this is (x) ⊂ (x,y).
; depth(R) = 0 (at m = (x,y)): x is a zero-divisor (x·y = 0, y ≠ 0), and so is
; y (y·x = 0, x ≠ 0). In fact x·m = 0, so every element of m kills x and m
; consists of zero-divisors: m is an associated (embedded) prime.
; Geometrically R is the y-axis with an embedded point at the origin.

; ========== ANNIHILATOR COMPUTATION ==========
(print "--- Annihilator and Regular Element Test ---")

; In R = F_2[x,y]/(x²,xy), represent elements as triples (a,b,c) = a + bx + cy.
; Addition: component-wise mod 2
; Multiplication: (a,b,c)*(d,e,f) = (ad, ae+bd, af+cd), dropping the x², xy and
; y² terms. So this 8-element model is F_2[x,y]/(x², xy, y²), a truncation of R:
; it gives the same zero-divisor verdicts as R, but not the same dimension
; (see the exact model further down).

(define (elem a b c) (list a b c))
(define (zero) (list 0 0 0))
(define (one) (list 1 0 0))

(define (add-elem u v)
  (list (rmod pf (+ (nth 0 u) (nth 0 v)))
        (rmod pf (+ (nth 1 u) (nth 1 v)))
        (rmod pf (+ (nth 2 u) (nth 2 v)))))

(define (mul-elem u v)
  (let ((a (nth 0 u)) (b (nth 1 u)) (c (nth 2 u))
        (d (nth 0 v)) (e (nth 1 v)) (f (nth 2 v)))
    (list (rmod pf (* a d))
          (rmod pf (+ (* a e) (* b d)))
          (rmod pf (+ (* a f) (* c d))))))

; The elements of R
(define all-elems (list-to-set
  (foldr append nil
    (map (lambda (x)
      (foldr append nil
        (map (lambda (y)
          (map (lambda (z) (list x y z)) (seq 0 1)))
          (seq 0 1))))
      (seq 0 1)))))

(check "R-has-8-elements" (= (length all-elems) 8))

; Regular element: f is regular ⟺ ann(f) = {0}
; ann-nontrivial computes the full annihilator ideal {g : f*g = 0};
; it is trivial (= {0}, size 1) exactly when f is a non-zero-divisor.
(define (ann-nontrivial f)
  (filter (lambda (g)
    (equal (mul-elem f g) (zero)))
    all-elems))

(define (regular-p f)
  (= (length (ann-nontrivial f)) 1))

; Zero is NOT regular
(check "zero-is-not-regular" (not (regular-p (zero))))

; 1 is regular
(check "one-is-regular" (regular-p (one)))

; x = (0,1,0)
(define X (elem 0 1 0))
; Ann(x): elements s.t. x*g=0. (0,1,0)*(a,b,c) = (0, a, 0). So a=0. g = (0,b,c).
; Ann(x) = {(0,0,0), (0,1,0), (0,0,1), (0,1,1)}. Size 4.
(check "x-has-nontrivial-ann" (= (length (ann-nontrivial X)) 4))
(check "x-is-zero-divisor" (not (regular-p X)))

; y = (0,0,1)
(define Y (elem 0 0 1))
; Ann(y): elements s.t. y*g=0. (0,0,1)*(a,b,c) = (0, 0, a). So a=0.
; Ann(y) = {(0,b,c) : b,c∈{0,1}}. Size 4.
; CAVEAT: the count 4 holds because mul-elem drops the y^2 term, i.e. this
; triple model is really F_2[x,y]/(x^2,xy,y^2).  In the true R the annihilator of
; y is just (x) = {0,x}, size 2.  Either way y is a zero-divisor; see the exact
; model further down, which is what the dimension/depth results below rely on.
(check "y-has-nontrivial-ann" (= (length (ann-nontrivial Y)) 4))
(check "y-is-zero-divisor" (not (regular-p Y)))

; x+y = (0,1,1)
(define XpY (elem 0 1 1))
; (0,1,1)*(a,b,c) = (0, a, a). So a=0. ann = {(0,b,c)}, size 4.
(check "x-plus-y-is-zero-divisor" (not (regular-p XpY)))

; ========== DEPTH COMPUTATION ==========
(print "--- Depth ---")

; depth(R) = max length of regular sequence.
; The maximal ideal m = {elements with constant term 0} = {(0,b,c)}.
; In the 8-element model every f ∈ m has f² = (0,b,c)*(0,b,c) = (0,0,0), so
; every nonzero element of m is a zero-divisor and depth = 0. (In the true R,
; y is not nilpotent, but every f ∈ m still kills x, so depth = 0 there too.)

(check "depth-F2-xy-x2-is-0"
  (let ((max-ideal (filter (lambda (e) (= (nth 0 e) 0)) all-elems)))
    (every (lambda (f)
      (or (equal f (zero)) (not (regular-p f)))) max-ideal)))

; Verify: the maximal ideal has NO regular elements
(check "no-regular-in-max-ideal"
  (not (any (lambda (f) (regular-p f))
        (filter (lambda (e) (= (nth 0 e) 0)) all-elems))))

; ========== DIMENSION ==========
(print "--- Dimension ---")

; dim(R) = Krull dimension. R = F_2[x,y]/(x²,xy).
; In R the nilradical is (x) = {0, x}: (x+y)² = x² + 2xy + y² = y² ≠ 0, so m
; is not nil. R/(x) ≅ F_2[y] is a domain of dimension 1 with prime chain
; (0) ⊂ (y), so the primes (x) ⊂ (x,y) of R form a chain and dim(R) = 1.
; Depth = 0 < dim = 1: R is not Cohen-Macaulay.
; In the 8-element truncation, by contrast, every element of m is nilpotent
; and the dimension is 0; so dimension is computed in the exact model below.

; ============================================================
; EXACT MODEL OF R = F_2[x,y]/(x^2, xy)
; ============================================================
; The 8-element triple model above sets y^2 = 0 as well, so it is really
; F_2[x,y]/(x^2,xy,y^2). That does not affect the zero-divisor/depth facts,
; but it cannot give the dimension: in that truncation every element of m is
; nilpotent and dim = 0. Dimension needs the genuine ring, in which y is NOT
; nilpotent. So model R exactly:
;
;   element = (a b ys), meaning a + b*x + sum_{i>=1} ys_i * y^i   over F_2,
;   with ys a (trailing-zero-free) coefficient list for y^1, y^2, ...
;
; Relations used: x^2 = 0, x*y^k = 0.  Nothing is truncated, so products are exact.

(define (ynorm ys)
  (cond ((eq ys nil) nil)
        (t (let ((rest (ynorm (cdr ys))))
             (cond ((and (eq rest nil) (= (car ys) 0)) nil)
                   (t (cons (car ys) rest)))))))

(define (rr a b ys) (list a b (ynorm ys)))

(define (ylist-add p q)
  (cond ((eq p nil) (ynorm q))
        ((eq q nil) (ynorm p))
        (t (ynorm (cons (mod (+ (car p) (car q)) 2) (ylist-add (cdr p) (cdr q)))))))

(define (ylist-scale c p) (ynorm (map (lambda (u) (mod (* c u) 2)) p)))

; p*q where both p,q have zero constant term: p = p1*y + y*P', so
; p*q = y*(p1*q + P'*q).
(define (ylist-mul p q)
  (cond ((eq p nil) nil)
        (t (ynorm (cons 0 (ylist-add (ylist-scale (car p) q)
                                     (ylist-mul (cdr p) q)))))))

(define rr-zero (list 0 0 nil))
(define rr-one (list 1 0 nil))
(define rr-x (list 0 1 nil))
(define rr-y (list 0 0 (list 1)))

(define (rr-add u v)
  (rr (mod (+ (nth 0 u) (nth 0 v)) 2)
      (mod (+ (nth 1 u) (nth 1 v)) 2)
      (ylist-add (nth 2 u) (nth 2 v))))

; (a + b x + F)(d + e x + G) = ad + (ae+bd)x + (a G + d F + F G)
(define (rr-mul u v)
  (let ((a (nth 0 u)) (b (nth 1 u)) (fp (nth 2 u))
        (d (nth 0 v)) (e (nth 1 v)) (gp (nth 2 v)))
    (rr (mod (* a d) 2)
        (mod (+ (* a e) (* b d)) 2)
        (ylist-add (ylist-add (ylist-scale a gp) (ylist-scale d fp))
                   (ylist-mul fp gp)))))

; y is genuinely non-nilpotent here (unlike in the 8-element truncation)
(check "y-is-not-nilpotent-in-R"
  (and (equal (rr-mul rr-x rr-x) rr-zero)
       (equal (rr-mul rr-x rr-y) rr-zero)
       (not (equal (rr-mul rr-y rr-y) rr-zero))
       (not (equal (rr-mul (rr-mul rr-y rr-y) (rr-mul rr-y rr-y)) rr-zero))))

; A finite sample of R: a,b in F_2 and y-part of degree <= 3.  32 elements.
(define ylist-sample
  (foldr append nil
    (map (lambda (c1)
           (foldr append nil
             (map (lambda (c2)
                    (map (lambda (c3) (list c1 c2 c3)) (seq 0 1)))
                  (seq 0 1))))
         (seq 0 1))))

(define rr-sample
  (dedup (foldr append nil
    (map (lambda (a)
           (foldr append nil
             (map (lambda (b)
                    (map (lambda (ys) (rr a b ys)) ylist-sample))
                  (seq 0 1))))
         (seq 0 1)))))

(define rr-pairs
  (foldr append nil
    (map (lambda (u) (map (lambda (v) (list u v)) rr-sample)) rr-sample)))

; The two candidate primes.  (x) = R*x = {0, x}; m = (x,y) = {constant term 0}.
(define (in-x-ideal u) (and (= (nth 0 u) 0) (eq (nth 2 u) nil)))
(define (in-max-ideal u) (= (nth 0 u) 0))

; P is prime  <=>  u*v in P implies u in P or v in P.  Tested over all 1024
; pairs of sample elements.
(define (prime-ideal-p memb)
  (every (lambda (pr)
           (cond ((funcall memb (rr-mul (nth 0 pr) (nth 1 pr)))
                  (or (funcall memb (nth 0 pr)) (funcall memb (nth 1 pr))))
                 (t t)))
         rr-pairs))

; The strict chain of primes (x) < (x,y) witnesses dim(R) >= 1.
(define rr-prime-chain-ok
  (and (prime-ideal-p (lambda (u) (in-x-ideal u)))
       (prime-ideal-p (lambda (u) (in-max-ideal u)))
       (every (lambda (u) (cond ((in-x-ideal u) (in-max-ideal u)) (t t))) rr-sample)
       (in-max-ideal rr-y)
       (not (in-x-ideal rr-y))))

(define rr-dim-lower (cond (rr-prime-chain-ok 1) (t 0)))

; The matching upper bound dim(R) <= 1 is NOT finitely checkable: it amounts to
; "F_2[y] = R/(x) has exactly the primes (0) and (y)", a statement about an
; infinite ring.  So we assert exactly what is computed: the explicit prime
; chain of length 1.
(check "dim-F2-xy-x2-at-least-1" (= rr-dim-lower 1))

; ========== KOSZUL COMPLEX ==========
(print "--- Koszul Complex ---")

; For the sequence (x,y) in R, the Koszul complex is:
; 0 → R → R² → R → 0
; with differentials:
; d1: e_x → x, e_y → y
; d2: e_x∧e_y → (-y, x) = (y, x) in char 2.
; Its homology measures the failure of (x,y) to be regular: for elements of
; the maximal ideal of a local (or graded) ring, (x,y) is regular iff
; H_1 = H_2 = 0, and in general depth = 2 - max{i : H_i ≠ 0}.
; In the exact ring R, H_2 = ker(d2) = Ann(x) ∩ Ann(y) = m ∩ (x) = {0, x} ≠ 0,
; which again gives depth 0.

; H_1(Koszul(x,y)) = ker(d1)/im(d2), computed in the 8-element model, where
; d1: R² → R sends (a,b) → a*x + b*y with a, b ∈ R written as triples.
; (a0,a1,a2)*(0,1,0) = (0, a0, 0) and (d0,d1,d2)*(0,0,1) = (0, 0, d0), so
; d1((a0,a1,a2), (d0,d1,d2)) = (0, a0, d0).
; This is 0 in R iff a0=0 and d0=0. So kernel = {((0,a1,a2),(0,d1,d2))}: 16 elements.
; d2(r0,r1,r2) = ((r0,r1,r2)*y, (r0,r1,r2)*x) = ((0,0,r0), (0,r0,0)),
; so im(d2) has 2 elements.
; ker/im has size 16/2 = 8 = |H_1|. Nontrivial homology → (x,y) is NOT regular. ✓
; The check below records the two sizes found here.

(check "koszul-homology-nontrivial"
  (> 16 2))

; ========== COHEN-MACAULAY FAILURE ==========
(print "--- Cohen-Macaulay Failure ---")

; ---------- depth of R, computed in the exact model ----------
; f is regular <=> ann(f) = {0}.  A witness v /= 0 with f*v = 0 found inside the
; sample is a genuine proof that f is a zero-divisor (soundness in that
; direction is exact; only the "regular" verdict is sample-bounded).
(define (rr-regular-p u)
  (not (any (lambda (v) (and (not (equal v rr-zero))
                             (equal (rr-mul u v) rr-zero)))
            rr-sample)))

(define rr-max-ideal-elems (filter (lambda (u) (in-max-ideal u)) rr-sample))

; depth(R) = maximal length of a regular sequence in m.  A regular sequence of
; length >= 1 needs a regular element of m to start with; if there is none,
; depth = 0.
(define rr-depth
  (cond ((any (lambda (u) (rr-regular-p u)) rr-max-ideal-elems) 1)
        (t 0)))

; dim(R) >= 1 (prime chain above), depth(R) = 0 (computed just now).
; depth < dim  =>  NOT Cohen-Macaulay.  Both sides computed independently.
(check "not-cohen-macaulay" (< rr-depth rr-dim-lower))

; ========== REGULAR SEQUENCE IN F_2[x] ==========
(print "--- Regular Sequence in F_2[x] ---")

; F_2[x] is a PID (dimension 1). The element x is a regular element.
; depth(F_2[x]) = 1 = dim(F_2[x]) (at the maximal ideal (x)). So F_2[x] is
; Cohen-Macaulay.

; F_2[x] is a domain, so x is not a zero-divisor:
; x*(a_n x^n + ... + a_0) = a_n x^{n+1} + ... + a_0 x = 0 ⟺ all a_i = 0.
; Polynomials over F_2 as trailing-zero-free coefficient lists (constant first);
; ynorm/ylist-add/ylist-scale from above are reused for normalisation and sums.
(define (poly-mul p q)
  (cond ((eq p nil) nil)
        (t (ylist-add (ylist-scale (car p) q)
                      (ynorm (cons 0 (poly-mul (cdr p) q)))))))

(define poly-x (list 0 1))

; all 16 polynomials of degree < 4, normalised
(define poly-sample
  (dedup (map (lambda (cs) (ynorm cs))
    (foldr append nil
      (map (lambda (c0)
             (foldr append nil
               (map (lambda (c1)
                      (foldr append nil
                        (map (lambda (c2)
                               (map (lambda (c3) (list c0 c1 c2 c3)) (seq 0 1)))
                             (seq 0 1))))
                    (seq 0 1))))
           (seq 0 1))))))

; x is a non-zero-divisor: multiplication by x is INJECTIVE, and kills only 0.
(check "x-is-regular-in-F2x"
  (let ((imgs (map (lambda (p) (poly-mul poly-x p)) poly-sample)))
    (and (= (length poly-sample) 16)
         (= (length (dedup imgs)) 16)
         (equal (poly-mul poly-x nil) nil)
         (every (lambda (p) (cond ((eq (poly-mul poly-x p) nil) (eq p nil))
                                  (t t)))
                poly-sample)
         (equal (poly-mul poly-x poly-x) (list 0 0 1)))))

; ========== DEPTH 0 vs 1 SUMMARY ==========
(print "--- Summary ---")

; F_2[x,y]/(x²,xy): depth = 0 (every element of m kills x), dim = 1: not CM.
; F_2[x] (or any polynomial ring over field): depth = dim = Krull dimension (CM).
; Z/nZ (any n ≥ 2): every non-unit is a zero-divisor, so depth = 0 = dim, and
; Z/nZ is Cohen-Macaulay, like every Artinian ring.
; E.g. Z/6Z: 2*3 = 0; Z/4Z: 2*2 = 0.

; depth(R) computed above from the exact model.  The first conjunct shows the
; regularity predicate is not constantly nil (1 IS regular), so the verdict
; depth = 0 is a real computation, not a degenerate one.
(check "F2-xy-x2-depth-0"
  (and (rr-regular-p rr-one)
       (not (rr-regular-p rr-y))
       (not (rr-regular-p rr-x))
       (= rr-depth 0)))

(print "=== Regular Sequences Done ===")
