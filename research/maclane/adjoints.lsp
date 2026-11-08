; ============================================================
; Mac Lane — Adjoint Functors
; F -| G: Hom(FX, Y) = Hom(X, GY) natural in X, Y.
; Unit eta: 1 => GF, counit eps: FG => 1.
; Free-forgetful adjunction. Galois connection.
; ============================================================

(print "=== Mac Lane: Adjoint Functors ===")

(define (upto lo hi) (cond ((> lo hi) nil) (t (cons lo (upto (+ lo 1) hi)))))
(define (concat ls) (foldr append nil ls))

; ---------- The free-forgetful adjunction, computed ----------
(print "--- Free-Forgetful Adjunction ---")

; U: Mon -> Set forgets structure; Free: Set -> Mon builds words.
; Base set X = {a, b}; monoid mon = (Z/6, +, 0).
; The free monoid on X is infinite, so every quantifier below ranges over
; the words of length <= wmax and every product stays inside that bound.

(define alph (list (quote a) (quote b)))
(define mon-n 6)
(define mon-unit 0)
(define (mon-op u v) (mod (+ u v) mon-n))
(define mon-carrier (upto 0 (- mon-n 1)))

(define (words-of-len k)
  (cond ((= k 0) (list nil))
        (t (concat (map (lambda (w) (map (lambda (s) (cons s w)) alph))
                        (words-of-len (- k 1)))))))
(define (words-upto n)
  (cond ((< n 0) nil) (t (append (words-upto (- n 1)) (words-of-len n)))))

(define wmax 3)
(define all-words (words-upto wmax))

; A set map X -> U(mon) is an assoc list ((a v) (b w)).
(define (setmap-val sm s) (cadr (assoc s sm)))
(define all-setmaps
  (concat (map (lambda (va)
                 (map (lambda (vb) (list (list (quote a) va) (list (quote b) vb)))
                      mon-carrier))
               mon-carrier)))

; Phi-inverse: extend a set map to the unique monoid hom on words.
(define (extend sm w)
  (foldr (lambda (s acc) (mon-op (setmap-val sm s) acc)) mon-unit w))
; Phi: restrict a monoid hom along the unit eta_X (x |-> [x]).
(define (restrict h) (map (lambda (s) (list s (h (list s)))) alph))

; The extension really is a monoid homomorphism: it sends concatenation to
; the monoid operation, and the empty word to the unit.
(check "extension-is-a-monoid-hom"
  (every (lambda (sm)
           (and (= mon-unit (extend sm nil))
                (every (lambda (u)
                         (every (lambda (v)
                                  (cond ((> (+ (length u) (length v)) wmax) t)
                                        (t (= (extend sm (append u v))
                                              (mon-op (extend sm u) (extend sm v))))))
                                all-words))
                       all-words)))
         all-setmaps))

; Phi and Phi-inverse are mutually inverse: Hom_Mon(FX, mon) = Hom_Set(X, U mon).
(check "adjunction-bijection-round-trips"
  (every (lambda (sm) (equal (restrict (lambda (w) (extend sm w))) sm)) all-setmaps))

; Distinct set maps give distinct homs, so the two hom-sets have equal size:
; |Hom_Set(X, U mon)| = |U mon|^|X| = 36.
(check "hom-sets-have-equal-size"
  (= (length (dedup (map (lambda (sm) (map (lambda (w) (extend sm w)) all-words))
                         all-setmaps)))
     (^ mon-n (length alph))))

; Negative control: a map that is NOT the multiplicative extension (it adds
; one to every value) must be rejected by the homomorphism test.
(check "hom-test-rejects-non-hom"
  (not (let ((sm (list (list (quote a) 1) (list (quote b) 2))))
         (every (lambda (u)
                  (every (lambda (v)
                           (cond ((> (+ (length u) (length v)) wmax) t)
                                 (t (= (mod (+ 1 (extend sm (append u v))) mon-n)
                                       (mon-op (mod (+ 1 (extend sm u)) mon-n)
                                               (mod (+ 1 (extend sm v)) mon-n))))))
                         all-words))
                all-words))))

; ---------- Unit and counit ----------
(print "--- Unit eta and Counit eps ---")

; eta_X: X -> U(F X) sends x to the one-letter word [x].
(define (eta-x s) (list s))
; eps_mon: F(U mon) -> mon multiplies out a word of monoid elements.
(define (eps-mon w) (foldr mon-op mon-unit w))

; Triangle 1:  U(eps) . eta_U = 1_U  on every element of the monoid.
(check "triangle-identity-U-eps-eta-U"
  (every (lambda (m) (= m (eps-mon (list m)))) mon-carrier))

; Triangle 2:  eps_F . F(eta) = 1_F  on every word of the free monoid.
; F(eta_X) sends [x1..xn] to the word of one-letter words; eps_{F X}
; concatenates them, which must give [x1..xn] back.
(check "triangle-identity-eps-F-F-eta"
  (every (lambda (w) (equal w (concat (map (lambda (s) (eta-x s)) w)))) all-words))

; eps is itself a monoid homomorphism F(U mon) -> mon.
(define mon-words
  (concat (map (lambda (u) (map (lambda (v) (list u v)) mon-carrier)) mon-carrier)))
(check "counit-is-a-monoid-hom"
  (every (lambda (u)
           (every (lambda (v) (= (eps-mon (append u v)) (mon-op (eps-mon u) (eps-mon v))))
                  mon-words))
         mon-words))

; ---------- Hom-tensor adjunction ----------
(print "--- Hom-Tensor Adjunction ---")

; Over F_2 with C = F_2: a linear map A (x) B -> C is a list of a*b bits,
; indexed by the basis e_i (x) f_j. Currying reshapes it into a linear map
; A -> Hom(B, C). The two sides are compared by EVALUATION, not by shape.

(define dim-a 2)
(define dim-b 3)

(define (bit-vectors n)
  (cond ((= n 0) (list nil))
        (t (concat (map (lambda (v) (list (cons 0 v) (cons 1 v))) (bit-vectors (- n 1)))))))

(define (dot u v) (mod (foldr + 0 (map (lambda (pr) (* (car pr) (cadr pr))) (zip u v))) 2))
(define (scale c v) (map (lambda (u) (mod (* c u) 2)) v))
(define (vadd u v) (map (lambda (pr) (mod (+ (car pr) (cadr pr)) 2)) (zip u v)))

; phi as a flat list of a*b coefficients; row i is the i-th block of length b.
(define (row phi i) (take (drop phi (* i dim-b)) dim-b))
; Uncurried evaluation: phi(x (x) y) = sum_ij phi_ij x_i y_j.
(define (eval-tensor phi x y)
  (mod (foldr + 0
              (map (lambda (i)
                     (* (nth i x) (dot (row phi i) y)))
                   (upto 0 (- dim-a 1))))
       2))
; Curried evaluation: (curry phi)(x) is the element sum_i x_i * row_i of
; Hom(B,C), which is then applied to y.
(define (curry-apply phi x)
  (foldr vadd (map (lambda (j) 0) (upto 1 dim-b))
         (map (lambda (i) (scale (nth i x) (row phi i))) (upto 0 (- dim-a 1)))))

(define all-phis (bit-vectors (* dim-a dim-b)))
(define vecs-a (bit-vectors dim-a))
(define vecs-b (bit-vectors dim-b))

(check "hom-tensor-adjunction-evaluations-agree"
  (every (lambda (phi)
           (every (lambda (x)
                    (every (lambda (y) (= (eval-tensor phi x y) (dot (curry-apply phi x) y)))
                           vecs-b))
                  vecs-a))
         all-phis))

; The currying map is a bijection: |Hom(A(x)B, C)| = |Hom(A, Hom(B,C))| = 2^(ab).
(check "hom-tensor-currying-is-injective"
  (= (length all-phis)
     (length (dedup (map (lambda (phi) (map (lambda (x) (curry-apply phi x)) vecs-a))
                         all-phis)))))
(check "hom-tensor-both-sides-have-size-2-to-the-ab"
  (= (length all-phis) (^ 2 (* dim-a dim-b))))

; ---------- Naturality of the adjunction ----------
(print "--- Naturality ---")

; Naturality in X: for a set map f: X' -> X, Phi(h . F f) = Phi(h) . f.
; Take f to be the swap a <-> b.
(define (swap-sym s) (cond ((eq s (quote a)) (quote b)) (t (quote a))))
(define (free-swap w) (map (lambda (s) (swap-sym s)) w))

(check "adjunction-is-natural-in-X"
  (every (lambda (sm)
           (equal (restrict (lambda (w) (extend sm (free-swap w))))
                  (map (lambda (s) (list s (setmap-val sm (swap-sym s)))) alph)))
         all-setmaps))

; The reindexing must be a genuinely nontrivial map, or naturality in X
; would hold for the empty reason.
(check "swap-is-not-the-identity"
  (not (equal (free-swap (list (quote a) (quote a))) (list (quote a) (quote a)))))

; Naturality also for a NON-invertible reindexing, f(a) = f(b) = a.
(define (collapse-sym s) (quote a))
(define (free-collapse w) (map (lambda (s) (collapse-sym s)) w))
(check "adjunction-is-natural-in-X-for-non-invertible-map"
  (every (lambda (sm)
           (equal (restrict (lambda (w) (extend sm (free-collapse w))))
                  (map (lambda (s) (list s (setmap-val sm (collapse-sym s)))) alph)))
         all-setmaps))

; Naturality in the monoid: for a monoid hom g: mon -> mon, Phi(g . h) = U(g) . Phi(h).
; g = multiplication by 2 on Z/6 is additive, hence a monoid hom.
(define (g-hom m) (mod (* 2 m) mon-n))
(check "g-hom-is-a-monoid-hom"
  (every (lambda (u)
           (every (lambda (v) (= (g-hom (mon-op u v)) (mon-op (g-hom u) (g-hom v))))
                  mon-carrier))
         mon-carrier))
(check "adjunction-is-natural-in-the-monoid"
  (every (lambda (sm)
           (equal (restrict (lambda (w) (g-hom (extend sm w))))
                  (map (lambda (s) (list s (g-hom (setmap-val sm s)))) alph)))
         all-setmaps))

; ---------- Galois connection as adjunction ----------
(print "--- Galois connection = poset adjunction ---")

; F_64/F_2 has Galois group Z/6. Subgroups and intermediate fields are both
; indexed by the divisors of 6: the subgroup of ORDER d, and the field
; F_{2^e}. Order: H_d <= H_d' iff d | d'; F_{2^e} <= F_{2^e'} iff e | e'.
(define divisors6 (filter (lambda (d) (= 0 (mod 6 d))) (upto 1 6)))
(check "divisors-of-6" (equal divisors6 (list 1 2 3 6)))

; Fix(H_d) = F_{2^(6/d)};  Gal(F_{2^e}) = subgroup of order 6/e.
(define (fix-of d) (truncate (/ 6 d)))
(define (gal-of e) (truncate (/ 6 e)))

; The Galois connection (contravariant adjunction):
;   H_d <= Gal(F_{2^e})   iff   F_{2^e} <= Fix(H_d).
(check "galois-adjunction"
  (every (lambda (d)
           (every (lambda (e)
                    (eq (= 0 (mod (gal-of e) d)) (= 0 (mod (fix-of d) e))))
                  divisors6))
         divisors6))

; Closure: the connection is a bijection, so both round trips are the identity.
(check "galois-fix-gal-round-trip"
  (every (lambda (e) (= e (fix-of (gal-of e)))) divisors6))
(check "galois-gal-fix-round-trip"
  (every (lambda (d) (= d (gal-of (fix-of d)))) divisors6))

; The correspondence reverses order.
(check "galois-correspondence-is-order-reversing"
  (every (lambda (d)
           (every (lambda (e)
                    (eq (= 0 (mod e d)) (= 0 (mod (fix-of d) (fix-of e)))))
                  divisors6))
         divisors6))

; Negative control: the NAIVE assignment Fix(H_d) = F_{2^d} is order-preserving
; instead of order-reversing, and the adjunction condition must reject it.
(check "galois-adjunction-rejects-wrong-correspondence"
  (not (every (lambda (d)
                (every (lambda (e) (eq (= 0 (mod (gal-of e) d)) (= 0 (mod d e))))
                       divisors6))
              divisors6)))

(print "=== Adjoints Done ===")
