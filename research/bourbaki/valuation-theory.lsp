; ============================================================
; Bourbaki -- Valuation Theory: Discrete Valuations,
; Valuation Rings, Completions of Finite DVRs, Hensel's Lemma.
; ============================================================

(print "=== Valuation Theory ===")

; ---------- pow ----------
(define (pow b e)
  (cond ((= e 0) 1)
        (t (* b (pow b (- e 1))))))

; ============================================================
; 1. DISCRETE VALUATION ON Q_p / Z_p
; ============================================================
; For the p-adic integers Z_p (profinite completion of Z at p):
; v_p(a) = max{k : p^k | a} for a ∈ Z.
; The completion Z/p^nZ for large n approximates Z_p.

(define (v-padic p-val a)
  (cond ((= a 0) -1)  ; infinite in limit, but for finite: indicator
        ((= (mod a p-val) 0) (+ 1 (v-padic p-val (/ a p-val))))
        (t 0)))

; ---------- Z/p^nZ as finite approximation to Z_p ----------
; For Z/8Z (p=2, n=3):
; v_2(0)=undefined (formally infinite), v_2(1)=0, v_2(2)=1, v_2(4)=2
(print "--- Valuation on Z/8Z ---")

(check "v2-z8-0-signal" (= (v-padic 2 1) 0))
(check "v2-z8-1" (= (v-padic 2 2) 1))
(check "v2-z8-2" (= (v-padic 2 4) 2))
(check "v2-z8-3" (= (v-padic 2 3) 0))
(check "v2-z8-5" (= (v-padic 2 5) 0))
(check "v2-z8-6" (= (v-padic 2 6) 1))

; ============================================================
; 2. VALUATION RING PROPERTIES
; ============================================================
; A valuation ring R_v = {x : v(x) ≥ 0}
; The maximal ideal m_v = {x : v(x) > 0}
; Units: {x : v(x) = 0}
; Additive property: v(x+y) ≥ min(v(x), v(y)) (non-archimedean)

(print "--- Valuation ring properties ---")

; For Z/8Z as a valuation ring approximation:
; R_v = all elements (v ≥ 0 for all elements of Z/8Z)
; m_v = {0,2,4,6} (v > 0, i.e., v ≥ 1)
; Units = {1,3,5,7} (v = 0, coprime to 2)

(define (unit-p8 a) (not (= (mod a 2) 0)))

(check "units-Z8-v2-0" (and (unit-p8 1) (unit-p8 3) (unit-p8 5) (unit-p8 7)))
(check "nonunits-Z8-even" (and (not (unit-p8 0)) (not (unit-p8 2))
                                 (not (unit-p8 4)) (not (unit-p8 6))))

; Ultrametric: v(x+y) ≥ min(v(x), v(y))
(define (v-mod8 a) (v-padic 2 a))

(check "ultrametric-2+4"
  (let ((vxy (v-mod8 (mod (+ 2 4) 8))))
    (>= vxy (min (v-mod8 2) (v-mod8 4)))))
(check "ultrametric-1+3"
  (let ((vxy (v-mod8 (mod (+ 1 3) 8))))
    (>= vxy (min (v-mod8 1) (v-mod8 3)))))

; ============================================================
; 3. VALUATION ON Z/27Z (p=3)
; ============================================================
(print "--- Valuation on Z/27Z (p=3) ---")

(check "v3-z27-1" (= (v-padic 3 1) 0))
(check "v3-z27-3" (= (v-padic 3 3) 1))
(check "v3-z27-9" (= (v-padic 3 9) 2))
(check "v3-z27-18" (= (v-padic 3 18) 2))
(check "v3-z27-6" (= (v-padic 3 6) 1))
(check "v3-z27-2" (= (v-padic 3 2) 0))

; ============================================================
; 4. DISCRETE VALUATION RING (DVR)
; ============================================================
; A DVR is a local PID with exactly one nonzero prime ideal.
; Z_p (p-adic integers) is a DVR with maximal ideal (p).
; The residue field is F_p.
; The fraction field is Q_p.
; Every nonzero element of Q_p can be uniquely written as p^n * u,
; with u a unit in Z_p.

; For the finite ring Z/p^nZ (a chain ring):
; It is NOT a DVR (not an integral domain), but a quotient of a DVR.
; The maximal ideal is (p), residue field is F_p.
; The valuation ring R_v = {x : v(x) ≥ 0} is the whole ring.

(print "--- DVR properties ---")

; Uniformizer: element with valuation 1.
; For Z/p^nZ: the uniformizer is p (mod p^n).
; The residue field is Z/p^nZ / (p) ≅ F_p.

(check "uniformizer-z8-is-2" (= (v-padic 2 2) 1))
(check "uniformizer-z27-is-3" (= (v-padic 3 3) 1))

; ============================================================
; 5. COMPLETION OF FINITE DVRs
; ============================================================
; The I-adic completion of a ring R with respect to ideal I:
; R̂_I = lim_{←n} R/I^n.
; For Z/p^nZ with I=(p), the I-adic completion of Z/p^kZ is Z/p^kZ
; (it's already complete/modules of finite length).

(print "--- I-adic completion ---")

; For R = Z, I = (p): completion is Z_p = lim Z/p^nZ.
; For the finite quotient: Z/p^nZ is already complete for the I-adic
; topology when n is finite (the filtration stabilizes).

; Z/8Z with I=(2): R/I = Z/2Z, R/I^2 = Z/4Z, R/I^3 = Z/8Z.
; The limit gives back Z/8Z (finite ring, already complete).

; Real computation: build the inverse system Z/2Z <- Z/4Z <- Z/8Z explicitly and
; enumerate its coherent sequences (a1 a2 a3), a_n in Z/2^n, a_{n+1} = a_n mod 2^n.
; The filtration on Z/8Z stabilises (2^3 = 0), so the limit must be Z/8Z itself:
; there are exactly 8 coherent sequences and their last coordinates run over all
; of Z/8Z exactly once (i.e. the projection lim <- Z/8Z is a bijection).
(define coherent-3
  (filter (lambda (s)
            (and (= (mod (nth 1 s) 2) (nth 0 s))
                 (= (mod (nth 2 s) 4) (nth 1 s))))
          (foldr append nil
            (map (lambda (a)
                   (foldr append nil
                     (map (lambda (b)
                            (map (lambda (c) (list a b c)) (seq 0 7)))
                          (seq 0 3))))
                 (seq 0 1)))))

(check "z8-is-complete-for-2-adic"
  (and (= (length coherent-3) 8)
       (equal (sort < (map (lambda (s) (nth 2 s)) coherent-3)) (seq 0 7))))

; ============================================================
; 6. HENSEL'S LEMMA FOR FINITE RINGS
; ============================================================
; Hensel's Lemma (simplified): Let f(x) ∈ R[x] where R is complete
; with respect to an ideal m. If a is a root modulo m (i.e., f(a) ≡ 0 mod m)
; and f'(a) is a unit modulo m, then a lifts uniquely to a root in R.
;
; For Z/8Z, m=(2): mod m means mod 2.
; f(x) = x^2 - 1 over Z/8Z.
; mod 2: f(1) = 0, f'(1) = 2*1 ≡ 0 mod 2 => f'(1) is NOT a unit mod 2!
; So Hensel fails: x^2=1 mod 8 has 4 solutions (1,3,5,7).
;
; f(x) = x^2 + x + 1. Mod 2: f(0)=1≠0, f(1)=3≡1≠0. No root mod 2.
; Mod 4: f(0)=1, f(1)=3, f(2)=7≡3, f(3)=13≡1. No root => no root in Z/8Z.
;
; f(x) = x - 1. Mod 2: f(1)=0, f'(1)=1 (unit mod 2). Lifts to 1 mod 8.

(print "--- Hensel's lemma ---")
(print "f(x)=x^2-1 mod 8: f'(1) not unit mod 2 => multiple lifts {1,3,5,7}")
(print "f(x)=x-1: f'(1)=1 unit => unique lift 1")

; Verify: x^2 ≡ 1 mod 8 has 4 solutions
(define (sq1-mod8 x) (= (mod (* x x) 8) 1))
(define sq1-solutions
  (filter (lambda (xr) (sq1-mod8 xr)) (list-to-set (seq 0 7))))
(check "x^2=1-mod8-has-4-solutions" (= (length sq1-solutions) 4))

; Hensel lemma: for x^2 ≡ 1 mod 4, lift to mod 8
(define (sq1-mod4 x) (= (mod (* x x) 4) 1))
(define sq1-mod4-sols
  (filter (lambda (xr) (sq1-mod4 xr)) (list-to-set (seq 0 3))))
(check "x^2=1-mod4-has-2-solutions" (= (length sq1-mod4-sols) 2))

; Each mod-4 solution lifts to two mod-8 solutions:
; 1 mod 4 -> {1,5} mod 8; 3 mod 4 -> {3,7} mod 8.
; Real computation: group the mod-8 roots of x^2=1 by their residue mod 4 and
; count the fibres.  f'(a)=2a is NOT a unit mod 2, so Hensel gives no uniqueness:
; every mod-4 root has exactly TWO lifts, and the fibres are {1,5} and {3,7}.
(define sols8 (filter (lambda (u) (= (mod (* u u) 8) 1)) (seq 0 7)))
(define sols4 (filter (lambda (u) (= (mod (* u u) 4) 1)) (seq 0 3)))
(define (lifts-of r) (filter (lambda (u) (= (mod u 4) r)) sols8))

(check "hensen-lifting-1to8"
  (and (every (lambda (r) (= (length (lifts-of r)) 2)) sols4)
       (equal (lifts-of 1) (list 1 5))
       (equal (lifts-of 3) (list 3 7))
       (= (length sols4) 2)))

; ============================================================
; 7. HENSEL FOR F_q[t]/(t^n) (finite chain ring)
; ============================================================
; Similar to Z/p^nZ, we can consider F_2[t]/(t^3).
; m = (t), residue field = F_2.
; f(x) = x^2 + x + t over F_2[t]/(t^3).
; Mod t: f(x) = x^2 + x. Roots: x=0,1 mod t.
; f'(x) = 2x + 1 = 1 mod t (since 2=0 in F_2). So f'(x) is a unit.
; Each root lifts uniquely to a root in F_2[t]/(t^3).

(print "--- Hensel for F_2[t]/(t^3) ---")
(print "f(x)=x^2+x+t: mod t roots {0,1}, f'=1 unit => unique lifts")

; ---------- explicit model of the chain ring F_2[t]/(t^3) ----------
; An element is the coefficient list (c0 c1 c2) of c0 + c1 t + c2 t^2 over F_2.
(define (t3-add u v)
  (map (lambda (pr) (mod (+ (nth 0 pr) (nth 1 pr)) 2)) (zip u v)))

; truncated convolution: (u*v)_k = sum_{j<=k} u_j v_{k-j}, mod 2, for k = 0,1,2
(define (t3-mul u v)
  (map (lambda (k)
         (mod (foldr (lambda (p q) (+ p q)) 0
                (map (lambda (j) (* (nth j u) (nth (- k j) v))) (seq 0 k)))
              2))
       (seq 0 2)))

(define t3-elems
  (foldr append nil
    (map (lambda (a)
           (foldr append nil
             (map (lambda (b)
                    (map (lambda (c) (list a b c)) (seq 0 1)))
                  (seq 0 1))))
         (seq 0 1))))

(define t3-zero (list 0 0 0))
(define t3-one (list 1 0 0))
(define t3-unif (list 0 1 0))          ; the uniformizer t
(define (t3-f u) (t3-add (t3-add (t3-mul u u) u) t3-unif))
(define t3-roots (filter (lambda (u) (equal (t3-f u) t3-zero)) t3-elems))

; Hensel: f has two roots mod t (namely 0 and 1), f' = 1 is a unit, so each
; lifts UNIQUELY: exactly two roots in F_2[t]/(t^3), one over each residue root.
(check "hensel-z2t-t3"
  (and (= (length t3-elems) 8)
       (= (length t3-roots) 2)
       (equal (sort < (map (lambda (u) (nth 0 u)) t3-roots)) (list 0 1))))

; ============================================================
; 8. VALUATION ON FUNCTION FIELDS
; ============================================================
(print "--- Valuation on F_q((t)) ---")

; The ring of formal power series F_q[[t]] is a DVR.
; Valuation: v(Σ a_i t^i) = min{i : a_i ≠ 0}.
; Uniformizer: t.
; Residue field: F_q.

; For the finite quotient F_q[t]/(t^n), we get a finite chain ring
; with valuation v(t^k) = k for k < n.

; Real computation on the model of F_2[t]/(t^3) built above:
; v(f) = min{i : c_i /= 0}, with v(0) = 3 (the "infinity" of this truncation).
(define (v-t3 u)
  (cond ((not (= (nth 0 u) 0)) 0)
        ((not (= (nth 1 u) 0)) 1)
        ((not (= (nth 2 u) 0)) 2)
        (t 3)))

(define t3-pairs
  (foldr append nil
    (map (lambda (u) (map (lambda (w) (list u w)) t3-elems)) t3-elems)))

; v(t^k) = k for k < 3, and v is additive: v(fg) = v(f)+v(g) whenever that is
; still < 3 (otherwise the product is 0 and v = 3).  Checked over all 64 pairs.
(check "valuation-function-field"
  (and (= (v-t3 t3-one) 0)
       (= (v-t3 t3-unif) 1)
       (= (v-t3 (t3-mul t3-unif t3-unif)) 2)
       (= (v-t3 t3-zero) 3)
       (every (lambda (pr)
                (let ((vf (v-t3 (nth 0 pr)))
                      (vg (v-t3 (nth 1 pr)))
                      (vp (v-t3 (t3-mul (nth 0 pr) (nth 1 pr)))))
                  (cond ((< (+ vf vg) 3) (= vp (+ vf vg)))
                        (t (= vp 3)))))
              t3-pairs)))

(print "=== Valuation Theory Done ===")
