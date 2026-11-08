; ============================================================
; SGA 1 — Étale Morphisms of Finite Rings
; Étale = finitely presented + flat + unramified.
; Over finite fields: étale <=> separable polynomial.
; Ω_{S/R} = S·dx / (f'(x)·dx) vanishes iff f'(x) unit in S.
; ============================================================

(print "=== Étale Morphisms ===")

; ------------------------------------------------------------
; Polynomial machinery over Z/mZ.
; A polynomial is a list of coefficients LOW-to-HIGH.
; f is monic of degree dd, so S = (Z/mZ)[x]/(f) is free of rank dd
; with basis 1,x,...,x^(dd-1); an element of S is a coeff list of
; length exactly dd.
; ------------------------------------------------------------

(define (zn a m) (mod (+ (mod a m) m) m))
(define (zeros k) (cond ((<= k 0) nil) (t (cons 0 (zeros (- k 1))))))
(define (padto p dd) (append p (zeros (- dd (length p)))))
(define (vsub a b m) (map (lambda (pr) (zn (- (car pr) (cadr pr)) m)) (zip a b)))
(define (scale-poly c p m) (map (lambda (co) (zn (* c co) m)) p))

(define (coefat p i)
  (cond ((< i 0) 0) ((>= i (length p)) 0) (t (nth i p))))

; schoolbook product, length (len a + len b - 1)
(define (poly-mul a b m)
  (map (lambda (k)
         (zn (foldl + 0 (map (lambda (i) (* (coefat a i) (coefat b (- k i))))
                             (seq 0 k)))
             m))
       (seq 0 (- (+ (length a) (length b)) 2))))

; reduce modulo monic f (leading coeff 1), repeatedly killing the top term
(define (poly-reduce p f m)
  (cond ((< (length p) (length f)) p)
        (t (let ((cc (last p)) (shift (- (length p) (length f))))
             (poly-reduce
               (butlast (vsub p (append (zeros shift) (scale-poly cc f m)) m))
               f m)))))

; formal derivative: coefficient j of f' is (j+1)*c_{j+1}
(define (deriv-h l i m)
  (cond ((eq l nil) nil) (t (cons (zn (* i (car l)) m) (deriv-h (cdr l) (+ i 1) m)))))
(define (poly-deriv p m) (cond ((eq (cdr p) nil) (list 0)) (t (deriv-h (cdr p) 1 m))))

; ring operations inside S
(define (s-deg f) (- (length f) 1))
(define (s-one f) (padto (list 1) (s-deg f)))
(define (s-mul a b f m) (padto (poly-reduce (poly-mul a b m) f m) (s-deg f)))
(define (s-of p f m) (padto (poly-reduce p f m) (s-deg f)))

; every element of S: all coefficient vectors of length dd over Z/mZ
(define (s-elements dd m)
  (cond ((= dd 0) (list nil))
        (t (foldr append nil
                  (map (lambda (v) (map (lambda (c) (cons c v)) (seq 0 (- m 1))))
                       (s-elements (- dd 1) m))))))

; a in S is a unit iff some u in S has a*u = 1
(define (s-unit-p a f m)
  (any (lambda (u) (equal (s-mul a u f m) (s-one f))) (s-elements (s-deg f) m)))

; THE étale criterion: R -> R[x]/(f) is étale iff Ω = S·dx/(f'(x)dx) = 0
; iff f'(x) is a unit in S.
(define (etale-p f m) (s-unit-p (s-of (poly-deriv f m) f m) f m))

; roots of f in Z/mZ, for the separability commentary
(define (poly-eval p a m)
  (zn (foldl + 0 (map (lambda (i) (* (coefat p i) (^ a i))) (seq 0 (- (length p) 1)))) m))
(define (roots-of f m) (filter (lambda (a) (= (poly-eval f a m) 0)) (seq 0 (- m 1))))

; ============================================================
; 1. R → R[x]/(x²-x) over Z/2Z IS étale.
; ============================================================
; S = Z/2Z[x]/(x²-x) = Z/2Z[x]/(x(x+1)) ≅ Z/2Z × Z/2Z (product of fields).
; f'(x) = 2x-1 ≡ 1 mod 2, which is a unit. So Ω=0.
; Also x²-x = x(x-1) = x(x+1) has distinct roots {0,1} => separable => étale.
(print "--- 1. R→R[x]/(x²-x) over Z/2Z ---")
(define f1 (list 0 1 1))          ; x² - x  ≡  x² + x  mod 2
(print "f'(x) mod 2 =")
(print (poly-deriv f1 2))
(check "x^2-x_over_Z2_is_etale" (etale-p f1 2))
(check "x^2-x_over_Z2_has_roots_0_1" (equal (roots-of f1 2) (list 0 1)))

; ============================================================
; 2. R → R[x]/(x²) over Z/2Z is NOT étale.
; ============================================================
; f'(x) = 2x ≡ 0 mod 2. Nonzero differential dx killed by 0, so Ω ≠ 0.
; Also x is nilpotent (x²=0), so ramified.
(print "--- 2. R→R[x]/(x²) over Z/2Z ---")
(define f2 (list 0 0 1))          ; x²
(check "x^2_over_Z2_is_NOT_etale" (not (etale-p f2 2)))
; x is nilpotent in S: x·x = 0
(check "x-is-nilpotent-in-Z2[x]/(x^2)"
       (equal (s-mul (list 0 1) (list 0 1) f2 2) (list 0 0)))

; ============================================================
; 3. Ω for Z/4Z → (Z/4Z)[x]/(x²-1) — check if étale.
; ============================================================
; f(x) = x²-1, f'(x) = 2x. In S = (Z/4Z)[x]/(x²-1): (2x)² = 4x² = 4·1 = 0 mod 4.
; So 2x is nilpotent => not a unit => Ω ≠ 0 => NOT étale.
(print "--- 3. Z/4Z[x]/(x²-1) ---")
(define f3 (list 3 0 1))          ; x² - 1 over Z/4Z
(check "x^2-1_over_Z4_is_NOT_etale" (not (etale-p f3 4)))
; the reason: (2x)·(2x) = 4·x² = 0, so f'(x) is nilpotent
(check "f-prime-is-nilpotent-over-Z4"
       (equal (s-mul (list 0 2) (list 0 2) f3 4) (list 0 0)))

; ============================================================
; 4. Finite étale covers of Spec(F_p) = separable algebras.
; ============================================================
; Over perfect fields like F_p: étale = smooth of relative dimension 0
; = reduced finite algebra = product of separable field extensions.
;
; Example: F_7[x]/(x²+1). f = x²+1, f' = 2x, and they share no root,
; so f is separable and F_7[x]/(x²+1) = F_49 is étale over F_7.
(print "--- 4. Finite étale covers of Spec(F_p) ---")
(define f4 (list 1 0 1))          ; x² + 1 over F_7
(check "x^2+1_over_F7_is_etale" (etale-p f4 7))
; x²+1 is irreducible over F_7 (no roots), so S is the field F_49
(check "x^2+1_has_no_roots_in_F7" (eq (roots-of f4 7) nil))

; ============================================================
; 5. Counterexample: F_5[x]/(x²-2x+1) NOT étale.
; ============================================================
; x²-2x+1 = (x-1)² has a repeated root, so inseparable => ramified.
; f'(x) = 2x-2 = 2(x-1), which shares the root x=1 with f.
(print "--- 5. F_5[x]/(x²-2x+1) NOT étale ---")
(define f5 (list 1 3 1))          ; x² - 2x + 1 over F_5
(check "x^2-2x+1_over_F5_is_NOT_etale" (not (etale-p f5 5)))
; only ONE distinct root, though the degree is 2 — the repetition is the defect
(check "x^2-2x+1_has_one_distinct_root" (equal (roots-of f5 5) (list 1)))
(check "f-and-f-prime-share-root-1"
       (and (= (poly-eval f5 1 5) 0) (= (poly-eval (poly-deriv f5 5) 1 5) 0)))

; ============================================================
; 6. F_5[x]/(x³-x) is étale.
; ============================================================
; x³-x = x(x-1)(x+1) has 3 distinct roots in F_5: {0,1,4}.
; f'(x) = 3x²-1 vanishes at none of them, so gcd(f,f') = 1 => separable.
(print "--- 6. F_5[x]/(x³-x) is étale ---")
(define f6 (list 0 4 0 1))        ; x³ - x over F_5
(check "x^3-x_over_F5_is_etale" (etale-p f6 5))
(check "x^3-x_has_three_distinct_roots_in_F5"
       (equal (roots-of f6 5) (list 0 1 4)))
(check "f-prime-nonzero-at-every-root"
       (every (lambda (r) (/= (poly-eval (poly-deriv f6 5) r 5) 0)) (roots-of f6 5)))

; ------------------------------------------------------------
; The criterion is genuinely discriminating: it says YES exactly for the
; separable cases and NO for the ramified ones.
; ------------------------------------------------------------
(check "etale-verdicts-are-not-all-equal"
       (equal (map (lambda (pr) (etale-p (car pr) (cadr pr)))
                   (list (list f1 2) (list f2 2) (list f3 4)
                         (list f4 7) (list f5 5) (list f6 5)))
              (list t nil nil t nil t)))

(print "=== Étale Morphisms Done ===")
