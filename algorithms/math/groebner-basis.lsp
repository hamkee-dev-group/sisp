; ============================================================
; name: groebner-basis
; file: algorithms/math/groebner-basis.lsp
; subject: algebra
; capability: solve reduce decide
; aliases: "buchberger" "groebner" "ideal membership" "polynomial ideal"
; signature: (buchberger generators) -> groebner-basis
;            (reduce-poly p basis) -> normal-form
;            (in-ideal-p p generators) -> t/nil
; complexity: doubly-exponential worst case (Buchberger)
; see-also: (none)
; derived-from: Buchberger 1965; Cox, Little & O'Shea, "Ideals,
;               Varieties, and Algorithms", ch. 2
; status: example (tested)
; checks: 13 — Buchberger over F_5[x,y] for (x^2+y, xy) and the monomial
;         ideal (x^2, y^2): S-polynomial reduction, ideal-membership
;         decisions, and normal-form checks;
;         run: src/sisp <file>
; ------------------------------------------------------------
; Groebner basis via Buchberger's algorithm over a finite field.
; S-polynomials, monomial orders, polynomial reduction, modular inverse,
; ideal-membership test.  Worked example: (x^2+y, xy) in F_5[x,y].
; Term lists are merged with the lex comparator (poly-add); the grlex
; comparator is defined for reference but not used.
; ============================================================

(print "=== Bourbaki: Groebner Basis ===")

; ---------- field operations for F_5 ----------
(define p5 5)
(define (rf a) (mod a p5))
(define (rf-add a b) (mod (+ a b) p5))
(define (rf-mul a b) (mod (* a b) p5))
(define (rf-neg a) (mod (- 0 a) p5))

; ---------- monomial in x,y: (deg_x, deg_y) represented as list (dx dy) ----------
; Graded lex (grlex): compare total degree first, then x-degree.
; Monomial comparison: (dx1, dy1) < (dx2, dy2) in grlex order
(define (monomial-less-grlex m1 m2)
  (let ((t1 (+ (car m1) (cadr m1)))
        (t2 (+ (car m2) (cadr m2))))
    (cond ((< t1 t2) t)
          ((> t1 t2) nil)
          ((< (car m1) (car m2)) t)
          (t nil))))

; Lexicographic order: x > y, so compare x-deg first
(define (monomial-less-lex m1 m2)
  (cond ((< (car m1) (car m2)) t)
        ((> (car m1) (car m2)) nil)
        ((< (cadr m1) (cadr m2)) t)
        (t nil)))

; ---------- polynomial as list of (coeff . monomial) pairs, sorted by monomial order ----------
; Coeff = 0,...,p-1 in F_p. Leading term is first.

(define (poly-zero) nil)

(define (poly-zero-p p)
  (eq p nil))

(define (lt-monomial p)
  (if (eq p nil) (list 0 0) (cdar p)))

(define (lt-coeff p)
  (if (eq p nil) 0 (caar p)))

(define (lt p)
  (if (eq p nil) nil (car p)))

; ---------- scalar multiplication ----------
(define (poly-scale p c)
  (cond ((eq p nil) nil)
        ((= c 0) nil)
        (t (cons (cons (rf-mul c (caar p)) (cdar p))
                 (poly-scale (cdr p) c)))))

; ---------- monomial multiplication ----------
(define (monomial-mul m1 m2)
  (list (+ (car m1) (car m2)) (+ (cadr m1) (cadr m2))))

(define (poly-mul-monomial p m)
  (cond ((eq p nil) nil)
        (t (cons (cons (caar p) (monomial-mul (cdar p) m))
                 (poly-mul-monomial (cdr p) m)))))

; ---------- polynomial addition (merge sorted) ----------
(define (poly-add p q)
  (cond ((eq p nil) q)
        ((eq q nil) p)
        ((equal (cdar p) (cdar q))
         (let ((c (rf-add (caar p) (caar q))))
           (if (= c 0)
               (poly-add (cdr p) (cdr q))
               (cons (cons c (cdar p)) (poly-add (cdr p) (cdr q))))))
        ((monomial-less-lex (cdar p) (cdar q))
         (cons (car p) (poly-add (cdr p) q)))
        (t (cons (car q) (poly-add p (cdr q))))))

(define (poly-sub p q)
  (poly-add p (poly-scale q (rf-neg 1))))

; ---------- polynomial multiplication ----------
(define (poly-mul p q)
  (cond ((eq p nil) nil)
        (t (poly-add (poly-mul-monomial q (cdar p))
                     (poly-mul (cdr p) q)))))

; ---------- S-polynomial ----------
; S(f,g) = (lcm(LT(f),LT(g)) / LT(f)) * f - (lcm(LT(f),LT(g)) / LT(g)) * g
(define (monomial-lcm m1 m2)
  (list (max (car m1) (car m2)) (max (cadr m1) (cadr m2))))

(define (monomial-quotient m1 m2)
  (list (- (car m1) (car m2)) (- (cadr m1) (cadr m2))))

(define (s-polynomial p q)
  (cond ((eq p nil) nil)
        ((eq q nil) nil)
        (t (let* ((m1 (lt-monomial p))
                  (m2 (lt-monomial q))
                  (lcm (monomial-lcm m1 m2))
                  (q1 (monomial-quotient lcm m1))
                  (q2 (monomial-quotient lcm m2))
                  (c1 (lt-coeff p))
                  (c2 (lt-coeff q)))
             (poly-sub (poly-mul-monomial p q1)
                       (poly-mul-monomial q q2))))))

; ---------- polynomial reduction ----------
; Reduce p modulo set G: repeatedly replace leading term using elements of G
(define (poly-reduce p G)
  (poly-reduce-step p G))

(define (poly-reduce-step p G)
  (cond ((eq p nil) nil)
        ((eq G nil) p)
        (t (let ((ltp (lt-monomial p))
                 (ltc (lt-coeff p)))
             (labels (((try-divide polylist)
               (cond ((eq polylist nil) nil)
                     ((poly-reducible-by (car polylist) ltp) (car polylist))
                     (t (try-divide (cdr polylist))))))
               (let ((divisor (try-divide (set-to-list G))))
                 (if (eq divisor nil)
                     (cons (car p) (poly-reduce-step (cdr p) G))
                     (let* ((ltg (lt-monomial divisor))
                            (ltcg (lt-coeff divisor))
                            (q (monomial-quotient ltp ltg))
                            (factor (rf-mul ltc (rf-neg 1)))
                            (inv-ltcg (modular-inverse ltcg p5))
                            (scaled (poly-scale divisor (rf-mul factor inv-ltcg)))
                            (reduced (poly-add p (poly-mul-monomial scaled q))))
                       (poly-reduce-step reduced G)))))))))

(define (poly-reducible-by g ltm)
  (and (not (eq g nil))
       (let ((glt (lt-monomial g)))
         (and (>= (car ltm) (car glt))
              (>= (cadr ltm) (cadr glt))))))

; ---------- modular inverse for F_5 ----------
(define (modular-inverse a p)
  (labels (((try x)
    (cond ((>= x p) 0)
          ((= (mod (* a x) p) 1) x)
          (t (try (+ x 1))))))
    (try 1)))

; ---------- Buchberger's algorithm ----------
(define (buchberger G)
  (labels (((pairs l)
    (cond ((eq l nil) nil)
          (t (append (map (lambda (g) (list (car l) g)) (cdr l))
                     (pairs (cdr l)))))))
    (let ((G-set (list-to-set (map (lambda (p) (poly-normalize p)) G))))
      (buchberger-loop G-set (pairs (set-to-list G-set))))))

(define (buchberger-loop G pairs)
  (cond ((eq pairs nil) G)
        (t (let* ((pair (car pairs))
                  (p (car pair))
                  (q (cadr pair))
                  (sp (s-polynomial p q))
                  (r (poly-reduce sp G)))
             (if (poly-zero-p r)
                 (buchberger-loop G (cdr pairs))
                 (buchberger-loop (cons r (set-to-list G))
                                  (append (map (lambda (g) (list r g))
                                               (set-to-list G))
                                          (cdr pairs))))))))

(define (poly-normalize p)
  (if (eq p nil) nil
      (let ((lc (lt-coeff p)))
        (if (= lc 0) nil
            (poly-scale p (modular-inverse lc p5))))))

; ---------- ideal membership: normalize f w.r.t. Groebner basis ----------
(define (ideal-member-p f G)
  (poly-zero-p (poly-reduce f G)))

; ---------- make polynomial from list of (coeff degx degy) ----------
(define (make-poly terms)
  (if (eq terms nil) nil
      (cons (cons (car (car terms)) (list (cadar terms) (caddar terms)))
            (make-poly (cdr terms)))))

; ========== F_5[x,y]: GROEBNER BASIS FOR (x²+y, xy) ==========
(print "--- Groebner Basis for (x²+y, xy) in F_5[x,y] ---")

; f1 = x² + y = (1,2,0) + (1,0,1)
(define f1 (make-poly (list (list 1 2 0) (list 1 0 1))))
; f2 = xy = (1,1,1)
(define f2 (make-poly (list (list 1 1 1))))

; Compute Groebner basis
(define G-xy (buchberger (list f1 f2)))
(print (list (quote groebner-basis) G-xy))

; The Groebner basis should be: {x²+y, xy, y²}
; Because: S(f1,f2) = y*f1 - x*f2 = y*(x²+y) - x*(xy) = x²y + y² - x²y = y²
; So y² is added.
(check "groebner-basis-y2"
  (find (lambda (p) (equal (lt-monomial p) (list 0 2))) G-xy))

; Now check S-polynomials reduce to 0
(check "sp-f1-f2-reduces-to-0"
  (poly-zero-p (poly-reduce (s-polynomial f1 f2) G-xy)))

; --- Test: y² is in ideal ---
(define y2 (make-poly (list (list 1 0 2))))
(check "y2-in-ideal" (ideal-member-p y2 G-xy))

; --- Test: x² is not in the ideal ---
(define x2 (make-poly (list (list 1 2 0))))
(check "x2-not-in-ideal" (not (ideal-member-p x2 G-xy)))

; x² ≡ -y (mod I) and -y is already reduced, so x² is not in the ideal;
; x² + y is.

; --- Test: x²+y is in ideal (it's a generator) ---
(check "x2-plus-y-in-ideal" (ideal-member-p f1 G-xy))

; --- Test: xy is in ideal (generator) ---
(check "xy-in-ideal" (ideal-member-p f2 G-xy))

; ========== MONOMIAL IDEAL IN F_3[x,y]: (x², y²) ==========
(print "--- Monomial ideal (x², y²) in F_3[x,y] ---")

(define pf3 3)
(define (rf3 a) (mod a pf3))

(define (make-poly-f3 terms)
  (if (eq terms nil) nil
      (cons (cons (rf3 (car (car terms))) (list (cadar terms) (caddar terms)))
            (make-poly-f3 (cdr terms)))))

(define f1-3 (make-poly-f3 (list (list 1 2 0))))
(define f2-3 (make-poly-f3 (list (list 1 0 2))))

; Groebner basis: {x², y²} already a Groebner basis since monomials.
(define G-mono (buchberger (list f1-3 f2-3)))
(print (list (quote G-mono) G-mono))

(check "groebner-mono-includes-x2"
  (find (lambda (p) (equal (lt-monomial p) (list 2 0))) G-mono))

(check "groebner-mono-includes-y2"
  (find (lambda (p) (equal (lt-monomial p) (list 0 2))) G-mono))

; --- Test membership ---
(define x3-mono (make-poly-f3 (list (list 1 3 0))))
(check "x3-in-mono-ideal" (ideal-member-p x3-mono G-mono))

(define y3-mono (make-poly-f3 (list (list 1 0 3))))
(check "y3-in-mono-ideal" (ideal-member-p y3-mono G-mono))

; xy is not divisible by x² or y², so it is NOT in the monomial ideal (x², y²).
; (Note: xy² = x*y² would BE in the ideal, since it is divisible by y².)
(define xy-mono (make-poly-f3 (list (list 1 1 1))))
(check "xy-not-in-mono-ideal" (not (ideal-member-p xy-mono G-mono)))

; ========== POLYNOMIAL REDUCTION ==========
(print "--- Polynomial Reduction ---")

; Reduce x³ + y² modulo (x², y²) in F_3
(define poly-test (make-poly-f3 (list (list 1 3 0) (list 2 0 2))))
(define reduced (poly-reduce poly-test G-mono))
; x³ reduces to 0 (divisible by x²), y² reduces to 0. Result = 0.
(check "reduce-x3-plus-y2-mono-zero" (poly-zero-p reduced))

; Reduce x² + xy + y² modulo (x², y²):
(define poly-test2 (make-poly-f3 (list (list 1 2 0) (list 1 1 1) (list 1 0 2))))
(define reduced2 (poly-reduce poly-test2 G-mono))
; x² → 0 (eliminated), y² → 0 (eliminated), xy remains.
(check "reduce-mono-xy-remains"
  (equal (lt-monomial reduced2) (list 1 1)))

(print "=== Groebner Basis Done ===")
