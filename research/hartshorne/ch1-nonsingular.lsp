; ============================================================
; Hartshorne Ch.1 S5: Nonsingular Varieties
; Tangent space, regular local rings, smoothness.
; Verified on finite varieties over F_p.
; ============================================================

(print "=== Hartshorne S5: Nonsingular Varieties ===")

(define (fp-add p a b) (mod (+ a b) p))
(define (fp-mul p a b) (mod (* a b) p))
(define (fp-sub p a b) (mod (- a b) p))

(define (upto lo hi) (cond ((> lo hi) nil) (t (cons lo (upto (+ lo 1) hi)))))
(define (fp-elts p) (upto 0 (- p 1)))

; A plane polynomial is a list of monomials (coef xdeg ydeg).
; Partial derivatives are computed FORMALLY from that representation,
; not transcribed by hand, so the Jacobian criterion below is a real
; computation rather than a restatement of a comment.

(define (poly-eval pol p x y)
  (mod (foldr + 0
              (map (lambda (trm) (* (car trm) (^ x (cadr trm)) (^ y (caddr trm))))
                   pol))
       p))

(define (poly-dx pol)
  (map (lambda (trm) (list (* (car trm) (cadr trm)) (- (cadr trm) 1) (caddr trm)))
       (filter (lambda (trm) (> (cadr trm) 0)) pol)))

(define (poly-dy pol)
  (map (lambda (trm) (list (* (car trm) (caddr trm)) (cadr trm) (- (caddr trm) 1)))
       (filter (lambda (trm) (> (caddr trm) 0)) pol)))

; All F_p-points of the affine plane, and the points of the curve f = 0.
(define (plane-points p)
  (foldr append nil
         (map (lambda (a) (map (lambda (b) (list a b)) (fp-elts p))) (fp-elts p))))

(define (curve-points pol p)
  (filter (lambda (pt) (= 0 (poly-eval pol p (car pt) (cadr pt)))) (plane-points p)))

; Jacobian criterion: the rank of the 1 x 2 Jacobian (f_x, f_y) at a point.
(define (jac-rank pol p pt)
  (cond ((and (= 0 (poly-eval (poly-dx pol) p (car pt) (cadr pt)))
              (= 0 (poly-eval (poly-dy pol) p (car pt) (cadr pt)))) 0)
        (t 1)))

; dim T_P = 2 - rank J. A plane curve has dimension 1, so P is nonsingular
; exactly when dim T_P = 1.
(define (tangent-dim pol p pt) (- 2 (jac-rank pol p pt)))

(define (singular-points pol p)
  (filter (lambda (pt) (= 0 (jac-rank pol p pt))) (curve-points pol p)))

(define (smoothp pol p) (eq nil (singular-points pol p)))

; Independent point counts: for each x, count the y with y^2 = c.
(define (sqrt-count p c)
  (count (lambda (b) (= (mod (* b b) p) (mod c p))) (fp-elts p)))

(print "--- Formal differentiation: Euler identity ---")
; For f homogeneous of degree d, x f_x + y f_y = d f. This tests the
; differentiator itself against an identity it was not built from.
(define conic-h (list (list 1 2 0) (list 1 0 2)))
(check "euler-identity-degree-2"
       (every (lambda (pt)
                (= (mod (+ (* (car pt) (poly-eval (poly-dx conic-h) 5 (car pt) (cadr pt)))
                           (* (cadr pt) (poly-eval (poly-dy conic-h) 5 (car pt) (cadr pt))))
                        5)
                   (mod (* 2 (poly-eval conic-h 5 (car pt) (cadr pt))) 5)))
              (plane-points 5)))

(print "--- F_5: Conic x^2 + y^2 = 1 ---")
(define p5 5)
(define conic5 (list (list 1 2 0) (list 1 0 2) (list -1 0 0)))

; Gradient: (2x, 2y). In char 5, 2 != 0. Singular only if 2x=0 and 2y=0 => x=0,y=0.
; (0,0) is not on the curve: 0+0-1=-1 != 0.
(check "conic5-is-smooth" (smoothp conic5 p5))
; Every point of a smooth plane curve has a 1-dimensional tangent space.
(check "conic5-tangent-dim-1-everywhere"
       (every (lambda (pt) (= 1 (tangent-dim conic5 p5 pt))) (curve-points conic5 p5)))
; Closed form: #{x^2+y^2=1} over F_q is q-1 when q = 1 mod 4, q+1 when q = 3 mod 4.
(check "conic5-has-points"
       (= (length (curve-points conic5 p5))
          (cond ((= 1 (mod p5 4)) (- p5 1)) (t (+ p5 1)))))
(check "conic7-point-count-matches-closed-form"
       (= (length (curve-points conic5 7))
          (cond ((= 1 (mod 7 4)) (- 7 1)) (t (+ 7 1)))))

(print "--- F_3: Cusp y^2 = x^3 ---")
(define p3 3)
(define cusp3 (list (list 1 0 2) (list -1 3 0)))

; f = y^2 - x^3. d/dx = -3x^2 = 0 (char 3). d/dy = 2y.
; Singular where 2y=0 => y=0, and on the curve x^3=0 => x=0.
(check "cusp3-0-0-sing" (= 0 (jac-rank cusp3 p3 (list 0 0))))
(check "cusp3-one-singular-pt" (equal (singular-points cusp3 p3) (list (list 0 0))))
(check "cusp3-not-smooth" (not (smoothp cusp3 p3)))
; At the cusp the tangent space jumps to dimension 2 > 1 = dim of the curve.
(check "cusp3-tangent-dim-2-at-origin" (= 2 (tangent-dim cusp3 p3 (list 0 0))))
(check "cusp3-smooth-away-from-origin"
       (every (lambda (pt) (= 1 (tangent-dim cusp3 p3 pt)))
              (remove (lambda (pt) (equal pt (list 0 0))) (curve-points cusp3 p3))))

(print "--- F_2: Nodal cubic y^2 = x^3 + x^2 ---")
(define p2 2)
(define nodal2 (list (list 1 0 2) (list -1 3 0) (list -1 2 0)))

; d/dx = -3x^2 - 2x = x^2 = x (char 2); d/dy = 2y = 0.
; Singular where x = 0 and the point is on the curve: only (0,0).
(check "nodal2-has-singular-pt" (contains (list 0 0) (singular-points nodal2 p2)))
(check "nodal2-exactly-one-singular-pt" (= 1 (length (singular-points nodal2 p2))))

(print "--- F_5: Elliptic curve y^2 = x^3 + x + 1 ---")
(define ell5 (list (list 1 0 2) (list -1 3 0) (list -1 1 0) (list -1 0 0)))

; Independent count: sum over x of #{y : y^2 = x^3+x+1}.
(check "elliptic5-has-points"
       (= (length (curve-points ell5 p5))
          (foldr + 0 (map (lambda (x) (sqrt-count p5 (+ (^ x 3) x 1))) (fp-elts p5)))))
(check "elliptic5-is-smooth" (smoothp ell5 p5))
; Independent smoothness criterion: y^2 = x^3+ax+b is nonsingular iff
; the discriminant 4a^3 + 27b^2 is nonzero. Here a = b = 1.
(check "elliptic5-discriminant-nonzero"
       (eq (smoothp ell5 p5) (not (= 0 (mod (+ (* 4 (^ 1 3)) (* 27 (^ 1 2))) p5)))))

(print "--- Negative control: singular cubic y^2 = x^3 over F_5 ---")
(define cusp5 (list (list 1 0 2) (list -1 3 0)))
; a = b = 0, so the discriminant vanishes and the detector must reject it.
(check "cusp5-discriminant-zero-and-detector-agrees"
       (eq (smoothp cusp5 p5) (not (= 0 (mod (+ (* 4 (^ 0 3)) (* 27 (^ 0 2))) p5)))))
(check "cusp5-is-not-smooth" (not (smoothp cusp5 p5)))
(check "cusp5-singular-locus-is-origin" (equal (singular-points cusp5 p5) (list (list 0 0))))

(print "=== Hartshorne Nonsingular Varieties Done ===")
