; ============================================================
; Hartshorne Ch.1 S6: Nonsingular Curves
; Discrete valuation rings. Divisors. Riemann-Roch for curves.
; Verified on small curves over F_p.
; ============================================================

(print "=== Hartshorne S6: Nonsingular Curves ===")

(define (fp-add p a b) (mod (+ a b) p))
(define (fp-mul p a b) (mod (* a b) p))
(define (fp-sub p a b) (mod (- a b) p))

(define (upto lo hi) (cond ((> lo hi) nil) (t (cons lo (upto (+ lo 1) hi)))))
(define (fp-elts p) (upto 0 (- p 1)))
(define (pairs-over p)
  (foldr append nil
         (map (lambda (a) (map (lambda (b) (list a b)) (fp-elts p))) (fp-elts p))))

(print "--- P^1(F_5) ---")
(define p5 5)

; Enumerate P^1(F_q) honestly: nonzero vectors modulo scaling, by
; normalising each to a canonical representative. Compared against the
; closed form q+1, which is derived independently of the enumeration.
(define (inv-mod p a) (car (filter (lambda (b) (= 1 (mod (* a b) p))) (upto 1 (- p 1)))))

(define (normalize-vec p v)
  (cond ((= 0 (car v)) (list 0 1))
        (t (list 1 (mod (* (cadr v) (inv-mod p (car v))) p)))))

(define (p1-points p)
  (dedup (map (lambda (v) (normalize-vec p v))
              (remove (lambda (v) (and (= 0 (car v)) (= 0 (cadr v)))) (pairs-over p)))))

(check "P1-F5-has-6-points" (= (length (p1-points p5)) (+ p5 1)))
(check "P1-count-matches-closed-form-over-2-3-7"
  (every (lambda (q) (= (length (p1-points q)) (+ q 1))) (list 2 3 7)))
; The normalisation must be constant on scaling orbits: normalise(c v) =
; normalise(v) for every nonzero scalar c. Without this the point count
; above could come out right for the wrong reason.
(check "P1-normalisation-is-scaling-invariant"
  (every (lambda (v)
           (every (lambda (c)
                    (equal (normalize-vec p5 (list (mod (* c (car v)) p5) (mod (* c (cadr v)) p5)))
                           (normalize-vec p5 v)))
                  (upto 1 (- p5 1))))
         (remove (lambda (v) (and (= 0 (car v)) (= 0 (cadr v)))) (pairs-over p5))))
; Negative control: the normalisation really does collapse scalar multiples,
; so the raw vector count is strictly larger than the projective count.
(check "P1-F5-vectors-outnumber-points"
  (> (length (remove (lambda (v) (and (= 0 (car v)) (= 0 (cadr v)))) (pairs-over p5)))
     (length (p1-points p5))))

(print "--- Divisors on P^1(F_5) ---")
(define (divisor-degree D)
  (foldl (lambda (acc pair) (+ acc (cdr pair))) 0 D))

(define div1 (list (cons (list 1 0) 1)))
(check "div-1-degree-1" (= (divisor-degree div1) 1))
(check "div-2-degree-2" (= (divisor-degree (list (cons (list 1 0) 2))) 2))
(check "div-minus1-degree-minus1"
  (= (divisor-degree (list (cons (list 1 0) -1))) -1))
(check "divisor-degree-is-additive"
  (let ((da (list (cons (list 1 0) 3) (cons (list 0 1) -2)))
        (db (list (cons (list 1 1) 4))))
    (= (divisor-degree (append da db))
       (+ (divisor-degree da) (divisor-degree db)))))

(print "--- Riemann-Roch for P^1 (genus 0) ---")
; l(D) for a divisor of degree d on P^1.
(define (l-p1 d) (cond ((>= d 0) (+ d 1)) (t 0)))
(define (riemann-roch-p1 D) (l-p1 (divisor-degree D)))

(check "RR-P1-deg1-lD-2" (= (riemann-roch-p1 div1) 2))
(check "RR-P1-deg2-lD-3"
  (= (riemann-roch-p1 (list (cons (list 1 0) 2))) 3))
(check "RR-P1-deg-minus1-lD-0"
  (= (riemann-roch-p1 (list (cons (list 1 0) -1))) 0))

; The theorem itself: l(D) - l(K-D) = deg D + 1 - g, with g = 0 and
; deg K = 2g-2 = -2. Checked over a range of degrees, both signs.
(check "riemann-roch-identity-genus-0"
  (every (lambda (d) (= (- (l-p1 d) (l-p1 (- -2 d))) (+ d 1)))
         (upto -6 6)))

(print "--- Genus of Smooth Plane Curves ---")
(define (plane-curve-genus d)
  (/ (* (- d 1) (- d 2)) 2))

(check "genus-d1-is-0" (= (plane-curve-genus 1) 0))
(check "genus-d2-is-0" (= (plane-curve-genus 2) 0))
(check "genus-d3-is-1" (= (plane-curve-genus 3) 1))
(check "genus-d4-is-3" (= (plane-curve-genus 4) 3))

; Independent derivation: g equals dim of the degree-(d-3) polynomials in
; two variables, i.e. the number of monomials x^i y^j with i+j <= d-3.
; Counted by enumeration, not by a closed form.
(define (monomials-of-degree-le k)
  (cond ((< k 0) nil)
        (t (filter (lambda (pr) (<= (+ (car pr) (cadr pr)) k))
                   (foldr append nil
                          (map (lambda (i) (map (lambda (j) (list i j)) (upto 0 k)))
                               (upto 0 k)))))))

(check "genus-equals-adjoint-monomial-count"
  (every (lambda (d) (= (plane-curve-genus d) (length (monomials-of-degree-le (- d 3)))))
         (upto 1 9)))

(print "--- Elliptic Curve over F_5: y^2 = x^3 + x + 1 ---")
; Counted by enumeration: affine solutions plus the single point at infinity.
(define (ell-rhs p x) (mod (+ (^ x 3) x 1) p))
(define (ell-affine-points p)
  (filter (lambda (pt) (= (mod (* (cadr pt) (cadr pt)) p) (ell-rhs p (car pt))))
          (pairs-over p)))
(define (ell-total p) (+ (length (ell-affine-points p)) 1))

; Cross-check the 2-D enumeration against a 1-D count: for each x, the
; number of square roots of the right-hand side.
(check "elliptic-F5-affine-count-two-ways"
  (= (length (ell-affine-points p5))
     (foldr + 0 (map (lambda (x)
                       (count (lambda (y) (= (mod (* y y) p5) (ell-rhs p5 x))) (fp-elts p5)))
                     (fp-elts p5)))))
(check "elliptic-F5-9-total" (= (ell-total p5) 9))
; Hasse-Weil: |N - (q+1)| <= 2 sqrt(q), stated squared to stay exact.
(check "hasse-weil-holds"
  (let ((dev (- (ell-total p5) (+ p5 1))))
    (<= (* dev dev) (* 4 p5))))
(check "hasse-weil-holds-over-2-3-7"
  (every (lambda (q) (let ((dev (- (ell-total q) (+ q 1)))) (<= (* dev dev) (* 4 q))))
         (list 2 3 7)))

(print "--- Riemann-Roch for Elliptic Curves (genus 1) ---")
; On a genus-1 curve K = 0, so deg K = 0 and l(0) = 1.
(define (l-elliptic d) (cond ((> d 0) d) ((= d 0) 1) (t 0)))
(define (rr-elliptic d) (l-elliptic d))

(check "RR-elliptic-deg3-lD-3" (= (rr-elliptic 3) 3))
(check "RR-elliptic-deg1-lD-1" (= (rr-elliptic 1) 1))
(check "RR-elliptic-deg0-lD-1" (= (rr-elliptic 0) 1))
(check "riemann-roch-identity-genus-1"
  (every (lambda (d) (= (- (l-elliptic d) (l-elliptic (- 0 d))) d))
         (upto -6 6)))

(print "=== Hartshorne Nonsingular Curves Done ===")
