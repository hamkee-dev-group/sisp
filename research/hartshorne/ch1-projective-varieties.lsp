; ============================================================
; Hartshorne Ch.1 S3: Projective Varieties
; P^n, Segre embedding, Veronese embedding. Rational maps.
; Verified on finite F_q projective spaces.
; ============================================================

(print "=== Hartshorne S3: Projective Varieties ===")

(define (fp-add p a b) (mod (+ a b) p))
(define (fp-mul p a b) (mod (* a b) p))

(define (int-power a k)
  (cond ((= k 0) 1)
        (t (* a (int-power a (- k 1))))))

(define (projective-count p n)
  (let ((numer (- (int-power p (+ n 1)) 1))
        (denom (- p 1)))
    (/ numer denom)))

(define (segre-p1p1 p xpt ypt)
  (let ((x0 (nth 0 xpt)) (x1 (nth 1 xpt))
        (y0 (nth 0 ypt)) (y1 (nth 1 ypt)))
    (list (fp-mul p x0 y0) (fp-mul p x0 y1)
          (fp-mul p x1 y0) (fp-mul p x1 y1))))

(define (segre-quadric p z)
  (let ((z0 (nth 0 z)) (z1 (nth 1 z))
        (z2 (nth 2 z)) (z3 (nth 3 z)))
    (= (fp-mul p z0 z3) (fp-mul p z1 z2))))

(define (veronese-p1d2 p pt)
  (let ((x (nth 0 pt)) (y (nth 1 pt)))
    (list (fp-mul p x x) (fp-mul p x y) (fp-mul p y y))))

(define (veronese-cond p z)
  (let ((z0 (nth 0 z)) (z1 (nth 1 z)) (z2 (nth 2 z)))
    (= (fp-mul p z1 z1) (fp-mul p z0 z2))))

(print "--- P^1(F_2) ---")
(define p2 2)
(check "P1-F2-3-points" (= (projective-count p2 1) 3))

(print "--- P^1(F_3) ---")
(define p3 3)
(check "P1-F3-4-points" (= (projective-count p3 1) 4))

(print "--- P^2(F_2) ---")
(check "P2-F2-7-points" (= (projective-count p2 2) 7))

; Canonical representatives of P^1(F_q): (1:y) for y in F_q, plus (0:1).
(define (upto lo hi) (cond ((> lo hi) nil) (t (cons lo (upto (+ lo 1) hi)))))
(define (p1-reps p) (cons (list 0 1) (map (lambda (y) (list 1 y)) (upto 0 (- p 1)))))
(check "p1-reps-count-matches-formula"
  (every (lambda (q) (= (length (p1-reps q)) (projective-count q 1))) (list 2 3 5)))

(print "--- Segre Embedding ---")
; The image of P^1 x P^1 lies on the quadric z0 z3 = z1 z2 -- checked at
; EVERY point of the product, not at three hand-picked ones.
(check "segre-quadric-holds"
  (every (lambda (xpt)
           (every (lambda (ypt) (segre-quadric p3 (segre-p1p1 p3 xpt ypt))) (p1-reps p3)))
         (p1-reps p3)))
; Negative control: the quadric is a genuine condition, so some point of
; P^3 must violate it.
(check "segre-quadric-rejects-off-quadric-point"
  (not (segre-quadric p3 (list 1 1 1 2))))

(print "--- Veronese Embedding ---")
(check "veronese-cond-holds"
  (every (lambda (pt) (veronese-cond p3 (veronese-p1d2 p3 pt))) (p1-reps p3)))
(check "veronese-cond-rejects-off-conic-point"
  (not (veronese-cond p3 (list 1 1 2))))
; The Veronese is injective on P^1, so the image has as many points as P^1.
(check "veronese-is-injective-on-P1"
  (= (length (dedup (map (lambda (pt) (veronese-p1d2 p3 pt)) (p1-reps p3))))
     (projective-count p3 1)))

(print "=== Hartshorne Projective Varieties Done ===")
