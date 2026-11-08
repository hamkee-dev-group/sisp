; ============================================================
; Hartshorne Ch.1 S4: Morphisms of Varieties
; Morphisms, finite morphisms, dimension of fibers.
; Constructible sets. Verified on finite variety examples.
; ============================================================

(print "=== Hartshorne S4: Morphisms ===")

(define (fp-add p a b) (mod (+ a b) p))
(define (fp-mul p a b) (mod (* a b) p))
(define (fp-sub p a b) (mod (- a b) p))

(define (affine-line p) (list-to-set (seq 0 (- p 1))))
(define (affine-plane p) (cartesian-product (affine-line p) (affine-line p)))

(define (regular-map-1d p coeffs x)
  (labels (((go-eval cs acc pow)
    (cond ((eq cs nil) (mod acc p))
          (t (go-eval (cdr cs) (mod (+ acc (* (car cs) pow)) p) (mod (* pow x) p))))))
    (go-eval coeffs 0 1)))

(define (morphism-p domain codomain f)
  (every (lambda (x) (in (f x) codomain)) domain))

(define (fiber domain f w)
  (filter (lambda (x) (equal (f x) w)) domain))

(print "--- A (F_5) Morphisms ---")
(define p5 5)
(define A1-5 (affine-line p5))

(define (sq-map p x) (mod (* x x) p))
(check "sq-map-is-morphism-A1-to-A1"
  (morphism-p A1-5 A1-5 (lambda (x) (sq-map p5 x))))

(define fiber0 (fiber A1-5 (lambda (x) (sq-map p5 x)) 0))
(define fiber1 (fiber A1-5 (lambda (x) (sq-map p5 x)) 1))
(define fiber4 (fiber A1-5 (lambda (x) (sq-map p5 x)) 4))

(check "fiber-over-0-size-1" (= (length fiber0) 1))
(check "fiber-over-1-size-2" (= (length fiber1) 2))
(check "fiber-over-4-size-2" (= (length fiber4) 2))

(print "--- A^2(F_3) Morphisms ---")
(define p3 3)
(define A2-3 (affine-plane p3))
(define A1-3 (affine-line p3))

(define (proj-sum p pt)
  (fp-add p (nth 0 pt) (nth 1 pt)))

(check "proj-sum-is-morphism"
  (morphism-p A2-3 A1-3 (lambda (pt) (proj-sum p3 pt))))

(define fiber0-sum (fiber A2-3 (lambda (pt) (proj-sum p3 pt)) 0))
(define fiber1-sum (fiber A2-3 (lambda (pt) (proj-sum p3 pt)) 1))
(check "fiber-sum-over-0-size-3" (= (length fiber0-sum) 3))
(check "fiber-sum-over-1-size-3" (= (length fiber1-sum) 3))

(define (proj-tuple p pt)
  (let ((x (nth 0 pt)) (y (nth 1 pt)))
    (list x (fp-mul p x y))))

(check "proj-tuple-is-morphism"
  (morphism-p A2-3 A2-3 (lambda (pt) (proj-tuple p3 pt))))

; Negative control: morphism-p must REJECT a map whose values leave the
; stated codomain (here the reduction mod 3 is deliberately dropped).
(check "morphism-p-rejects-map-out-of-codomain"
  (not (morphism-p A2-3 A1-3 (lambda (pt) (+ 3 (nth 0 pt) (nth 1 pt))))))

(define fiber00 (fiber A2-3 (lambda (pt) (proj-tuple p3 pt)) (list 0 0)))
(check "fiber-over-00-size-3" (= (length fiber00) 3))

(print "--- Finite Morphisms ---")
(check "sq-map-finite-fibers"
  (every (lambda (y)
    (<= (length (fiber A1-5 (lambda (x) (sq-map p5 x)) y)) 2))
    A1-5))

(print "--- Dimension of Fibers ---")
; Fiber dimension is 0 everywhere for a finite morphism, and the fiber
; cardinalities must sum to the size of the source (fibers partition it).
(check "fibers-of-sq-map-partition-A1"
  (= (length A1-5)
     (foldr + 0 (map (lambda (y) (length (fiber A1-5 (lambda (x) (sq-map p5 x)) y)))
                     (set-to-list A1-5)))))
(check "fibers-of-proj-sum-partition-A2"
  (= (length A2-3)
     (foldr + 0 (map (lambda (w) (length (fiber A2-3 (lambda (pt) (proj-sum p3 pt)) w)))
                     (set-to-list A1-3)))))

(print "--- Constructible Sets ---")
; The closed set V(x^2-1) and its complement, both computed rather than
; written down as literals.
(define closed-set-f5
  (filter (lambda (x) (= 0 (mod (- (* x x) 1) p5))) A1-5))
(define (set-minus a b) (filter (lambda (x) (not (in x b))) a))
(define (set-meet a b) (filter (lambda (x) (in x b)) a))
(define open-set-f5 (set-minus A1-5 closed-set-f5))
(check "closed-set-is-V-x2-minus-1" (equal closed-set-f5 {1 4}))
(check "union-is-whole-space" (equal (union closed-set-f5 open-set-f5) A1-5))
(check "closed-and-open-are-disjoint"
  (equal (set-meet closed-set-f5 open-set-f5) {}))

; Chevalley: the image of the squaring map is constructible. Here it is the
; set of squares, which is a proper subset of A^1 -- so this can fail.
(define sq-image (map (lambda (x) (sq-map p5 x)) A1-5))
(check "chevally-sq-image-is-the-squares" (equal sq-image {0 1 4}))
(check "chevally-sq-image-is-proper-subset"
  (and (equal (set-minus sq-image A1-5) {}) (not (equal sq-image A1-5))))
; The image is exactly the set of y whose fiber is nonempty.
(check "sq-image-equals-nonempty-fiber-locus"
  (equal sq-image
         (filter (lambda (y) (not (equal (fiber A1-5 (lambda (x) (sq-map p5 x)) y) {})))
                     A1-5)))

(print "=== Hartshorne Morphisms Done ===")
