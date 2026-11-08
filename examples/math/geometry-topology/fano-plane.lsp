; ============================================================
; 63 — The Fano Plane PG(2,2)
; The projective plane over GF(2), constructed from the seven
; nonzero vectors of GF(2)^3. Lines arise from vector addition;
; every projective axiom, the duality, and the Steiner system
; property are verified by enumeration.
; ============================================================

(load "lib/collections/relations.lsp")

(print "=== The Fano Plane ===")


; ---------- points: nonzero vectors of GF(2)^3 ----------
; NB: cartesian-product does NOT flatten -- (cartesian-product a (cartesian-product b c))
; yields nested pairs (x (y z)), not 3-lists.  Build the triples directly.
(define bits (list 0 1))
(define gf2-cubed
  (list-to-set
    (foldr append nil
      (map (lambda (x)
             (foldr append nil
               (map (lambda (y)
                      (map (lambda (z) (list x y z)) bits))
                    bits)))
           bits))))
(check "gf2-cubed-has-eight-vectors" (= (length gf2-cubed) 8))

(define points
  (filter (lambda (v) (not (equal v (list 0 0 0)))) gf2-cubed))
(print (list (quote points) points))
(check "seven-points" (= (length points) 7))

; ---------- lines: {p, q, p+q} for each pair ----------
(define (vec-add u v)
  (list (mod (+ (nth 0 u) (nth 0 v)) 2)
        (mod (+ (nth 1 u) (nth 1 v)) 2)
        (mod (+ (nth 2 u) (nth 2 v)) 2)))

; (cartesian-product points points) pairs the vectors as (p q); the two
; components are car and cadr, NOT take/drop of a flattened 6-list.
(define vector-pairs
  (filter (lambda (pq) (not (equal (car pq) (cadr pq))))
    (cartesian-product points points)))
(check "vector-pairs-count-is-42" (= (length vector-pairs) 42))  ; 7*6 ordered pairs
(define lines
  (map (lambda (pq)
    (let ((p (car pq)) (q (cadr pq)))
      (list-to-set (list p q (vec-add p q)))))
    vector-pairs))
(check "seven-lines" (= (length lines) 7))
(check "every-line-has-three-points"
  (every (lambda (l) (= (length l) 3)) lines))

; lines are closed under GF(2) addition: p + q is on the line of p,q
(check "lines-are-2d-subspaces-minus-zero"
  (every (lambda (l)
    (every (lambda (p)
      (every (lambda (q)
        (or (equal p q) (in (vec-add p q) l))) l)) l)) lines))

; ---------- projective plane axioms ----------
(define (lines-through p) (filter (lambda (l) (in p l)) lines))
(define (line-of p q)
  (filter (lambda (l) (and (in p l) (in q l))) lines))

(check "two-points-determine-one-line"
  (every (lambda (pair2)
    (let ((pl (set-to-list pair2)))
      (= (length (line-of (car pl) (cadr pl))) 1)))
    (k-subsets points 2)))
(check "two-lines-meet-in-one-point"
  (every (lambda (l1)
    (every (lambda (l2)
      (or (equal l1 l2) (= (length (cap l1 l2)) 1))) lines)) lines))
(check "every-point-on-three-lines"
  (every (lambda (p) (= (length (lines-through p)) 3)) points))
; a quadrilateral exists: four points, no three collinear
(check "four-points-in-general-position"
  (let ((quad {(1 0 0) (0 1 0) (0 0 1) (1 1 1)}))
    (every (lambda (l) (<= (length (cap l quad)) 2)) lines)))

; ---------- duality: swap points and lines ----------
; The incidence structure (lines, points-through) is again a
; projective plane; verified by checking the dual axioms above
; (already done: lines meet in one point <-> points on one line).
(check "self-duality-counts"
  (and (= (length points) (length lines))
       (every (lambda (p) (= (length (lines-through p)) 3)) points)
       (every (lambda (l) (= (length l) 3)) lines)))

; ---------- Steiner system S(2,3,7) ----------
; Every 2-subset of the 7 points lies in exactly one 3-block.
(check "steiner-2-3-7"
  (every (lambda (pair2)
    (= 1 (count (lambda (l) (subset pair2 l)) lines)))
    (k-subsets points 2)))

; ---------- the difference-set model {1,2,4} mod 7 ----------
; Labeling points 0..6, the translates of {1,2,4} give the same
; incidence structure: {1,2,4} is a perfect difference set.
(define (translate s k)
  (map (lambda (x) (mod (+ x k) 7)) s))
(define ds-lines
  (map (lambda (k) (translate {1 2 4} k)) {0 1 2 3 4 5 6}))
(check "difference-set-gives-seven-blocks" (= (length ds-lines) 7))
(check "difference-set-is-steiner-2-3-7"
  (every (lambda (pair2)
    (= 1 (count (lambda (l) (subset pair2 l)) ds-lines)))
    (k-subsets {0 1 2 3 4 5 6} 2)))
; every nonzero residue mod 7 is a difference of {1,2,4} exactly once
(check "perfect-difference-set"
  (every (lambda (d)
    (= 1 (count (lambda (pq)
      (= d (mod (- (car pq) (cadr pq)) 7)))
      (pairs-such-that (lambda (a b) (not (= a b))) {1 2 4} {1 2 4}))))
    {1 2 3 4 5 6}))

(print "=== The Fano Plane Done ===")
