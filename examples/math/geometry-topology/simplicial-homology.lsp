; ============================================================
; 64 — Simplicial Homology over GF(2)
; Chain complexes with exact mod-2 linear algebra: boundary
; matrices, the fundamental identity d.d = 0, Betti numbers by
; Gaussian elimination, and the Euler-Poincare theorem — all on
; the circle, the sphere, and the 7-vertex Csaszar torus.
; ============================================================

(load "lib/collections/relations.lsp")

(print "=== Simplicial Homology over GF(2) ===")


; ---------- exact GF(2) linear algebra on integer lists ----------
(define (row-xor a b)
  (cond ((eq a nil) nil)
        (t (cons (mod (+ (car a) (car b)) 2) (row-xor (cdr a) (cdr b))))))
(define (leading-index row i)
  (cond ((eq row nil) nil)
        ((= (car row) 1) i)
        (t (leading-index (cdr row) (+ i 1)))))
(define (rank2 rows)
  (cond ((eq rows nil) 0)
        ((eq (leading-index (car rows) 0) nil) (rank2 (cdr rows)))
        (t (let ((pivot (car rows)) (j (leading-index (car rows) 0)))
             (+ 1 (rank2 (map (lambda (r)
               (cond ((= (nth j r) 1) (row-xor r pivot)) (t r)))
               (cdr rows))))))))
(define (transpose2 m)
  (cond ((eq m nil) nil)
        ((eq (car m) nil) nil)
        (t (cons (map car m)
                 (transpose2 (map cdr m))))))
(define (dot2 a b)
  (mod (foldl + 0
    (map (lambda (pr) (* (car pr) (cadr pr))) (zip a b))) 2))
(define (matmul2 a b)
  (let ((bt (transpose2 b)))
    (map (lambda (row) (map (lambda (col) (dot2 row col)) bt)) a)))
(define (zero-matrix-p m)
  (every (lambda (row) (every (lambda (x) (= x 0)) row)) m))

; ---------- simplicial machinery ----------
; A k-simplex is a sorted list of vertices; s is a face of t when
; s omits exactly one vertex of t.
; note: t is the truth constant in SISP, so the bigger simplex is
; named w.
(define (sublist-p s w) (every (lambda (v) (contains v w)) s))
(define (face-p s w)
  (and (= (length s) (- (length w) 1)) (sublist-p s w)))
(define (boundary-matrix small big)
  (map (lambda (s)
    (map (lambda (b) (cond ((face-p s b) 1) (t 0))) big)) small))
(define (betti nk rank-in rank-out) (- nk (+ rank-in rank-out)))

; homology of a 2-complex given as (vertices edges faces),
; returns (b0 b1 b2 euler-characteristic)
(define (homology vs es fs)
  (let ((d1 (boundary-matrix (map (lambda (v) (list v)) vs) es))
        (d2 (boundary-matrix es fs)))
    (let ((r1 (rank2 d1))
          (r2 (cond ((eq fs nil) 0) (t (rank2 d2)))))
      (list (betti (length vs) 0 r1)
            (betti (length es) r1 r2)
            (betti (length fs) r2 0)
            (+ (- (length vs) (length es)) (length fs))))))

; ---------- the circle: boundary of a triangle ----------
(define circle-v (list 0 1 2))
(define circle-e (list (list 0 1) (list 0 2) (list 1 2)))
(define circle-h (homology circle-v circle-e nil))
(print (list (quote circle) circle-h))
(check "circle-connected" (= (nth 0 circle-h) 1))
(check "circle-has-one-loop" (= (nth 1 circle-h) 1))
(check "circle-euler-0" (= (nth 3 circle-h) 0))

; ---------- the sphere: boundary of a tetrahedron ----------
(define (k-subsets-list s k) (map (lambda (x) (sort (lambda (a b) (< a b)) (set-to-list x))) (set-to-list (k-subsets s k))))
(define sphere-v (list 0 1 2 3))
(define sphere-e (k-subsets-list {0 1 2 3} 2))
(define sphere-f (k-subsets-list {0 1 2 3} 3))
(define sphere-d1 (boundary-matrix (map (lambda (v) (list v)) sphere-v) sphere-e))
(define sphere-d2 (boundary-matrix sphere-e sphere-f))
(check "boundary-of-boundary-is-zero"
  (zero-matrix-p (matmul2 sphere-d1 sphere-d2)))
(define sphere-h (homology sphere-v sphere-e sphere-f))
(print (list (quote sphere) sphere-h))
(check "sphere-connected" (= (nth 0 sphere-h) 1))
(check "sphere-no-loops" (= (nth 1 sphere-h) 0))
(check "sphere-encloses-one-void" (= (nth 2 sphere-h) 1))
(check "sphere-euler-2" (= (nth 3 sphere-h) 2))

; ---------- the Csaszar torus: 7 vertices, 21 edges, 14 faces ----------
; Faces {i, i+1, i+3} and {i, i+2, i+3} mod 7; the edge set is the
; complete graph K7 and every edge borders exactly two faces.
(define (sorted3 a b c) (sort (lambda (x y) (< x y)) (list a b c)))
(define torus-f
  (append
    (map (lambda (i) (sorted3 i (mod (+ i 1) 7) (mod (+ i 3) 7))) (seq 0 6))
    (map (lambda (i) (sorted3 i (mod (+ i 2) 7) (mod (+ i 3) 7))) (seq 0 6))))
(define torus-v (seq 0 6))
(define torus-e (k-subsets-list {0 1 2 3 4 5 6} 2))
(check "torus-counts"
  (and (= (length torus-v) 7) (= (length torus-e) 21)
       (= (length torus-f) 14)))
(check "every-edge-borders-two-faces"
  (every (lambda (e) (= 2 (count (lambda (f) (face-p e f)) torus-f))) torus-e))
(define torus-d1 (boundary-matrix (map (lambda (v) (list v)) torus-v) torus-e))
(define torus-d2 (boundary-matrix torus-e torus-f))
(check "torus-boundary-squared-zero"
  (zero-matrix-p (matmul2 torus-d1 torus-d2)))
(define torus-h (homology torus-v torus-e torus-f))
(print (list (quote torus) torus-h))
(check "torus-connected" (= (nth 0 torus-h) 1))
(check "torus-has-two-independent-loops" (= (nth 1 torus-h) 2))
(check "torus-encloses-one-void" (= (nth 2 torus-h) 1))
(check "torus-euler-0" (= (nth 3 torus-h) 0))

; ---------- Euler-Poincare: chi computed two ways agrees ----------
(define (euler-poincare h) (+ (- (nth 0 h) (nth 1 h)) (nth 2 h)))
(check "euler-poincare-circle" (= (euler-poincare circle-h) (nth 3 circle-h)))
(check "euler-poincare-sphere" (= (euler-poincare sphere-h) (nth 3 sphere-h)))
(check "euler-poincare-torus" (= (euler-poincare torus-h) (nth 3 torus-h)))

; homology distinguishes the surfaces: same dimensions 0,1,2 but
; different Betti vectors.
(check "homology-separates-sphere-from-torus"
  (not (equal sphere-h torus-h)))

(print "=== Simplicial Homology Done ===")
