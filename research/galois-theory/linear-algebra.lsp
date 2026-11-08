; ============================================================
; galois-theory/linear-algebra.lsp — Vector Spaces, Linear Maps,
; Determinants, Eigenvectors, Characteristic Polynomial, Cayley-Hamilton
; over finite fields F_q.  Concrete on F_5.
; ============================================================

(print "=== Linear Algebra over F_5 ===")


; ---------- F_5 field ops ----------
(define (f+ a b) (mod (+ a b) 5))
(define (f- a b) (mod (- a b) 5))
(define (f* a b) (mod (* a b) 5))
(define (f-inv a) (cond ((= a 0) (error "division by zero"))
                        ((= a 1) 1) ((= a 2) 3) ((= a 3) 2) (t 4)))
(define (f/ a b) (f* a (f-inv b)))

(define f5 {0 1 2 3 4})


; ---------- vector space F_5^3 ----------
(define (vec+ u v) (list (f+ (nth 0 u) (nth 0 v))
                          (f+ (nth 1 u) (nth 1 v))
                          (f+ (nth 2 u) (nth 2 v))))

(define (vec-scalar c v)
  (list (f* c (nth 0 v)) (f* c (nth 1 v)) (f* c (nth 2 v))))

(define (vec-neg v) (list (mod (- 0 (nth 0 v)) 5)
                           (mod (- 0 (nth 1 v)) 5)
                           (mod (- 0 (nth 2 v)) 5)))

(define zero-v (list 0 0 0))

; cartesian-product nests to the right: (a (b c)).  Flatten to plain
; 3-element vectors (a b c), otherwise every arithmetic op below is handed
; a list instead of a number.
(define vspace
  (map (lambda (p) (list (car p) (car (cadr p)) (cadr (cadr p))))
           (cartesian-product f5 (cartesian-product f5 f5))))

(check "vspace-size-125" (= (length vspace) 125))

; NOTE: the carrier parameter is named `vs`, not `V` — identifiers fold to
; lowercase, so `V` would be captured by the `(lambda (v) ...)` binders and
; the inner `every` calls would receive a vector where a set is meant.
(define (vec-space-p vs add smul zero)
  (and
    (every (lambda (u) (every (lambda (v) (every (lambda (w)
      (equal (add (add u v) w) (add u (add v w)))) vs)) vs)) vs)
    (every (lambda (u) (equal (add u zero) u)) vs)
    (every (lambda (u) (equal (add zero u) u)) vs)
    (every (lambda (u) (any (lambda (w) (equal (add u w) zero)) vs)) vs)
    (every (lambda (u) (every (lambda (v) (equal (add u v) (add v u))) vs)) vs)
    (every (lambda (c) (every (lambda (d) (every (lambda (u)
      (equal (smul (f+ c d) u) (add (smul c u) (smul d u)))) vs)) f5)) f5)
    (every (lambda (c) (every (lambda (u) (every (lambda (v)
      (equal (smul c (add u v)) (add (smul c u) (smul c v)))) vs)) vs)) f5)
    (every (lambda (c) (every (lambda (d) (every (lambda (u)
      (equal (smul c (smul d u)) (smul (f* c d) u))) vs)) f5)) f5)
    (every (lambda (u) (equal (smul 1 u) u)) vs)))

(check "f5^3-is-vector-space" (vec-space-p vspace vec+ vec-scalar zero-v))

(check "vec-neg-is-additive-inverse"
  (every (lambda (u) (equal (vec+ u (vec-neg u)) zero-v)) vspace))


; ---------- linear transformations (matrices) ----------
(define (mat-row m i) (list (nth 0 (nth i m)) (nth 1 (nth i m)) (nth 2 (nth i m))))
(define (mat-col m j) (list (nth j (nth 0 m)) (nth j (nth 1 m)) (nth j (nth 2 m))))

(define (mat-apply m v)
  (let ((r0 (nth 0 m)) (r1 (nth 1 m)) (r2 (nth 2 m)))
    (list (f+ (f+ (f* (nth 0 r0) (nth 0 v)) (f* (nth 1 r0) (nth 1 v))) (f* (nth 2 r0) (nth 2 v)))
          (f+ (f+ (f* (nth 0 r1) (nth 0 v)) (f* (nth 1 r1) (nth 1 v))) (f* (nth 2 r1) (nth 2 v)))
          (f+ (f+ (f* (nth 0 r2) (nth 0 v)) (f* (nth 1 r2) (nth 1 v))) (f* (nth 2 r2) (nth 2 v))))))

(define id3 (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define A (list (list 1 2 0) (list 0 3 1) (list 0 0 4)))

(check "mat-row-and-col-read-correctly"
  (and (equal (mat-row A 1) (list 0 3 1))
       (equal (mat-col A 1) (list 2 3 0))
       (equal (mat-col id3 2) (list 0 0 1))))

(check "id-applies-identically"
  (every (lambda (v) (equal (mat-apply id3 v) v)) vspace))

(define (matrix-linear-p m)
  (every (lambda (u)
    (every (lambda (v)
      (equal (mat-apply m (vec+ u v))
             (vec+ (mat-apply m u) (mat-apply m v)))) vspace)) vspace))

(check "id-is-linear" (matrix-linear-p id3))
(check "A-is-linear" (matrix-linear-p A))


; ---------- composition (matrix multiplication) ----------
; NOTE: `mat-mul` and scalar `*` operate on the dense
; OBJ_MATRIX type; they cannot be redefined and reject these list-of-lists
; matrices.  Use distinct names `mmul` / `mscale`.
(define (mmul m1 m2)
  (let ((r0 (nth 0 m1)) (r1 (nth 1 m1)) (r2 (nth 2 m1)))
    (list (list (f+ (f+ (f* (nth 0 r0) (nth 0 (nth 0 m2))) (f* (nth 1 r0) (nth 0 (nth 1 m2)))) (f* (nth 2 r0) (nth 0 (nth 2 m2))))
                (f+ (f+ (f* (nth 0 r0) (nth 1 (nth 0 m2))) (f* (nth 1 r0) (nth 1 (nth 1 m2)))) (f* (nth 2 r0) (nth 1 (nth 2 m2))))
                (f+ (f+ (f* (nth 0 r0) (nth 2 (nth 0 m2))) (f* (nth 1 r0) (nth 2 (nth 1 m2)))) (f* (nth 2 r0) (nth 2 (nth 2 m2)))))
          (list (f+ (f+ (f* (nth 0 r1) (nth 0 (nth 0 m2))) (f* (nth 1 r1) (nth 0 (nth 1 m2)))) (f* (nth 2 r1) (nth 0 (nth 2 m2))))
                (f+ (f+ (f* (nth 0 r1) (nth 1 (nth 0 m2))) (f* (nth 1 r1) (nth 1 (nth 1 m2)))) (f* (nth 2 r1) (nth 1 (nth 2 m2))))
                (f+ (f+ (f* (nth 0 r1) (nth 2 (nth 0 m2))) (f* (nth 1 r1) (nth 2 (nth 1 m2)))) (f* (nth 2 r1) (nth 2 (nth 2 m2)))))
          (list (f+ (f+ (f* (nth 0 r2) (nth 0 (nth 0 m2))) (f* (nth 1 r2) (nth 0 (nth 1 m2)))) (f* (nth 2 r2) (nth 0 (nth 2 m2))))
                (f+ (f+ (f* (nth 0 r2) (nth 1 (nth 0 m2))) (f* (nth 1 r2) (nth 1 (nth 1 m2)))) (f* (nth 2 r2) (nth 1 (nth 2 m2))))
                (f+ (f+ (f* (nth 0 r2) (nth 2 (nth 0 m2))) (f* (nth 1 r2) (nth 2 (nth 1 m2)))) (f* (nth 2 r2) (nth 2 (nth 2 m2))))))))

(check "mat-comp-is-assoc"
  (every (lambda (v)
    (equal (mat-apply A (mat-apply A v))
           (mat-apply (mmul A A) v))) vspace))

(check "id-is-mul-identity"
  (and (equal (mmul A id3) A) (equal (mmul id3 A) A)))


; ---------- determinant ----------
(define (det3 m)
  (let ((a (nth 0 (nth 0 m))) (b (nth 1 (nth 0 m))) (c (nth 2 (nth 0 m)))
        (d (nth 0 (nth 1 m))) (e (nth 1 (nth 1 m))) (f (nth 2 (nth 1 m)))
        (g (nth 0 (nth 2 m))) (h (nth 1 (nth 2 m))) (i (nth 2 (nth 2 m))))
    (mod (+ (* a (mod (- (* e i) (* f h)) 5))
            (- (* b (mod (- (* d i) (* f g)) 5)))
            (* c (mod (- (* d h) (* e g)) 5))) 5)))

(check "det-id-is-1" (= (det3 id3) 1))
; A is upper triangular, so det A is the product of its diagonal: 1*3*4 = 12 = 2 in F_5.
(check "det-A-product" (= (det3 A) (f* (f* 1 3) 4)))
(check "det-product-rule"
  (let ((bigb (list (list 2 0 0) (list 0 3 0) (list 0 0 1))))
    (= (f* (det3 A) (det3 bigb)) (det3 (mmul A bigb)))))

(define zero3 (list (list 0 0 0) (list 0 0 0) (list 0 0 0)))
(check "det-zero-matrix" (= (det3 zero3) 0))


; ---------- characteristic polynomial ----------
(define (mat-trace m)
  (mod (+ (nth 0 (nth 0 m)) (nth 1 (nth 1 m)) (nth 2 (nth 2 m))) 5))

; det(x*I - M) = x^3 - tr(M) x^2 + c2 x - det(M), with
; c2 = (tr(M)^2 - tr(M^2)) / 2.  The division by 2 must happen in F_5
; (multiply by 2^-1 = 3); `/` in SISP is exact rational division and would
; produce a non-integer once the traces are already reduced mod 5.
; Coefficients are returned low-degree-first: (c0 c1 c2 c3).
(define (char-poly m)
  (let ((tr1 (mat-trace m)))
    (let ((tr2 (mat-trace (mmul m m))))
      (let ((dt (det3 m)))
        (list (f- 0 dt)
              (f/ (f- (f* tr1 tr1) tr2) 2)
              (f- 0 tr1)
              1)))))

(define chara (char-poly A))
(check "char-poly-deg-3" (= (last chara) 1))
; A is upper triangular with diagonal 1,3,4, so its characteristic polynomial is
; (x-1)(x-3)(x-4) = x^3 - 8x^2 + 19x - 12 = x^3 + 2x^2 + 4x + 3 over F_5.
(check "char-poly-of-A" (equal chara (list 3 4 2 1)))

(define (poly-eval poly x)
  (cond ((eq (cdr poly) nil) (car poly))
        (t (f+ (car poly) (f* x (poly-eval (cdr poly) x))))))


; ---------- eigenvectors ----------
(define (eigenvalue-p m lam)
  (any (lambda (v)
    (and (not (equal v zero-v))
         (equal (mat-apply m v) (vec-scalar lam v)))) vspace))

(define (eigenvalues m)
  (filter (lambda (lam) (eigenvalue-p m lam)) f5))

(define evals (eigenvalues A))
(print (list (quote eigenvalues-of-A) (set-to-list evals)))
(check "A-has-eigenvalues"
  (and (= (length evals) 3) (in 1 evals) (in 3 evals) (in 4 evals)))

(check "eigenvalues-are-exactly-char-poly-roots"
  (every (lambda (lam)
    (equal (= (poly-eval chara lam) 0) (eigenvalue-p A lam))) f5))

(define (eigenvector-of m lam)
  (find (lambda (v)
    (and (not (equal v zero-v))
         (equal (mat-apply m v) (vec-scalar lam v)))) vspace))

(define v1 (eigenvector-of A (car (set-to-list evals))))
(check "eigenvector-satisfies-Av=lam*v"
  (and (not (equal v1 zero-v))
       (equal (mat-apply A v1) (vec-scalar (car (set-to-list evals)) v1))))


; ---------- diagonalizable: check with a diagonal matrix ----------
(define bigd (list (list 2 0 0) (list 0 3 0) (list 0 0 1)))
(define evalsd (eigenvalues bigd))
(check "D-has-three-eigenvalues" (= (length evalsd) 3))
(check "D-eigenvalues-are-its-diagonal"
  (and (in 1 evalsd) (in 2 evalsd) (in 3 evalsd)))


; ---------- row reduction (Gaussian elimination over F_5) ----------
(define (row-zero-p r) (and (= (nth 0 r) 0) (= (nth 1 r) 0) (= (nth 2 r) 0)))

(define (row-axpy r p c)
  (list (f+ (nth 0 r) (f* c (nth 0 p)))
        (f+ (nth 1 r) (f* c (nth 1 p)))
        (f+ (nth 2 r) (f* c (nth 2 p)))))

; Full row echelon form: at each column pick a pivot row, clear that column
; from every row (the pivot clears itself to zero and is dropped by `remove`),
; then recurse on the next column.  Returns only the non-zero pivot rows.
(define (ref-rows rows j)
  (cond ((eq rows nil) nil)
        ((> j 2) nil)
        (t (let ((p (find (lambda (r) (not (= (nth j r) 0))) rows)))
             (cond ((null p) (ref-rows rows (+ j 1)))
                   (t (cons p
                        (ref-rows
                          (remove row-zero-p
                            (map (lambda (r)
                                   (row-axpy r p (f- 0 (f* (nth j r) (f-inv (nth j p))))))
                                 rows))
                          (+ j 1)))))))))

(define (mat-to-rows m) (list (nth 0 m) (nth 1 m) (nth 2 m)))
(define (row-reduce m) (ref-rows (mat-to-rows m) 0))
(define (rank m) (length (row-reduce m)))

(check "rank-id-is-3" (= (rank id3) 3))
(check "rank-A-is-3" (= (rank A) 3))
(check "rank-zero-is-0" (= (rank zero3) 0))
(check "rank-rank1-matrix"
  (= (rank (list (list 1 1 1) (list 2 2 2) (list 3 3 3))) 1))
(check "rank-rank2-matrix"
  (= (rank (list (list 1 0 0) (list 0 1 0) (list 0 2 0))) 2))
(check "rank-detects-singular-with-nonzero-first-column"
  (= (rank (list (list 1 2 3) (list 2 4 6) (list 0 1 1))) 2))
(check "rank-3-iff-det-nonzero"
  (and (equal (= (rank id3) 3) (not (= (det3 id3) 0)))
       (equal (= (rank A) 3) (not (= (det3 A) 0)))
       (equal (= (rank zero3) 3) (not (= (det3 zero3) 0)))
       (equal (= (rank (list (list 1 1 1) (list 2 2 2) (list 3 3 3))) 3)
              (not (= (det3 (list (list 1 1 1) (list 2 2 2) (list 3 3 3))) 0)))))


; ---------- Cayley-Hamilton: p(A) = 0 ----------
(define (mscale c m)
  (list (list (f* c (nth 0 (nth 0 m))) (f* c (nth 1 (nth 0 m))) (f* c (nth 2 (nth 0 m))))
        (list (f* c (nth 0 (nth 1 m))) (f* c (nth 1 (nth 1 m))) (f* c (nth 2 (nth 1 m))))
        (list (f* c (nth 0 (nth 2 m))) (f* c (nth 1 (nth 2 m))) (f* c (nth 2 (nth 2 m))))))

(define (mat-add m1 m2)
  (list (list (f+ (nth 0 (nth 0 m1)) (nth 0 (nth 0 m2)))
              (f+ (nth 1 (nth 0 m1)) (nth 1 (nth 0 m2)))
              (f+ (nth 2 (nth 0 m1)) (nth 2 (nth 0 m2))))
        (list (f+ (nth 0 (nth 1 m1)) (nth 0 (nth 1 m2)))
              (f+ (nth 1 (nth 1 m1)) (nth 1 (nth 1 m2)))
              (f+ (nth 2 (nth 1 m1)) (nth 2 (nth 1 m2))))
        (list (f+ (nth 0 (nth 2 m1)) (nth 0 (nth 2 m2)))
              (f+ (nth 1 (nth 2 m1)) (nth 1 (nth 2 m2)))
              (f+ (nth 2 (nth 2 m1)) (nth 2 (nth 2 m2))))))

(define (mat-eval-poly poly m)
  (let ((sq (mmul m m)))
    (let ((cu (mmul sq m)))
      (mat-add (mat-add (mscale (nth 0 poly) id3)
                        (mscale (nth 1 poly) m))
               (mat-add (mscale (nth 2 poly) sq)
                        (mscale (nth 3 poly) cu))))))

(check "cayley-hamilton-A"
  (equal (mat-eval-poly chara A) zero3))

(check "cayley-hamilton-D"
  (equal (mat-eval-poly (char-poly bigd) bigd) zero3))

(check "cayley-hamilton-id"
  (equal (mat-eval-poly (char-poly id3) id3) zero3))


; ---------- rank-nullity ----------
(define (kernel m)
  (filter (lambda (v) (equal (mat-apply m v) zero-v)) vspace))

(define (image m)
  (list-to-set (map (lambda (v) (mat-apply m v)) (set-to-list vspace))))

; A is invertible (det A = 2 != 0), so ker A = {0} and im A is all of F_5^3.
; The image therefore has 5^3 = 125 elements, not 25.  In cardinality form
; rank-nullity reads |ker| * |im| = |F_5^3|.
(check "rank-nullity-A"
  (and (= (length (kernel A)) 1)
       (= (length (image A)) 125)
       (= (* (length (kernel A)) (length (image A))) 125)))

; A singular example: rank 1, so nullity 2 -> |ker| = 25, |im| = 5.
(check "rank-nullity-singular"
  (let ((sing (list (list 1 1 1) (list 2 2 2) (list 3 3 3))))
    (and (= (rank sing) 1)
         (= (length (image sing)) 5)
         (= (length (kernel sing)) 25)
         (= (* (length (kernel sing)) (length (image sing))) 125))))


; ---------- eigenvalues of D characterised by the determinant ----------
; lam is an eigenvalue of D  <=>  det(D - lam*I) = 0.
(check "det-D-zero-if-0-eigenvalue"
  (every (lambda (lam)
    (equal (= (det3 (mat-add bigd (mscale (f- 0 lam) id3))) 0)
           (eigenvalue-p bigd lam))) f5))


(print "=== Linear Algebra Done ===")
