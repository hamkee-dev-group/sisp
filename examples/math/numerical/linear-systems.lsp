; ============================================================
; linear-systems.lsp — Gaussian elimination, partial pivoting.
; LU decomposition. Forward/back substitution. Hilbert H_3.
; Sensitivity to a perturbation of b.
; ============================================================

(print "=== Linear Systems ===")


(define EPS 0.000000000000001)

; --- list-of-lists -> matrix (with EPS for float path) ---
(define (->mat rows)
  (let* ((r (length rows)) (c (length (car rows)))
         (m (make-matrix r c)))
    (->mat-fill m rows 0)))

(define (->mat-fill m rows i)
  (cond ((eq rows nil) m)
        (t (->mat-row m (car rows) i 0)
           (->mat-fill m (cdr rows) (+ i 1)))))

(define (->mat-row m row i j)
  (cond ((eq row nil) nil)
        (t (mat-set m i j (+ (car row) EPS))
           (->mat-row m (cdr row) i (+ j 1)))))

; --- Augmented matrix [A | b] ---
(define (mk-aug A-rows b-vec)
  (let* ((n (length A-rows)) (nc (+ n 1))
         (M (make-matrix n nc)))
    (copy-blk M (->mat A-rows) n n)
    (fill-vec M b-vec n 0)
    M))

(define (copy-blk dst src rows cols)
  (copy-blk-ij dst src 0 rows 0 cols))

(define (copy-blk-ij dst src i nr j nc)
  (cond ((>= i nr) nil)
        ((>= j nc) (copy-blk-ij dst src (+ i 1) nr 0 nc))
        (t (mat-set dst i j (mat-ref src i j))
           (copy-blk-ij dst src i nr (+ j 1) nc))))

(define (fill-vec M vec n i)
  (cond ((>= i n) nil)
        (t (mat-set M i n (nth i vec))
           (fill-vec M vec n (+ i 1)))))

; --- Gaussian elimination with partial pivoting ---
(define (gauss-elim M n k)
  (cond ((>= k n) nil)
        (t (let ((pvt (best-pvt M k n)))
          (swap M k pvt (+ n 1))
          (elim M k n (+ n 1))
          (gauss-elim M n (+ k 1))))))

; kcol (the column being pivoted on) has to be threaded through --
; it is not in scope inside best-pvt-i otherwise.
(define (best-pvt M k n)
  (best-pvt-i M k (mat-ref M k k) k k n))

(define (best-pvt-i M best bestv kcol i n)
  (cond ((>= i n) best)
        (t (let ((aik (mat-ref M i kcol)))
          (cond ((> (abs aik) (abs bestv)) (best-pvt-i M i aik kcol (+ i 1) n))
                (t (best-pvt-i M best bestv kcol (+ i 1) n)))))))

(define (swap M i j nc)
  (cond ((= i j) nil)
        (t (swap-c M i j 0 nc))))

(define (swap-c M i j c nc)
  (cond ((>= c nc) nil)
        (t (let ((tmp (mat-ref M i c)))
          (mat-set M i c (mat-ref M j c))
          (mat-set M j c tmp)
          (swap-c M i j (+ c 1) nc)))))

(define (elim M k n nc)
  (let ((piv (mat-ref M k k)))
    (cond ((< (abs piv) 0.00000000000001) nil)
          (t (elim-rows M k piv n (+ k 1) nc)))))

(define (elim-rows M k piv n r nc)
  (cond ((>= r n) nil)
        (t (let ((factor (/ (mat-ref M r k) piv)))
          (elim-cols M k r factor k nc)
          (elim-rows M k piv n (+ r 1) nc)))))

(define (elim-cols M k r factor c nc)
  (cond ((>= c nc) nil)
        (t (mat-set M r c (- (mat-ref M r c) (* factor (mat-ref M k c))))
           (elim-cols M k r factor (+ c 1) nc))))

; --- Back substitution ---
(define (gauss-back M n)
  (gauss-back-i M n (- n 1) nil))

(define (gauss-back-i M n i xs)
  (cond ((< i 0) xs)
        (t (let* ((bi (mat-ref M i n))
                  (s (gauss-dot M i (+ i 1) n xs))
                  (ai (mat-ref M i i))
                  (xi (/ (- bi s) ai)))
          (gauss-back-i M n (- i 1) (cons xi xs))))))

; xs holds only x[i+1..n-1], so the absolute column j maps to xs index j-(i+1)
(define (gauss-dot M i j n xs)
  (cond ((>= j n) 0.0)
        (t (+ (* (mat-ref M i j) (nth (- j (+ i 1)) xs))
              (gauss-dot M i (+ j 1) n xs)))))

; --- Solve via Gaussian elimination ---
(define (gauss-solve A b)
  (let* ((n (length A)) (M (mk-aug A b)))
    (gauss-elim M n 0)
    (gauss-back M n)))

; --- LU decomposition (Doolittle: unit diagonal in L, no pivoting) ---
(define (lu-decomp A-rows)
  (let* ((n (length A-rows)) (A (->mat A-rows))
         (L (make-matrix n n)) (U (make-matrix n n)))
    (lu-init L 0 n)
    (lu-decomp-k A L U 0 n)
    (list L U)))

(define (lu-init L i n)
  (cond ((>= i n) nil)
        (t (mat-set L i i 1.0) (lu-init L (+ i 1) n))))

(define (lu-decomp-k A L U k n)
  (cond ((>= k n) nil)
        (t (lu-comp-u A L U k k n)
           (lu-comp-l A L U k (+ k 1) n)
           (lu-decomp-k A L U (+ k 1) n))))

(define (lu-comp-u A L U k j n)
  (cond ((>= j n) nil)
        (t (let ((s (lu-dot-uk L U k j 0 k 0.0)))
          (mat-set U k j (- (mat-ref A k j) s))
          (lu-comp-u A L U k (+ j 1) n)))))

(define (lu-comp-l A L U k i n)
  (cond ((>= i n) nil)
        (t (let ((s (lu-dot-lk L U i k 0 k 0.0))
                 (ukk (mat-ref U k k)))
          (cond ((> (abs ukk) 0.00000000000001)
                 (mat-set L i k (/ (- (mat-ref A i k) s) ukk))))
          (lu-comp-l A L U k (+ i 1) n)))))

(define (lu-dot-uk L U k j p kend acc)
  (cond ((>= p kend) acc)
        (t (lu-dot-uk L U k j (+ p 1) kend
                       (+ acc (* (mat-ref L k p) (mat-ref U p j)))))))

(define (lu-dot-lk L U i k p kend acc)
  (cond ((>= p kend) acc)
        (t (lu-dot-lk L U i k (+ p 1) kend
                       (+ acc (* (mat-ref L i p) (mat-ref U p k)))))))

; --- Forward solve: Ly = b ---
(define (lu-fwd L b)
  (let* ((n (length b)) (y nil))
    (lu-fwd-i L b 0 n y)))

(define (lu-fwd-i L b i n ys)
  (cond ((>= i n) (reverse ys))
        (t (let ((s (lu-fwd-dot L i ys 0 0.0)))
          (lu-fwd-i L b (+ i 1) n
                    (cons (- (nth i b) s) ys))))))

(define (lu-fwd-dot L i ys j acc)
  (cond ((>= j i) acc)
        (t (lu-fwd-dot L i ys (+ j 1)
                       (+ acc (* (mat-ref L i j) (nth (- i 1 j) ys)))))))

; --- Back solve: Ux = y ---
(define (lu-back U y)
  (let* ((n (length y)) (x nil))
    (lu-back-i U y (- n 1) n x)))

(define (lu-back-i U y i n xs)
  (cond ((< i 0) xs)
        (t (let ((s (lu-back-dot U i (+ i 1) n xs 0.0)))
          (lu-back-i U y (- i 1) n
                     (cons (/ (- (nth i y) s) (mat-ref U i i)) xs))))))

; xs holds only x[i+1..n-1], so the absolute column j maps to xs index j-(i+1)
(define (lu-back-dot U i j n xs acc)
  (cond ((>= j n) acc)
        (t (lu-back-dot U i (+ j 1) n xs
                       (+ acc (* (mat-ref U i j) (nth (- j (+ i 1)) xs)))))))

; --- Solve via LU ---
(define (lu-solve A b)
  (let* ((decomp (lu-decomp A))
         (L (car decomp)) (U (cadr decomp))
         (y (lu-fwd L b)))
    (lu-back U y)))

; ========== TESTS ==========

; --- 3x3: 2x+y-z=8, -3x-y+2z=-11, -2x+y+2z=-3 → x=(2,3,-1) ---
(define a3 (list (list 2 1 -1) (list -3 -1 2) (list -2 1 2)))
(define b3 (list 8 -11 -3))
(define x3 (gauss-solve a3 b3))
(print "Gauss 3x3:" x3)
(check "gauss-3x3" (and (< (abs (- (car x3) 2.0)) 0.0001)
                     (< (abs (- (cadr x3) 3.0)) 0.0001)
                     (< (abs (- (caddr x3) -1.0)) 0.0001)))

; --- 2x2: 4x+3y=10, 6x+3y=12 → x=1, y=2 ---
; Subtracting the equations gives 2x=2 → x=1, then y=(10-4)/3=2.
(define a2 (list (list 4 3) (list 6 3)))
(define b2 (list 10 12))
(define x2lu (lu-solve a2 b2))
(print "LU 2x2:" x2lu)
(check "lu-2x2" (and (< (abs (- (car x2lu) 1.0)) 0.0001)
                   (< (abs (- (cadr x2lu) 2.0)) 0.0001)))

; --- Hilbert H_3: solve H_3 * x = (1,1,1) ---
(define (hilbert n)
  (let ((rows nil))
    (hilb-i 0 n rows)))

(define (hilb-i i n acc)
  (cond ((>= i n) (reverse acc))
        (t (let ((row nil))
          (hilb-j i 0 n row acc)))))

(define (hilb-j i j n row acc)
  (cond ((>= j n) (hilb-i (+ i 1) n (cons (reverse row) acc)))
        (t (hilb-j i (+ j 1) n (cons (/ 1.0 (+ i j 1)) row) acc))))

(define h3 (hilbert 3))
(define bh3 (list 1.0 1.0 1.0))
(define xh3 (gauss-solve h3 bh3))
(print "H_3:" xh3)
; exact solution of H_3 x = (1,1,1) is (3, -24, 30)
(check "hilbert-solved" (and (< (abs (- (car xh3) 3.0)) 0.001)
                           (< (abs (- (cadr xh3) -24.0)) 0.01)
                           (< (abs (- (caddr xh3) 30.0)) 0.01)))

; --- Sensitivity |dx|/|db| for a small perturbation of b (a crude stand-in
;     for the condition number) ---
(define b3p (list 8.001 -11.001 -3.001))
(define x3p (gauss-solve a3 b3p))
(define cond-est (/ (abs (- (car x3) (car x3p))) 0.001))
(print "Sensitivity ~" cond-est)
(check "cond-reasonable" (> cond-est 0))

(print "=== Linear Systems Done ===")
