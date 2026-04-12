; ============================================================
; 2x2 Matrix Operations with Exact Rational Entries
; Matrix multiply, determinant, inverse, and solving Ax = b.
; ============================================================

(print "=== Rational Matrix Arithmetic ===")

(define (sub a b) (+ a (* -1 b)))

; A 2x2 matrix ((a b) (c d)) represents:
;   | a  b |
;   | c  d |

; --- Matrix operations ---

; Get matrix elements
(define (mat-a m) (car (car m)))
(define (mat-b m) (car (cdr (car m))))
(define (mat-c m) (car (car (cdr m))))
(define (mat-d m) (car (cdr (car (cdr m)))))

; Matrix multiply: A * B
(define (mat-mul a-mat b-mat)
  (let* ((a (mat-a a-mat)) (b (mat-b a-mat))
         (c (mat-c a-mat)) (d (mat-d a-mat))
         (e (mat-a b-mat)) (f (mat-b b-mat))
         (g (mat-c b-mat)) (h (mat-d b-mat)))
    (list (list (+ (* a e) (* b g)) (+ (* a f) (* b h)))
          (list (+ (* c e) (* d g)) (+ (* c f) (* d h))))))

; Determinant
(define (mat-det m)
  (sub (* (mat-a m) (mat-d m)) (* (mat-b m) (mat-c m))))

; Matrix inverse: A^{-1} = (1/det) * adj(A)
(define (mat-inv m)
  (let* ((det (mat-det m))
         (inv-det (/ 1 det)))
    (list (list (* inv-det (mat-d m)) (* inv-det (* -1 (mat-b m))))
          (list (* inv-det (* -1 (mat-c m))) (* inv-det (mat-a m))))))

; Print a matrix
(define (print-mat m)
  (print (car m))
  (print (car (cdr m))))

; --- Example 1: Integer matrix ---
(print "--- Matrix A ---")
(define A (list (list 2 3) (list 1 4)))
(print-mat A)

(print "--- det(A) ---")
(print (mat-det A))

(print "--- A^(-1) ---")
(define A-inv (mat-inv A))
(print-mat A-inv)

(print "--- A * A^(-1) = I (identity verification) ---")
(print-mat (mat-mul A A-inv))

; --- Example 2: Rational matrix ---
(print "--- Matrix B (rational entries) ---")
(define B (list (list 1/2 1/3) (list 1/4 1/5)))
(print-mat B)

(print "--- det(B) ---")
(print (mat-det B))

(print "--- B^(-1) ---")
(define B-inv (mat-inv B))
(print-mat B-inv)

(print "--- B * B^(-1) = I ---")
(print-mat (mat-mul B B-inv))

; --- Solve linear system Ax = b ---
; Solution: x = A^(-1) * b
; We represent b as a 2x1 column vector ((b1) (b2))
; and compute x = A^(-1) * b via mat-mul with a 2x1 trick
(define (solve-2x2 m b1 b2)
  (let* ((inv (mat-inv m))
         (x1 (+ (* (mat-a inv) b1) (* (mat-b inv) b2)))
         (x2 (+ (* (mat-c inv) b1) (* (mat-d inv) b2))))
    (list x1 x2)))

; Example: 2x + 3y = 8, x + 4y = 9
(print "--- Solve: 2x + 3y = 8, x + 4y = 9 ---")
(define solution (solve-2x2 A 8 9))
(print "Solution (x, y):")
(print solution)

; Verify: plug back in
(let* ((x (car solution)) (y (car (cdr solution))))
  (print "Verify 2x + 3y =") (print (+ (* 2 x) (* 3 y)))
  (print "Verify x + 4y =") (print (+ x (* 4 y))))

; Example: system with rational coefficients
; (1/2)x + (1/3)y = 1, (1/4)x + (1/5)y = 1
(print "--- Solve: (1/2)x + (1/3)y = 1, (1/4)x + (1/5)y = 1 ---")
(define solution2 (solve-2x2 B 1 1))
(print "Solution (x, y):")
(print solution2)

; Verify
(let* ((x (car solution2)) (y (car (cdr solution2))))
  (print "Verify (1/2)x + (1/3)y =")
  (print (+ (* 1/2 x) (* 1/3 y)))
  (print "Verify (1/4)x + (1/5)y =")
  (print (+ (* 1/4 x) (* 1/5 y))))

(print "=== Rational Matrix Done ===")
