; ============================================================
; Lang's Algebra Ch.3: Modules
; Modules, free modules, Mat_n(R), structure theorem for
; modules over PID (Z/nZ as finite ring with Z-structure).
; ============================================================

(print "=== Lang Ch3: Modules ===")

(define (r+ n a b) (mod (+ a b) n))
(define (r* n a b) (mod (* a b) n))
(define (rneg n a) (mod (- 0 a) n))

(define (divides n d) (= (mod n d) 0))
(define (divisors n)
  (labels (((go-iter k acc)
    (cond ((> k n) (reverse acc))
          ((divides n k) (go-iter (+ k 1) (cons k acc)))
          (t (go-iter (+ k 1) acc)))))
    (go-iter 1 nil)))

(define (ideal-from-divisor n d)
  (list-to-set
   (labels (((go-iter k acc)
     (cond ((>= k n) (reverse acc))
           (t (go-iter (+ k d) (cons k acc))))))
     (go-iter 0 nil))))

(define (free-module n k)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (labels (((prod-sets sets)
      (cond ((eq sets nil) (list-to-set (list nil)))
            (t (cartesian-product (car sets) (prod-sets (cdr sets)))))))
      (labels (((mk-acc i acc)
        (cond ((= i 0) (prod-sets acc))
              (t (mk-acc (- i 1) (cons ring acc))))))
        (mk-acc k nil)))))

(define (ann-element n elem)
  (filter (lambda (r) (= (r* n r elem) 0))
    (list-to-set (seq 0 (- n 1)))))

(define (mat2-add n M nn)
  (list (list (r+ n (nth 0 (nth 0 M)) (nth 0 (nth 0 nn)))
              (r+ n (nth 1 (nth 0 M)) (nth 1 (nth 0 nn))))
        (list (r+ n (nth 0 (nth 1 M)) (nth 0 (nth 1 nn)))
              (r+ n (nth 1 (nth 1 M)) (nth 1 (nth 1 nn))))))

(define (mat2-mul n M nn)
  (let ((a (nth 0 (nth 0 M))) (b (nth 1 (nth 0 M)))
        (c (nth 0 (nth 1 M))) (d (nth 1 (nth 1 M)))
        (e (nth 0 (nth 0 nn))) (f (nth 1 (nth 0 nn)))
        (g (nth 0 (nth 1 nn))) (h (nth 1 (nth 1 nn))))
    (list (list (r+ n (r* n a e) (r* n b g))
                (r+ n (r* n a f) (r* n b h)))
          (list (r+ n (r* n c e) (r* n d g))
                (r+ n (r* n c f) (r* n d h))))))

(define (mat2-det n M)
  (let ((a (nth 0 (nth 0 M))) (b (nth 1 (nth 0 M)))
        (c (nth 0 (nth 1 M))) (d (nth 1 (nth 1 M))))
    (r+ n (r* n a d) (rneg n (r* n b c)))))

(define (mat2-unit-p n M)
  (let ((det (mat2-det n M)))
    (and (not (= det 0))
         (any (lambda (x) (= (r* n det x) 1))
           (list-to-set (seq 0 (- n 1)))))))

(define (all-mat2 n)
  (let ((ring-lst (seq 0 (- n 1))))
    (list-to-set
     (foldl (lambda (acc a)
       (foldl (lambda (acc2 b)
         (foldl (lambda (acc3 c)
           (foldl (lambda (acc4 d)
             (cons (list (list a b) (list c d)) acc4))
             acc3 ring-lst))
           acc2 ring-lst))
         acc ring-lst))
       nil ring-lst))))

(print "--- Z/6Z Free Module R^2 ---")
(define n6 6)
(define R2-free-Z6 (free-module n6 2))
(check "R2-free-Z6-size-36" (= (length R2-free-Z6) 36))

(print "--- Z/12Z Free Module R^2 ---")
(define n12 12)
(define R2-free-Z12 (free-module n12 2))
(check "R2-free-Z12-size-144" (= (length R2-free-Z12) 144))

(print "--- Mat_2(Z/6Z) ---")
(define Mat2-6 (all-mat2 n6))
(check "Mat2-Z6-size-1296" (= (length Mat2-6) 1296))

(define GL2-6 (filter
  (lambda (M) (mat2-unit-p n6 M)) Mat2-6))
(check "GL2-Z6-is-nonempty" (>= (length GL2-6) 1))
; |GL_2(Z/6)| = |GL_2(F_2)| . |GL_2(F_3)| by CRT, and |GL_2(F_q)| =
; (q^2-1)(q^2-q). The enumeration above must match that closed form.
(define (gl2-order q) (* (- (^ q 2) 1) (- (^ q 2) q)))
(check "GL2-Z6-order-matches-CRT-formula"
  (= (length GL2-6) (* (gl2-order 2) (gl2-order 3))))
; The units are a proper subset of all matrices, so the test discriminates.
(check "GL2-Z6-is-a-proper-subset" (< (length GL2-6) (length Mat2-6)))

(define I2-6 (list (list 1 0) (list 0 1)))
(check "I2-is-unit" (mat2-unit-p n6 I2-6))

(print "--- Annihilators ---")
(define ann4 (ann-element n12 4))
(check "ann-4-in-Z12-is-3Z" (= (length ann4) 4))

(define ann6 (ann-element n12 6))
(check "ann-6-in-Z12-is-2Z" (= (length ann6) 6))

(print "--- Invariant Factors ---")
; The invariant factor of a cyclic module Z/nZ is its exponent: the least
; m > 0 killing every element, equivalently the largest order of an
; element. Computed from the annihilators, not asserted.
(define (elem-order n a)
  (car (filter (lambda (m) (= 0 (r* n m a))) (seq 1 n))))
(define (module-exponent n)
  (foldr (lambda (a acc) (cond ((> (elem-order n a) acc) (elem-order n a)) (t acc)))
         1 (seq 0 (- n 1))))

(check "Z12-invariant-factor-is-12" (= (module-exponent n12) 12))
; A module is cyclic exactly when its exponent equals its order.
(check "Z12-is-cyclic" (= (module-exponent n12) n12))
(check "Z6-invariant-factor-is-6" (= (module-exponent n6) 6))

; Negative control: Z/2 x Z/6 has order 12 but exponent 6, so it is NOT
; cyclic and its invariant factors are (2, 6) rather than (12).
(define (pair-order a b)
  (car (filter (lambda (m) (and (= 0 (mod (* m a) 2)) (= 0 (mod (* m b) 6))))
               (seq 1 12))))
(define z2xz6-orders
  (foldr append nil
         (map (lambda (a) (map (lambda (b) (pair-order a b)) (seq 0 5))) (seq 0 1))))
(check "Z2-times-Z6-is-not-cyclic"
  (and (= 12 (length z2xz6-orders))
       (= 6 (foldr (lambda (o acc) (cond ((> o acc) o) (t acc))) 1 z2xz6-orders))))

(print "--- Faithful Modules ---")
(define ann-M2 (ann-element n6 2))
(check "ann-2Z6-is-0-3" (= (length ann-M2) 2))

(define ann-M3 (ann-element n6 3))
(check "ann-3Z6-is-0-2-4" (= (length ann-M3) 3))

(print "=== Lang Ch3 Done ===")
