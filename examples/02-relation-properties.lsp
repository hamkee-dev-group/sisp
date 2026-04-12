; ============================================================
; Relation Properties Checker
; Given a binary relation R on set S (as a list of ordered pairs),
; check reflexive, symmetric, antisymmetric, transitive properties
; and classify as equivalence relation or partial order.
; ============================================================

(print "=== Relation Properties ===")

; Utility: check if pair (a,b) is in the relation
(define (has-pair a b R)
  (if (null R) nil
    (if (and (= (car (car R)) a) (= (cadr (car R)) b))
        t
        (has-pair a b (cdr R)))))

; Convert set to list for iteration
(define (set-to-list s)
  (if (null s) nil
    (cons (car s) (set-to-list (cdr s)))))

; Reflexive: for all x in S, (x,x) in R
(define (check-reflexive-inner sl R)
  (if (null sl) t
    (if (has-pair (car sl) (car sl) R)
        (check-reflexive-inner (cdr sl) R)
        nil)))

(define (is-reflexive S R) (check-reflexive-inner (set-to-list S) R))

; Symmetric: for all (a,b) in R, (b,a) in R
(define (check-symmetric-inner pairs R)
  (if (null pairs) t
    (let ((a (car (car pairs)))
          (b (cadr (car pairs))))
      (if (has-pair b a R)
          (check-symmetric-inner (cdr pairs) R)
          nil))))

(define (is-symmetric R) (check-symmetric-inner R R))

; Antisymmetric: for all (a,b) in R with a != b, (b,a) not in R
(define (check-antisym-inner pairs R)
  (if (null pairs) t
    (let ((a (car (car pairs)))
          (b (cadr (car pairs))))
      (if (= a b)
          (check-antisym-inner (cdr pairs) R)
          (if (has-pair b a R)
              nil
              (check-antisym-inner (cdr pairs) R))))))

(define (is-antisymmetric R) (check-antisym-inner R R))

; Transitive: for all (a,b),(b,c) in R, (a,c) in R
(define (check-trans-c a b pairs R)
  (if (null pairs) t
    (let ((x (car (car pairs)))
          (y (cadr (car pairs))))
      (if (= b x)
          (if (has-pair a y R)
              (check-trans-c a b (cdr pairs) R)
              nil)
          (check-trans-c a b (cdr pairs) R)))))

(define (check-trans-inner pairs R)
  (if (null pairs) t
    (let ((a (car (car pairs)))
          (b (cadr (car pairs))))
      (if (check-trans-c a b R R)
          (check-trans-inner (cdr pairs) R)
          nil))))

(define (is-transitive R) (check-trans-inner R R))

; Classify the relation
(define (classify S R)
  (let* ((refl (is-reflexive S R))
         (sym (is-symmetric R))
         (asym (is-antisymmetric R))
         (trans (is-transitive R)))
    (progn
      (print (cat "  Reflexive:     " (if refl "yes" "no")))
      (print (cat "  Symmetric:     " (if sym "yes" "no")))
      (print (cat "  Antisymmetric: " (if asym "yes" "no")))
      (print (cat "  Transitive:    " (if trans "yes" "no")))
      (if (and refl (and sym trans))
          (print "  => EQUIVALENCE RELATION")
          nil)
      (if (and refl (and asym trans))
          (print "  => PARTIAL ORDER")
          nil))))

; --- Test 1: Equivalence relation (equality on {1,2,3}) ---
(print "--- R1: Equality on {1,2,3} ---")
(define S1 {1 2 3})
(define R1 (list '(1 1) '(2 2) '(3 3)))
(print "R1:")
(print R1)
(classify S1 R1)

; --- Test 2: Partial order (<=) on {1,2,3} ---
(print "--- R2: <= on {1,2,3} ---")
(define R2 (list '(1 1) '(2 2) '(3 3) '(1 2) '(1 3) '(2 3)))
(print "R2:")
(print R2)
(classify S1 R2)

; --- Test 3: Symmetric but not transitive ---
(print "--- R3: Symmetric, reflexive, but not transitive ---")
(define R3 (list '(1 1) '(2 2) '(3 3) '(1 2) '(2 1) '(2 3) '(3 2)))
(print "R3:")
(print R3)
(classify S1 R3)

; --- Test 4: Equivalence relation with classes ---
(print "--- R4: Parity equivalence on {1,2,3,4} ---")
(define S4 {1 2 3 4})
(define R4 (list '(1 1) '(2 2) '(3 3) '(4 4)
                 '(1 3) '(3 1) '(2 4) '(4 2)))
(print "R4:")
(print R4)
(classify S4 R4)

(print "=== Relation Properties Done ===")
