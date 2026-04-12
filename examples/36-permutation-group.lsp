; ============================================================
; 36-permutation-group.lsp
; Represent permutations as lists. Compose permutations.
; Find the order of a permutation. Verify S_3 has 6 elements
; and demonstrate group properties (closure, identity, inverse).
; ============================================================

(print "=== Permutation Groups ===")

; --- Helpers ---
(define (my-append a b)
  (if (equal a nil) b
    (cons (car a) (my-append (cdr a) b))))

(define (remove-first x lst)
  (if (equal lst nil) nil
    (if (equal (car lst) x) (cdr lst)
        (cons (car lst) (remove-first x (cdr lst))))))

(define (prepend-all x lsts)
  (if (equal lsts nil) nil
    (cons (cons x (car lsts)) (prepend-all x (cdr lsts)))))

; --- Generate all permutations of a list ---
(define (perms lst)
  (if (equal lst nil) (list nil)
    (labels (((helper remaining)
       (if (equal remaining nil) nil
           (my-append
             (prepend-all (car remaining)
                          (perms (remove-first (car remaining) lst)))
             (helper (cdr remaining))))))
      (helper lst))))

; --- Apply permutation: perm[i] gives the image of position i ---
(define (apply-perm perm i)
  (nth i perm))

; --- Compose two permutations: (compose p q)[i] = p[q[i]] ---
(define (compose p q)
  (labels (((build i)
    (if (> i (ord q)) nil
      (cons (apply-perm p (apply-perm q i))
            (build (+ i 1))))))
    (build 1)))

; --- Identity permutation of size n ---
(define (identity-perm n) (seq 1 n))

; --- Check if a permutation is the identity ---
(define (is-identity p)
  (equal p (identity-perm (ord p))))

; --- Order of a permutation: smallest k > 0 such that p^k = identity ---
(define (perm-order p)
  (labels (((find-order current k)
    (if (is-identity current) k
      (find-order (compose p current) (+ k 1)))))
    (find-order p 1)))

; --- Find inverse: compose until we get identity, then take p^(k-1) ---
(define (perm-inverse p)
  (let ((k (perm-order p)))
    (labels (((power perm n)
      (if (= n 1) perm
        (power (compose p perm) (+ n -1)))))
      (if (= k 1) p
        (power p (+ k -1))))))

; ============================================================
; Generate S_3 (all permutations of {1,2,3})
; ============================================================
(print "--- S_3: Symmetric group on {1,2,3} ---")
(define S3 (perms '(1 2 3)))
(print "Elements of S_3:")
(print S3)
(print "Order of S_3: |S_3| =")
(print (ord S3))
(if (= (ord S3) 6)
    (print "VERIFIED: |S_3| = 6 = 3!")
    (print "FAILED"))

; ============================================================
; Identity element
; ============================================================
(print "--- Identity ---")
(define e (identity-perm 3))
(print "Identity permutation:")
(print e)

; ============================================================
; Closure: composing any two permutations in S_3 gives another in S_3
; ============================================================
(print "--- Closure check ---")
(define (my-memberp x lst)
  (if (equal lst nil) nil
    (if (equal (car lst) x) t
        (my-memberp x (cdr lst)))))

(define (check-closure perms-list all-perms)
  (if (equal perms-list nil) t
    (labels (((check-one p others)
      (if (equal others nil) t
        (if (my-memberp (compose p (car others)) all-perms)
            (check-one p (cdr others))
            nil))))
      (if (check-one (car perms-list) all-perms)
          (check-closure (cdr perms-list) all-perms)
          nil))))

(if (check-closure S3 S3)
    (print "VERIFIED: S_3 is closed under composition")
    (print "FAILED: Not closed"))

; ============================================================
; Orders of individual permutations
; ============================================================
(print "--- Orders of permutations ---")
(define (show-order p)
  (print (list p (perm-order p))))

(show-order '(1 2 3))
(show-order '(1 3 2))
(show-order '(2 1 3))
(show-order '(2 3 1))
(show-order '(3 1 2))
(show-order '(3 2 1))

; ============================================================
; Inverses
; ============================================================
(print "--- Inverse check ---")
(define p1 '(2 3 1))
(define p1-inv (perm-inverse p1))
(print "Permutation:")
(print p1)
(print "Inverse:")
(print p1-inv)
(print "p * p^-1 =")
(print (compose p1 p1-inv))
(if (is-identity (compose p1 p1-inv))
    (print "VERIFIED: p * p^-1 = identity")
    (print "FAILED"))

(print "=== Permutation Groups Complete ===")
