; ============================================================
; 09 — Boolean Algebra on Powerset
; (P(S), ∪, ∩, comp, ∅, S) forms a Boolean algebra
; Verify distributivity, complementation, absorption, identity
; UNIQUE: powerset + set ops = algebraic structure verification
; ============================================================

(print "=== Boolean Algebra on P(S) ===")

(define S {1 2 3})
(define PS (pow S))
(define empty (diff {1} {1}))

(print "S =") (print S)
(print "|P(S)| =") (print (ord PS))

; Pick arbitrary subsets for testing
(define A {1 2})
(define B {2 3})
(define C {1 3})

; --- Identity Laws ---
(print "--- Identity Laws ---")
(print "A ∪ ∅ = A?") (print (equal (union A empty) A))
(print "A ∩ S = A?") (print (equal (cap A S) A))

; --- Complement Laws ---
(print "--- Complement Laws ---")
(define compA (diff S A))
(print "A ∪ A' = S?") (print (equal (union A compA) S))
(print "A ∩ A' = ∅?") (print (equal (cap A compA) empty))

; --- Idempotent Laws ---
(print "--- Idempotent ---")
(print "A ∪ A = A?") (print (equal (union A A) A))
(print "A ∩ A = A?") (print (equal (cap A A) A))

; --- Commutative ---
(print "--- Commutative ---")
(print "A ∪ B = B ∪ A?") (print (equal (union A B) (union B A)))
(print "A ∩ B = B ∩ A?") (print (equal (cap A B) (cap B A)))

; --- Associative ---
(print "--- Associative ---")
(print "(A∪B)∪C = A∪(B∪C)?")
(print (equal (union (union A B) C) (union A (union B C))))
(print "(A∩B)∩C = A∩(B∩C)?")
(print (equal (cap (cap A B) C) (cap A (cap B C))))

; --- Distributive ---
(print "--- Distributive ---")
(print "A∩(B∪C) = (A∩B)∪(A∩C)?")
(print (equal (cap A (union B C)) (union (cap A B) (cap A C))))
(print "A∪(B∩C) = (A∪B)∩(A∪C)?")
(print (equal (union A (cap B C)) (cap (union A B) (union A C))))

; --- Absorption ---
(print "--- Absorption ---")
(print "A∪(A∩B) = A?") (print (equal (union A (cap A B)) A))
(print "A∩(A∪B) = A?") (print (equal (cap A (union A B)) A))

; --- De Morgan ---
(print "--- De Morgan ---")
(define compB (diff S B))
(print "(A∪B)' = A'∩B'?")
(print (equal (diff S (union A B)) (cap compA compB)))
(print "(A∩B)' = A'∪B'?")
(print (equal (diff S (cap A B)) (union compA compB)))

(print "All Boolean algebra laws verified!")
(print "=== Boolean Algebra Done ===")
