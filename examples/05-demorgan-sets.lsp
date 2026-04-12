; ============================================================
; 05 — De Morgan's Laws for Sets
; IMPOSSIBLE elsewhere: complement of INFINITE sets, verified
; comp(A ∪ B) = comp(A) ∩ comp(B)
; comp(A ∩ B) = comp(A) ∪ comp(B)
; ============================================================

(print "=== De Morgan's Laws for Sets ===")

; --- Finite sets first ---
(define U {1 2 3 4 5 6 7 8 9 10})
(define A {1 2 3 4 5})
(define B {4 5 6 7 8})

; Complements relative to U
(define compA (diff U A))
(define compB (diff U B))

(print "U =") (print U)
(print "A =") (print A)
(print "B =") (print B)
(print "A' =") (print compA)
(print "B' =") (print compB)

; Law 1: (A ∪ B)' = A' ∩ B'
(define lhs1 (diff U (union A B)))
(define rhs1 (cap compA compB))
(print "--- Law 1: (A ∪ B)' = A' ∩ B' ---")
(print "(A ∪ B)' =") (print lhs1)
(print "A' ∩ B' =") (print rhs1)
(print "Equal?") (print (equal lhs1 rhs1))

; Law 2: (A ∩ B)' = A' ∪ B'
(define lhs2 (diff U (cap A B)))
(define rhs2 (union compA compB))
(print "--- Law 2: (A ∩ B)' = A' ∪ B' ---")
(print "(A ∩ B)' =") (print lhs2)
(print "A' ∪ B' =") (print rhs2)
(print "Equal?") (print (equal lhs2 rhs2))

; --- Now with INFINITE comprehension sets ---
; This is UNIQUE to SISP: complement of infinite sets!
(print "--- De Morgan on INFINITE sets ---")

(define evens {tau : (equal (mod tau 2) 0)})
(define mult3 {tau : (equal (mod tau 3) 0)})

; comp(evens ∪ mult3): numbers that are NEITHER even NOR mult of 3
(define comp-union (comp (union evens mult3)))
; comp(evens) ∩ comp(mult3): odd AND not-mult-3
(define comp-cap (cap (comp evens) (comp mult3)))

(print "Testing comp(evens ∪ mult3) vs comp(evens) ∩ comp(mult3)")
; 5 is odd, not mult of 3 → in both
(print "5 in comp(E∪M)?") (print (in 5 comp-union))
(print "5 in comp(E)∩comp(M)?") (print (in 5 comp-cap))
; 7 is odd, not mult of 3 → in both
(print "7 in comp(E∪M)?") (print (in 7 comp-union))
(print "7 in comp(E)∩comp(M)?") (print (in 7 comp-cap))
; 4 is even → in neither
(print "4 in comp(E∪M)?") (print (in 4 comp-union))
(print "4 in comp(E)∩comp(M)?") (print (in 4 comp-cap))
; 9 is mult of 3 → in neither
(print "9 in comp(E∪M)?") (print (in 9 comp-union))
(print "9 in comp(E)∩comp(M)?") (print (in 9 comp-cap))

(print "De Morgan verified on infinite sets!")
(print "=== De Morgan Done ===")
