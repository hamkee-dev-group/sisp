; ============================================================
; 13 — Fixed Point Computation (Knaster-Tarski)
; Iterate monotone operators on sets to find fixpoints
; UNIQUE: reasoning about set lattices natively
; ============================================================

(print "=== Fixed Point Computation ===")

; --- Transitive closure as fixed point ---
; Graph: 1→2, 2→3, 3→4, 4→5
; Reachability from node 1
; F(X) = {1} ∪ {y : ∃x∈X with edge x→y}

; We compute iteratively
(define reach0 {1})
(print "Step 0: reach =") (print reach0)

; Apply F: add nodes reachable from reach0
; 1 has edge to 2
(define reach1 (union reach0 {2}))
(print "Step 1: reach =") (print reach1)

; 2 has edge to 3
(define reach2 (union reach1 {3}))
(print "Step 2: reach =") (print reach2)

; 3 has edge to 4
(define reach3 (union reach2 {4}))
(print "Step 3: reach =") (print reach3)

; 4 has edge to 5
(define reach4 (union reach3 {5}))
(print "Step 4: reach =") (print reach4)

; Step 5: no new nodes — fixed point!
(define reach5 (union reach4 (diff {1} {1})))
(print "Step 5: reach =") (print reach5)
(print "Fixed point reached!")
(print "F(X) = X: ") (print (equal reach4 reach5))

; --- Monotonicity verification ---
(print "--- Monotonicity: X ⊆ Y ⟹ F(X) ⊆ F(Y) ---")
(print "reach0 ⊆ reach1?") (print (subset reach0 reach1))
(print "reach1 ⊆ reach2?") (print (subset reach1 reach2))
(print "reach2 ⊆ reach3?") (print (subset reach2 reach3))
(print "reach3 ⊆ reach4?") (print (subset reach3 reach4))
(print "Chain: ∅ ⊆ {1} ⊆ {1,2} ⊆ ... ⊆ {1,2,3,4,5}")

; --- Closure under addition mod 5 ---
(print "--- Closure under +2 mod 5, starting from {0} ---")
(define c0 {0})
(define c1 (union c0 {2}))
(define c2 (union c1 {4}))
(define c3 (union c2 {1}))
(define c4 (union c3 {3}))
(print "Closed set =") (print c4)
(print "|closed| =") (print (ord c4))
(print "This is all of Z_5!")

; Verify: for each x, (x+2) mod 5 is in the set
(print "0+2=2 in set?") (print (in 2 c4))
(print "2+2=4 in set?") (print (in 4 c4))
(print "4+2≡1 in set?") (print (in 1 c4))
(print "1+2=3 in set?") (print (in 3 c4))
(print "3+2≡0 in set?") (print (in 0 c4))
(print "Fixed point: closed under +2 mod 5")

(print "=== Fixed Point Done ===")
