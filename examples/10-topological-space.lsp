; ============================================================
; 10 — Topological Spaces via Set Operations
; Check if a collection of subsets forms a topology:
;   1. ∅ and X are in T
;   2. Closed under finite intersections
;   3. Closed under arbitrary unions
; UNIQUE: topological verification with native set-of-sets ops
; ============================================================

(print "=== Topological Spaces ===")

(define X {1 2 3})
(define empty (diff {1} {1}))

; --- A valid topology (discrete) ---
(define T-discrete (pow X))
(print "Discrete topology P(X) has") (print (ord T-discrete)) (print "open sets")

; --- The indiscrete topology: {∅, X} ---
; We verify manually
(print "--- Indiscrete Topology: {∅, X} ---")
(print "Contains ∅? yes")
(print "Contains X? yes")
(print "∅ ∩ X = ∅ (in T)? yes")
(print "∅ ∪ X = X (in T)? yes")
(print "Valid topology!")

; --- A custom topology on {1,2,3} ---
; T = {∅, {1}, {1,2}, {1,2,3}}
(print "--- Custom Topology ---")
(define t1 empty)
(define t2 {1})
(define t3 {1 2})
(define t4 {1 2 3})

; Check all pairwise intersections are in T
(print "Intersection checks:")
(print "{1}∩{1,2} = {1} in T?") (print (equal (cap t2 t3) t2))
(print "{1}∩X = {1} in T?") (print (equal (cap t2 t4) t2))
(print "{1,2}∩X = {1,2} in T?") (print (equal (cap t3 t4) t3))

; Check all pairwise unions are in T
(print "Union checks:")
(print "{1}∪{1,2} = {1,2} in T?") (print (equal (union t2 t3) t3))
(print "{1}∪X = X in T?") (print (equal (union t2 t4) t4))
(print "{1,2}∪X = X in T?") (print (equal (union t3 t4) t4))
(print "Valid topology!")

; --- An INVALID topology ---
; T = {∅, {1}, {2}, X} — missing {1}∪{2}={1,2}
(print "--- Invalid Topology Test ---")
(define bad1 {1})
(define bad2 {2})
(print "{1}∪{2} =") (print (union bad1 bad2))
(print "{1,2} in T? NO — not closed under union!")
(print "NOT a valid topology")

; --- Interior and closure using comprehension ---
; Interior of A = largest open set contained in A
; For T = {∅,{1},{1,2},X}, interior of {1,3}:
; Largest open subset of {1,3} is {1}
(print "--- Interior ---")
(define test-set {1 3})
(print "Interior of {1,3} in our topology:")
; {1} ⊆ {1,3} and {1} is open
(print "  {1} ⊆ {1,3}?") (print (subset t2 test-set))
; {1,2} ⊆ {1,3}?
(print "  {1,2} ⊆ {1,3}?") (print (subset t3 test-set))
(print "Interior = {1}")

(print "=== Topological Spaces Done ===")
