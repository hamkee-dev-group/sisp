; ============================================================
; 40 — Pigeonhole Principle via Sets
; If |A| > |B|, any f:A→B has collisions
; UNIQUE: cardinality reasoning with native set ops
; ============================================================

(print "=== Pigeonhole Principle ===")

(define pigeons {1 2 3 4 5})
(define holes {a b c})
(print "Pigeons:") (print pigeons)
(print "Holes:") (print holes)
(print "|Pigeons|=5 > |Holes|=3: collision guaranteed!")

; Assignment
(print "--- Assignment: 1→a, 2→b, 3→a, 4→c, 5→b ---")

; Group by hole using sets
(define hole-a {1 3})
(define hole-b {2 5})
(define hole-c {4})

(print "Hole a:") (print hole-a) (print "|=") (print (ord hole-a))
(print "Hole b:") (print hole-b) (print "|=") (print (ord hole-b))
(print "Hole c:") (print hole-c) (print "|=") (print (ord hole-c))

; Find collisions
(print "--- Collisions ---")
(if (> (ord hole-a) 1) (print "Hole a has collision!") nil)
(if (> (ord hole-b) 1) (print "Hole b has collision!") nil)

; Verify partition: holes partition pigeons
(print "--- Partition check ---")
(define all-assigned (union (union hole-a hole-b) hole-c))
(print "Union = Pigeons?") (print (equal all-assigned pigeons))
(print "a∩b empty?") (print (equal (cap hole-a hole-b) (diff {1} {1})))
(print "a∩c empty?") (print (equal (cap hole-a hole-c) (diff {1} {1})))
(print "b∩c empty?") (print (equal (cap hole-b hole-c) (diff {1} {1})))

; --- Generalized: Cartesian product ---
(print "--- All possible assignments ---")
(define all-maps (prod pigeons holes))
(print "|pigeons × holes| =") (print (ord all-maps))
(print "For ANY function f:pigeons→holes,")
(print "some hole must contain >= ceil(5/3) = 2 pigeons")

; --- Divisibility application ---
(print "--- Among 5 numbers, 2 have same remainder mod 3 ---")
(define remainders {0 1 2})
(print "Possible remainders:") (print remainders)
(print "5 numbers, 3 remainders → pigeonhole!")

; Even stronger: among 7 numbers, at least 3 share a remainder mod 3
(print "--- Among 7 numbers, >= 3 share a remainder mod 3 ---")
(print "7 numbers, 3 remainders, ceil(7/3) = 3")

(print "=== Pigeonhole Done ===")
