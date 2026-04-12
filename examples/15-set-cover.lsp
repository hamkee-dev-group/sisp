; ============================================================
; 15 — Greedy Set Cover
; Given universe U and subsets, find minimal cover
; UNIQUE: native set diff/union makes greedy cover trivial
; ============================================================

(print "=== Greedy Set Cover ===")

; Universe
(define U {1 2 3 4 5 6 7 8 9 10})
(print "Universe U =") (print U)

; Collection of subsets
(define S1 {1 2 3})
(define S2 {2 4 5})
(define S3 {5 6 7 8})
(define S4 {7 8 9 10})
(define S5 {1 3 5 7 9})

(print "S1 =") (print S1)
(print "S2 =") (print S2)
(print "S3 =") (print S3)
(print "S4 =") (print S4)
(print "S5 =") (print S5)

; Greedy: pick set covering most uncovered elements, repeat

; Step 1: Which covers most of U?
; |S1∩U|=3, |S2∩U|=3, |S3∩U|=4, |S4∩U|=4, |S5∩U|=5
(print "--- Greedy Step 1 ---")
(print "S5 covers most:") (print S5) (print "covers 5 elements")
(define remaining1 (diff U S5))
(print "Remaining:") (print remaining1)

; Step 2: best for {2,4,6,8,10}
; S1∩R={2}, S2∩R={2,4}, S3∩R={6,8}, S4∩R={8,10}
(print "--- Greedy Step 2 ---")
(print "|S2∩R|=") (print (ord (cap S2 remaining1)))
(print "|S3∩R|=") (print (ord (cap S3 remaining1)))
(print "|S4∩R|=") (print (ord (cap S4 remaining1)))
(print "S4 covers most of remaining")
(define remaining2 (diff remaining1 S4))
(print "Remaining:") (print remaining2)

; Step 3: remaining = {2,4,6}
(print "--- Greedy Step 3 ---")
(print "|S2∩R|=") (print (ord (cap S2 remaining2)))
(print "S2 covers {2,4}")
(define remaining3 (diff remaining2 S2))
(print "Remaining:") (print remaining3)

; Step 4: remaining = {6}
(print "--- Greedy Step 4 ---")
(print "S3 covers {6}")
(define remaining4 (diff remaining3 S3))
(print "Remaining:") (print remaining4)

; Verify cover
(print "--- Verification ---")
(define cover (union (union S5 S4) (union S2 S3)))
(print "Cover = S5 ∪ S4 ∪ S2 ∪ S3 =") (print cover)
(print "Covers U?") (print (equal (cap cover U) U))
(print "Used 4 subsets (greedy solution)")

(print "=== Set Cover Done ===")
