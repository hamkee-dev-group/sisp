; ============================================================
; 06 — Inclusion-Exclusion Principle
; |A∪B∪C| = |A|+|B|+|C| - |A∩B| - |A∩C| - |B∩C| + |A∩B∩C|
; Verified with native set ops — trivial in SISP, painful elsewhere
; ============================================================

(print "=== Inclusion-Exclusion Principle ===")

(define A {1 2 3 4 5 6})
(define B {4 5 6 7 8 9})
(define C {2 4 6 8 10})

(print "A =") (print A)
(print "B =") (print B)
(print "C =") (print C)

; Direct computation
(define AuBuC (union (union A B) C))
(print "A ∪ B ∪ C =") (print AuBuC)
(print "|A ∪ B ∪ C| =") (print (length AuBuC))

; Pairwise intersections
(define AB (cap A B))
(define AC (cap A C))
(define BC (cap B C))
(define ABC (cap (cap A B) C))

(print "A ∩ B =") (print AB)
(print "A ∩ C =") (print AC)
(print "B ∩ C =") (print BC)
(print "A ∩ B ∩ C =") (print ABC)

; Inclusion-exclusion formula
(define ie (+ (length A) (length B) (length C)
              (* -1 (length AB))
              (* -1 (length AC))
              (* -1 (length BC))
              (length ABC)))

(print "--- Verification ---")
(print "|A| + |B| + |C| =") (print (+ (length A) (length B) (length C)))
(print "- |A∩B| - |A∩C| - |B∩C| =") (print (* -1 (+ (length AB) (length AC) (length BC))))
(print "+ |A∩B∩C| =") (print (length ABC))
(print "Formula result =") (print ie)
(print "Direct |A∪B∪C| =") (print (length AuBuC))
(print "Match?") (print (= ie (length AuBuC)))

; --- Two-set case ---
(print "--- Two-set case ---")
(define AuB (union A B))
(print "|A∪B| =") (print (length AuB))
(print "|A|+|B|-|A∩B| =") (print (+ (length A) (length B) (* -1 (length AB))))
(print "Match?") (print (= (length AuB) (+ (length A) (length B) (* -1 (length AB)))))

; --- Symmetric difference via inclusion-exclusion ---
(print "--- Symmetric Difference ---")
(define sd (symdiff A B))
(print "A △ B =") (print sd)
(print "|A △ B| = |A| + |B| - 2|A∩B|")
(print (= (length sd) (+ (length A) (length B) (* -2 (length AB)))))

(print "=== Inclusion-Exclusion Done ===")
