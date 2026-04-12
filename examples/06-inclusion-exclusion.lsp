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
(print "|A ∪ B ∪ C| =") (print (ord AuBuC))

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
(define ie (+ (ord A) (ord B) (ord C)
              (* -1 (ord AB))
              (* -1 (ord AC))
              (* -1 (ord BC))
              (ord ABC)))

(print "--- Verification ---")
(print "|A| + |B| + |C| =") (print (+ (ord A) (ord B) (ord C)))
(print "- |A∩B| - |A∩C| - |B∩C| =") (print (* -1 (+ (ord AB) (ord AC) (ord BC))))
(print "+ |A∩B∩C| =") (print (ord ABC))
(print "Formula result =") (print ie)
(print "Direct |A∪B∪C| =") (print (ord AuBuC))
(print "Match?") (print (= ie (ord AuBuC)))

; --- Two-set case ---
(print "--- Two-set case ---")
(define AuB (union A B))
(print "|A∪B| =") (print (ord AuB))
(print "|A|+|B|-|A∩B| =") (print (+ (ord A) (ord B) (* -1 (ord AB))))
(print "Match?") (print (= (ord AuB) (+ (ord A) (ord B) (* -1 (ord AB)))))

; --- Symmetric difference via inclusion-exclusion ---
(print "--- Symmetric Difference ---")
(define sd (symdiff A B))
(print "A △ B =") (print sd)
(print "|A △ B| = |A| + |B| - 2|A∩B|")
(print (= (ord sd) (+ (ord A) (ord B) (* -2 (ord AB)))))

(print "=== Inclusion-Exclusion Done ===")
