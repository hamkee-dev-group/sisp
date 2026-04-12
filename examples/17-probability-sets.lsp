; ============================================================
; 17 — Probability via Set Theory
; Sample spaces as sets, events as subsets, exact rational probs
; UNIQUE: exact probability with set ops + rational arithmetic
; ============================================================

(print "=== Probability via Set Theory ===")

; --- Two dice: sample space = {1..6} × {1..6} ---
(define die {1 2 3 4 5 6})
(define omega (prod die die))
(print "Sample space |Ω| =") (print (ord omega))

; P(event) = |event| / |Ω| — exact as rational!

; Event A: sum = 7
; Pairs: (1,6)(2,5)(3,4)(4,3)(5,2)(6,1)
(define sum7 {(1 . 6) (2 . 5) (3 . 4) (4 . 3) (5 . 2) (6 . 1)})
(print "--- Event A: sum = 7 ---")
(print "|A| =") (print (ord sum7))
(print "P(A) = |A|/|Ω| =") (print (/ (ord sum7) (ord omega)))

; Event B: at least one die shows 1
(define has1 {(1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (1 . 6)
              (2 . 1) (3 . 1) (4 . 1) (5 . 1) (6 . 1)})
(print "--- Event B: at least one 1 ---")
(print "|B| =") (print (ord has1))
(print "P(B) =") (print (/ (ord has1) (ord omega)))

; Event C: both dice even
(define both-even {(2 . 2) (2 . 4) (2 . 6) (4 . 2) (4 . 4) (4 . 6)
                   (6 . 2) (6 . 4) (6 . 6)})
(print "--- Event C: both even ---")
(print "P(C) =") (print (/ (ord both-even) (ord omega)))

; --- Set operations on events ---
(print "--- P(A ∪ B) ---")
(define AuB (union sum7 has1))
(print "P(A∪B) =") (print (/ (ord AuB) (ord omega)))

(print "--- P(A ∩ B) ---")
(define AcapB (cap sum7 has1))
(print "A∩B =") (print AcapB)
(print "P(A∩B) =") (print (/ (ord AcapB) (ord omega)))

; Verify: P(A∪B) = P(A) + P(B) - P(A∩B)
(print "--- Verify addition rule ---")
(define lhs (/ (ord AuB) (ord omega)))
(define rhs (+ (/ (ord sum7) (ord omega))
               (/ (ord has1) (ord omega))
               (* -1 (/ (ord AcapB) (ord omega)))))
(print "P(A∪B) =") (print lhs)
(print "P(A)+P(B)-P(A∩B) =") (print rhs)
(print "Equal?") (print (= lhs rhs))

; --- Conditional probability: P(A|B) = P(A∩B)/P(B) ---
(print "--- Conditional Probability ---")
(define pAgivenB (/ (ord AcapB) (ord has1)))
(print "P(sum=7 | at least one 1) =") (print pAgivenB)

; --- Complement ---
(print "--- P(A') = 1 - P(A) ---")
(define pA (/ (ord sum7) (ord omega)))
(define pAcomp (+ 1 (* -1 pA)))
(print "P(A') =") (print pAcomp)
(print "P(A) + P(A') = 1?") (print (= (+ pA pAcomp) 1))

(print "=== Probability Done ===")
