; ============================================================
; 16 — Voting Theory & Social Choice via Sets
; Condorcet paradox, preference aggregation, majority sets
; UNIQUE: model voter preferences as set relations
; ============================================================

(print "=== Voting Theory ===")

; Candidates
(define candidates {1 2 3})
(print "Candidates:") (print candidates)

; 3 voters with strict preference orderings (most to least preferred)
; Voter 1: A > B > C  (1 > 2 > 3)
; Voter 2: B > C > A  (2 > 3 > 1)
; Voter 3: C > A > B  (3 > 1 > 2)
; This is the classic Condorcet cycle!

; Pairwise preferences as sets of (winner . loser)
(define v1-prefs {(1 . 2) (1 . 3) (2 . 3)})
(define v2-prefs {(2 . 3) (2 . 1) (3 . 1)})
(define v3-prefs {(3 . 1) (3 . 2) (1 . 2)})

; Majority rule: pair (a,b) wins if majority prefer a over b
; For (1 vs 2): V1 and V3 prefer 1, V2 prefers 2 → 1 wins (2-1)
; For (2 vs 3): V1 and V2 prefer 2, V3 prefers 3 → 2 wins (2-1)
; For (1 vs 3): V2 and V3 prefer 3, V1 prefers 1 → 3 wins (2-1)

(print "--- Pairwise Majorities ---")
(print "1 vs 2: voters {1,3} prefer 1")
(print "  1 beats 2 (2-1)")
(print "2 vs 3: voters {1,2} prefer 2")
(print "  2 beats 3 (2-1)")
(print "1 vs 3: voters {2,3} prefer 3")
(print "  3 beats 1 (2-1)")

; Majority relation
(define majority {(1 . 2) (2 . 3) (3 . 1)})
(print "Majority relation:") (print majority)

; Check for Condorcet winner (beats all others)
(print "--- Condorcet Winner ---")
(print "1 beats everyone?")
(print (and (in '(1 . 2) majority) (in '(1 . 3) majority)))
(print "2 beats everyone?")
(print (and (in '(2 . 1) majority) (in '(2 . 3) majority)))
(print "3 beats everyone?")
(print (and (in '(3 . 1) majority) (in '(3 . 2) majority)))
(print "No Condorcet winner exists! (Condorcet Paradox)")

; --- Transitivity check ---
(print "--- Transitivity ---")
(print "1>2 and 2>3 but 1>3?")
(print (in '(1 . 3) majority))
(print "NO! Majority preferences are NOT transitive")
(print "This is Arrow's impossibility theorem in action")

; --- Plurality voting ---
(print "--- Plurality (first choice) ---")
; V1 first: 1, V2 first: 2, V3 first: 3
(print "Votes: 1=one, 2=one, 3=one")
(print "Three-way tie! Plurality fails too")

; --- Borda count ---
(print "--- Borda Count ---")
; Points: 1st=2, 2nd=1, 3rd=0
; Candidate 1: 2+0+1 = 3
; Candidate 2: 1+2+0 = 3
; Candidate 3: 0+1+2 = 3
(print "Borda scores: 1=3, 2=3, 3=3")
(print "Perfect three-way tie under all methods!")
(print "The Condorcet paradox demonstrates a fundamental")
(print "impossibility in social choice theory")

(print "=== Voting Theory Done ===")
