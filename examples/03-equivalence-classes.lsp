; ============================================================
; 03 — Equivalence Classes & Quotient Sets
; Partition a set by an equivalence relation, prove disjointness
; IMPOSSIBLE in most languages: infinite equivalence classes
; ============================================================

(print "=== Equivalence Classes (mod 3) ===")

(define S {1 2 3 4 5 6 7 8 9})
(define s0 {3 6 9})
(define s1 {1 4 7})
(define s2 {2 5 8})

(print "S =") (print S)
(print "[0] mod 3 =") (print s0)
(print "[1] mod 3 =") (print s1)
(print "[2] mod 3 =") (print s2)

; Verify disjointness
(print "--- Disjointness ---")
(print "c0 cap c1 empty?") (print (equal (cap s0 s1) (diff {1} {1})))
(print "c0 cap c2 empty?") (print (equal (cap s0 s2) (diff {1} {1})))
(print "c1 cap c2 empty?") (print (equal (cap s1 s2) (diff {1} {1})))

; Verify union covers S
(define reconstructed (union (union s0 s1) s2))
(print "Union of all classes =") (print reconstructed)
(print "Equals S?") (print (equal reconstructed S))

; Cardinality check
(print "--- Cardinality ---")
(print "|S| =") (print (ord S))
(print "|c0|+|c1|+|c2| =") (print (+ (ord s0) (ord s1) (ord s2)))

; UNIQUE: infinite equivalence class via comprehension
(define class0 {tau : (equal (mod tau 3) 0)})
(define class1 {tau : (equal (mod tau 3) 1)})
(define class2 {tau : (equal (mod tau 3) 2)})

(print "--- Infinite Classes via Comprehension ---")
(print "12 in [0]?") (print (in 12 class0))
(print "13 in [1]?") (print (in 13 class1))
(print "14 in [2]?") (print (in 14 class2))
(print "99 in [0]?") (print (in 99 class0))
(print "100 in [1]?") (print (in 100 class1))

; The universe is partitioned by these three infinite sets
(define all (union (union class0 class1) class2))
(print "--- Union of infinite classes covers Z ---")
(print "42 in union?") (print (in 42 all))
(print "77 in union?") (print (in 77 all))
(print "0 in union?") (print (in 0 all))

; Infinite sets are disjoint
(define c01 (cap class0 class1))
(print "--- Disjoint infinite classes ---")
(print "6 in class0 cap class1?") (print (in 6 c01))
(print "15 in class0 cap class1?") (print (in 15 c01))

(print "=== Equivalence Classes Done ===")
