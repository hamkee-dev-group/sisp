; ============================================================
; 20 — Group Theory: Z_n under Addition
; Cayley tables, subgroup verification, Lagrange's theorem
; UNIQUE: groups as sets with verified algebraic properties
; ============================================================

(print "=== Group Theory: Z_n ===")

; Z_4 = {0, 1, 2, 3} under addition mod 4
(define Z4 {0 1 2 3})
(print "Z_4 =") (print Z4)

; Group operation: addition mod 4
(define (addmod4 a b) (mod (+ a b) 4))

; --- Cayley Table ---
(print "--- Cayley Table (+ mod 4) ---")
(print "+  | 0 1 2 3")
(print "---+--------")
(define (cayley-row a)
  (list a (addmod4 a 0) (addmod4 a 1) (addmod4 a 2) (addmod4 a 3)))
(print (cayley-row 0))
(print (cayley-row 1))
(print (cayley-row 2))
(print (cayley-row 3))

; --- Verify group axioms ---
(print "--- Group Axioms ---")

; 1. Closure: a+b mod 4 always in {0,1,2,3}
(print "Closure: all results in Z_4?")
(print (and (in (addmod4 2 3) Z4) (in (addmod4 3 3) Z4)))

; 2. Identity: 0 is the identity
(print "Identity (0):")
(print "  0+0=") (print (addmod4 0 0))
(print "  1+0=") (print (addmod4 1 0))
(print "  3+0=") (print (addmod4 3 0))

; 3. Inverses: every element has an inverse
(print "Inverses:")
(print "  inv(0)=0:") (print (addmod4 0 0))
(print "  inv(1)=3:") (print (addmod4 1 3))
(print "  inv(2)=2:") (print (addmod4 2 2))
(print "  inv(3)=1:") (print (addmod4 3 1))

; 4. Associativity: (a+b)+c = a+(b+c)
(print "Associativity:")
(print "  (1+2)+3 =") (print (addmod4 (addmod4 1 2) 3))
(print "  1+(2+3) =") (print (addmod4 1 (addmod4 2 3)))

; --- Subgroups ---
(print "--- Subgroups of Z_4 ---")
; {0} is trivial subgroup
; {0, 2} is a subgroup (Z_2)
(define H {0 2})
(print "H = {0,2}")
(print "Closed? 0+0=0, 0+2=2, 2+0=2, 2+2=") (print (addmod4 2 2))
(print "H is a subgroup!")

; --- Lagrange's Theorem: |H| divides |G| ---
(print "--- Lagrange's Theorem ---")
(print "|Z_4| =") (print (ord Z4))
(print "|H| =") (print (ord H))
(print "|G|/|H| =") (print (/ (ord Z4) (ord H)))
(print "Index [G:H] = 2 (two cosets)")

; Cosets: 0+H and 1+H
(define coset0 H)
(define coset1 {1 3})
(print "Coset 0+H =") (print coset0)
(print "Coset 1+H =") (print coset1)
(print "Disjoint?") (print (equal (cap coset0 coset1) (diff {1} {1})))
(print "Union = Z_4?") (print (equal (union coset0 coset1) Z4))

; --- Generators ---
(print "--- Generators ---")
; 1 generates Z_4: 1, 1+1=2, 1+1+1=3, 1+1+1+1=0
(print "Powers of 1: 1,2,3,0")
(print "1 generates Z_4!")
; 2 generates only {0,2}
(print "Powers of 2: 2,0")
(print "2 generates only H={0,2}")

(print "=== Group Theory Done ===")
