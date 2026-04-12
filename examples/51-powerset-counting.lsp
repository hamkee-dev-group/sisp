; ============================================================
; 51 — Powerset Counting & Subset Lattice
; Verify binomial coefficients by enumerating the powerset
; UNIQUE: powerset is a first-class operation
; ============================================================

(print "=== Powerset Counting ===")

(define S {1 2 3 4})
(define PS (pow S))
(print "S =") (print S)
(print "|P(S)| =") (print (ord PS))

; Verify |P(S)| = 2^|S|
(print "2^|S| =") (print (^ 2 (ord S)))
(print "Match?") (print (= (ord PS) (^ 2 (ord S))))

; --- Check specific elements ---
(print "--- Elements of P(S) ---")
(print "{} in P(S)?")  (print (in (diff {1} {1}) PS))
(print "{1} in P(S)?") (print (in {1} PS))
(print "{1,2} in P(S)?") (print (in {1 2} PS))
(print "{1,2,3} in P(S)?") (print (in {1 2 3} PS))
(print "{1,2,3,4} in P(S)?") (print (in {1 2 3 4} PS))
(print "{5} in P(S)?") (print (in {5} PS))

; --- Subset lattice properties ---
(print "--- Lattice ---")
(define A {1 2})
(define B {2 3})
(print "A =") (print A)
(print "B =") (print B)
(print "Meet (A∩B) =") (print (cap A B))
(print "Join (A∪B) =") (print (union A B))
(print "Both in P(S)?")
(print (and (in (cap A B) PS) (in (union A B) PS)))

; --- Powerset of powerset ---
(define S2 {1 2})
(define PS2 (pow S2))
(define PPS2 (pow PS2))
(print "--- P(P({1,2})) ---")
(print "P({1,2}) =") (print PS2)
(print "|P({1,2})| =") (print (ord PS2))
(print "|P(P({1,2}))| =") (print (ord PPS2))
(print "2^(2^2) = 2^4 =") (print (^ 2 (^ 2 2)))

; --- Binomial identity: sum C(n,k) = 2^n ---
(define (fact n) (if (= n 0) 1 (* n (fact (+ n -1)))))
(define (choose n k) (/ (fact n) (* (fact k) (fact (+ n (* -1 k))))))

(print "--- Binomial Coefficients C(4,k) ---")
(print "C(4,0) =") (print (choose 4 0))
(print "C(4,1) =") (print (choose 4 1))
(print "C(4,2) =") (print (choose 4 2))
(print "C(4,3) =") (print (choose 4 3))
(print "C(4,4) =") (print (choose 4 4))
(define binom-sum (+ (choose 4 0) (choose 4 1) (choose 4 2)
                     (choose 4 3) (choose 4 4)))
(print "Sum =") (print binom-sum)
(print "= 2^4?") (print (= binom-sum (^ 2 4)))

(print "=== Powerset Counting Done ===")
