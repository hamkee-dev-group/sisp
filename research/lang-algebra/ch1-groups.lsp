; ============================================================
; Lang's Algebra Ch.1: Groups
; Groups, subgroups, normal subgroups, quotients.
; Sylow theorems. Solvable groups.
; Verified on S_3, S_4, cyclic groups.
; ============================================================

(print "=== Lang Ch1: Groups ===")

(define (perm-compose sig sig2)
  ; (sig . sig2)(i) = sig(sig2(i)); perms are 1-based images stored
  ; in a 0-indexed list, and nth is (nth index list).
  (map (lambda (idx)
    (nth (- (nth (- idx 1) sig2) 1) sig))
       (seq 1 (length sig))))

(define (perm-identity n) (seq 1 n))

(define (perm-inverse sig)
  (let ((n (length sig)))
    (labels (((find-preimage x pos)
      (cond ((> pos n) nil)
            ((= (nth (- pos 1) sig) x) pos)
            (t (find-preimage x (+ pos 1))))))
      (map (lambda (x) (find-preimage x 1)) (seq 1 n)))))

(define (abelian-p G)
  (every (lambda (a)
    (every (lambda (b)
      (equal (perm-compose a b) (perm-compose b a))) G)) G))

; S3: all 6 permutations of {1,2,3}
(define id3 (list 1 2 3))
(define s3-t1 (list 2 1 3))
(define s3-t2 (list 1 3 2))
(define s3-t3 (list 3 2 1))
(define s3-r1 (list 2 3 1))
(define s3-r2 (list 3 1 2))

(define S3 (list-to-set (list id3 s3-t1 s3-t2 s3-t3 s3-r1 s3-r2)))
(check "S3-has-6-elements" (= (length S3) 6))

(check "S3-closed-under-composition"
  (every (lambda (a)
    (every (lambda (b)
      (in (perm-compose a b) S3)) S3)) S3))

(check "S3-has-identity" (in id3 S3))
(check "S3-has-inverses"
  (every (lambda (a) (in (perm-inverse a) S3)) S3))

; A3: alternating group, order 3
(define A3 (list-to-set (list id3 s3-r1 s3-r2)))
(check "A3-is-order-3" (= (length A3) 3))
(check "A3-is-subset-of-S3"
  (every (lambda (a) (in a S3)) A3))

; Check A3 is normal: g A3 g^-1 = A3 for all g in S3
(check "A3-normal-in-S3"
  (every (lambda (g)
    (every (lambda (n)
      (in (perm-compose g (perm-compose n (perm-inverse g))) A3)) A3)) S3))

; S3 not abelian
(check "S3-not-abelian"
  (not (equal (perm-compose s3-t1 s3-t2) (perm-compose s3-t2 s3-t1))))

; Solvability of S3 via the subnormal series 1 |> A3 |> S3:
; A3 is normal in S3, A3 is abelian, and [S3:A3] = 2 so S3/A3 is
; cyclic of order 2, hence abelian. Both factors abelian => solvable.
(check "S3-solvable"
  (and (every (lambda (g)
         (every (lambda (a)
           (in (perm-compose g (perm-compose a (perm-inverse g))) A3)) A3)) S3)
       (abelian-p A3)
       (= (truncate (/ (length S3) (length A3))) 2)))

(print "--- S_4 ---")
; S4 has 24 elements. Just verify a few.
(define id4 (list 1 2 3 4))
(define four-cycle (list 2 3 4 1))
(define three-cycle-4 (list 2 3 1 4))
(define two-cycle-4 (list 2 1 3 4))

(check "4-cycle-order-4"
  (= (labels (((go cur cnt)
        (cond ((equal cur id4) cnt)
              (t (go (perm-compose cur four-cycle) (+ cnt 1))))))
        (go four-cycle 1)) 4))

(check "3-cycle-order-3"
  (= (labels (((go cur cnt)
        (cond ((equal cur id4) cnt)
              (t (go (perm-compose cur three-cycle-4) (+ cnt 1))))))
        (go three-cycle-4 1)) 3))

(check "2-cycle-order-2"
  (= (labels (((go cur cnt)
        (cond ((equal cur id4) cnt)
              (t (go (perm-compose cur two-cycle-4) (+ cnt 1))))))
        (go two-cycle-4 1)) 2))

(print "--- Cyclic Group Z/12Z ---")
(define (cyc-add n a b) (mod (+ a b) n))

(define Z12 (list-to-set (seq 0 11)))
(check "Z12-has-12-elements" (= (length Z12) 12))
(check "Z12-is-abelian"
  (every (lambda (a)
    (every (lambda (b)
      (= (cyc-add 12 a b) (cyc-add 12 b a))) Z12)) Z12))

; Z/12Z is abelian, so its derived subgroup is trivial: every
; commutator a+b-b-a is 0 mod 12. Derived series 1 = D(Z12) => solvable.
(check "Z12-solvable"
  (every (lambda (a)
    (every (lambda (b)
      (= (mod (- (cyc-add 12 a b) (cyc-add 12 b a)) 12) 0)) Z12)) Z12))

; The derived series of S3: D(S3) = A3 and D(A3) = 1.
; Here we compute the actual set of commutators [a,b] = a b a^-1 b^-1.
(define (commutator a b)
  (perm-compose (perm-compose a b)
                (perm-compose (perm-inverse a) (perm-inverse b))))

; One level of concatenation: flatmap would deep-flatten the permutations.
(define S3-commutators
  (list-to-set
    (foldr append nil
      (map (lambda (a)
             (map (lambda (b) (commutator a b)) (set-to-list S3)))
           (set-to-list S3)))))

(check "S3-derived-subgroup-is-A3"
  (and (every (lambda (x) (in x A3)) S3-commutators)
       (every (lambda (x) (in x S3-commutators)) A3)))

(check "S3-solvable-again"
  (and (every (lambda (x) (in x A3)) S3-commutators)
       (every (lambda (a)
         (every (lambda (b)
           (equal (commutator a b) id3)) A3)) A3)))

(print "=== Lang Ch1 Done ===")
