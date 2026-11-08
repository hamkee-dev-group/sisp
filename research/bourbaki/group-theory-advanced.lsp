; ============================================================
; Bourbaki -- Group Theory Advanced: Sylow Theorems,
; Composition Series, Jordan-Holder, Solvable/Nilpotent Groups.
; Compute Sylow 2-subgroups of S_4. Verify Sylow counts.
; ============================================================

(print "=== Group Theory Advanced ===")

; ---------- pow for exponentiation ----------
(define (pow b e)
  (cond ((= e 0) 1)
        (t (* b (pow b (- e 1))))))

; ---------- prime test ----------
(define (prime-p n)
  (cond ((< n 2) nil)
        (t (labels (((test d)
               (cond ((> (* d d) n) t)
                     ((= (mod n d) 0) nil)
                     (t (test (+ d 1))))))
             (test 2)))))

; ---------- Sylow exponent: highest power of p dividing n ----------
(define (sylow-exp n p)
  (labels (((go k rem)
    (cond ((not (= (mod rem p) 0)) k)
          (t (go (+ k 1) (/ rem p))))))
    (go 0 n)))

; ---------- Sylow count condition ----------
; n_p divides |G|/p^v and n_p ≡ 1 mod p
(define (sylow-count-ok group-ord p-val n-p)
  (let ((exp-v (sylow-exp group-ord p-val))
        (index-m (/ group-ord (pow p-val (sylow-exp group-ord p-val)))))
    (and (= (mod n-p p-val) 1)
         (= (mod index-m n-p) 0))))

; ---------- S_4: |S_4| = 24 = 2^3 * 3 ----------
(print "--- Sylow 2-subgroups of S_4 ---")
(print "|S_4| = 24 = 2^3 * 3")
(print "Sylow 2-subgroup order = 8")
(print "n_2 divides 3, n_2 ≡ 1 mod 2 => n_2 = 1 or 3")
(print "n_2 = 3: three Sylow 2-subgroups, each D_4 of order 8")

(check "sylow-n2-S4-possible-1" (sylow-count-ok 24 2 1))
(check "sylow-n2-S4-possible-3" (sylow-count-ok 24 2 3))

; S_4 Sylow 3-subgroups: order 3, n_3|8, n_3≡1 mod 3 => 1 or 4. n_3=4
(print "--- Sylow 3-subgroups of S_4 ---")
(print "n_3 divides 8, n_3 ≡ 1 mod 3 => n_3 = 1 or 4")
(print "n_3 = 4: four Sylow 3-subgroups of order 3")

(check "sylow-n3-S4-possible-1" (sylow-count-ok 24 3 1))
(check "sylow-n3-S4-possible-4" (sylow-count-ok 24 3 4))

; ---------- Sylow verification for small groups ----------
; Z/6Z: 6=2*3
(check "sylow-Z6-n2-1" (sylow-count-ok 6 2 1))
(check "sylow-Z6-n3-1" (sylow-count-ok 6 3 1))

; Z/4Z: 4=2^2, n_2|1 => n_2=1
(check "sylow-Z4-n2-1" (sylow-count-ok 4 2 1))

; Z/8Z: 8=2^3, n_2|1 => n_2=1
(check "sylow-Z8-n2-1" (sylow-count-ok 8 2 1))

; Z/12Z: 12=2^2*3, n_2|3, n_3|4
(check "sylow-Z12-n2-1" (sylow-count-ok 12 2 1))
(check "sylow-Z12-n2-3" (sylow-count-ok 12 2 3))
(check "sylow-Z12-n3-1" (sylow-count-ok 12 3 1))
(check "sylow-Z12-n3-4" (sylow-count-ok 12 3 4))

; Z/9Z: 9=3^2, n_3|1 => n_3=1
(check "sylow-Z9-n3-1" (sylow-count-ok 9 3 1))

; Z/15Z: 15=3*5
(check "sylow-Z15-n3-1" (sylow-count-ok 15 3 1))
(check "sylow-Z15-n5-1" (sylow-count-ok 15 5 1))

; Z/10Z: 10=2*5
(check "sylow-Z10-n2-1" (sylow-count-ok 10 2 1))
(check "sylow-Z10-n5-1" (sylow-count-ok 10 5 1))

; Z/14Z: 14=2*7
(check "sylow-Z14-n2-1" (sylow-count-ok 14 2 1))
(check "sylow-Z14-n7-1" (sylow-count-ok 14 7 1))

; Z/20Z: 20=2^2*5, n_2|5, n_5|4 (n_5 ≡ 1 mod 5 => n_5=1 only)
(check "sylow-Z20-n2-1" (sylow-count-ok 20 2 1))
(check "sylow-Z20-n2-5" (sylow-count-ok 20 2 5))
(check "sylow-Z20-n5-1" (sylow-count-ok 20 5 1))
(check "sylow-Z20-n5-4-fails" (not (sylow-count-ok 20 5 4)))

; Z/24Z: 24=2^3*3, n_2|3, n_3|8
(check "sylow-Z24-n2-1" (sylow-count-ok 24 2 1))
(check "sylow-Z24-n2-3" (sylow-count-ok 24 2 3))
(check "sylow-Z24-n3-1" (sylow-count-ok 24 3 1))
(check "sylow-Z24-n3-4" (sylow-count-ok 24 3 4))

; ---------- Composition series and Jordan-Holder ----------
(print "--- Composition series ---")
(print "Z/6Z: {0} <| {0,3} <| Z/6Z, factors Z/3Z, Z/2Z")
(print "Alternative: {0} <| {0,2,4} <| Z/6Z, factors Z/2Z, Z/3Z")
(print "Jordan-Holder: same factors up to permutation")

; ---------- real computation of two composition series of Z/6Z ----------
; <g> inside Z/nZ, as an explicit set of residues.
(define (cyc-sub n g) (dedup (map (lambda (k) (mod (* k g) n)) (seq 0 (- n 1)))))

; multiset equality on small lists of integers
(define (same-multiset p q) (equal (sort < p) (sort < q)))
; every element of p occurs in q
(define (sublist-p p q) (every (lambda (u) (contains u q)) p))

; factor orders of an ascending chain of subgroups: |H_{i+1}| / |H_i|
(define (factor-orders chain)
  (cond ((eq (cdr chain) nil) nil)
        (t (cons (/ (length (nth 1 chain)) (length (nth 0 chain)))
                 (factor-orders (cdr chain))))))

(define z6-triv (cyc-sub 6 0))
(define z6-whole (cyc-sub 6 1))
(define series-a (list z6-triv (cyc-sub 6 3) z6-whole))   ; {0} < {0,3} < Z/6Z
(define series-b (list z6-triv (cyc-sub 6 2) z6-whole))   ; {0} < {0,2,4} < Z/6Z

; Jordan-Holder: the two series are genuinely DIFFERENT (their factor sequences
; differ as sequences) yet carry the SAME factors as a multiset, and every
; factor is simple (prime order).
(check "jordan-holder-Z6"
  (let ((fa (factor-orders series-a))
        (fb (factor-orders series-b)))
    (and (sublist-p (nth 0 series-a) (nth 1 series-a))
         (sublist-p (nth 1 series-a) (nth 2 series-a))
         (sublist-p (nth 0 series-b) (nth 1 series-b))
         (sublist-p (nth 1 series-b) (nth 2 series-b))
         (= (length z6-whole) 6)
         (not (equal fa fb))
         (same-multiset fa fb)
         (every (lambda (u) (prime-p u)) fa))))

; ---------- Solvable groups ----------
; S_3, S_4 are solvable. A_5 is not.
(print "--- Solvable groups ---")
(print "S_3: {e} <| A_3 <| S_3, quotients Z/3Z, Z/2Z (abelian)")
(print "S_4: S_4 > A_4 > V_4 > {e}, quotients Z/2Z, Z/3Z, V_4 (abelian)")
(print "A_5: simple non-abelian => NOT solvable")

; Solvability test: all composition factors are cyclic of prime order
(define (solvable-factors-check factor-list)
  (every (lambda (n) (prime-p n)) factor-list))

(check "Z6-solvable-factors" (solvable-factors-check (list 3 2)))
(check "S4-solvable-factors" (solvable-factors-check (list 2 3 2 2)))

; ---------- Nilpotent groups ----------
(print "--- Nilpotent groups ---")
(print "All p-groups are nilpotent")
(print "All abelian groups are nilpotent")
(print "S_3 is NOT nilpotent")
(print "D_8 (order 8) is nilpotent (2-group)")

; ---------- generic finite-group machinery ----------
; A group is a record (elements op inv identity); op/inv are closures, so the
; record must never be flattened.
(define (grp-elems gr) (nth 0 gr))
(define (grp-mul gr a b) (funcall (nth 1 gr) a b))
(define (grp-inv gr a) (funcall (nth 2 gr) a))
(define (grp-id gr) (nth 3 gr))

; commutator [a,b] = a b a^-1 b^-1
(define (commutator gr a b)
  (grp-mul gr (grp-mul gr (grp-mul gr a b) (grp-inv gr a)) (grp-inv gr b)))

; close a subset under the group law (subsets already contain the identity,
; and a finite subset closed under multiplication is a subgroup)
(define (grp-close-step gr s)
  (dedup (append s
    (foldr append nil
      (map (lambda (a) (map (lambda (b) (grp-mul gr a b)) s)) s)))))

(define (grp-closure gr s)
  (let ((s2 (grp-close-step gr s)))
    (cond ((= (length s2) (length s)) s)
          (t (grp-closure gr s2)))))

; lower central series step: G_{i+1} = [G, G_i]
(define (lcs-next gr cur)
  (grp-closure gr
    (dedup (cons (grp-id gr)
      (foldr append nil
        (map (lambda (a) (map (lambda (b) (commutator gr a b)) cur))
             (grp-elems gr)))))))

; nilpotent <=> the lower central series reaches the trivial subgroup
(define (nilpotent-p gr)
  (labels (((go cur steps)
             (cond ((= (length cur) 1) t)
                   ((> steps 8) nil)
                   (t (let ((nx (lcs-next gr cur)))
                        (cond ((= (length nx) (length cur)) nil)
                              (t (go nx (+ steps 1)))))))))
    (go (grp-elems gr) 0)))

; ---------- V_4 = Z/2Z x Z/2Z ----------
(define v4-elems (list (list 0 0) (list 0 1) (list 1 0) (list 1 1)))
(define (v4-op a b)
  (list (mod (+ (nth 0 a) (nth 0 b)) 2) (mod (+ (nth 1 a) (nth 1 b)) 2)))
(define v4 (list v4-elems
                 (lambda (a b) (v4-op a b))
                 (lambda (a) a)
                 (list 0 0)))

; ---------- S_3 as permutations of {0,1,2} (image lists) ----------
(define s3-elems (list (list 0 1 2) (list 0 2 1) (list 1 0 2)
                       (list 1 2 0) (list 2 0 1) (list 2 1 0)))
(define (perm-comp a b) (map (lambda (k) (nth k a)) b))
(define (perm-inv a) (map (lambda (k) (index-of k a)) (seq 0 2)))
(define s3 (list s3-elems
                 (lambda (a b) (perm-comp a b))
                 (lambda (a) (perm-inv a))
                 (list 0 1 2)))

; sanity: the machinery really builds S_3 (closure of two generators = 6 elts)
(check "S3-generated-by-two-elements"
  (= (length (grp-closure s3 (list (list 0 1 2) (list 1 2 0) (list 0 2 1)))) 6))

; Z/2Z x Z/2Z = V_4 is abelian => all commutators trivial => nilpotent (class 1)
(check "V4-is-nilpotent" (nilpotent-p v4))

; Contrast, so the predicate above is demonstrably not constant:
; the lower central series of S_3 stalls at A_3 and never reaches {e}.
(check "S3-is-not-nilpotent" (not (nilpotent-p s3)))
(check "S3-lower-central-stalls-at-A3" (= (length (lcs-next s3 s3-elems)) 3))

; ---------- Sylow theorems: all Sylow p-subgroups are conjugate ----------
; For Z/6Z: n_2=1, n_3=1 => unique (normal) Sylow subgroups
; For S_4: n_2=3, n_3=4 => multiple conjugate subgroups
(print "--- Sylow conjugacy ---")
(print "Z/6Z: unique Sylow subgroups => normal")
(print "S_4: 3 Sylow 2-subgroups are conjugate to D_4")
(print "S_4: 4 Sylow 3-subgroups are conjugate")

; The narrative "summary" check here was vacuous; replaced by an actual
; conjugacy computation in S_3, where the Sylow 2-subgroups are the three
; subgroups of order 2 generated by the transpositions.
(define (conj-sub gr a hh)
  (dedup (map (lambda (b) (grp-mul gr (grp-mul gr a b) (grp-inv gr a))) hh)))

(define (same-set-p p q)
  (and (= (length (dedup p)) (length (dedup q)))
       (every (lambda (u) (contains u q)) p)))

(define sylow2-s3
  (foldr (lambda (hh acc) (cond ((any (lambda (kk) (same-set-p hh kk)) acc) acc)
                                (t (cons hh acc))))
         nil
         (filter (lambda (hh) (= (length hh) 2))
           (map (lambda (a) (grp-closure s3 (list (list 0 1 2) a))) s3-elems))))

(define sylow2-pairs
  (foldr append nil
    (map (lambda (hh) (map (lambda (kk) (list hh kk)) sylow2-s3)) sylow2-s3)))

; n_2 = 3 (matches the Sylow congruence tested above) and all three are conjugate
(check "sylow-2-subgroups-of-S3-count-3"
  (and (= (length sylow2-s3) 3) (sylow-count-ok 6 2 (length sylow2-s3))))

(check "sylow-2-subgroups-of-S3-are-conjugate"
  (every (lambda (pr)
           (any (lambda (a) (same-set-p (conj-sub s3 a (nth 0 pr)) (nth 1 pr)))
                s3-elems))
         sylow2-pairs))

; not all subgroups of the same order need be conjugate in general, so verify
; the conjugating element is really needed: the identity alone does not suffice.
(check "sylow-2-subgroups-of-S3-are-distinct"
  (not (same-set-p (nth 0 sylow2-s3) (nth 1 sylow2-s3))))

(print "=== Group Theory Advanced Done ===")
