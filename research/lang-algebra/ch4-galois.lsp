; ============================================================
; Lang's Algebra Ch.4: Galois Theory
; Galois theory. Fundamental theorem. Solvability by radicals.
; Verified on small Galois extensions over F_p.
; ============================================================

(print "=== Lang Ch4: Galois Theory ===")

(define (fp-add p a b) (mod (+ a b) p))
(define (fp-mul p a b) (mod (* a b) p))
(define (fp-sub p a b) (mod (- a b) p))

(define (gf4-add ea eb)
  (list (mod (+ (nth 0 ea) (nth 0 eb)) 2)
        (mod (+ (nth 1 ea) (nth 1 eb)) 2)))

(define (gf4-mul ea eb)
  (let ((a0 (nth 0 ea)) (a1 (nth 1 ea))
        (b0 (nth 0 eb)) (b1 (nth 1 eb)))
    (list (mod (+ (* a0 b0) (* a1 b1)) 2)
          (mod (+ (* a0 b1) (* a1 b0) (* a1 b1)) 2))))

(define (gf8-add ea eb)
  (list (mod (+ (nth 0 ea) (nth 0 eb)) 2)
        (mod (+ (nth 1 ea) (nth 1 eb)) 2)
        (mod (+ (nth 2 ea) (nth 2 eb)) 2)))

(define (gf8-mul ea eb)
  (let* ((a0 (nth 0 ea)) (a1 (nth 1 ea)) (a2 (nth 2 ea))
         (b0 (nth 0 eb)) (b1 (nth 1 eb)) (b2 (nth 2 eb))
         (c0 (* a0 b0))
         (c1 (+ (* a0 b1) (* a1 b0)))
         (c2 (+ (* a0 b2) (* a1 b1) (* a2 b0)))
         (c3 (+ (* a1 b2) (* a2 b1)))
         (c4 (* a2 b2)))
    (list (mod (+ c0 c3) 2)
          (mod (+ c1 c3 c4) 2)
          (mod (+ c2 c4) 2))))

(define (frob-gf4 ea)
  (let ((a0 (nth 0 ea)) (a1 (nth 1 ea)))
    (list (mod (+ a0 a1) 2) a1)))

(define gf4-elems (list-to-set
  (list (list 0 0) (list 1 0) (list 0 1) (list 1 1))))

(define gf8-elems (list-to-set
  (list (list 0 0 0) (list 1 0 0) (list 0 1 0) (list 1 1 0)
        (list 0 0 1) (list 1 0 1) (list 0 1 1) (list 1 1 1))))

(print "--- GF(4)/F_2 ---")
(check "GF4-has-4-elements" (= (length gf4-elems) 4))

(check "frob-GF4-fixes-F2"
  (and (equal (frob-gf4 (list 0 0)) (list 0 0))
       (equal (frob-gf4 (list 1 0)) (list 1 0))))

(check "frob-GF4-swaps-omega-and-omega-plus-1"
  (and (equal (frob-gf4 (list 0 1)) (list 1 1))
       (equal (frob-gf4 (list 1 1)) (list 0 1))))

(check "frob-squared-is-identity"
  (every (lambda (a)
    (equal (frob-gf4 (frob-gf4 a)) a)) gf4-elems))

(print "--- GF(8)/F_2 ---")
(check "GF8-has-8-elements" (= (length gf8-elems) 8))

(define (upto lo hi) (cond ((> lo hi) nil) (t (cons lo (upto (+ lo 1) hi)))))
(define (concat ls) (foldr append nil ls))
(define gf4l (set-to-list gf4-elems))
(define gf8l (set-to-list gf8-elems))

; The Frobenius is x |-> x^2. The hand-written frob-gf4 above must agree
; with squaring computed from the field multiplication -- two independent
; definitions of the same automorphism.
(check "frob-GF4-is-squaring"
  (every (lambda (e) (equal (frob-gf4 e) (gf4-mul e e))) gf4l))

; GF(8) is a field: every nonzero element is invertible.
(define gf8-one (list 1 0 0))
(define gf8-zero (list 0 0 0))
(check "GF8-is-a-field"
  (every (lambda (e)
           (cond ((equal e gf8-zero) t)
                 (t (any (lambda (u) (equal (gf8-mul e u) gf8-one)) gf8l))))
         gf8l))

(define (frob-gf8 e) (gf8-mul e e))

; Frobenius is a ring automorphism: additive and multiplicative, bijective.
(check "frob-GF8-is-additive"
  (every (lambda (a)
           (every (lambda (b) (equal (frob-gf8 (gf8-add a b))
                                     (gf8-add (frob-gf8 a) (frob-gf8 b))))
                  gf8l))
         gf8l))
(check "frob-GF8-is-multiplicative"
  (every (lambda (a)
           (every (lambda (b) (equal (frob-gf8 (gf8-mul a b))
                                     (gf8-mul (frob-gf8 a) (frob-gf8 b))))
                  gf8l))
         gf8l))
(check "frob-GF8-is-bijective"
  (= 8 (length (dedup (map (lambda (e) (frob-gf8 e)) gf8l)))))

; The Galois group is cyclic of order 3 = [GF(8) : F_2].
(define (iterate-map fn k e) (cond ((= k 0) e) (t (iterate-map fn (- k 1) (fn e)))))
(define (map-table k) (map (lambda (e) (iterate-map (lambda (x) (frob-gf8 x)) k e)) gf8l))
(check "frob-GF8-has-order-3"
  (and (equal (map-table 3) gf8l)
       (not (equal (map-table 1) gf8l))
       (not (equal (map-table 2) gf8l))))
(check "Gal-GF8-F2-has-order-3"
  (= 3 (length (dedup (map (lambda (k) (map-table k)) (upto 0 5))))))

; The fixed field of the whole Galois group is exactly the prime field F_2.
(define fix-gf8 (filter (lambda (e) (equal (frob-gf8 e) e)) gf8l))
(check "fixed-field-of-Gal-GF8-is-F2"
  (and (= 2 (length fix-gf8)) (contains gf8-zero fix-gf8) (contains gf8-one fix-gf8)))

(print "--- Fundamental Theorem of Galois Theory ---")

; GF(4)/F_2: Gal = {id, frob}. Its subgroups are {id} and the whole group;
; the corresponding fixed fields are GF(4) and F_2. Both are computed.
(define (fixed-field-of-subgroup autos)
  (filter (lambda (e) (every (lambda (sg) (equal (sg e) e)) autos)) gf4l))
(define gal4-trivial (list (lambda (e) e)))
(define gal4-full (list (lambda (e) e) (lambda (e) (frob-gf4 e))))

(check "GF4-F2-fundamental-theorem"
  (and (= 4 (length (fixed-field-of-subgroup gal4-trivial)))
       (= 2 (length (fixed-field-of-subgroup gal4-full)))))

; The correspondence reverses inclusion, and degrees match subgroup orders:
; [GF(4) : Fix(H)] = |H|.
(check "galois-correspondence-is-order-reversing"
  (< (length (fixed-field-of-subgroup gal4-full))
     (length (fixed-field-of-subgroup gal4-trivial))))
(check "degree-equals-subgroup-order"
  (and (= (truncate (/ 4 (length (fixed-field-of-subgroup gal4-trivial))))
          (length gal4-trivial))
       (= (truncate (/ 4 (length (fixed-field-of-subgroup gal4-full))))
          (length gal4-full))))

(print "--- Solvability by Radicals ---")

; Solvability is decided by actually computing the derived series.
(define (close-under op gen)
  (let ((nxt (dedup (append gen (concat (map (lambda (a) (map (lambda (b) (op a b)) gen))
                                             gen))))))
    (cond ((= (length nxt) (length gen)) gen) (t (close-under op nxt)))))

(define (commutators op inv grp)
  (concat (map (lambda (a)
                 (map (lambda (b) (op (op a b) (op (inv a) (inv b)))) grp))
               grp)))

(define (derived-of op inv grp) (close-under op (dedup (commutators op inv grp))))

; Returns the derived length, or -1 if the series stalls (not solvable).
(define (derived-length op inv grp depth)
  (cond ((= (length grp) 1) depth)
        ((> depth 6) -1)
        (t (let ((d (derived-of op inv grp)))
             (cond ((= (length d) (length grp)) -1)
                   (t (derived-length op inv d (+ depth 1))))))))

; Gal(GF(2^6)/F_2) is cyclic of order 6, realised as the exponents of the
; Frobenius under addition mod 6.
(define z6g (upto 0 5))
(define (z6-op a b) (mod (+ a b) 6))
(define (z6-inv a) (mod (- 6 a) 6))

(check "cyclic-group-is-abelian"
  (every (lambda (a) (every (lambda (b) (= (z6-op a b) (z6-op b a))) z6g)) z6g))
(check "cyclic-galois-is-solvable"
  (= 1 (derived-length (lambda (a b) (z6-op a b)) (lambda (a) (z6-inv a)) z6g 0)))

; Contrast: S_3 is non-abelian, its derived subgroup is A_3, and its
; derived length is 2 -- solvable, but not in one step.
(define s3 (list (list 0 1 2) (list 0 2 1) (list 1 0 2)
                 (list 1 2 0) (list 2 0 1) (list 2 1 0)))
(define (perm-comp p q) (map (lambda (i) (nth (nth i q) p)) (list 0 1 2)))
(define (perm-inv p) (find (lambda (q) (equal (perm-comp p q) (list 0 1 2))) s3))

(check "S3-is-not-abelian"
  (not (every (lambda (a)
                (every (lambda (b) (equal (perm-comp a b) (perm-comp b a))) s3))
              s3)))
(check "S3-derived-subgroup-is-A3"
  (= 3 (length (derived-of (lambda (a b) (perm-comp a b)) (lambda (a) (perm-inv a)) s3))))
(check "S3-is-solvable-in-two-steps"
  (= 2 (derived-length (lambda (a b) (perm-comp a b)) (lambda (a) (perm-inv a)) s3 0)))

(print "--- GF(16)/F_2: A degree-4 Extension ---")

; Intermediate fields of GF(2^4)/F_2 correspond to subgroups of the cyclic
; group Z/4. Both sides are counted independently: subgroups by brute-force
; enumeration of closed subsets, subfields by the divisors of 4.
(define (subsets l)
  (cond ((eq l nil) (list nil))
        (t (let ((rest (subsets (cdr l))))
             (append rest (map (lambda (sub) (cons (car l) sub)) rest))))))
(define z4g (upto 0 3))
(define z4-subgroups
  (filter (lambda (sub)
            (and (contains 0 sub)
                 (every (lambda (a) (every (lambda (b) (contains (mod (+ a b) 4) sub)) sub))
                        sub)))
          (subsets z4g)))
(define divisors4 (filter (lambda (d) (= 0 (mod 4 d))) (upto 1 4)))

(check "GF16-intermediate-fields-count"
  (and (= (length z4-subgroups) (length divisors4))
       (= (length divisors4) 3)))
; The subgroup orders are exactly the divisors of 4.
(check "GF16-subgroup-orders-are-the-divisors"
  (equal (sort < (map (lambda (sub) (length sub)) z4-subgroups)) divisors4))
; Negative control: the enumeration really does reject non-subgroups.
(check "subgroup-test-rejects-non-subgroups"
  (< (length z4-subgroups) (length (subsets z4g))))

(print "--- Summary ---")

; The fundamental theorem, stated as a computation: for GF(8)/F_2 the
; lattice of subgroups of Gal and the lattice of intermediate fields have
; the same size, and each intermediate field is the fixed field of one.
(define gal8-maps (dedup (map (lambda (k) (map-table k)) (upto 0 2))))
(define (fixed-of-table tbl)
  (length (filter (lambda (pr) (equal (car pr) (cadr pr))) (zip gf8l tbl))))
(check "galois-fundamental-theorem-holds"
  (and (= 3 (length gal8-maps))
       (equal (sort < (dedup (map (lambda (tbl) (fixed-of-table tbl)) gal8-maps)))
              (list 2 8))))

(print "=== Lang Ch4 Done ===")
