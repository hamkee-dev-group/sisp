; ============================================================
; galois-theory/galois-correspondence.lsp — Galois Group, Fixed
; Field, Fundamental Theorem. Examples: F_8/F_2 (degree 3, cyclic),
; F_16/F_2 (degree 4, cyclic), F_16/F_4 (degree 2).
;
; Everything below is COMPUTED in explicit models of F_8 and F_16:
; automorphism groups are found by exhaustive search over candidate
; images of a generator, subgroups by filtering the powerset, and
; fixed fields by filtering the field. No property is asserted that
; is not derived twice, by independent routes.
; ============================================================

(print "=== Galois Correspondence ===")


; ---------- Build F_8 = GF(2)[x]/(x^3+x+1) ----------
; Elements: (a0 a1 a2) for a0 + a1·α + a2·α², where α³ = α + 1.
(define f2 {0 1})
; cartesian-product nests as (a (b c)); flatten to flat triples (a b c) so the
; field arithmetic below (which indexes with nth 0/1/2) sees numbers, not sublists.
(define f8 (list-to-set
             (map (lambda (p) (flatten p))
                  (set-to-list (cartesian-product f2 (cartesian-product f2 f2))))))

(define f8-zero (list 0 0 0))
(define f8-one  (list 1 0 0))
(define alpha-8 (list 0 1 0))

(define (b+ a b) (mod (+ a b) 2))
(define (f8-add u v)
  (list (b+ (nth 0 u) (nth 0 v))
        (b+ (nth 1 u) (nth 1 v))
        (b+ (nth 2 u) (nth 2 v))))

(define (f8-mul u v)
  (let ((a0 (nth 0 u)) (a1 (nth 1 u)) (a2 (nth 2 u))
        (b0 (nth 0 v)) (b1 (nth 1 v)) (b2 (nth 2 v)))
    (let ((c0 (mod (* a0 b0) 2))
          (c1 (mod (+ (* a0 b1) (* a1 b0)) 2))
          (c2 (mod (+ (* a0 b2) (* a1 b1) (* a2 b0)) 2))
          (c3 (mod (+ (* a1 b2) (* a2 b1)) 2))
          (c4 (mod (* a2 b2) 2)))
      (list (mod (+ c0 c3) 2)
            (mod (+ c1 c3 c4) 2)
            (mod (+ c2 c4) 2)))))

(check "f8-has-8-elements" (= (length f8) 8))

; smallest e with 2^e >= m; for m a power of two this is log_2 m
(define (log2-of m)
  (labels (((go e acc) (cond ((>= acc m) e) (t (go (+ e 1) (* acc 2))))))
    (go 0 1)))

(check "log2-of-is-correct"
  (and (= (log2-of 2) 1) (= (log2-of 4) 2) (= (log2-of 8) 3) (= (log2-of 16) 4)))


; ---------- Frobenius automorphism Fr(x) = x^2 ----------
(define (frob8 a) (f8-mul a a))
(define (frob8-2 a) (frob8 (frob8 a)))
(define (frob8-iter e u) (cond ((= e 0) u) (t (frob8-iter (- e 1) (frob8 u)))))

(check "frobenius-is-additive-f8"
  (every (lambda (a)
    (every (lambda (b)
      (equal (frob8 (f8-add a b)) (f8-add (frob8 a) (frob8 b)))) f8)) f8))

(check "frobenius-is-multiplicative-f8"
  (every (lambda (a)
    (every (lambda (b)
      (equal (frob8 (f8-mul a b)) (f8-mul (frob8 a) (frob8 b)))) f8)) f8))

(check "frobenius-cubed-is-identity-f8"
  (every (lambda (a) (equal (frob8 (frob8-2 a)) a)) f8))

(check "frobenius-not-identity-f8"
  (any (lambda (a) (not (equal (frob8 a) a))) f8))


; ---------- Galois group Gal(F_8/F_2) = {id, Fr, Fr²} ≅ Z/3Z ----------
; The fixed field of the full Galois group is F_2.
(define fixed-f8 (filter (lambda (a) (equal (frob8 a) a)) f8))
(define f8-subfield-f2 (list-to-set (list f8-zero f8-one)))
(check "fixed-field-of-frobenius-is-f2"
  (equal fixed-f8 f8-subfield-f2))

; ORDER OF FROBENIUS: the least e >= 1 with Fr^e = id on the whole field.
(define (frob-order-of stepper carrier)
  (labels (((go e)
    (cond ((> e 8) 0)
          ((every (lambda (u) (equal (stepper e u) u)) carrier) e)
          (t (go (+ e 1))))))
    (go 1)))

(define frob8-order (frob-order-of frob8-iter f8))

; |Gal(F_8/F_2)| = 3 and [F_8 : F_2] = 3.  The left side is the dynamical order
; of Frobenius; the right side comes from the CARDINALITY of F_8.  Two
; independent computations, asserted equal.
(check "galois-group-order-3-degree-3"
  (and (= frob8-order 3)
       (= frob8-order (log2-of (length f8)))
       (= (length fixed-f8) 2)))

(define (gal-apply-f8 k a)
  (cond ((= k 0) a) ((= k 1) (frob8 a)) (t (frob8-2 a))))

(check "galois-group-is-cyclic-order-3"
  (every (lambda (i)
    (every (lambda (j)
      (every (lambda (a)
        (equal (gal-apply-f8 i (gal-apply-f8 j a))
               (gal-apply-f8 (mod (+ i j) 3) a))) f8)) {0 1 2})) {0 1 2}))


; ---------- Fundamental Theorem: subgroup ⟷ intermediate field ----------
; Subgroups of Z/nZ: every subset of {0..n-1} containing 0 and closed under
; addition mod n.  Found by filtering the powerset, not by quoting the answer.
(define (cyclic-subgroups nn)
  (filter (lambda (s)
    (and (in 0 s)
         (every (lambda (i)
           (every (lambda (j) (in (mod (+ i j) nn) s)) s)) s)))
    (powerset (list-to-set (seq 0 (- nn 1))))))

(define subgroups-3 (cyclic-subgroups 3))
(check "z3-has-two-subgroups" (= (length subgroups-3) 2))

; Fix(H) = the elements held still by every element of H.
(define (fix-8 h)
  (filter (lambda (u) (every (lambda (e) (equal (frob8-iter e u) u)) h)) f8))

; The intermediate fields, found INDEPENDENTLY of the Galois group: F_2(a) is
; the F_2-span of {1, a, a^2} (as [F_8:F_2] = 3), so form it for every a.
(define bits3 (list (list 0 0 0) (list 1 0 0) (list 0 1 0) (list 0 0 1)
                    (list 1 1 0) (list 1 0 1) (list 0 1 1) (list 1 1 1)))

(define (pick-8 flag u) (cond ((= flag 1) u) (t f8-zero)))

(define (f2-adjoin-8 a)
  (let ((a2 (f8-mul a a)))
    (list-to-set
      (map (lambda (c)
             (f8-add (f8-add (pick-8 (nth 0 c) f8-one) (pick-8 (nth 1 c) a))
                     (pick-8 (nth 2 c) a2)))
           bits3))))

(define intermediate-fields-8
  (list-to-set (map (lambda (a) (f2-adjoin-8 a)) (set-to-list f8))))

; THE FUNDAMENTAL THEOREM for F_8/F_2: H |-> Fix(H) is an inclusion-reversing
; bijection between subgroups of Gal and intermediate fields, and it satisfies
; |H| * [Fix(H) : F_2] = [F_8 : F_2].
(check "fundamental-theorem-f8-over-f2"
  (and (= (length subgroups-3) (length intermediate-fields-8))
       ; the image of the subgroup lattice is exactly the field lattice
       (equal (list-to-set (map (lambda (h) (fix-8 h)) (set-to-list subgroups-3)))
              intermediate-fields-8)
       ; inclusion-reversing at the two ends
       (equal (fix-8 (list-to-set (list 0))) f8)
       (equal (fix-8 (list-to-set (list 0 1 2))) f8-subfield-f2)
       ; the degree formula, both sides computed
       (every (lambda (h)
         (= (* (length h) (log2-of (length (fix-8 h)))) (log2-of (length f8))))
         subgroups-3)))

; negative control: the correspondence really is injective -- distinct
; subgroups give distinct fixed fields, so no two collapse together.
(check "fixed-field-map-is-injective-f8"
  (= (length (list-to-set (map (lambda (h) (fix-8 h)) (set-to-list subgroups-3))))
     (length subgroups-3)))


; ---------- F_16 = GF(2)[x]/(x^4 + x + 1) ----------
; Elements: flat 4-tuples (a0 a1 a2 a3).  cartesian-product NESTS, so flatten.
; x^4 = x+1, x^5 = x^2+x, x^6 = x^3+x^2.
(define f16 (list-to-set
              (map (lambda (p) (flatten p))
                   (set-to-list
                     (cartesian-product f2
                       (cartesian-product f2 (cartesian-product f2 f2)))))))
(check "f16-has-16-elements" (= (length f16) 16))

(define f16-zero (list 0 0 0 0))
(define f16-one  (list 1 0 0 0))
(define alpha-16 (list 0 1 0 0))

(define (f16-add u v)
  (list (b+ (nth 0 u) (nth 0 v))
        (b+ (nth 1 u) (nth 1 v))
        (b+ (nth 2 u) (nth 2 v))
        (b+ (nth 3 u) (nth 3 v))))

(define (f16-mul u v)
  (let ((a0 (nth 0 u)) (a1 (nth 1 u)) (a2 (nth 2 u)) (a3 (nth 3 u))
        (b0 (nth 0 v)) (b1 (nth 1 v)) (b2 (nth 2 v)) (b3 (nth 3 v)))
    (let ((c0 (* a0 b0))
          (c1 (+ (* a0 b1) (* a1 b0)))
          (c2 (+ (* a0 b2) (* a1 b1) (* a2 b0)))
          (c3 (+ (* a0 b3) (* a1 b2) (* a2 b1) (* a3 b0)))
          (c4 (+ (* a1 b3) (* a2 b2) (* a3 b1)))
          (c5 (+ (* a2 b3) (* a3 b2)))
          (c6 (* a3 b3)))
      (list (mod (+ c0 c4) 2)
            (mod (+ c1 c4 c5) 2)
            (mod (+ c2 c5 c6) 2)
            (mod (+ c3 c6) 2)))))

; the defining relation alpha^4 = alpha + 1 must hold in the model
(check "alpha16-satisfies-x^4+x+1"
  (equal (f16-mul (f16-mul alpha-16 alpha-16) (f16-mul alpha-16 alpha-16))
         (f16-add alpha-16 f16-one)))

(define f16-nonzero (filter (lambda (a) (not (equal a f16-zero))) f16))
(check "f16-every-nonzero-invertible"
  (every (lambda (a)
    (any (lambda (b) (equal (f16-mul a b) f16-one)) f16-nonzero)) f16-nonzero))

(define (f16-pow a n)
  (cond ((= n 0) f16-one) (t (f16-mul a (f16-pow a (- n 1))))))

(define (frob16 a) (f16-mul a a))
(define (frob16-iter e u) (cond ((= e 0) u) (t (frob16-iter (- e 1) (frob16 u)))))

(define frob16-order (frob-order-of frob16-iter f16))

(check "galois-group-f16-f2-is-cyclic-order-4"
  (and (= frob16-order 4)
       (= frob16-order (log2-of (length f16)))
       ; cyclic: Fr^i o Fr^j = Fr^(i+j mod 4) on every element
       (every (lambda (i)
         (every (lambda (j)
           (every (lambda (a)
             (equal (frob16-iter i (frob16-iter j a))
                    (frob16-iter (mod (+ i j) 4) a))) f16)) {0 1 2 3})) {0 1 2 3})))


; ---------- Subgroup correspondence for F_16/F_2 ----------
(define subgroups-4 (cyclic-subgroups 4))

(define (fix-16 h)
  (filter (lambda (u) (every (lambda (e) (equal (frob16-iter e u) u)) h)) f16))

; The 4-element subfield, obtained WITHOUT reference to Frobenius: an element of
; multiplicative order 3 generates it, since F_4^x is cyclic of order 3.
(define omega-16 (find (lambda (a)
  (and (not (equal a f16-one)) (equal (f16-pow a 3) f16-one))) f16-nonzero))

(define f16-f4 (list-to-set (list f16-zero f16-one omega-16
                                  (f16-add omega-16 f16-one))))

(check "f4-has-4-elements"
  (and (not (eq omega-16 nil))
       (= (length f16-f4) 4)
       ; it really is a subfield: closed under + and *
       (every (lambda (a)
         (every (lambda (b)
           (and (in (f16-add a b) f16-f4) (in (f16-mul a b) f16-f4))) f16-f4)) f16-f4)))

(print "Fixed field of subgroup {0,2} (index 2) = F_4")
(print "Fixed field of subgroup {0,1,2,3} (index 4) = F_2")

; Subgroups of Z/4Z are {0}, {0,2} and all of it -- three of them -- and the
; correspondence sends them to F_16, F_4 and F_2, of sizes 16, 4 and 2.
; The sizes are counted in the model; the predicted sizes 2^(4/|H|) come from
; the subgroup orders.  Both sides computed, then compared.
(check "subgroup-correspondence-f16-f2"
  (and (= (length subgroups-4) 3)
       (= (length (list-to-set (map (lambda (h) (fix-16 h)) (set-to-list subgroups-4)))) 3)
       (every (lambda (h)
         (= (length (fix-16 h)) (^ 2 (truncate (/ 4 (length h)))))) subgroups-4)
       ; the index-2 subgroup {0,2} fixes exactly the F_4 built from omega
       (equal (fix-16 (list-to-set (list 0 2))) f16-f4)
       ; the whole group fixes only F_2, the trivial subgroup fixes everything
       (equal (fix-16 (list-to-set (list 0 1 2 3)))
              (list-to-set (list f16-zero f16-one)))
       (equal (fix-16 (list-to-set (list 0))) f16)))


; ---------- F_16/F_4: degree 2 ----------
; Gal(F_16/F_4) is generated by Fr^2 (x |-> x^4) and has order 2.
(define (frob16-sq u) (frob16 (frob16 u)))
(define (frob16-sq-iter e u) (cond ((= e 0) u) (t (frob16-sq-iter (- e 1) (frob16-sq u)))))

(check "galois-group-f16-f4-is-order-2"
  (and (= (frob-order-of frob16-sq-iter f16) 2)
       ; it fixes F_4 pointwise and nothing more
       (equal (filter (lambda (u) (equal (frob16-sq u) u)) f16) f16-f4)))

; NO NONTRIVIAL INTERMEDIATE FIELD between F_4 and F_16.  Checked directly:
; for every a in F_16, the field F_4(a) -- the F_4-span of {1,a}, since
; [F_16:F_4] = 2 -- is either F_4 itself or all of F_16.  Nothing of size 8
; can appear, and no theorem is quoted to rule it out.
(define (f4-adjoin-16 a)
  (list-to-set
    (foldr append nil
      (map (lambda (c0)
             (map (lambda (c1) (f16-add c0 (f16-mul c1 a))) (set-to-list f16-f4)))
           (set-to-list f16-f4)))))

(check "no-nontrivial-intermediate-extensions-f16-f4"
  (and (every (lambda (a)
         (or (equal (f4-adjoin-16 a) f16-f4) (equal (f4-adjoin-16 a) f16))) f16)
       ; both cases really occur, so the statement is not vacuously satisfied
       (equal (list-to-set (map (lambda (a) (length (f4-adjoin-16 a))) (set-to-list f16)))
              (list-to-set (list 4 16)))))


; ---------- Verify F_8 ^× is cyclic of order 7 ----------
(define (f8-pow a n)
  (cond ((= n 0) f8-one)
        (t (f8-mul a (f8-pow a (- n 1))))))

(define f8-nonzero (filter (lambda (a) (not (equal a f8-zero))) f8))
(check "f8-nonzero-order-7" (= (length f8-nonzero) 7))

(define gen8 (find (lambda (a)
  (and (not (equal a f8-zero))
       (every (lambda (x)
         (any (lambda (k) (equal (f8-pow a k) x)) (list-to-set (seq 0 6))))
         f8-nonzero))) f8))

(check "f8-multiplicative-group-is-cyclic"
  (not (eq gen8 nil)))


; ---------- Every Galois group here is cyclic, generated by Frobenius ----------
; Enumerate the FULL automorphism group in each case.  An F_2-automorphism of
; F_8 is determined by the image b of alpha (it sends sum a_i alpha^i to
; sum a_i b^i); keep the b whose induced map is additive, multiplicative and
; bijective, then check every survivor is a power of Frobenius.
(define (sigma8 b u)
  (let ((b2 (f8-mul b b)))
    (f8-add (f8-add (pick-8 (nth 0 u) f8-one) (pick-8 (nth 1 u) b))
            (pick-8 (nth 2 u) b2))))

(define (aut8-p b)
  (and (= (length (list-to-set (map (lambda (u) (sigma8 b u)) (set-to-list f8)))) 8)
       (every (lambda (u)
         (every (lambda (w)
           (and (equal (sigma8 b (f8-add u w)) (f8-add (sigma8 b u) (sigma8 b w)))
                (equal (sigma8 b (f8-mul u w)) (f8-mul (sigma8 b u) (sigma8 b w)))))
           f8)) f8)))

(define aut8 (filter (lambda (b) (aut8-p b)) f8))

(define (pick-16 flag u) (cond ((= flag 1) u) (t f16-zero)))

(define (sigma16 b u)
  (let ((b2 (f16-mul b b)))
    (let ((b3 (f16-mul b2 b)))
      (f16-add (f16-add (pick-16 (nth 0 u) f16-one) (pick-16 (nth 1 u) b))
               (f16-add (pick-16 (nth 2 u) b2) (pick-16 (nth 3 u) b3))))))

(define (aut16-p b)
  (and (= (length (list-to-set (map (lambda (u) (sigma16 b u)) (set-to-list f16)))) 16)
       (every (lambda (u)
         (every (lambda (w)
           (and (equal (sigma16 b (f16-add u w)) (f16-add (sigma16 b u) (sigma16 b w)))
                (equal (sigma16 b (f16-mul u w)) (f16-mul (sigma16 b u) (sigma16 b w)))))
           f16)) f16)))

(define aut16 (filter (lambda (b) (aut16-p b)) f16))

; automorphisms of F_16 that additionally fix F_4 pointwise = Gal(F_16/F_4)
(define aut16-over-f4
  (filter (lambda (b)
    (every (lambda (c) (equal (sigma16 b c) c)) f16-f4)) aut16))

(check "all-galois-groups-cyclic"
  (and (= (length aut8) 3)
       (= (length aut16) 4)
       (= (length aut16-over-f4) 2)
       ; every automorphism of F_8 is some power of Frobenius
       (every (lambda (b)
         (any (lambda (e)
           (every (lambda (u) (equal (sigma8 b u) (frob8-iter e u))) f8))
           {0 1 2})) aut8)
       ; likewise for F_16
       (every (lambda (b)
         (any (lambda (e)
           (every (lambda (u) (equal (sigma16 b u) (frob16-iter e u))) f16))
           {0 1 2 3})) aut16)
       ; and Gal(F_16/F_4) is generated by Fr^2
       (every (lambda (b)
         (any (lambda (e)
           (every (lambda (u) (equal (sigma16 b u) (frob16-sq-iter e u))) f16))
           {0 1})) aut16-over-f4)))

; negative control: not every candidate image of the generator yields an
; automorphism, so the counts above are the result of real filtering.
(check "most-candidates-are-not-automorphisms"
  (and (< (length aut8) (length f8)) (< (length aut16) (length f16))))


; ---------- Fixed field cardinality formula ----------
; For sigma = Fr^k of order d inside a cyclic Gal of order n over F_q, the
; fixed field of <sigma> has q^(n/d) elements.  Compare the counted size of
; Fix(<Fr^k>) against that formula, for every k, in both fields.
(define (cyclic-gen-subgroup nn k)
  (list-to-set (map (lambda (e) (mod (* e k) nn)) (seq 0 (- nn 1)))))

(check "fixed-field-computation-matches-cardinality-formula"
  (and (every (lambda (k)
         (let ((h (cyclic-gen-subgroup 3 k)))
           (= (length (fix-8 h)) (^ 2 (truncate (/ 3 (length h))))))) {0 1 2})
       (every (lambda (k)
         (let ((h (cyclic-gen-subgroup 4 k)))
           (= (length (fix-16 h)) (^ 2 (truncate (/ 4 (length h))))))) {0 1 2 3})
       ; spot values: <Fr^2> in Gal(F_16/F_2) has order 2 and fixes F_4
       (= (length (cyclic-gen-subgroup 4 2)) 2)
       (= (length (fix-16 (cyclic-gen-subgroup 4 2))) 4)
       ; and <Fr^1> is everything, fixing only F_2
       (= (length (fix-16 (cyclic-gen-subgroup 4 1))) 2)))


(print "=== Galois Correspondence Done ===")
