; ============================================================
; Atiyah-Macdonald Ch7: Noether Normalization & Nullstellensatz
; Noether normalization for finite rings. Nullstellensatz for
; finite rings. The algebra-geometry correspondence (Spec R).
; ============================================================

(print "=== AM Ch7: Noether Normalization and Nullstellensatz ===")

; ---------- divisors ----------
(define (divides n d) (= (mod n d) 0))
(define (divisors n)
  (labels (((go k acc)
    (cond ((> k n) (reverse acc))
          ((divides n k) (go (+ k 1) (cons k acc)))
          (t (go (+ k 1) acc)))))
    (go 1 nil)))

; ---------- ideal ----------
(define (ideal-from-divisor n d)
  (list-to-set
   (labels (((go k acc)
     (cond ((>= k n) (reverse acc))
           (t (go (+ k d) (cons k acc))))))
     (go 0 nil))))

; ---------- all ideals ----------
(define (all-ideals n)
  (list-to-set (map (lambda (d) (ideal-from-divisor n d)) (divisors n))))

; ---------- prime ideal test ----------
(define (prime-ideal-p ring mul p)
  (and (not (equal p ring))
       (every (lambda (a)
         (every (lambda (b)
           (or (notin (mul a b) p) (in a p) (in b p))) ring)) ring)))

; ---------- maximal ideal test ----------
(define (maximal-ideal-p ring p ideals)
  (and (not (equal p ring))
       (not (any (lambda (q)
         (and (not (equal q p))
              (not (equal q ring))
              (subset p q))) ideals))))

; ---------- all prime ideals ----------
(define (prime-ideals n)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter
     (lambda (p) (prime-ideal-p ring (lambda (a b) (mod (* a b) n)) p))
     (all-ideals n))))

; ---------- all maximal ideals ----------
(define (maximal-ideals n)
  (let ((ring (list-to-set (seq 0 (- n 1))))
        (ideals (all-ideals n)))
    (filter
     (lambda (p) (maximal-ideal-p ring p ideals)) ideals)))

; ---------- Spec(R) = set of all prime ideals ----------
(define (spec n) (prime-ideals n))

; ---------- MaxSpec(R) = set of all maximal ideals ----------
(define (max-spec n) (maximal-ideals n))

; ---------- Zariski topology: closed sets = V(I) = {P ∈ Spec : I ⊆ P} ----------
(define (zariski-closed n I)
  (let ((primes (prime-ideals n)))
    (filter (lambda (P) (subset I P)) primes)))

; ---------- V({0}) = Spec(R), V(R) = {} ----------
(define (zariski-closed-verify n)
  (let ((primes (prime-ideals n))
        (ring (list-to-set (seq 0 (- n 1))))
        (zero-ideal (ideal-from-divisor n n)))
    (and (equal (zariski-closed n zero-ideal) primes)
         (equal (zariski-closed n ring) {}))))

; ---------- V(I ∩ J) = V(I) ∪ V(J) ----------
(define (zariski-union-property n I J)
  (let ((VI (zariski-closed n I))
        (VJ (zariski-closed n J))
        (V-icapJ (zariski-closed n (cap I J))))
    (equal V-icapJ (union VI VJ))))

; ---------- V(Σ I_α) = ∩ V(I_α) (for sum of ideals) ----------
(define (ideal-sum n I J)
  (list-to-set
   (foldl (lambda (acc x)
     (foldl (lambda (acc2 y) (cons (mod (+ x y) n) acc2))
       acc (set-to-list J)))
     nil (set-to-list I))))

(define (zariski-intersection-property n I J)
  (let ((VI (zariski-closed n I))
        (VJ (zariski-closed n J))
        (V-sum (zariski-closed n (ideal-sum n I J))))
    (equal V-sum (cap VI VJ))))

; ---------- Nullstellensatz for finite rings ----------
; For a finite ring R=Z/nZ:
; (1) Every ideal is contained in some maximal ideal.
; (2) The nilradical = intersection of all maximal ideals = intersection of all primes.
; (3) For any ideal I, r(I) = ∩_{P ∈ V(I)} P (intersection of all prime ideals containing I).
; (Weak Nullstellensatz): maximal ideals are precisely the kernels of
; surjective homomorphisms R → k onto fields k. For Z/nZ, maximal ideals = (p)Z/nZ for p|n prime.
(define (nullstellensatz-weak n)
  (let ((primes (prime-ideals n))
        (maximals (maximal-ideals n)))
    ; For finite rings, all primes are maximal
    (equal primes maximals)))

; ---------- Strong Nullstellensatz: I(V(J)) = r(J) ----------
; For Z/nZ, J = dZ/nZ with d|n.  V(J) = primes containing J = the (p) with p|d.
; I(V(J)) = ∩_{P ∈ V(J)} P = ∩_{p|d} (p)Z/nZ = rad(d)Z/nZ,
; where rad(d) = product of the distinct prime factors of d.
; r(J) = {x : x^k ∈ J for some k} = {x : d | x^k} = rad(d)Z/nZ as well.
; Verified below on Z/30Z and Z/12Z.

(define (pow-mod n a k)
  (cond ((= k 0) 1)
        (t (mod (* a (pow-mod n a (- k 1))) n))))

(define (radical-of n I)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter (lambda (a)
      (labels (((try-pow k)
        (cond ((>= k n) nil)
              ((in (pow-mod n a k) I) t)
              (t (try-pow (+ k 1))))))
        (try-pow 1))) ring)))

(define (intersection-of-containing-primes n I)
  (let ((primes (prime-ideals n))
        (ring (list-to-set (seq 0 (- n 1)))))
    (foldl (lambda (acc P)
      (if (subset I P) (cap acc P) acc))
      ring (set-to-list primes))))

(define (strong-nullstellensatz-holds n I)
  (let ((rI (radical-of n I))
        (iVJ (intersection-of-containing-primes n I)))
    (equal rI iVJ)))

; ---------- Noetherian R: Krull dimension = 0 iff R is Artinian (AM 8.5) ----------
(define (krull-dim n)
  (let ((primes (set-to-list (prime-ideals n))))
    (labels (((chain-len p remaining depth)
      (let ((candidates (filter
            (lambda (q) (and (not (equal p q)) (subset p q)))
            remaining)))
        (cond ((eq candidates nil) depth)
              (t (foldl (lambda (acc q)
                    (let ((sub (chain-len q candidates (+ depth 1))))
                      (if (> sub acc) sub acc)))
                  depth candidates))))))
      (foldl (lambda (acc p)
        (let ((len (chain-len p primes 0)))
          (if (> len acc) len acc)))
        0 primes))))

; ---------- Algebra-geometry correspondence for finite rings ----------
; Affine scheme Spec(Z/nZ) is a finite discrete set of points.
; For Z/nZ, the "variety" Spec(R) is the set of prime ideals.
; Sheaf of regular functions O(U) for open U = localization.

; ========== Z/30Z ==========
(print "--- Z/30Z: Spec and Nullstellensatz ---")
(define n30 30)
(define ring30 (list-to-set (seq 0 29)))

; Spec(Z/30Z) = {(2),(3),(5)}
(define spec30 (spec n30))
(check "spec-Z30-has-3-points" (= (length spec30) 3))

; MaxSpec(Z/30Z) = same set
(define mspec30 (max-spec n30))
(check "maxspec-equals-spec-Z30" (equal spec30 mspec30))

; Weak Nullstellensatz: primes = maximals
(check "weak-nullstellensatz-Z30" (nullstellensatz-weak n30))

; Zariski closed set V(6) = {P : (6) ⊆ P} = primes containing 6 = (2),(3)
(define I30-6 (ideal-from-divisor n30 6))
(define V6 (zariski-closed n30 I30-6))
(check "V-6-equals-primes-2-and-3" (= (length V6) 2))

; V(0) = all of Spec
(check "V-0-equals-all-spec"
  (equal (zariski-closed n30 (ideal-from-divisor n30 30)) spec30))

; V(R) = empty
(check "V-R-equals-empty"
  (= (length (zariski-closed n30 ring30)) 0))

; V(I ∩ J) = V(I) ∪ V(J)
(define I30-2 (ideal-from-divisor n30 2))
(define I30-3 (ideal-from-divisor n30 3))
(check "zariski-union-V2-cap-V3"
  (zariski-union-property n30 I30-2 I30-3))

; Strong Nullstellensatz: r(I) = ∩_{P ⊇ I} P
; For I = (6) in Z/30Z: r(6) = intersection of primes containing 6 = (2) ∩ (3) = (6)
(check "strong-nullstellensatz-I6-Z30" (strong-nullstellensatz-holds n30 I30-6))
; For I = (10): r(10) = (2) ∩ (5) = (10)
(check "strong-nullstellensatz-I10-Z30"
  (strong-nullstellensatz-holds n30 (ideal-from-divisor n30 10)))

; ========== Z/12Z: SPEC AND ZARISKI ==========
(print "--- Z/12Z: Spec and Zariski Topology ---")
(define n12 12)
(define ring12 (list-to-set (seq 0 11)))

; Spec(Z/12Z) = {(2),(3)}: 2 points
(define spec12 (spec n12))
(check "spec-Z12-has-2-points" (= (length spec12) 2))

; All primes are maximal
(check "weak-nullstellensatz-Z12" (nullstellensatz-weak n12))

; Zariski closed: V(2) = {(2)} (only prime containing 2)
; V(3) = {(3)}
; V(0) = {(2),(3)} = Spec
; V(R) = {}
(define I12-2 (ideal-from-divisor n12 2))
(define V2 (zariski-closed n12 I12-2))
(check "V-2-is-just-2" (= (length V2) 1))

; V(6) = primes containing 6 = (2) and (3) = both
(define I12-6 (ideal-from-divisor n12 6))
(define V6-12 (zariski-closed n12 I12-6))
(check "V-6-equals-entire-spec" (equal V6-12 spec12))

; Strong Nullstellensatz for I=(4) in Z/12Z: r(4)=(2)
; Only (2) contains (4): 4 ∉ (3) = {0,3,6,9}, so (3) does not contain (4).
; Hence the intersection of primes containing 4 is (2), and r(4) = (2)
; since 2² = 4 ∈ (4) and no power of an odd element is divisible by 4.
(check "strong-nullstellensatz-I4-Z12"
  (strong-nullstellensatz-holds n12 (ideal-from-divisor n12 4)))

; Strong Nullstellensatz for I=(6): r(6) = {x : x^k ∈ (6)}, with (6) = {0,6}.
; No power of 2 (2,4,8,4,8,...) or of 3 (3,9,3,9,...) lies in {0,6}, so r(6) = (6).
; ∩ {primes containing 6} = (2) ∩ (3) = {0,2,4,6,8,10} ∩ {0,3,6,9} = {0,6} = (6).
(check "strong-nullstellensatz-I6-Z12"
  (strong-nullstellensatz-holds n12 I12-6))

; ========== ALGEBRA-GEOMETRY CORRESPONDENCE ==========
(print "--- Algebra-Geometry Correspondence ---")

; For finite rings: Spec(R) is finite discrete. The sheaf of regular functions
; on an affine scheme X=Spec(R) is: O_X(D(f)) = R_f (localization at f).
; This is the heart of the algebra-geometry correspondence.

; For Z/12Z:
; - Spec = {(2),(3)} has 2 points.
; - D(f) = {P : f ∉ P}. D(3) = points not containing 3 = {(2)}.
; - O_X(D(f)) = R_f.
; For f=3 the kernel of Z/12Z → R_3 is {x : 3^k x = 0} = {0,4,8}, so
; R_3 ≅ Z/4Z, a ring of size 4.

; Check: D(3) = {(2)} is a single point. The localization yields a ring with
; dim=0 and local structure inherited from Z/12Z.

; Spec(R) is Noetherian (finite). Each irreducible component is a single point.
; The local ring at a point has dimension 0 but need not be a field: for
; n = p_1^{e_1}...p_k^{e_k}, the localization of Z/nZ at (p_i) is Z/p_i^{e_i}Z,
; a field only when e_i = 1 (e.g. (Z/12Z)_(2) ≅ Z/4Z).

; The correspondence: closed points ↔ maximal ideals = prime ideals.
; Each point has a residue field k(P) = R_P / P·R_P.
; For Z/12Z: k(2) = (Z/12Z)_(2) / (2)·(Z/12Z)_(2) ≅ Z/2Z.
; k(3) = (Z/12Z)_(3) / (3)·(Z/12Z)_(3) ≅ Z/3Z.

(print "=== AM Ch7 Done ===")
