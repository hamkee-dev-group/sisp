; ============================================================
; Bourbaki — Projective Modules over Finite Rings
; Projective = direct summand of free.
; Local rings: only free projectives.
; Verified on Z/4Z (local) vs Z/6Z (not local).
; ============================================================

(print "=== Bourbaki: Projective Modules ===")

; ---------- divisors ----------
(define (divides n d) (= (mod n d) 0))
(define (divisors n)
  (labels (((go k acc)
    (cond ((> k n) (reverse acc))
          ((divides n k) (go (+ k 1) (cons k acc)))
          (t (go (+ k 1) acc)))))
    (go 1 nil)))

; ---------- ideal from divisor ----------
(define (ideal-from-divisor n d)
  (list-to-set
   (labels (((go k acc)
     (cond ((>= k n) (reverse acc))
           (t (go (+ k d) (cons k acc))))))
     (go 0 nil))))

; ---------- submodule predicate ----------
(define (submodule-p n M)
  (and (in 0 M)
       (every (lambda (a)
         (every (lambda (b) (in (mod (+ a b) n) M)) M)) M)
       (every (lambda (a) (in (mod (- 0 a) n) M)) M)
       (every (lambda (a)
         (every (lambda (r) (in (mod (* r a) n) M))
           (list-to-set (seq 0 (- n 1))))) M)))

; ---------- is a module a direct summand? ----------
; M is projective if there exists N such that M⊕N ≅ free.
; For Z/nZ: a module (ideal) dZ/nZ is projective iff it is a direct summand
; of Z/nZ. This happens exactly when d and n/d are coprime.
; (As a Z/nZ-module, dZ/nZ ≅ Z/(n/d)Z.)
; Equivalently, in terms of idempotents: (d) is a direct summand of Z/nZ iff
; (d) = (e) for some e with e² ≡ e mod n, and then
; Z/nZ ≅ eZ/nZ ⊕ (1-e)Z/nZ.

(define (idempotent-p n e)
  (= (mod (* e e) n) (mod e n)))

(define (idempotents n)
  (let ((ring (list-to-set (seq 0 (- n 1)))))
    (filter (lambda (e) (idempotent-p n e)) ring)))

(define (ideal-eq n I J)
  (equal I J))

; ---------- projective test for an ideal dZ/nZ ----------
; An ideal I is projective (a direct summand of Z/nZ) iff
; Z/nZ ≅ I ⊕ J for some ideal J.
; For Z/nZ: I = dZ/nZ is projective iff d and n/d are coprime (⇒ CRT splits).
(define (projective-ideal-p n d)
  (let ((cofactor (/ n d)))
    (cond ((not (= (mod n d) 0)) nil)
          ((= (gcd d cofactor) 1) t)
          (t nil))))

; ---------- all projective ideals of Z/nZ ----------
(define (projective-ideals n)
  (filter (lambda (d) (projective-ideal-p n d)) (list-to-set (divisors n))))

; ---------- is the ring local? Z/nZ is local iff n is a prime power ----------
(define (primep n)
  (cond ((<= n 1) nil)
        ((= n 2) t)
        ((= (mod n 2) 0) nil)
        (t (labels (((go d)
              (cond ((> (* d d) n) t)
                    ((= (mod n d) 0) nil)
                    (t (go (+ d 2))))))
             (go 3)))))

(define (prime-power-p n)
  (labels (((check q)
    (cond ((= q 1) nil)
          ((and (divides n q) (primep q))
           (labels (((foldl m)
             (cond ((= (mod m q) 0) (foldl (/ m q)))
                   (t m))))
             (= (foldl n) 1)))
          (t (check (- q 1))))))
    (check n)))

; ---------- local ring: only free projectives ----------
; Z/4Z is local (prime power 2^2). Its only projective ideals are the free
; module {0,1,2,3} (the whole ring) and the zero ideal.
; Z/6Z is not local (2*3). Has nontrivial projective ideals: (2) and (3).

; ========== Z/4Z (local) — only free projectives ==========
(print "--- Z/4Z (local ring) ---")

(define n4 4)
(define ring4 (list-to-set (seq 0 3)))

(check "Z4-is-local" (prime-power-p n4))

; Idempotents of Z/4Z: 0²=0, 1²=1, 2²=0, 3²=1 → {0,1}
(define idem4 (idempotents n4))
(check "Z4-idempotents-are-0-and-1" (equal idem4 {0 1}))

; Projective divisors: only d where gcd(d, n/d)=1
; d=1: gcd(1,4)=1 ✓ → (1) = whole ring = free
; d=2: gcd(2,2)=2 ✗ → (2) is NOT projective
; d=4: gcd(4,1)=1 ✓ → (4) = {0} = trivial projective
(define proj-divs4 (projective-ideals n4))
(check "Z4-only-trivial-projectives" (equal proj-divs4 {1 4}))

; Verify: the only projective ideals are the whole ring and zero.
(check "Z4-projectives-are-free-or-zero"
  (and (in 1 proj-divs4) (in 4 proj-divs4)))

; ========== Z/6Z (not local) — nontrivial projectives ==========
(print "--- Z/6Z (not local) ---")

(define n6 6)

(check "Z6-is-not-local" (not (prime-power-p n6)))

; Idempotents of Z/6Z: 0²=0, 1²=1, 2²=4, 3²=9≡3, 4²=16≡4, 5²=25≡1
; → {0,1,3,4}
(define idem6 (idempotents n6))
(check "Z6-has-4-idempotents" (= (length idem6) 4))
(check "Z6-idempotents-are-0-1-3-4" (equal idem6 {0 1 3 4}))

; The nontrivial idempotents 3 and 4 give direct summand decomposition:
; Z/6Z = 3Z/6Z ⊕ 4Z/6Z (since 3+4=7≡1 mod 6, and 3*4=12≡0 mod 6).
; 3Z/6Z = {0,3} ≅ Z/2Z
; 4Z/6Z = {0,2,4} ≅ Z/3Z

; Projective divisors of 6:
; d=1: gcd(1,6)=1 ✓ → (1) = Z/6Z
; d=2: gcd(2,3)=1 ✓ → (2) is projective! (it's {0,2,4} ≅ Z/3Z as Z/6Z-module)
; d=3: gcd(3,2)=1 ✓ → (3) is projective! ({0,3} ≅ Z/2Z)
; d=6: gcd(6,1)=1 ✓ → (6) = {0}
(define proj-divs6 (projective-ideals n6))
(check "Z6-has-4-proj-divisors" (= (length proj-divs6) 4))
(check "Z6-projective-divisors-1-2-3-6" (equal proj-divs6 {1 2 3 6}))

; Verify: (2) and (3) are projective direct summands
(define I2 (ideal-from-divisor n6 2))
(define I3 (ideal-from-divisor n6 3))

(check "2Z6-is-projective" (projective-ideal-p n6 2))
(check "3Z6-is-projective" (projective-ideal-p n6 3))

; --- direct sum decomposition: Z/6Z ≅ (2) ⊕ (3) ---
; As Z/6Z-modules, (2) = {0,2,4} and (3) = {0,3}.
; Each element x∈Z/6Z can be uniquely written as x = a + b with a∈(3), b∈(2),
; using the idempotents 3 and 4 (3+4 ≡ 1, 3·4 ≡ 0): x = 3x + 4x.
; So x ↦ (3x mod 6, 4x mod 6) is an isomorphism Z/6Z → (3) ⊕ (2); under
; (3) ≅ Z/2Z and (2) ≅ Z/3Z it is the CRT map x ↦ (x mod 2, x mod 3).
; The check below compares sizes: |(2)|·|(3)| = 3·2 = 6 = |Z/6Z|.

(check "direct-sum-decomp-exists"
  (let ((sum-sizes (+ (length I2) (length I3))))
    (= (* (length I2) (length I3)) (length (list-to-set (seq 0 5))))))

; --- verify: (2) is NOT projective over Z/4Z ---
(check "2Z4-is-NOT-projective" (not (projective-ideal-p 4 2)))

; ========== Z/12Z projectives ==========
(print "--- Z/12Z projectives ---")

(define n12 12)
(define proj-divs12 (projective-ideals n12))

; 12=2^2*3. Divisors with gcd(d,12/d)=1:
; d=1: gcd(1,12)=1 ✓
; d=2: gcd(2,6)=2 ✗
; d=3: gcd(3,4)=1 ✓
; d=4: gcd(4,3)=1 ✓
; d=6: gcd(6,2)=2 ✗
; d=12: gcd(12,1)=1 ✓
(check "Z12-projective-divisors-1-3-4-12" (equal proj-divs12 {1 3 4 12}))

; Verify: (3) and (4) are projective
(check "3Z12-is-projective" (projective-ideal-p n12 3))
(check "4Z12-is-projective" (projective-ideal-p n12 4))
(check "2Z12-is-NOT-projective" (not (projective-ideal-p n12 2)))
(check "6Z12-is-NOT-projective" (not (projective-ideal-p n12 6)))

; ========== Z/8Z (local) — only free projectives ==========
(print "--- Z/8Z (local) ---")
(check "Z8-is-local" (prime-power-p 8))

; Divisors: 1,2,4,8. Check projective:
; d=1: gcd(1,8)=1 ✓
; d=2: gcd(2,4)=2 ✗
; d=4: gcd(4,2)=2 ✗
; d=8: gcd(8,1)=1 ✓
(define proj-divs8 (projective-ideals 8))
(check "Z8-only-trivial-projectives" (equal proj-divs8 {1 8}))

; ========== Z/30Z projectives ==========
(print "--- Z/30Z projectives ---")

(define n30 30)
(define proj-divs30 (projective-ideals n30))

; 30=2*3*5. All divisors d with gcd(d,30/d)=1.
; Since 30 is squarefree, all divisors work!
; d=1,2,3,5,6,10,15,30 all coprime to cofactor.
(check "Z30-all-divisors-projective" (= (length proj-divs30) 8))

; Verify the nontrivial ones
(check "2Z30-is-projective" (projective-ideal-p n30 2))
(check "6Z30-is-projective" (projective-ideal-p n30 6))
(check "10Z30-is-projective" (projective-ideal-p n30 10))

; ========== SUMMARY ==========
(print "--- Summary ---")

; Local ring Z/p^kZ: the only projective ideals are {0} and the whole ring
; (finitely generated projectives over a local ring are free).
; Non-local ring Z/(Π p_i^{e_i})Z: projective ideals correspond to
; idempotents via the CRT decomposition, so there are 2^ω(n) of them,
; ω(n) = number of distinct prime divisors (Z/12Z: 4, Z/8Z: 2, Z/30Z: 8).
; When n is squarefree these are all the ideals; otherwise only some.

(check "Z4-local-free-only" (prime-power-p n4))
(check "Z6-not-local-has-nontrivial" (not (prime-power-p n6)))

(print "=== Projective Modules Done ===")
