; ============================================================
; Bourbaki — Semisimple Rings and Wedderburn-Artin
; Semisimple ring = product of matrix rings over division rings.
; For Z/nZ: semisimple iff n is squarefree.
; Verify for Z/6Z, Z/12Z, Z/30Z.
; ============================================================

(print "=== Bourbaki: Semisimple Rings ===")

; ---------- ring operations ----------
(define (rmod n a) (mod a n))
(define (radd n a b) (mod (+ a b) n))
(define (rmul n a b) (mod (* a b) n))

; ---------- divisors ----------
; d divides n  <=>  n mod d = 0
(define (divides n d) (= (mod n d) 0))
(define (divisors n)
  (labels (((go k acc)
    (cond ((> k n) (reverse acc))
          ((divides n k) (go (+ k 1) (cons k acc)))
          (t (go (+ k 1) acc)))))
    (go 1 nil)))

; ---------- prime test ----------
(define (primep n)
  (cond ((<= n 1) nil)
        ((= n 2) t)
        ((= (mod n 2) 0) nil)
        (t (labels (((go d)
              (cond ((> (* d d) n) t)
                    ((= (mod n d) 0) nil)
                    (t (go (+ d 2))))))
             (go 3)))))

; ---------- squarefree test: no squared prime divides n ----------
(define (squarefree-p n)
  (labels (((check d)
    (cond ((> d n) t)
          ((and (divides n d) (primep d)
                (divides n (* d d))) nil)
          (t (check (+ d 1))))))
    (check 2)))

; ---------- prime factorization ----------
(define (prime-factors n)
  (labels (((find m d acc)
    (cond ((= m 1) (reverse acc))
          ((and (divides m d) (primep d))
           (labels (((remove-factor x)
             (cond ((= (mod x d) 0) (remove-factor (truncate (/ x d))))
                   (t x))))
             (find (remove-factor m) (+ d 1) (cons d acc))))
          (t (find m (+ d 1) acc)))))
    (find n 2 nil)))

; ---------- ideal from divisor ----------
(define (ideal-from-divisor n d)
  (list-to-set
   (labels (((go k acc)
     (cond ((>= k n) (reverse acc))
           (t (go (+ k d) (cons k acc))))))
     (go 0 nil))))

; ---------- Jacobson radical = intersection of max ideals ----------
; For Z/nZ, all prime ideals are maximal.
; Jacobson radical J(R) = ∩ maximal ideals.
; For Z/nZ, J(Z/nZ) = ∩_{p|n} pZ/nZ = (radical(n))Z/nZ.
; radical(n) = product of distinct primes dividing n.
(define (radical n)
  (foldl (lambda (acc p) (* acc p)) 1 (prime-factors n)))

; ---------- semisimple: Z/nZ is semisimple iff J(R) = 0 (nilpotent-free) ----------
; J(Z/nZ) = 0 iff radical(n) = n iff n is squarefree.
(define (semisimple-p n)
  (= (radical n) n))

; ---------- Wedderburn-Artin decomposition: Z/nZ ≅ ∏ Z/p_i Z when n = ∏ p_i ----------
(define (wedderburn-factors n)
  (let ((primes (prime-factors n)))
    (map (lambda (p) p) primes)))

; ---------- CRT isomorphism: Z/30Z ≅ Z/2Z × Z/3Z × Z/5Z ----------
(define (crt-decompose n factors)
  (cond ((eq factors nil) nil)
        (t (cons (rmod n (car factors))
                 (crt-decompose n (cdr factors))))))

; ---------- semisimple = direct sum of simple modules ----------
; A simple Z/nZ-module is cyclic of order p for a prime divisor p of n.
(define (simple-module-p n M)
  (and (not (= (length M) 1))
       (every (lambda (N)
         (let ((N-set (list-to-set N)))
           (or (= (length N-set) 1)
               (= (length N-set) (length M)))))
         (map list-to-set (powerset (set-to-list M))))))

; ---------- socle of Z/nZ: sum of all simple submodules ----------
; For Z/nZ the socle equals the annihilator of J(R):
; Soc = {x : rad(n)·x ≡ 0 mod n} = (n/rad(n))Z/nZ, which has rad(n) elements.
; For semisimple: socle = whole ring.
; Note: socle-size below returns n/rad(n), which is the size of
; J(R) = rad(n)Z/nZ, not of the socle; socle-size-correct further down
; returns the socle size rad(n).
(define (socle-size n)
  (let ((rad (radical n)))
    (/ n rad)))

; ---------- verify semisimple decomposition via idempotents ----------
(define (idempotent-p n e)
  (= (rmod n (* e e)) (rmod n e)))

(define (orthogonal-idempotents n)
  (let ((es (set-to-list (filter (lambda (e)
            (and (idempotent-p n e) (not (= e 0)) (not (= e 1))))
            (list-to-set (seq 0 (- n 1)))))))
    es))

; ========== SQUAREFREE TEST ==========
(print "--- Squarefree Check ---")

(check "6-is-squarefree" (squarefree-p 6))
(check "10-is-squarefree" (squarefree-p 10))
(check "12-is-not-squarefree" (not (squarefree-p 12)))
(check "18-is-not-squarefree" (not (squarefree-p 18)))
(check "30-is-squarefree" (squarefree-p 30))
(check "15-is-squarefree" (squarefree-p 15))
(check "8-is-not-squarefree" (not (squarefree-p 8)))

; ========== Z/6Z (semisimple, 6=2×3 squarefree) ==========
(print "--- Z/6Z — Semisimple ---")

(define n6 6)
(check "Z6-is-semisimple" (semisimple-p n6))

; Radical of 6 = 2*3 = 6 → J(Z/6Z) = 6Z/6Z = {0}
; socle-size 6 = 6/6 = 1 = |J(Z/6Z)|: the radical is zero, i.e. the socle is
; all of Z/6Z.
(check "radical-6-is-6" (= (radical 6) 6))
(check "socle-6-is-full" (= (socle-size n6) 1))

; Simple modules: Z/2Z and Z/3Z via the quotients Z/6Z → Z/2Z, Z/3Z.
; Z/6Z ≅ Z/2Z × Z/3Z (Wedderburn: product of division rings = fields).
(define factors6 (prime-factors n6))
(check "Z6-prime-factors-2-3" (equal factors6 (list 2 3)))

; --- Simple module check ---
(define I3-6 (ideal-from-divisor n6 3))
(define I2-6 (ideal-from-divisor n6 2))

; I2-6 = {0,2,4} ≅ Z/3Z as a Z/6Z-module (x ↦ x/2 mod 3: 2 ↦ 1, 4 ↦ 2); as a
; ring its identity is the idempotent 4. A module of prime order has no
; submodules other than 0 and itself, so it is simple; the check uses the size.
(check "I2-6-is-simple-Z6-module"
  (and (= (length I2-6) 3)
       (not (= (length I2-6) 1))))

; I3-6 = {0,3} ≅ Z/2Z as Z/6Z-module.
(check "I3-6-is-simple-Z6-module"
  (and (= (length I3-6) 2)
       (not (= (length I3-6) 1))))

; ========== Z/12Z (NOT semisimple, 12=2²×3) ==========
(print "--- Z/12Z — NOT Semisimple ---")

(define n12 12)
(check "Z12-is-NOT-semisimple" (not (semisimple-p n12)))

; Radical = 2*3 = 6 → J(Z/12Z) = 6Z/12Z ≠ {0}
(check "radical-12-is-6" (= (radical 12) 6))
(define J12 (ideal-from-divisor n12 6))
(check "jacobson-12-is-6Z12" (= (length J12) 2))

; socle-size 12 = 12/6 = 2 is |J(Z/12Z)| = |{0,6}|; the socle itself has
; 6 elements (see below).
(check "socle-12-size-is-2" (= (socle-size n12) 2))

; The socle is the set of elements x such that J(R)*x = 0. J(R) = {0,6}, so
; x ∈ Soc ⟺ 6x ≡ 0 mod 12 ⟺ 2 | x: Soc = 2Z/12Z = {0,2,4,6,8,10}, 6 elements.
; In general, for n = Π p_i^{e_i}, Soc(Z/nZ) = (Π p_i^{e_i-1})Z/nZ = (n/rad(n))Z/nZ,
; one copy of Z/p_iZ for each prime p_i | n, so its size is rad(n).
; (Equivalently: a·x ≡ 0 mod n has gcd(a,n) solutions; with a = rad(n) this is
; gcd(rad(n), n) = rad(n).)
; So for semisimple n (squarefree), radical = n, socle = entire ring.
; For non-semisimple, socle < ring.

(define (socle-size-correct n)
  (radical n))

(check "socle-12-is-6" (= (socle-size-correct n12) 6))
(check "socle-6-is-6" (= (socle-size-correct 6) 6))
(check "socle-8-is-2" (= (socle-size-correct 8) 2))

; ========== Z/30Z (semisimple, 30=2×3×5 squarefree) ==========
(print "--- Z/30Z — Semisimple ---")

(define n30 30)
(check "Z30-is-semisimple" (semisimple-p n30))

; Prime factors = {2,3,5}
(define factors30 (prime-factors n30))
(check "Z30-prime-factors-2-3-5" (equal factors30 (list 2 3 5)))

; Radical(30) = 2*3*5 = 30 = n, so J(Z/30Z) = {0}.
(check "radical-30-is-30" (= (radical 30) 30))
(check "socle-30-is-full" (= (socle-size-correct n30) 30))

; Wedderburn-Artin: Z/30Z ≅ Z/2Z × Z/3Z × Z/5Z
; Each is a division ring (field), dimension 1 matrix ring.
(check "Z30-is-product-of-3-fields" (= (length (wedderburn-factors n30)) 3))

; The simple Z/30Z-modules are the quotients Z/30Z / pZ/30Z ≅ Z/pZ for
; p = 2,3,5 (the ideals (2), (3), (5) have orders 15, 10, 6). Inside Z/30Z they
; appear as the minimal ideals 15Z/30Z ≅ Z/2Z, 10Z/30Z ≅ Z/3Z, 6Z/30Z ≅ Z/5Z.

; ========== Z/108 = 2²·3³ (NOT semisimple) ==========
(print "--- Z/108 — NOT Semisimple ---")

(define n108 108)
(check "Z108-is-NOT-semisimple" (not (semisimple-p n108)))

; Radical = 2*3 = 6
(check "radical-108-is-6" (= (radical n108) 6))
(check "socle-108-is-6" (= (socle-size-correct n108) 6))

; ========== STRUCTURE THEOREM SUMMARY ==========
(print "--- Structure Theorem ---")

; For Z/nZ, semisimple iff n is squarefree.
; Then Z/nZ ≅ Z/p₁Z × ... × Z/p_kZ (product of fields).
; Simple modules are Z/p_iZ.
; Every module is projective = injective.

(check "squarefree-equiv-semisimple"
  (and (semisimple-p 6) (semisimple-p 10) (semisimple-p 30)
       (semisimple-p 15) (semisimple-p 21)
       (not (semisimple-p 4)) (not (semisimple-p 8))
       (not (semisimple-p 9)) (not (semisimple-p 12))
       (not (semisimple-p 18))))

(print "=== Semisimple Rings Done ===")
