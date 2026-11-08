; ============================================================
; Bourbaki — Injective Modules over Finite Rings
; Injective = extension property. Baer criterion.
; Injective hull exists. Injectivity for Z/nZ modules.
; Verified on Z/4Z, Z/6Z.
; ============================================================

(print "=== Bourbaki: Injective Modules ===")

; ---------- ring operations ----------
(define (rmod n a) (mod a n))
(define (radd n a b) (mod (+ a b) n))
(define (rmul n a b) (mod (* a b) n))

; ---------- divisors ----------
(define (divides n d) (= (rmod n d) 0))
(define (divisors n)
  (labels (((go k acc)
    (cond ((> k n) (reverse acc))
          ((divides n k) (go (+ k 1) (cons k acc)))
          (t (go (+ k 1) acc)))))
    (go 1 nil)))

; ---------- ideal dZ/nZ ----------
(define (ideal-from-divisor n d)
  (list-to-set
   (labels (((go k acc)
     (cond ((>= k n) (reverse acc))
           (t (go (+ k d) (cons k acc))))))
     (go 0 nil))))

; ---------- submodule test ----------
(define (submodule-p n M)
  (and (in 0 M)
       (every (lambda (a)
         (every (lambda (b) (in (radd n a b) M)) M)) M)
       (every (lambda (a) (in (rmod n (- 0 a)) M)) M)
       (every (lambda (a)
         (every (lambda (r) (in (rmul n r a) M))
           (list-to-set (seq 0 (- n 1))))) M)))

; ---------- extension property for an injective module ----------
; A module E is injective if every diagram
;   0 → N → M
;       ↓f
;       E
; can be completed: there exists f': M → E extending f.
; Baer's criterion (valid over any ring): it suffices to take N = an ideal of R.
; E is injective ⟺ every R-linear map I → E extends to R → E, for every ideal I.

; ---------- Baer criterion for cyclic modules of Z/nZ ----------
; Let I = eZ/nZ be an ideal and E a Z/nZ-module. A linear map f: I → E is
; fixed by y = f(e), which can be any element of E killed by Ann(e) = (n/e).
; A linear map g: Z/nZ → E is fixed by a = g(1) ∈ E, and g extends f iff
; a·e = y. So E is injective iff for every e | n and every such y there is
; an a ∈ E with a·e = y. hom-ideal-to-module below searches for this a.

; ---------- Z/nZ is injective over itself ----------
; Take E = Z/nZ. An element y ∈ Z/nZ killed by n/e is a multiple of e, say
; y = e·r, and then g(x) = r·x extends f. So Z/nZ is self-injective for
; every n (it is a quasi-Frobenius ring). Since Z/nZ is finite, the
; checks below also enumerate the extensions directly.
; z-nz-is-injective-p records this fact (it simply returns t).

(define (z-nz-is-injective-p n)
  t)

; ---------- injective hull: the smallest injective module containing a given module ----------
; The injective hull E(N) of N is a maximal essential extension of N; it is
; injective, and unique up to isomorphism over N.
; For the local ring Z/p^kZ, the injective hull of the residue field Z/pZ
; (embedded as the socle p^{k-1}Z/p^kZ) is Z/p^kZ itself.
; For Z/4Z: the submodule {0,2} = 2Z/4Z is ≅ Z/2Z = Z/4Z/(2) as a Z/4Z-module
; (2 acts as 0 on it), and its injective hull is the whole of Z/4Z.

; ---------- check if a module is an essential extension of a submodule ----------
; N ⊆ M is essential if for every nonzero submodule K ⊆ M, K ∩ N ≠ {0}.
(define (essential-extension-p n M nn)
  (and (subset nn M)
       (submodule-p n nn)
       (every (lambda (K)
         (let ((K-mod (list-to-set K)))
           (or (and (= (length K-mod) 1) (in 0 K-mod))
               (not (= (length (cap K-mod nn)) 1)))))
         (let ((M-lst (set-to-list M)))
           (set-to-list
            (filter (lambda (S)
              (submodule-p n (list-to-set S)))
              (map list-to-set (powerset (list-to-set M)))))))))

; ---------- injective hulls in Z/4Z ----------
; The injective hull of the zero module is the zero module; that of {0,2} is
; Z/4Z (checked in the Injective Hull section below).

; ---------- projective = injective over Z/nZ ----------
; Z/nZ is QF (quasi-Frobenius) for every n: a commutative Artinian ring is QF
; iff it is a product of local rings each with a simple socle, and
; Z/nZ ≅ ∏ Z/p^kZ, where the socle of Z/p^kZ is p^{k-1}Z/p^kZ ≅ Z/pZ.
; Over a QF ring a module is projective iff it is injective. So the injective
; ideals dZ/nZ are exactly the projective ones, the direct summands with
; gcd(d, n/d) = 1.
; If n is squarefree, Z/nZ is semisimple (a product of fields, von Neumann
; regular), and every module is projective and injective.
; If n is not squarefree, Z/nZ is not semisimple, so not every module is
; injective.
; Z/4Z: the ideal 2Z/4Z is not projective (gcd(2, 4/2) ≠ 1), so it is not
; injective either, although Z/4Z itself is. The Baer check below shows this
; directly. qf-ring-p records that Z/nZ is QF (it simply returns t).

(define (qf-ring-p n)
  t)

; ========== BAER CRITERION FOR Z/4Z ==========
(print "--- Baer Criterion on Z/4Z ---")

(define n4 4)
(define ring4 (list-to-set (seq 0 3)))
(define I0 (ideal-from-divisor n4 4))
(define I2-4 (ideal-from-divisor n4 2))

; Check if I2-4 = {0,2} is injective, using Baer's criterion.
; The ideals of Z/4Z are {0}, {0,2}, Z/4Z; J = {0} and J = Z/4Z are trivial
; (for J = Z/4Z take g = f).
; J = {0,2}: f(2) ∈ {0,2} (= I2-4).
;   If f(2)=0: extend by g(x)=0 for all x. ✓
;   If f(2)=2: need g: Z/4Z → I2-4 with g(2)=2. Since g is Z/4Z-linear,
;     g(x) = a·x with a = g(1) ∈ {0,2} (the image must lie in I2-4).
;     If a=0: g(2)=0 ≠ 2. If a=2: g(2)=4≡0 ≠ 2.
;     So NO extension exists: I2-4 is NOT injective.

(define (hom-ideal-to-module n I E gen-val)
  ; Linear map f: I → E determined by f(gen(I)) = gen-val.
  ; Check it extends to Z/nZ → E.
  (let ((I-gen (car (set-to-list (filter (lambda (x) (not (= x 0))) I)))))
    (labels (((find-extender a)
      (cond ((>= a n) nil)
            ((and (= (rmod n (* a I-gen)) (rmod n gen-val))
                  (in (rmod n a) E)) t)
            (t (find-extender (+ a 1))))))
      (find-extender 0))))

; For I = {0,2}, gen = 2, E = {0,2} = 2Z/4Z.
; f(2) = 2: check if extension exists.
(check "baer-fail-2Z4-extend-2"
  (not (hom-ideal-to-module n4 I2-4 I2-4 2)))

; f(2) = 0: trivial extension
(check "baer-pass-2Z4-extend-0"
  (hom-ideal-to-module n4 I2-4 I2-4 0))

; --- For E = Z/4Z itself ---
; f(2) = 2: extend by g(x) = x (g(1)=1, g(2)=2). Works!
(check "baer-Z4-self-injective-extend-2"
  (hom-ideal-to-module n4 I2-4 ring4 2))

; f(2) = 0: g(x)=0
(check "baer-Z4-self-injective-extend-0"
  (hom-ideal-to-module n4 I2-4 ring4 0))

; So Z/4Z is injective over itself, but 2Z/4Z is not.

; --- For E = {0} ---
; f(2) = 0: extend by g(x)=0. Works trivially.
(check "baer-zero-module-extend-0"
  (hom-ideal-to-module n4 I2-4 I0 0))

; ========== BAER CRITERION FOR Z/6Z ==========
(print "--- Baer Criterion on Z/6Z ---")

(define n6 6)
(define ring6 (list-to-set (seq 0 5)))
(define I0-6 (ideal-from-divisor n6 6))
(define I2-6 (ideal-from-divisor n6 2))
(define I3-6 (ideal-from-divisor n6 3))

; Z/6Z ≅ Z/2Z × Z/3Z (semisimple, hence all modules are injective).
; Verify: I3-6 = {0,3} is injective.
; J = {0}, trivial.
; J = {0,3}: f(3)=3. Find a ∈ I3-6 = {0,3} with a*3 ≡ 3 mod 6.
; a=1 would work but 1 ∉ I3-6; a=3 gives 3*3 = 9 ≡ 3 mod 6, so g(x) = 3x
; extends f. ✓ (3 is the idempotent generating 3Z/6Z.)
(check "baer-3Z6-injective-extend-3"
  (hom-ideal-to-module n6 I3-6 I3-6 3))

; Check I2-6 = {0,2,4} injective:
; J = {0,3}: f(3) must be killed by Ann(3) = (2), and the only such element
; of I2-6 is 0 (2·2 = 4, 2·4 = 2), so f = 0, which extends.
; J = {0,2,4}: f(2) = 2. Find a ∈ {0,2,4} with a*2 ≡ 2 mod 6, i.e. a ≡ 1 mod 3.
; a = 4 works: g(x) = 4x, g(2) = 8 ≡ 2 mod 6. ✓
; (4 is the idempotent generating 2Z/6Z: 4·4 = 16 ≡ 4.)
(check "baer-2Z6-injective-extend-2"
  (hom-ideal-to-module n6 I2-6 I2-6 2))

; So Z/6Z is semisimple → all modules injective.
; Z/4Z is not semisimple → not all modules injective.

; ========== INJECTIVE HULL ==========
(print "--- Injective Hull ---")

; Injective hull of {0,2} as Z/4Z-module = Z/4Z itself.
; Z/4Z is injective, and it is an essential extension of {0,2}: its
; submodules are {0}, {0,2} and Z/4Z, and each nonzero one meets {0,2}
; nontrivially. So Z/4Z is the injective hull of {0,2}.

; Injective hull of {0} as Z/4Z-module = {0}.
; Injective hull of Z/4Z as Z/4Z-module = Z/4Z itself.

; Verifying essential extension:
(define (proper-submodules n M)
  (filter (lambda (S)
    (and (not (equal (list-to-set S) M))
         (submodule-p n (list-to-set S))))
    (map list-to-set (powerset M))))

(define (has-essential-extension-p n M nn)
  (and (subset nn M)
       (submodule-p n nn)
       (let ((subs (cons (set-to-list M)
                         (set-to-list (proper-submodules n M)))))
         (every (lambda (K)
           (let ((K-set (list-to-set K)))
             (or (= (length K-set) 1)
                 (not (= (length (cap K-set nn)) 1)))))
           subs))))

; Check Z/4Z is essential extension of {0,2}.
; Submodules of Z/4Z: {0}, {0,2} and Z/4Z itself.
; {0} is the zero submodule and imposes no condition.
; {0,2} ∩ {0,2} = {0,2} and Z/4Z ∩ {0,2} = {0,2}, both of size 2 > 1. ✓
(check "Z4-essential-over-2Z4"
  (has-essential-extension-p n4 ring4 I2-4))

; Check: {0,2} is NOT essential over {0}: the nonzero submodule {0,2}
; meets {0} only in {0}.
(check "2Z4-NOT-essential-over-0"
  (not (has-essential-extension-p n4 I2-4 I0)))

; ========== SUMMARY ==========
(print "--- Summary ---")

(check "Z6-is-semisimple-all-injective"
  (and (hom-ideal-to-module n6 I2-6 I2-6 2)
       (hom-ideal-to-module n6 I3-6 I3-6 3)))

(check "Z4-not-all-modules-injective"
  (not (hom-ideal-to-module n4 I2-4 I2-4 2)))

(print "=== Injective Modules Done ===")
