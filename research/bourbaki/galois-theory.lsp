; ============================================================
; Bourbaki — Galois Theory: Finite Separable Extensions
; Galois group, Galois correspondence, subfields.
; Verify for GF(8)=F_2[x]/(x^3+x+1) over F_2.
; ============================================================

(print "=== Bourbaki: Galois Theory ===")

; ---------- GF(8) as F_2[x]/(x^3+x+1) ----------
(define p2 2)

(define (gf8-add u v)
  (list (mod (+ (car u) (car v)) p2)
        (mod (+ (cadr u) (cadr v)) p2)
        (mod (+ (caddr u) (caddr v)) p2)))

; irreducible x^3+x+1: x^3 = x+1 in GF(2).
; Multiplying (a2 x^2 + a1 x + a0)*(b2 x^2 + b1 x + b0) mod x^3+x+1:
; raw = c0 + c1*x + c2*x^2 + c3*x^3 + c4*x^4 where
; c0=a0b0, c1=a0b1+a1b0, c2=a0b2+a1b1+a2b0, c3=a1b2+a2b1, c4=a2b2
; Replace x^3=x+1, x^4=x*x^3=x(x+1)=x^2+x
; = c0 + c1*x + c2*x^2 + c3*(x+1) + c4*(x^2+x)
; = (c0+c3) + (c1+c3+c4)*x + (c2+c4)*x^2  (all mod 2)
(define (gf8-mul u v)
  (let ((a0 (car u)) (a1 (cadr u)) (a2 (caddr u))
        (b0 (car v)) (b1 (cadr v)) (b2 (caddr v)))
    (let ((c0 (mod (* a0 b0) 2))
          (c1 (mod (+ (* a0 b1) (* a1 b0)) 2))
          (c2 (mod (+ (* a0 b2) (* a1 b1) (* a2 b0)) 2))
          (c3 (mod (+ (* a1 b2) (* a2 b1)) 2))
          (c4 (mod (* a2 b2) 2)))
      (list (mod (+ c0 c3) 2)
            (mod (+ c1 c3 c4) 2)
            (mod (+ c2 c4) 2)))))

; Elements: all 8 triples over F_2
; cartesian-product nests as (a (b c)), so flatten each element by hand:
; (nth 1 bits) is the inner PAIR (b c) and (nth 2 bits) is nil, hence the
; (car (cadr bits)) and (cadr (cadr bits)) below.
(define gf8 (list-to-set
  (map (lambda (bits)
    (list (car bits) (car (cadr bits)) (cadr (cadr bits))))
    (set-to-list (cartesian-product {0 1} (cartesian-product {0 1} {0 1}))))))

(define zero8 (list 0 0 0))
(define one8  (list 1 0 0))
(define x-gen (list 0 1 0))

(check "gf8-size-8" (= (length gf8) 8))

; ---------- Verify x^3 + x + 1 = 0 for generator x ----------
; x^3 = x+1 in GF(2). Compute x^3 and verify.
(define xsq (gf8-mul x-gen x-gen))
(define xcu (gf8-mul xsq x-gen))
(define xpl1 (gf8-add x-gen one8))
(check "x^3+x+1=0" (equal xcu xpl1))

; ---------- Field axioms ----------
(check "gf8-add-comm"
  (every (lambda (a) (every (lambda (b)
    (equal (gf8-add a b) (gf8-add b a))) gf8)) gf8))

(check "gf8-mul-comm"
  (every (lambda (a) (every (lambda (b)
    (equal (gf8-mul a b) (gf8-mul b a))) gf8)) gf8))

; Every nonzero has inverse (F_2[x]/(x^3+x+1) is a field)
(check "gf8-is-field"
  (every (lambda (a)
    (or (equal a zero8)
        (any (lambda (b) (equal (gf8-mul a b) one8)) gf8))) gf8))

; ---------- Galois group Gal(F_8/F_2) ----------
; Frobenius F(a) = a^2 (p=2)
(define (frob a) (gf8-mul a a))

; Frobenius is a field automorphism
(check "frob-is-hom-add"
  (every (lambda (a) (every (lambda (b)
    (equal (frob (gf8-add a b)) (gf8-add (frob a) (frob b)))) gf8)) gf8))

(check "frob-is-hom-mul"
  (every (lambda (a) (every (lambda (b)
    (equal (frob (gf8-mul a b)) (gf8-mul (frob a) (frob b)))) gf8)) gf8))

; Gal(F_8/F_2) = {id, Frob, Frob^2} ≅ Z/3Z
(define (frob-n a n)
  (cond ((= n 0) a)
        (t (frob-n (frob a) (- n 1)))))

(check "frob-3-is-identity"
  (every (lambda (a) (equal (frob-n a 3) a)) gf8))

; The Galois group has order 3
(define gal-group (list-to-set (list
  (lambda (a) a)
  (lambda (a) (frob a))
  (lambda (a) (frob (frob a))))))
(check "gal-group-order-3" (= (length gal-group) 3))

; ---------- Fixed fields of subgroups ----------
; Subgroup of order 1 {id}: fixed field = F_8
; The identity automorphism equals Frob^3; filtering by Frob^3(a) = a tests
; this (filtering by a = a would be vacuous).
(define fix-id (filter (lambda (a) (equal (frob-n a 3) a)) gf8))
(check "fix-id-is-F8" (= (length fix-id) 8))

; Subgroup of order 3 (whole group): fixed field = F_2
(define fix-gal (filter (lambda (a) (equal (frob a) a)) gf8))
(check "fix-gal-is-F2" (= (length fix-gal) 2))

; Verify F_2 = {0, 1}
(define F2-elements (list-to-set (list zero8 one8)))
(check "fixed-field-is-primitive" (equal fix-gal F2-elements))

; ---------- Galois correspondence ----------
; Subgroups of Z/3Z: {id} (order 1) and whole group (order 3)
; Corresponding fixed fields: F_8 and F_2
; Normal subgroups: both {id} and Z/3Z are normal in Z/3Z
; Both extensions are Galois (normal + separable)

; There are no proper nontrivial intermediate fields because 3 is prime.
; The lattice: F_2 ⊂ F_8 with degree [F_8:F_2]=3.

; ---------- Power function ----------
(define (gf8-pow a n)
  (cond ((= n 0) one8)
        (t (gf8-mul a (gf8-pow a (- n 1))))))

; ---------- Extension degree ----------
; [F_8 : F_2] = 3 since |F_8| = 2^3, and it equals |Gal(F_8/F_2)|
; (the extension is Galois).
(check "extension-degree-is-3"
  (and (= (length gf8) (^ p2 3))
       (= (length gal-group) 3)))

; ---------- Primitive elements ----------
; x is a root of x^3+x+1=0 in F_2[x]
; The other roots are x^2 and x^4 = x^2 + x (since x^3 = x+1)
(define root1 x-gen)
(define root2 (frob x-gen))
(define root3 (frob (frob x-gen)))

(check "three-conjugates"
  (and (not (equal root1 root2))
       (not (equal root2 root3))
       (not (equal root3 root1))))

; All three are roots of x^3+x+1
; let bindings are parallel, so a^3 is computed in a nested let.
(define (eval-polya a)
  (let ((asq (gf8-mul a a)))
    (let ((acu (gf8-mul asq a)))
      (gf8-add (gf8-add acu a) one8))))

(check "root1-is-root" (equal (eval-polya root1) zero8))
(check "root2-is-root" (equal (eval-polya root2) zero8))
(check "root3-is-root" (equal (eval-polya root3) zero8))

; ---------- Norm and trace ----------
; N(a) = a * a^2 * a^4 = a^{1+2+4} = a^7
; For GF(8)* = C_7: a^7=1 for all a≠0
; norm8 computes N(a) directly as a * Frob(a) * Frob^2(a).
(define (norm8 a)
  (gf8-mul (gf8-mul a (frob a)) (frob (frob a))))

(check "norm-maps-to-F2"
  (every (lambda (a) (in (norm8 a) F2-elements)) gf8))

; Trace: T(a) = a + a^2 + a^4
(define (trace8 a)
  (let ((a2 (frob a))
        (a4 (frob (frob a))))
    (gf8-add (gf8-add a a2) a4)))

(check "trace-maps-to-F2"
  (every (lambda (a) (in (trace8 a) F2-elements)) gf8))

(check "trace-is-surjective"
  (and (any (lambda (a) (equal (trace8 a) one8)) gf8)
       (equal (trace8 zero8) zero8)))

; ---------- All subfields of F_8 ----------
; F_8 has subfields: F_2 (the base field)
; There is no proper intermediate field since 3 is prime.
; So the only subfields are F_2 and F_8 itself.

; A subfield must contain both 0 and 1, hence has at least 2 elements.
; Testing that first also skips the empty set from (powerset gf8): (in <list> {})
; raises "=: NOT NUMERIC" rather than returning nil.
(define (subfield-p s)
  (cond ((< (length s) 2) nil)
        (t (and (subset s gf8)
       (in zero8 s) (in one8 s)
       (every (lambda (a) (every (lambda (b)
         (in (gf8-add a b) s)) s)) s)
       (every (lambda (a) (every (lambda (b)
         (in (gf8-mul a b) s)) s)) s)
                (every (lambda (a)
                  (or (equal a zero8)
                      (any (lambda (b) (equal (gf8-mul a b) one8)) s))) s)))))

(define all-subfields (filter (lambda (s) (subfield-p s)) (powerset gf8)))

(check "subfields-are-F2-and-F8"
  (let ((sizes (map (lambda (s) (length s)) all-subfields)))
    (equal sizes {2 8})))

; ---------- Discriminant ----------
; For the cubic x^3+x+1 over F_2:
; disc(x^3 + px + q) = -4p^3 - 27q^2 = -4 - 27 = -31 ≡ 1 mod 2.
; It is nonzero, so the cubic is separable (three distinct roots).
; The classical test "Galois group ⊆ A_3 iff disc is a square" needs
; characteristic ≠ 2; in F_2 every element is a square, so it says nothing
; here. Over a finite field the Galois group of a finite extension is always
; cyclic, generated by Frobenius, so Gal = A_3 ≅ Z/3Z, as checked below.

; Frob has order exactly 3: Frob^3 = id, but Frob and Frob^2 are not the identity.
(check "gal-group-is-cyclic-3"
  (and (every (lambda (a) (equal (frob-n a 3) a)) gf8)
       (any (lambda (a) (not (equal (frob-n a 1) a))) gf8)
       (any (lambda (a) (not (equal (frob-n a 2) a))) gf8)))

; ---------- F_9 = F_3[x]/(x^2+1) for comparison ----------
(print "--- GF(9) over F_3 ---")
(define p3 3)

(define (gf9-add u v)
  (list (mod (+ (car u) (car v)) p3)
        (mod (+ (cadr u) (cadr v)) p3)))

(define (gf9-mul u v)
  (let ((a0 (car u)) (a1 (cadr u))
        (b0 (car v)) (b1 (cadr v)))
    (list (mod (+ (* a0 b0) (mod (* 2 a1 b1) p3)) p3)
          (mod (+ (* a0 b1) (* a1 b0)) p3))))

(define gf9 (cartesian-product {0 1 2} {0 1 2}))
(define zero9 (list 0 0))
(define one9 (list 1 0))
(define i-gen (list 0 1))

(check "gf9-size-9" (= (length gf9) 9))

; Gal(F_9/F_3) ≅ Z/2Z: Frobenius a→a^3
(define (frob9 a) (gf9-mul (gf9-mul a a) a))
(check "frob9-is-automorphism"
  (every (lambda (a) (every (lambda (b)
    (equal (frob9 (gf9-add a b)) (gf9-add (frob9 a) (frob9 b)))) gf9)) gf9))

(check "frob9-2-is-identity"
  (every (lambda (a) (equal (frob9 (frob9 a)) a)) gf9))

; Fixed field = F_3
(define fix9 (filter (lambda (a) (equal (frob9 a) a)) gf9))
(check "fix-frob9-is-F3" (= (length fix9) 3))

(print "=== Galois Theory Done ===")
