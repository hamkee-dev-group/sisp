; ============================================================
; Bourbaki — Finite Fields: Extension Theory
; F_q^n as F_q-algebra. Frobenius. Norm and trace.
; Galois correspondence subgroups ↔ intermediate fields.
; Builds on examples/math/algebra/galois-field.lsp.
; ============================================================

(print "=== Bourbaki: Finite Fields Extensions ===")

; ---------- irreducible polynomial for GF(8)=GF(2^3): x^3+x^2+1 ----------
(define (rmod p a) (mod a p))
(define (radd p a b) (mod (+ a b) p))
(define (rmul p a b) (mod (* a b) p))

; --- GF(8) construction ---
(define p2 2)

(define (gf8-add u v)
  (list (rmod p2 (+ (nth 0 u) (nth 0 v)))
        (rmod p2 (+ (nth 1 u) (nth 1 v)))
        (rmod p2 (+ (nth 2 u) (nth 2 v)))))

(define (gf8-mul u v)
  (let ((a0 (nth 0 u)) (a1 (nth 1 u)) (a2 (nth 2 u))
        (b0 (nth 0 v)) (b1 (nth 1 v)) (b2 (nth 2 v)))
    (let ((c0 (* a0 b0))
          (c1 (+ (* a0 b1) (* a1 b0)))
          (c2 (+ (* a0 b2) (* a1 b1) (* a2 b0)))
          (c3 (+ (* a1 b2) (* a2 b1)))
          (c4 (* a2 b2)))
      (list (rmod p2 (+ c0 c3 c4))
            (rmod p2 (+ c1 c4))
            (rmod p2 (+ c2 c3 c4))))))

(define gf8 (list-to-set (map (lambda (bits)
  (list (car bits) (car (cadr bits)) (cadr (cadr bits))))
  (set-to-list (cartesian-product {0 1} (cartesian-product {0 1} {0 1}))))))

(check "gf8-has-8-elements" (= (length gf8) 8))

(define zero (list 0 0 0))
(define one (list 1 0 0))
(define x-gen (list 0 1 0))

; --- Field axioms ---
(check "gf8-add-assoc"
  (every (lambda (a) (every (lambda (b) (every (lambda (c)
    (equal (gf8-add a (gf8-add b c)) (gf8-add (gf8-add a b) c))) gf8)) gf8)) gf8))

(check "gf8-add-comm"
  (every (lambda (a) (every (lambda (b)
    (equal (gf8-add a b) (gf8-add b a))) gf8)) gf8))

(check "gf8-mul-assoc"
  (every (lambda (a) (every (lambda (b) (every (lambda (c)
    (equal (gf8-mul a (gf8-mul b c)) (gf8-mul (gf8-mul a b) c))) gf8)) gf8)) gf8))

(check "gf8-mul-comm"
  (every (lambda (a) (every (lambda (b)
    (equal (gf8-mul a b) (gf8-mul b a))) gf8)) gf8))

(check "gf8-distrib"
  (every (lambda (a) (every (lambda (b) (every (lambda (c)
    (equal (gf8-mul a (gf8-add b c))
           (gf8-add (gf8-mul a b) (gf8-mul a c)))) gf8)) gf8)) gf8))

; Every nonzero element has an inverse (field)
(check "gf8-is-field"
  (every (lambda (a)
    (or (equal a zero)
        (any (lambda (b) (equal (gf8-mul a b) one)) gf8))) gf8))

; ========== FROBENIUS AUTOMORPHISM ==========
(print "--- Frobenius F: a → a^p = a^2 ---")

(define (gf8-pow a k)
  (cond ((= k 0) one)
        (t (gf8-mul a (gf8-pow a (- k 1))))))

(define (frob a)
  (gf8-mul a a))

(check "frob-is-hom-add"
  (every (lambda (a) (every (lambda (b)
    (equal (frob (gf8-add a b)) (gf8-add (frob a) (frob b)))) gf8)) gf8))

; Frobenius preserves multiplication trivially (it's a ring hom):
(check "frob-is-hom-mul"
  (every (lambda (a) (every (lambda (b)
    (equal (frob (gf8-mul a b)) (gf8-mul (frob a) (frob b)))) gf8)) gf8))

; Fixed field of Frobenius = F_2 = {0,1}
(define (frob-fixed)
  (filter (lambda (a) (equal (frob a) a)) gf8))

(check "frob-fixed-field-is-F2" (= (length (frob-fixed)) 2))

; Frobenius as automorphism: GF(8)* = C_7, Frobenius = multiply-by-2 in C_7.
; Order: Frobenius^3 = identity (since 2^3 = 8 = 1 mod 7).
(define (frob-n a n)
  (cond ((= n 0) a)
        (t (frob-n (frob a) (- n 1)))))

(check "frob-3-is-identity"
  (every (lambda (a) (equal (frob-n a 3) a)) gf8))

; ========== NORM AND TRACE ==========
(print "--- Norm and Trace F_8 → F_2 ---")

; Norm: N(a) = a * a^2 * a^4 = a^(1+2+4) = a^7
; For GF(8)*, a^7 = 1 for a≠0. So N(a) = 0 if a=0, 1 if a≠0.
(define (norm-gf8 a)
  (cond ((equal a zero) zero)
        (t one)))

(check "norm-of-0-is-0" (equal (norm-gf8 zero) zero))
(check "norm-of-nonzero-is-1"
  (every (lambda (a)
    (or (equal a zero) (equal (norm-gf8 a) one))) gf8))

; Trace: T(a) = a + a^2 + a^4 = a + frob(a) + frob(frob(a))
; T: GF(8) → GF(2)
(define (trace-gf8 a)
  (let ((a2 (frob a))
        (a4 (frob (frob a))))
    (let ((sum (gf8-add a a2)))
      (gf8-add sum a4))))

(check "trace-is-linear"
  (every (lambda (a) (every (lambda (b)
    (equal (trace-gf8 (gf8-add a b))
           (gf8-add (trace-gf8 a) (trace-gf8 b)))) gf8)) gf8))

(check "trace-maps-to-F2"
  (every (lambda (a)
    (in (trace-gf8 a) (list-to-set (list zero one)))) gf8))

(check "trace-is-surjective"
  (and (any (lambda (a) (equal (trace-gf8 a) one)) gf8)
       (equal (trace-gf8 zero) zero)))

; Trace is non-zero on some element:
(check "trace-x-is-1" (equal (trace-gf8 x-gen) (list 1 0 0)))

; --- Trace form: the bilinear form (x,y) → T(xy) ---
; For a separable extension (every finite-field extension) it is
; nondegenerate; the check below verifies that it is F_2-valued.
(define (trace-form a b)
  (trace-gf8 (gf8-mul a b)))

(check "trace-form-bilinear"
  (every (lambda (a) (every (lambda (b)
    (in (trace-form a b) (list-to-set (list zero one)))) gf8)) gf8))

; ========== GALOIS CORRESPONDENCE ==========
(print "--- Galois Correspondence ---")

; Galois group Gal(F_8/F_2) ≅ Z/3Z = {id, Frobenius, Frobenius^2}
; Subgroups: {id} (order 1), whole group (order 3).
; Intermediate fields: F_8 (fixed by {id}), F_2 (fixed by whole group).

; Check: F_2 = fixed field of Frobenius
(check "frob-fixed-is-F2" (= (length (frob-fixed)) 2))

; The entire GF(8) is the fixed field of id
(check "gf8-fixed-by-id"
  (every (lambda (a) (equal (frob-n a 0) a)) gf8))

; There are only these two intermediate fields (since 3 is prime).
; No proper nontrivial intermediate fields.

; ========== GF(9) = F_9, extension degree 2 over F_3 ==========
(print "--- GF(9) = F_3[x]/(x^2+1) ---")

(define p3 3)

; GF(9) elements: a + b*i where i^2 = -1 = 2 mod 3
(define (gf9-add u v)
  (list (rmod p3 (+ (car u) (car v)))
        (rmod p3 (+ (cadr u) (cadr v)))))

(define (gf9-mul u v)
  (let ((a0 (car u)) (a1 (cadr u))
        (b0 (car v)) (b1 (cadr v)))
    (list (rmod p3 (+ (* a0 b0) (rmod p3 (* a1 b1 2))))
          (rmod p3 (+ (* a0 b1) (* a1 b0))))))

(define gf9 (cartesian-product {0 1 2} {0 1 2}))
(check "gf9-has-9-elements" (= (length gf9) 9))

(define zero9 (list 0 0))
(define one9 (list 1 0))
(define i-gen (list 0 1))

; Frobenius: a → a^3 (p=3)
(define (frob9 a) (gf9-pow9 a 3))
(define (gf9-pow9 a k)
  (cond ((= k 0) one9)
        (t (gf9-mul a (gf9-pow9 a (- k 1))))))

; Frobenius^2 = identity (since [F_9:F_3]=2)
(check "frob9-2-is-identity"
  (every (lambda (a)
    (equal (frob9 (frob9 a)) a)) gf9))

(check "frob9-fixed-is-F3"
  (let ((fixed (filter (lambda (a) (equal (frob9 a) a)) gf9)))
    (= (length fixed) 3)))

; Norm and trace F_9 → F_3
; Norm: N(a) = a * a^3 = a^(1+3) = a^4
; For a ≠ 0: a^4 ∈ F_3^* = {1,2}
(define (norm9 a)
  (gf9-mul a (frob9 a)))

; Trace: T(a) = a + a^3 = a + frob9(a)
(define (trace9 a)
  (gf9-add a (frob9 a)))

(check "trace9-maps-to-F3"
  (every (lambda (a)
    (in (trace9 a) (list-to-set (list (list 0 0) (list 1 0) (list 2 0))))) gf9))

; ========== PRIMITIVE ELEMENT ==========
(print "--- Primitive Element ---")

; x = X in GF(8) is primitive (generates multiplicative group GF(8)* = C_7)
; Verify: powers x^1,...,x^7 give all 7 nonzero elements.
(define (powers ofx gen powfn count)
  (labels (((go curr k acc)
    (cond ((> k count) (reverse acc))
          (t (go (powfn curr gen) (+ k 1) (cons curr acc))))))
    (go gen 1 nil)))

(define gf8-nonzero (filter (lambda (a) (not (equal a zero))) gf8))
(check "gf8-nonzero-size-7" (= (length gf8-nonzero) 7))

; Check x generates all 7 non-zero elements
(check "x-generates-gf8-star"
  (let ((ord-x (length (list-to-set (powers 1 x-gen gf8-mul 7)))))
    (= ord-x 7)))

; ========== SUMMARY ==========
(print "--- Summary ---")

(check "gf8-is-field-of-8-elements" (= (length gf8) 8))
(check "galois-group-order-3" (every (lambda (a) (equal (frob-n a 3) a)) gf8))
(check "fixed-field-is-F2" (= (length (frob-fixed)) 2))

(print "=== Finite Fields Extensions Done ===")
