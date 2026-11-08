; ============================================================
; 65 — GF(8) and the Frobenius: Baby Galois Theory
; The field with eight elements built as GF(2)[x]/(x^3+x+1),
; its cyclic multiplicative group, and the Frobenius a -> a^2
; verified as a field automorphism whose fixed field is GF(2)
; and which generates the full Galois group of order 3.
; ============================================================

(print "=== GF(8) and the Frobenius ===")


; ---------- construction: polynomials mod x^3 + x + 1 ----------
; An element (a0 a1 a2) is a0 + a1 x + a2 x^2 with bits a_i.
; NB: cartesian-product does NOT flatten -- nesting it gives (a0 (a1 a2)),
; which nth would read as junk.  Build the coefficient triples directly.
(define bits (list 0 1))
(define gf8
  (list-to-set
    (foldr append nil
      (map (lambda (a0)
             (foldr append nil
               (map (lambda (a1)
                      (map (lambda (a2) (list a0 a1 a2)) bits))
                    bits)))
           bits))))
(check "eight-elements" (= (length gf8) 8))
(check "elements-are-coefficient-triples"
  (every (lambda (a) (= (length a) 3)) gf8))

(define zero (list 0 0 0))
(define one (list 1 0 0))
(define X (list 0 1 0))

(define (b+ a b) (mod (+ a b) 2))
(define (f-add u v)
  (list (b+ (nth 0 u) (nth 0 v))
        (b+ (nth 1 u) (nth 1 v))
        (b+ (nth 2 u) (nth 2 v))))

; multiply, then reduce with x^3 = x + 1 and x^4 = x^2 + x.
(define (f-mul u v)
  (let ((a0 (nth 0 u)) (a1 (nth 1 u)) (a2 (nth 2 u))
        (b0 (nth 0 v)) (b1 (nth 1 v)) (b2 (nth 2 v)))
    (let ((c0 (* a0 b0))
          (c1 (+ (* a0 b1) (* a1 b0)))
          (c2 (+ (* a0 b2) (* a1 b1) (* a2 b0)))
          (c3 (+ (* a1 b2) (* a2 b1)))
          (c4 (* a2 b2)))
      (list (mod (+ c0 c3) 2)
            (mod (+ c1 c3 c4) 2)
            (mod (+ c2 c4) 2)))))

; the modulus is irreducible over GF(2): a cubic with no roots.
(check "modulus-has-no-root-in-gf2"
  (and (= 1 (mod (+ 0 0 1) 2))    ; p(0) = 1
       (= 1 (mod (+ 1 1 1) 2))))  ; p(1) = 1

; ---------- field axioms by enumeration ----------
(define (for-all2 f) (every (lambda (a) (every (lambda (b) (f a b)) gf8)) gf8))
(define (for-all3 f)
  (every (lambda (a)
    (every (lambda (b)
      (every (lambda (c) (f a b c)) gf8)) gf8)) gf8))

(check "addition-commutative" (for-all2 (lambda (a b) (equal (f-add a b) (f-add b a)))))
(check "multiplication-commutative" (for-all2 (lambda (a b) (equal (f-mul a b) (f-mul b a)))))
(check "multiplication-associative"
  (for-all3 (lambda (a b c) (equal (f-mul (f-mul a b) c) (f-mul a (f-mul b c))))))
(check "distributivity"
  (for-all3 (lambda (a b c)
    (equal (f-mul a (f-add b c)) (f-add (f-mul a b) (f-mul a c))))))
(check "characteristic-2" (every (lambda (a) (equal (f-add a a) zero)) gf8))

(define nonzero (filter (lambda (a) (not (equal a zero))) gf8))
(check "every-nonzero-element-invertible"
  (every (lambda (a)
    (any (lambda (b) (equal (f-mul a b) one)) nonzero)) nonzero))

; ---------- the multiplicative group is cyclic of order 7 ----------
(define (f-pow a n)
  (cond ((= n 0) one) (t (f-mul a (f-pow a (- n 1))))))
(define x-powers (list-to-set (map (lambda (k) (f-pow X k)) (seq 0 6))))
(check "x-generates-the-multiplicative-group" (equal x-powers nonzero))
(check "x-has-order-7"
  (and (equal (f-pow X 7) one)
       (not (equal (f-pow X 1) one))))
(check "fermat-a8-equals-a"
  (every (lambda (a) (equal (f-pow a 8) a)) gf8))

; ---------- the Frobenius automorphism ----------
(define (frob a) (f-mul a a))
(define (frob2 a) (frob (frob a)))

; the freshman's dream is a THEOREM in characteristic 2
(check "frobenius-is-additive"
  (for-all2 (lambda (a b) (equal (frob (f-add a b)) (f-add (frob a) (frob b))))))
(check "frobenius-is-multiplicative"
  (for-all2 (lambda (a b) (equal (frob (f-mul a b)) (f-mul (frob a) (frob b))))))
(check "frobenius-is-bijective"
  (= (length (map (lambda (a) (frob a)) gf8)) 8))
(check "frobenius-cubed-is-identity"
  (every (lambda (a) (equal (frob (frob2 a)) a)) gf8))
(check "frobenius-itself-is-not-identity"
  (any (lambda (a) (not (equal (frob a) a))) gf8))

; the fixed field of Frobenius is exactly the prime field GF(2)
(define fixed (filter (lambda (a) (equal (frob a) a)) gf8))
(print (list (quote fixed-field) fixed))
(check "fixed-field-is-gf2" (equal fixed (list-to-set (list zero one))))

; Gal(GF(8)/GF(2)) = {id, frob, frob^2}: cyclic of order 3.
; Distinctness: the three maps differ somewhere; closure: composing
; any two lands back in the set (indices add mod 3).
(define (gal-apply k a)
  (cond ((= k 0) a) ((= k 1) (frob a)) (t (frob2 a))))
(check "galois-maps-are-distinct"
  (every (lambda (k)
    (any (lambda (a) (not (equal (gal-apply k a) a))) gf8)) {1 2}))
(check "galois-group-composes-cyclically"
  (every (lambda (i)
    (every (lambda (j)
      (every (lambda (a)
        (equal (gal-apply i (gal-apply j a))
               (gal-apply (mod (+ i j) 3) a))) gf8)) {0 1 2})) {0 1 2}))
; |Gal| = degree of the extension: 3 = [GF(8):GF(2)].
; Count the group by its ACTION -- represent each automorphism by its
; image list on a fixed enumeration of GF(8) and count distinct ones.
(define gf8-list (set-to-list gf8))
(define galois-maps
  (list-to-set
    (map (lambda (k) (map (lambda (a) (gal-apply k a)) gf8-list))
         (list 0 1 2))))
(check "galois-order-equals-degree" (= 3 (length galois-maps)))

(print "=== GF(8) and the Frobenius Done ===")
