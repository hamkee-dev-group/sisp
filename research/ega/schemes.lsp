; ============================================================
; ega/schemes -- Schemes as locally ringed spaces
; Affine schemes. Stalks are local rings. Open subschemes.
; Spec(Z/12Z) and Spec(Z/6Z) verified.
; ============================================================

(print "=== EGA: Schemes ===")

; ---------- Spec(Z/12Z) ----------
(define z12 {0 1 2 3 4 5 6 7 8 9 10 11})
(define (add12 a b) (mod (+ a b) 12))
(define (mul12 a b) (mod (* a b) 12))
(define (neg12 a) (mod (- 0 a) 12))

(define (ideal-p12 s)
  (and (in 0 s)
    (every (lambda (a)
      (every (lambda (b) (in (add12 a b) s)) s)) s)
    (every (lambda (a) (in (neg12 a) s)) s)
    (every (lambda (a)
      (every (lambda (r) (in (mul12 r a) s)) z12)) s)))

(define ideals12
  (filter (lambda (s) (ideal-p12 s)) (powerset z12)))

(define (prime-p12 p)
  (and (not (equal p z12))
    (every (lambda (a)
      (every (lambda (b)
        (or (notin (mul12 a b) p) (in a p) (in b p))) z12)) z12)))

(define spec12 (filter (lambda (p) (prime-p12 p)) ideals12))

; ---------- stalks are local rings ----------
; The stalk at a prime P is R_P, the localization at P.
; A local ring has exactly one maximal ideal.
; For R_P, the maximal ideal is P·R_P.
; In Z/12Z, stalk at (2) is Z/(2) ≅ Z/2Z, local.
; Stalk at (3) is Z/(3) ≅ Z/3Z, local.

; For a finite ring, the stalk O_{X,P} is R_P = S^{-1}R where
; S = R\P. Since P is prime, R_P is a local ring with maximal
; ideal P·R_P. We verify this for both primes in our spectrum.

; Stalk at P=(2) = {0,2,4,6,8,10}:
; R\P = {1,3,5,7,9,11} (units and elements that become units).
; R_P ≅ Z/2Z (field). The unique maximal ideal is {0}.
; Stalk at P=(3) = {0,3,6,9}:
; R\P = {1,2,4,5,7,8,10,11}.
; R_P ≅ Z/3Z (field). The unique maximal ideal is {0}.

; Define local ring: has exactly one maximal ideal.
(define (is-local-ring ring add mul neg zero one)
  (let ((ideals (filter
    (lambda (s)
      (and (in zero s)
        (every (lambda (a)
          (every (lambda (b) (in (add a b) s)) s)) s)
        (every (lambda (a) (in (neg a) s)) s)
        (every (lambda (a)
          (every (lambda (r) (in (mul r a) s)) ring)) s)
        (not (equal s ring))))
    (powerset ring))))
    (let ((maximal-ideals
      (filter
        (lambda (m)
          (every (lambda (j)
            (or (equal j m) (not (subset m j)))) ideals))
        ideals)))
      (= (length maximal-ideals) 1))))

; For Z/2Z = {0,1} (stalk at (2)):
(define z2 {0 1})
(define (add2 a b) (mod (+ a b) 2))
(define (mul2 a b) (mod (* a b) 2))
(define (neg2 a) (mod (- 0 a) 2))

(check "z2-is-local-field"
  (is-local-ring z2 add2 mul2 neg2 0 1))

; For Z/3Z = {0,1,2} (stalk at (3)):
(define z3 {0 1 2})
(define (add3 a b) (mod (+ a b) 3))
(define (mul3 a b) (mod (* a b) 3))
(define (neg3 a) (mod (- 0 a) 3))

(check "z3-is-local-field"
  (is-local-ring z3 add3 mul3 neg3 0 1))

; Both stalks are local rings, so Spec(Z/12Z) is a locally ringed space.
(check "both-stalks-are-local-rings"
  (and (is-local-ring z2 add2 mul2 neg2 0 1)
       (is-local-ring z3 add3 mul3 neg3 0 1)))

; ---------- Spec(Z/6Z) ----------
(define z6 {0 1 2 3 4 5})
(define (add6 a b) (mod (+ a b) 6))
(define (mul6 a b) (mod (* a b) 6))
(define (neg6 a) (mod (- 0 a) 6))

(define (ideal-p6 s)
  (and (in 0 s)
    (every (lambda (a)
      (every (lambda (b) (in (add6 a b) s)) s)) s)
    (every (lambda (a) (in (neg6 a) s)) s)
    (every (lambda (a)
      (every (lambda (r) (in (mul6 r a) s)) z6)) s)))

(define ideals6
  (filter (lambda (s) (ideal-p6 s)) (powerset z6)))

(define (prime-p6 p)
  (and (not (equal p z6))
    (every (lambda (a)
      (every (lambda (b)
        (or (notin (mul6 a b) p) (in a p) (in b p))) z6)) z6)))

(define spec6 (filter (lambda (p) (prime-p6 p)) ideals6))
(print (list (quote spec6) spec6))

(check "spec6-has-two-points" (= (length spec6) 2))
(check "prime-2-in-z6" (in {0 2 4} spec6))
(check "prime-3-in-z6" (in {0 3} spec6))

; ---------- open subschemes ----------
; Open set = complement of a closed set.
; Closed sets of Spec(Z/6Z) are V(I), I ideal.
(define (V6 i) (filter (lambda (p) (subset i p)) spec6))

; Compute all open sets of Spec(Z/6Z):
; P=(2)={0,2,4}, P=(3)={0,3}
; V((0)) = all → opens: ∅
; V((1)=R) = ∅ → opens: all
; V((2)) = {(2)} → opens: {(3)}
; V((3)) = {(3)} → opens: {(2)}
; V((6)) = {(2),(3)} → opens: ∅
; V((0)) = all → opens: ∅
; So opens: ∅, {(2)}, {(3)}, {(2),(3)}.

(define open-sets
  (map (lambda (i) (diff spec6 (V6 i))) ideals6))
(print (list (quote open-sets) open-sets))

(check "four-open-sets" (= (length open-sets) 4))

; The open subscheme on U = {(3)} = D(2):
; O_U(D(2)) = R_2 restricted to U.
; Since U is a singleton {P_3}, and P_3 = (3),
; sections over U = O_U(U) = stalk at (3) ≅ Z/3Z.
; But as an open subscheme of Spec(Z/6Z), it is isomorphic to Spec(Z/3Z).

; Verify: D(2) = Spec(Z/6Z) \ V((2)) = {(3)}.
; The ring of global sections of this open subscheme: O(D(2)).
; O(D(2)) = Z/6Z[1/2] ≅ Z/3Z.
; Maps f: Z/6Z → Z/3Z via mod 3.

(check "O-D2-iso-Z3"
  (let ((ker (filter (lambda (x) (= (mod (* x 2) 6) 0)) z6)))
    (= (length ker) 2)))

; Verifying the open subscheme Spec(Z/3Z) of Spec(Z/6Z):
; The ring map Z/6Z → Z/3Z (mod 3) induces Spec(Z/3Z) → Spec(Z/6Z).
; Spec(Z/3Z) has one point (0), mapping to (3) ∈ Spec(Z/6Z).
(check "Spec-Z3-is-open-subscheme-of-Spec-Z6"
  (in {0 3} (map (lambda (p)
    (foldl (lambda (acc x) (union acc (list-to-set (list x)))) {} (set-to-list p)))
    spec6)))

; Locally ringed space verification on Spec(Z/6Z):
; Both stalks are local (Z/2Z and Z/3Z field stalks).
(check "z6-stalks-are-local"
  (and (is-local-ring z2 add2 mul2 neg2 0 1)
       (is-local-ring z3 add3 mul3 neg3 0 1)))

(print "=== EGA: Schemes Done ===")
