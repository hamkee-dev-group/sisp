; ============================================================
; 66 — An Elliptic Curve over F_13 and the Hasse Bound
; E: y^2 = x^3 + x + 1 over the field with 13 elements. The
; rational points are counted, the Hasse inequality is verified
; in exact integer arithmetic, and the chord-and-tangent addition
; is PROVEN to make E a finite abelian group by checking every
; axiom over all points — including associativity on all triples.
; ============================================================

(load "lib/collections/relations.lsp")

(print "=== Elliptic Curve over F_13 ===")


(define p 13)
(define (fp x) (mod x p))
(define (rhs x) (fp (+ (* x (* x x)) (+ x 1))))   ; x^3 + x + 1

; modular inverse by Fermat: a^(p-2) mod p
(define (modpow b e)
  (cond ((= e 0) 1) (t (fp (* b (modpow b (- e 1)))))))
(define (inv a) (modpow (fp a) (- p 2)))

; ---------- the rational points ----------
(define coords (list-to-set (seq 0 12)))
(define affine
  (pairs-such-that (lambda (x y) (= (fp (* y y)) (rhs x))) coords coords))
(define E (list-to-set (cons (quote inf) (set-to-list affine))))
(define N (length E))
(print (list (quote points) N (quote affine) (length affine)))

; ---------- Hasse: |N - (p+1)| <= 2 sqrt(p), exactly ----------
; squared form avoids square roots: (N - p - 1)^2 <= 4p.
(check "hasse-bound"
  (<= (* (- N (+ p 1)) (- N (+ p 1))) (* 4 p)))
(check "curve-is-nonsingular"
  ; discriminant of x^3 + ax + b: -16(4a^3 + 27b^2), a=b=1
  (not (= 0 (fp (+ (* 4 1) (* 27 1))))))

; ---------- chord-and-tangent addition ----------
(define (ec-neg q)
  (cond ((eq q (quote inf)) q)
        (t (list (car q) (fp (- 0 (cadr q)))))))
(define (ec-add q r)
  (cond ((eq q (quote inf)) r)
        ((eq r (quote inf)) q)
        (t (let ((x1 (car q)) (y1 (cadr q)) (x2 (car r)) (y2 (cadr r)))
          (cond ((and (= x1 x2) (= (fp (+ y1 y2)) 0)) (quote inf))
                (t (let ((slope
                    (cond ((equal q r)
                      (fp (* (+ (* 3 (* x1 x1)) 1) (inv (* 2 y1)))))
                      (t (fp (* (- y2 y1) (inv (- x2 x1))))))))
                  (let ((x3 (fp (- (* slope slope) (+ x1 x2)))))
                    (list x3 (fp (- (* slope (- x1 x3)) y1)))))))))))

; ---------- the group axioms, exhaustively ----------
(check "closure"
  (every (lambda (q)
    (every (lambda (r) (in (ec-add q r) E)) E)) E))
(check "identity"
  (every (lambda (q)
    (and (equal (ec-add (quote inf) q) q)
         (equal (ec-add q (quote inf)) q))) E))
(check "inverses"
  (every (lambda (q)
    (and (in (ec-neg q) E)
         (equal (ec-add q (ec-neg q)) (quote inf)))) E))
(check "commutativity"
  (every (lambda (q)
    (every (lambda (r) (equal (ec-add q r) (ec-add r q))) E)) E))
; the deep one: associativity over every triple of points
(check "associativity-all-triples"
  (every (lambda (q)
    (every (lambda (r)
      (every (lambda (s)
        (equal (ec-add (ec-add q r) s) (ec-add q (ec-add r s)))) E)) E)) E))

; ---------- Lagrange: N * P = O for every point ----------
(define (ec-mul k q)
  (cond ((= k 0) (quote inf)) (t (ec-add q (ec-mul (- k 1) q)))))
(check "lagrange-order-annihilates"
  (every (lambda (q) (equal (ec-mul N q) (quote inf))) E))

; a sample point's cyclic subgroup and its order dividing N
(define g (find (lambda (q) (not (eq q (quote inf)))) E))
(define (order-of q k)
  (cond ((equal (ec-mul k q) (quote inf)) k) (t (order-of q (+ k 1)))))
(define g-order (order-of g 1))
(print (list (quote sample-point) g (quote order) g-order))
(check "point-order-divides-group-order" (= 0 (mod N g-order)))

(print "=== Elliptic Curve Done ===")
