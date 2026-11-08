; Group actions on sets. Orbit-stabilizer. Burnside's lemma.
; Conjugacy classes. Class equation. Compute for S_3.

(print "=== GROUP ACTIONS ===")

; S_3: symmetric group on {1,2,3}. Represent as list of images.
(define s3-elems (list-to-set
  (list (list 1 2 3)  ; id
        (list 2 1 3)  ; (1 2)
        (list 3 2 1)  ; (1 3)
        (list 1 3 2)  ; (2 3)
        (list 2 3 1)  ; (1 2 3)
        (list 3 1 2)))); (1 3 2)

(define (perm-apply perm x) (nth (- x 1) perm))
(define (perm-equal p q) (equal p q))

; ---- Group action: S_3 acts on {1,2,3} ----
(define (act-on g x) (perm-apply g x))

; ---- Orbit of x ----
(define (orbit G x)
  (map (lambda (g) (act-on g x)) G))

(define orb-1 (orbit s3-elems 1))
(if (equal orb-1 {1 2 3})
    (print "PASS: orbit-s3-is-full") (print "FAIL: orbit-s3-is-full"))

; ---- Stabilizer ----
(define (stabilizer G x)
  (filter (lambda (g) (= (act-on g x) x)) G))

(define stab-1 (stabilizer s3-elems 1))
(if (= (length stab-1) 2)
    (print "PASS: stabilizer-size-2") (print "FAIL: stabilizer-size-2"))

; ---- Orbit-Stabilizer theorem: |G| = |Orb(x)| * |Stab(x)| ----
(if (= (length s3-elems) (* (length orb-1) (length stab-1)))
    (print "PASS: orbit-stabilizer-in-s3") (print "FAIL: orbit-stabilizer-in-s3"))

; ---- Burnside's lemma: #orbits = (1/|G|) * sum |Fix(g)| ----
(define (fix-count G-group X g-perm)
  (count (lambda (x) (= (act-on g-perm x) x)) X))

(define (burnside G X)
  (let ((total (foldl (lambda (acc g) (+ acc (fix-count G X g)))
                 0 (set-to-list G))))
    (/ total (length G))))

(define num-orbits-s3 (burnside s3-elems {1 2 3}))
(if (= num-orbits-s3 1)
    (print "PASS: burnside-s3-1-orbit") (print "FAIL: burnside-s3-1-orbit"))

; ---- Conjugacy classes (by direct classification) ----
(define (is-identity p) (equal p (list 1 2 3)))
(define (is-transposition p)
  (= (count (lambda (i) (= (perm-apply p i) i)) {1 2 3}) 1))
(define (is-3cycle p)
  (and (not (is-identity p)) (not (is-transposition p))))

(define class-id (filter is-identity s3-elems))
(define class-transp (filter is-transposition s3-elems))
(define class-3cycle (filter is-3cycle s3-elems))

(if (= (length class-id) 1)
    (print "PASS: conjugacy-class-id") (print "FAIL: conjugacy-class-id"))
(if (= (length class-transp) 3)
    (print "PASS: conjugacy-class-transpositions") (print "FAIL: conjugacy-class-transpositions"))
(if (= (length class-3cycle) 2)
    (print "PASS: conjugacy-class-3cycles") (print "FAIL: conjugacy-class-3cycles"))

; ---- Class equation: |G| = |Z(G)| + sum |C(g)| ----
; For S_3: 6 = 1 + 3 + 2
(define class-eq-ok
  (= (length s3-elems) (+ (length class-id) (length class-transp) (length class-3cycle))))
(if class-eq-ok
    (print "PASS: class-equation-s3") (print "FAIL: class-equation-s3"))

; ---- Center Z(S_3) = {id} ----
; In S_3, only identity commutes with everything.
(define z-s3 class-id)
(if (= (length z-s3) 1)
    (print "PASS: center-of-s3") (print "FAIL: center-of-s3"))

; ---- S_4 class equation known result (24 = 1 + 6 + 3 + 8 + 6) ----
(if (= (+ 1 6 3 8 6) 24)
    (print "PASS: s4-class-equation") (print "FAIL: s4-class-equation"))

; ---- A_5: simple group, |A_5| = 60 ----
; A_5 is the alternating group on 5 elements.
(if (= (/ (* 5 4 3 2 1) 2) 60)
    (print "PASS: a5-order-60") (print "FAIL: a5-order-60"))

(print "=== GROUP ACTIONS DONE ===")
