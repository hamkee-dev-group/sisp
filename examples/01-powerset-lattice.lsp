; ============================================================
; Powerset Lattice
; Generate P(S), display Hasse diagram levels by subset size,
; and verify |P(S)| = 2^|S|.
; ============================================================

(print "=== Powerset Lattice ===")

(define S {1 2 3})
(print "Base set S:")
(print S)

; Generate the powerset
(define PS (pow S))
(print "P(S):")
(print PS)

; Convert a set to a list for iteration
(define (set-to-list s)
  (if (null s) nil
    (cons (car s) (set-to-list (cdr s)))))

; Count elements of an extension set (handles pow empty-set marker)
(define (set-size s)
  (if (null s) 0
    (if (equal (typeof (car s)) 'undefined) 0
      (+ 1 (set-size (cdr s))))))

; Filter a list keeping elements where (fn elem) is true
(define (keep fn lst)
  (if (null lst) nil
    (if (fn (car lst))
        (cons (car lst) (keep fn (cdr lst)))
        (keep fn (cdr lst)))))

; Get all powerset elements of a given size
(define ps-list (set-to-list PS))

(define (size-is-0 x) (= (set-size x) 0))
(define (size-is-1 x) (= (set-size x) 1))
(define (size-is-2 x) (= (set-size x) 2))
(define (size-is-3 x) (= (set-size x) 3))

(define level-0 (keep size-is-0 ps-list))
(define level-1 (keep size-is-1 ps-list))
(define level-2 (keep size-is-2 ps-list))
(define level-3 (keep size-is-3 ps-list))

; Display Hasse diagram levels
(print "--- Hasse Diagram Levels (by subset size) ---")

(print "Level 0 (empty set):")
(print level-0)

(print "Level 1 (singletons):")
(print level-1)

(print "Level 2 (pairs):")
(print level-2)

(print "Level 3 (full set):")
(print level-3)

; Show cover relations: A covers B if A subset B and |B| = |A| + 1
(define (show-covers-inner lo-elem upper)
  (if (null upper) nil
    (progn
      (if (and (> (set-size lo-elem) 0) (subset lo-elem (car upper)))
          (print (list lo-elem "is covered by" (car upper)))
          nil)
      (show-covers-inner lo-elem (cdr upper)))))

(define (show-covers lower upper)
  (if (null lower) nil
    (progn
      (show-covers-inner (car lower) upper)
      (show-covers (cdr lower) upper))))

(print "--- Cover Relations (Level 1 -> Level 2) ---")
(show-covers level-1 level-2)

(print "--- Cover Relations (Level 2 -> Level 3) ---")
(show-covers level-2 level-3)

; Verify |P(S)| = 2^|S|
(print "--- Cardinality Verification ---")
(define ps-card (ord PS))
(define s-card (ord S))
(define expected (^ 2 s-card))

(print "|S|:")
(print s-card)
(print "|P(S)|:")
(print ps-card)
(print "2^|S|:")
(print expected)

(if (= ps-card expected)
    (print "VERIFIED: |P(S)| = 2^|S|")
    (print "FAILED: |P(S)| != 2^|S|"))

; Verify level sizes: C(3,0) + C(3,1) + C(3,2) + C(3,3) = 8
(define total (+ (ord level-0) (+ (ord level-1) (+ (ord level-2) (ord level-3)))))
(print "Sum of level sizes:")
(print total)
(if (= total ps-card)
    (print "VERIFIED: Sum of level sizes equals |P(S)|")
    (print "FAILED: Sum of level sizes does not equal |P(S)|"))

(print "=== Powerset Lattice Done ===")
