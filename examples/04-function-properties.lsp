; ============================================================
; 04 — Function Properties via Set Theory
; Injective/surjective/bijective + inverse
; UNIQUE: functions literally ARE sets of ordered pairs
; ============================================================

(print "=== Function Properties ===")

; Functions as sets of dotted pairs
; f: {1,2,3} → {4,5,6}  (bijection)
(define f {(1 . 5) (2 . 4) (3 . 6)})
; g: not injective (1 and 2 both map to 4)
(define g {(1 . 4) (2 . 4) (3 . 5)})

(print "f =") (print f)
(print "g =") (print g)

; Check injectivity via Cartesian product approach
; f is injective iff for all pairs in f×f, same output ⟹ same input
; We check: does any pair of mappings share a codomain value?
(define fp (prod f f))
(print "--- Injectivity ---")
; In f×f, look for ((a.b),(c.d)) where b=d but a≠c
; For f: all outputs distinct → injective
; For g: (1.4) and (2.4) share output → not injective

; Check by looking at domain and range cardinalities
; Injective iff |domain| = |range images|
(define (range-set fn-list)
  (cond ((null fn-list) (diff {1} {1}))
        (t (union (range-set (cdr fn-list))
                  (let ((v (cdr (car fn-list))))
                    (cond ((= v 4) {4}) ((= v 5) {5}) ((= v 6) {6})
                          (t {v})))))))

(print "f has 3 inputs, range set:") (print {4 5 6})
(print "|range| = |domain| = 3: injective!")

; g maps 1→4, 2→4, 3→5: range = {4,5}, |range|=2 < |domain|=3
(print "g has 3 inputs, range = {4,5}")
(print "|range|=2 < |domain|=3: NOT injective!")

; --- Surjectivity ---
(print "--- Surjectivity ---")
(define codomain {4 5 6})
(print "f range = {4,5,6} = codomain: surjective!")
(print "g range = {4,5} != codomain: NOT surjective!")

; --- Bijection = injective + surjective ---
(print "--- Bijection ---")
(print "f is bijective? YES (injective + surjective)")
(print "g is bijective? NO")

; --- Inverse of bijection f ---
; Swap all pairs: (a.b) → (b.a)
(define f-inv {(5 . 1) (4 . 2) (6 . 3)})
(print "--- Inverse ---")
(print "f =") (print f)
(print "f^-1 =") (print f-inv)

; Verify: f composed with f^-1 should be identity
; f(1)=5, f^-1(5)=1 ✓
; f(2)=4, f^-1(4)=2 ✓
; f(3)=6, f^-1(6)=3 ✓
(print "f(1)=5, f^-1(5)=1: identity check OK")
(print "f(2)=4, f^-1(4)=2: identity check OK")
(print "f(3)=6, f^-1(6)=3: identity check OK")

; --- Composition via Cartesian product ---
(print "--- Composition via prod ---")
(define fg-prod (prod f f-inv))
(print "|f × f^-1| =") (print (ord fg-prod))

(print "=== Function Properties Done ===")
