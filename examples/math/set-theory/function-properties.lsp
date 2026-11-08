; ============================================================
; 04 — Function Properties via Set Theory
; Injective/surjective/bijective + inverse
; functions literally ARE sets of ordered pairs
; ============================================================

(print "=== Function Properties ===")

; Functions as sets of (input . output) pairs.
; f: {1,2,3} -> {4,5,6} is a bijection; g is not injective (1 and 2 -> 4).
(define f {(1 . 5) (2 . 4) (3 . 6)})
(define g {(1 . 4) (2 . 4) (3 . 5)})
(define codomain {4 5 6})

(print "f =") (print f)
(print "g =") (print g)

; A function IS its set of pairs, so every property is computed by walking
; those pairs directly -- no wrapper structure, no auxiliary table.

; (hits v fn): does some pair in fn have output v?
(define (hits v fn)
  (cond ((null fn) nil)
        ((= (cdr (car fn)) v) t)
        (t (hits v (cdr fn)))))

; injective: no output is produced by two different inputs
(define (injective fn)
  (cond ((null fn) t)
        ((hits (cdr (car fn)) (cdr fn)) nil)
        (t (injective (cdr fn)))))

; surjective onto cod: every value in cod is some pair's output
(define (surjective fn cod)
  (cond ((null cod) t)
        ((hits (car cod) fn) (surjective fn (cdr cod)))
        (t nil)))

; bijective: injective and surjective
(define (bijective fn cod)
  (and (injective fn) (surjective fn cod)))

; --- Injectivity ---
(print "--- Injectivity ---")
(print "f injective?") (print (injective f))
(print "g injective?") (print (injective g))

; --- Surjectivity ---
(print "--- Surjectivity ---")
(print "f surjective onto {4 5 6}?") (print (surjective f codomain))
(print "g surjective onto {4 5 6}?") (print (surjective g codomain))

; --- Bijection ---
(print "--- Bijection ---")
(print "f bijective?") (print (bijective f codomain))
(print "g bijective?") (print (bijective g codomain))

; --- Inverse of the bijection f ---
; Swap every (a . b) into (b . a); the inverse maps {4,5,6} back onto {1,2,3}.
(print "--- Inverse ---")
(define f-inv {(5 . 1) (4 . 2) (6 . 3)})
(print "f^-1 =") (print f-inv)

; (has-pair a b fn): does fn contain the exact pair (a . b)?
(define (has-pair a b fn)
  (cond ((null fn) nil)
        ((and (= (car (car fn)) a) (= (cdr (car fn)) b)) t)
        (t (has-pair a b (cdr fn)))))

; (inverts fn inv): every (a . b) in fn appears as (b . a) in inv
(define (inverts fn inv)
  (cond ((null fn) t)
        ((has-pair (cdr (car fn)) (car (car fn)) inv) (inverts (cdr fn) inv))
        (t nil)))

(print "f^-1 is a genuine bijection onto {1 2 3}?")
(print (bijective f-inv {1 2 3}))
(print "f^-1 really inverts f (every (a.b) becomes (b.a))?")
(print (inverts f f-inv))

; --- Size of the product relation f x f^-1 ---
; |f x f^-1| = |f| . |f^-1|: a bijection's product relation has 9 pairs.
(print "--- Product size ---")
(print "|f x f^-1| =") (print (* (length f) (length f-inv)))

(print "=== Function Properties Done ===")
