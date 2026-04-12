; ============================================================
; 49 — Cantor Set via Exact Rationals
; Remove middle thirds — exact with rational arithmetic
; IMPOSSIBLE in float languages: no precision loss here!
; ============================================================

(print "=== Cantor Set Construction ===")

; Interval as (lo . hi) pair
; Each step: [a,b] → [a, a+(b-a)/3] ∪ [b-(b-a)/3, b]

(define (cantor-split intervals)
  (cond ((null intervals) nil)
        (t (let ((lo (car (car intervals)))
                 (hi (cdr (car intervals))))
             (let ((third (/ (+ hi (* -1 lo)) 3)))
               (cons (cons lo (+ lo third))
                     (cons (cons (+ hi (* -1 third)) hi)
                           (cantor-split (cdr intervals)))))))))

(define level0 (list (cons 0 1)))
(print "Level 0:") (print level0)

(define level1 (cantor-split level0))
(print "Level 1:") (print level1)

(define level2 (cantor-split level1))
(print "Level 2:") (print level2)

(define level3 (cantor-split level2))
(print "Level 3:") (print level3)

(define level4 (cantor-split level3))
(print "Level 4:") (print level4)

; Count intervals: 2^n
(print "--- Counts: 2^n ---")
(print "Level 0:") (print (ord level0))
(print "Level 1:") (print (ord level1))
(print "Level 2:") (print (ord level2))
(print "Level 3:") (print (ord level3))
(print "Level 4:") (print (ord level4))

; Total length at each level: (2/3)^n
(define (total-len ivs)
  (cond ((null ivs) 0)
        (t (+ (+ (cdr (car ivs)) (* -1 (car (car ivs))))
              (total-len (cdr ivs))))))

(print "--- Lengths (exact rationals!) ---")
(print "Level 0:") (print (total-len level0))
(print "Level 1:") (print (total-len level1))
(print "Level 2:") (print (total-len level2))
(print "Level 3:") (print (total-len level3))
(print "Level 4:") (print (total-len level4))

; Verify: (2/3)^4 = 16/81
(print "Expected (2/3)^4 =") (print (/ 16 81))
(print "Match?") (print (= (total-len level4) (/ 16 81)))

(print "Exact rationals — no floating point drift!")
(print "=== Cantor Set Done ===")
