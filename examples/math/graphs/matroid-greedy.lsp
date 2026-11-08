; ============================================================
; 121 — Matroid Greedy
; Graphic matroid: MST edges from file 120 via greedy (increasing weight)
; Uniform matroid: rank 3, ground set {a,b,c,d,e}, weights (5,3,8,1,2)
; Non-matroid: 3 items, at most 1 red. Greedy fails.
; ============================================================

(print "=== Matroid Greedy ===")


(define (has x lst)
  (cond ((equal lst nil) nil)
        ((equal (car lst) x) t)
        (t (has x (cdr lst)))))

; --- Graphic matroid (cycle = dependence) ---
(define g-edges '((1 2 4) (1 3 1) (2 3 2) (2 4 5) (3 4 3)))
(define g-n 4)

(define (makeset x) (list x))
(define (find-set x sets)
  (cond ((equal sets nil) nil)
        ((has x (car sets)) (car sets))
        (t (find-set x (cdr sets)))))
(define (union-sets s1 s2 sets)
  (cons (append s1 s2) (remove (lambda (s) (or (equal s s1) (equal s s2))) sets)))

; Greedy by increasing weight: take edge if no cycle
(define (greedy-graphic es n)
  (let ((sorted (sort (lambda (a b) (< (nth 2 a) (nth 2 b))) es)))
    (greedy-step sorted (map makeset (seq 1 n)) nil)))

(define (greedy-step rem sets result)
  (cond ((equal rem nil) (reverse result))
        (t (let ((e (car rem)))
          (let ((su (find-set (car e) sets))
                (sv (find-set (cadr e) sets)))
            (cond ((equal su sv)
                   (greedy-step (cdr rem) sets result))
                  (t (greedy-step (cdr rem)
                         (union-sets su sv sets)
                         (cons e result)))))))))

(define result-g (greedy-graphic g-edges g-n))
(print "Graphic matroid greedy (increasing weight):")
(print result-g)
(define w-g (foldl (lambda (a e) (+ a (nth 2 e))) 0 result-g))
(print "Weight:")
(print w-g)
(check "graphic-greedy-is-mst" (= w-g 6))
(check "graphic-greedy-3-edges" (= (length result-g) (- g-n 1)))

; --- Uniform matroid: U_{3,5} ---
; Ground set: {a,b,c,d,e}, rank 3. Any set of size ≤ 3 is independent.
; Greedy by increasing weight: weights (a:5, b:3, c:8, d:1, e:2)
; Sorted by weight: d(1), e(2), b(3), a(5), c(8)
; Greedy picks 3 lightest: d, e, b = {d,e,b}
(define ground '((a 5) (b 3) (c 8) (d 1) (e 2)))

(define (sort-by-weight items)
  (sort (lambda (x y) (< (cadr x) (cadr y))) items))

(define sorted-ground (sort-by-weight ground))
(print "Uniform matroid: ground set by weight:")
(print sorted-ground)

(define (greedy-uniform items rank)
  (labels (((pick lst count acc)
    (cond ((equal count rank) (reverse acc))
          ((equal lst nil) (reverse acc))
          (t (pick (cdr lst) (+ count 1) (cons (car lst) acc))))))
    (pick items 0 nil)))

(define result-u (greedy-uniform sorted-ground 3))
(print "Uniform matroid greedy result (3 lightest):")
(print result-u)
(define names-u (map car result-u))
(check "uniform-greedy-is-d-e-b" (equal names-u '(d e b)))

; --- Non-matroid: greedy fails ---
; 3 items each with weight and color. Constraint: at most 1 red.
; Items: (R,10,gold), (R,9,silver), (B,5,diamond), (B,4,bronze)
; Greedy by weight: picks gold (R,10), silver (R,9) = both red → INVALID.
; But constraint says at most 1 red → must reject one.
; Correct: gold (R,10), diamond (B,5), bronze (B,4) → weight 19
; Or: silver (R,9), diamond (B,5), bronze (B,4) → weight 18
; Optimal = 19 (gold + 2 best blues).

(define items-nm '((R 10 gold) (R 9 silver) (B 5 diamond) (B 4 bronze)))

(define (sort-by-wt items)
  (sort (lambda (x y) (> (cadr x) (cadr y))) items))

(define sorted-nm (sort-by-wt items-nm))
(print "Non-matroid items sorted by weight:")
(print sorted-nm)

; Greedy: pick 3 highest-weight items, no constraint check
; This picks two reds (invalid), demonstrating greedy failure
(define (greedy-nonmatroid items)
  (take items 3))

(define greedy-nm-result (greedy-nonmatroid sorted-nm))
(print "Greedy result (no constraints):")
(print greedy-nm-result)
(define greedy-nm-wt (foldl (lambda (a x) (+ a (cadr x))) 0 greedy-nm-result))
(print "Greedy weight sum:")
(print greedy-nm-wt)

; Check if greedy result is valid
(define (valid-p subset)
  (let ((reds (foldl (lambda (c x) (cond ((eq (car x) 'R) (+ c 1)) (t c))) 0 subset)))
    (<= reds 1)))

(print "Greedy result valid?")
(print (valid-p greedy-nm-result))
(check "greedy-result-has-two-reds"
  (let ((reds (foldl (lambda (c x) (cond ((eq (car x) 'R) (+ c 1)) (t c))) 0 greedy-nm-result)))
    (= reds 2)))

; Optimal: enumerate all valid 3-item subsets
(define (all-3-subsets lst)
  (labels (((combo remaining count)
    (cond ((= count 3) (list nil))
          ((equal remaining nil) nil)
          (t (append
               (map (lambda (r) (cons (car remaining) r))
                    (combo (cdr remaining) (+ count 1)))
               (combo (cdr remaining) count))))))
    (combo lst 0)))

(define (valid-combo subset)
  (let ((reds (foldl (lambda (c x) (cond ((eq (car x) 'R) (+ c 1)) (t c))) 0 subset)))
    (and (<= reds 1) (= (length subset) 3))))

(define valid-subsets (filter valid-combo (all-3-subsets items-nm)))
(define optimal-wt (foldl (lambda (a s) (let ((w (foldl (lambda (c x) (+ c (cadr x))) 0 s))) (cond ((> w a) w) (t a)))) 0 valid-subsets))
(print "Optimal weight (exhaustive):")
(print optimal-wt)
(check "greedy-fails-nonmatroid" (not (= greedy-nm-wt optimal-wt)))
(print "Greedy weight =") (print greedy-nm-wt)
(print "Optimal weight =") (print optimal-wt)

(print "=== Matroid Greedy Done ===")
