; Lob's theorem: provability logic GL. Verify box(box P -> P) -> box P
; holds in all finite transitive irreflexive Kripke models.

; Simple model: world 0, no accessibility relation
(define ws (list 0))

(define (member lst x)
  (contains x lst))

(define (satisfies val rel worlds w formula)
  (cond ((eq formula 'P) (member val w))
        ((consp formula)
         (cond ((eq (car formula) 'not)
                (not (satisfies val rel worlds w (cadr formula))))
               ((eq (car formula) 'and)
                (and (satisfies val rel worlds w (cadr formula))
                     (satisfies val rel worlds w (caddr formula))))
               ((eq (car formula) 'impl)
                (cond ((satisfies val rel worlds w (cadr formula))
                       (satisfies val rel worlds w (caddr formula)))
                      (t t)))
               ((eq (car formula) 'box)
                (every (lambda (v)
                         (cond ((member rel (list w v))
                                (satisfies val rel worlds v (cadr formula)))
                               (t t)))
                       worlds))
               (t nil)))
        (t nil)))

; Empty relation is irreflexive and transitive
(define empty-rel nil)

(if (not (any (lambda (p) (= (car p) (cadr p))) empty-rel))
    (print "PASS: lob-empty-irreflexive")
    (print "FAIL: lob-empty-irreflexive"))

; Lob axiom: impl(box(impl(box P) P), box P)
(define lob-ax '(impl (box (impl (box P) P)) (box P)))

; Check on 1-world empty model, all valuations
(define (check-on-model rel worldlist)
  (let ((all-val (list nil (list 0))))
    (every (lambda (val)
             (every (lambda (w)
                      (satisfies val rel ws w lob-ax))
                    worldlist))
           all-val)))

(if (check-on-model empty-rel ws)
    (print "PASS: lob-1world")
    (print "FAIL: lob-1world"))

; 2-world model: 0 -> 1 (irreflexive, transitive)
(define ws2 (list 0 1))
(define rel2 (list (list 0 1)))

(if (not (any (lambda (p) (= (car p) (cadr p))) rel2))
    (print "PASS: lob-2world-irreflexive")
    (print "FAIL: lob-2world-irreflexive"))

(define (check2 model-rel val-list)
  (every (lambda (val)
           (every (lambda (w)
                    (satisfies val model-rel ws2 w lob-ax))
                  ws2))
         val-list))

(define all-val2 (list nil (list 0) (list 1) (list 0 1)))

(if (check2 rel2 all-val2)
    (print "PASS: lob-2world-model")
    (print "FAIL: lob-2world-model"))

; 3-world tree: 0 -> 1, 0 -> 2 (root to leaves)
(define ws3 (list 0 1 2))
(define rel3 (list (list 0 1) (list 0 2)))

(define all-val3 (list nil (list 0) (list 1) (list 2)
                       (list 0 1) (list 0 2) (list 1 2)
                       (list 0 1 2)))

(define (check3 rel val-list)
  (every (lambda (val)
           (every (lambda (w)
                    (satisfies val rel ws3 w lob-ax))
                  ws3))
         val-list))

(if (check3 rel3 all-val3)
    (print "PASS: lob-3world-tree")
    (print "FAIL: lob-3world-tree"))

; Transitive chain: 0 -> 1 -> 2 (also 0 -> 2 via transitivity)
(define rel4 (list (list 0 1) (list 1 2) (list 0 2)))

(if (check3 rel4 all-val3)
    (print "PASS: lob-chain-model")
    (print "FAIL: lob-chain-model"))

; Verify Lob principle directly: if box(box P -> P) true at w, then box P true at w
(define (lob-property rel val w)
  (let ((ante (satisfies val rel ws3 w '(box (impl (box P) P)))))
    (cond (ante (satisfies val rel ws3 w '(box P)))
          (t t))))

(if (every (lambda (val)
             (every (lambda (w)
                      (lob-property rel4 val w))
                    ws3))
           all-val3)
    (print "PASS: lob-direct-property")
    (print "FAIL: lob-direct-property"))

; negative control (the old box iterated the 1-world global and always
; said t): box P at world 0 means P at the ACCESSIBLE world 1
(if (eq (satisfies nil rel2 ws2 0 '(box P)) nil)
    (print "PASS: lob-box-means-accessible-world")
    (print "FAIL: lob-box-means-accessible-world"))
(if (satisfies (list 1) rel2 ws2 0 '(box P))
    (print "PASS: lob-box-holds-when-P-at-target")
    (print "FAIL: lob-box-holds-when-P-at-target"))

(print "=== LOB THEOREM DONE ===")
