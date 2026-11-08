(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== Extensive-Form Games ===")

; half-open integer range [lo, hi)
(define (range lo hi) (cond ((>= lo hi) nil) (t (cons lo (range (+ lo 1) hi)))))

(define (make-node player payoff children)
  (list (quote node) player payoff children))
(define (node-player n) (cadr n))
(define (node-payoff n) (caddr n))
(define (node-children n) (cadddr n))

(define (leaf-node payoff)
  (list (quote leaf) nil payoff nil))
(define (leaf-payoff n) (caddr n))

; Index into `vals` (a list of payoff vectors) of the one that maximises
; `player`'s own payoff. Ties go to the earliest child.
(define (best-index vals player)
  (foldl (lambda (best i)
           (cond ((eq best nil) i)
                 ((> (nth player (nth i vals)) (nth player (nth best vals))) i)
                 (t best)))
         nil (range 0 (length vals))))

; Backward induction: the value of a node is the payoff VECTOR of the subgame
; the mover at that node would choose, each mover maximising their OWN entry.
(define (backward-induction node)
  (cond ((eq (car node) (quote leaf)) (leaf-payoff node))
        (t (let ((vals (map backward-induction (node-children node))))
             (nth (best-index vals (node-player node)) vals)))))

; Which action the mover takes at this node under backward induction.
(define (best-child-index node)
  (best-index (map backward-induction (node-children node)) (node-player node)))

; The subgame-perfect equilibrium path: the list of action indices actually
; taken, from the root down to the leaf that play reaches.
(define (spe-path tree) (spe-path-helper tree nil))
(define (spe-path-helper node path)
  (cond ((eq (car node) (quote leaf)) (reverse path))
        (t (let ((i (best-child-index node)))
             (spe-path-helper (nth i (node-children node)) (cons i path))))))

(define (centipede-game steps)
  (build-centipede 0 steps))

; Centipede game. At step s the mover (player s mod 2) may Take or Pass.
; Taking pays the mover s+2 and the opponent s, so the pot grows by passing;
; if nobody ever takes both players get max-step.
; Payoffs go into the slot belonging to the player they accrue to.
(define (build-centipede step max-step)
  (cond ((>= step max-step) (leaf-node (list max-step max-step)))
        (t (let ((mover (mod step 2)))
             (make-node mover (list 0 0)
               (list (leaf-node (cond ((= mover 0) (list (+ step 2) step))
                                      (t (list step (+ step 2)))))
                     (build-centipede (+ step 1) max-step)))))))

(define cent4 (centipede-game 4))
(define c-result (backward-induction cent4))
(check "centipede-spe-exists" (not (eq c-result nil)))
; Centipede unravels: the SPE is to Take at the very first node, paying (2 0),
; even though passing all the way would give both players 4. Verified by hand:
; s=3 take 5 > pass 4; s=2 take 4 > pass 3; s=1 take 3 > pass 2; s=0 take 2 > pass 1.
(check "centipede-unravels" (equal c-result (list 2 0)))
(check "centipede-take-immediately" (equal (spe-path cent4) (list 0)))

(define (ultimatum-game pie)
  (make-node 0 (list 0 0)
    (map (lambda (offer)
           (make-node 1 (list 0 0)
             (list (leaf-node (list offer (- pie offer)))
                   (leaf-node (list 0 0)))))
         (range 0 (+ pie 1)))))

(define ult (ultimatum-game 5))
(define u-result (backward-induction ult))
(check "ultimatum-spe-exists" (not (eq u-result nil)))
; Ultimatum SPE: the responder accepts anything weakly positive, so the proposer
; keeps the whole pie of 5 and offers nothing -- payoffs (5 0). The proposer's
; action index is the amount kept, so the SPE path is "keep 5, then accept".
(check "ultimatum-proposer-takes-all" (equal u-result (list 5 0)))
(check "ultimatum-spe-path" (equal (spe-path ult) (list 5 0)))

(define (solve-small-tree node)
  (backward-induction node))

(define (custom-game)
  (make-node 0 (list 0 0)
    (list
      (make-node 1 (list 0 0)
        (list (leaf-node (list 3 1)) (leaf-node (list 0 0))))
      (leaf-node (list 2 2)))))

(define cg (custom-game))
(define cg-result (backward-induction cg))
(check "custom-spe-exists" (not (eq cg-result nil)))
; Player 1 prefers (3 1) over (0 0), so the left subgame is worth (3 1) to
; player 0, which beats the right leaf (2 2). SPE value (3 1), path (0 0).
(check "custom-spe-value" (equal cg-result (list 3 1)))

(define (compute-spe-payoffs tree)
  (backward-induction tree))

(define sp-path (spe-path cg))
(check "spe-path-exists" (not (eq sp-path nil)))
(check "custom-spe-path" (equal sp-path (list 0 0)))

(define (tree-depth node)
  (cond ((eq (car node) (quote leaf)) 0)
        (t (+ 1 (foldl (lambda (d c) (max d (tree-depth c))) 0 (node-children node))))))

(check "tree-depth-finite" (> (tree-depth cg) 0))

(print "=== Extensive-Form Done ===")
