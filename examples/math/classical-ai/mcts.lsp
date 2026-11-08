(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== Monte Carlo Tree Search ===")

; half-open integer range [lo, hi)
(define (range lo hi) (cond ((>= lo hi) nil) (t (cons lo (range (+ lo 1) hi)))))
(seed 42)

; ---- tree ------------------------------------------------------------------
; A node is (state visits total children). The tree is IMMUTABLE, so a node
; cannot carry a parent pointer: the parent it pointed at would be a stale
; snapshot and every backpropagated statistic would be thrown away. Instead
; one iteration is a single recursion that returns the REBUILT subtree.
(define (mcts-node state) (list state 0 0.0 nil))
(define (mcts-state node) (car node))
(define (mcts-visits node) (cadr node))
(define (mcts-total node) (caddr node))
(define (mcts-children node) (nth 3 node))
(define (mcts-mean node) (cond ((= (mcts-visits node) 0) 0.0) (t (/ (mcts-total node) (mcts-visits node)))))

(define (mcts-update node result)
  (list (mcts-state node) (+ (mcts-visits node) 1) (+ (mcts-total node) result) (mcts-children node)))
(define (mcts-add-child node child)
  (list (mcts-state node) (mcts-visits node) (mcts-total node) (cons child (mcts-children node))))
(define (mcts-set-children node kids)
  (list (mcts-state node) (mcts-visits node) (mcts-total node) kids))

; swap in a rebuilt child, matched by STATE (the rebuilt child is a new object,
; so identity cannot be used)
(define (mcts-replace-child node child)
  (mcts-set-children node
    (replace-child child (mcts-children node))))
(define (replace-child child children)
  (cond ((eq children nil) nil)
        ((equal (mcts-state (car children)) (mcts-state child)) (cons child (cdr children)))
        (t (cons (car children) (replace-child child (cdr children))))))

; ---- UCB1 ------------------------------------------------------------------
(define (ucb child parent-visits c)
  (let ((visits (mcts-visits child)))
    (cond ((= visits 0) 10000.0)
          ; the child stores its value from the CHILD mover's point of view, so
          ; the parent must exploit (1 - Q), not Q
          (t (+ (- 1.0 (/ (mcts-total child) visits)) (* c (sqrt (/ (log parent-visits) visits))))))))

(define (mcts-best-ucb node c)
  (car (foldl (lambda (best child)
                (let ((score (ucb child (mcts-visits node) c)))
                  (cond ((or (eq best nil) (> score (cdr best))) (cons child score)) (t best))))
              nil (mcts-children node))))

(define (mcts-expand node moves apply-fn)
  (cond ((eq moves nil) node)
        (t (mcts-expand (mcts-add-child node (mcts-node (apply-fn (mcts-state node) (car moves))))
                        (cdr moves) apply-fn))))

; ---- random playout --------------------------------------------------------
; Normal play: a player with no legal move has LOST. `turn` is 0 while it is
; the turn of the player to move in the position the rollout started from, so
; the value returned is always from THAT player's point of view.
(define (random-choice l) (nth (mod (random 10000) (length l)) l))

(define (mcts-rollout state moves-fn apply-fn turn max-depth)
  (let ((moves (moves-fn state)))
    (cond ((eq moves nil) (cond ((= turn 0) 0.0) (t 1.0)))
          ((<= max-depth 0) 0.5)
          (t (mcts-rollout (apply-fn state (random-choice moves)) moves-fn apply-fn (- 1 turn) (- max-depth 1))))))

; ---- one iteration: select / expand / simulate / backpropagate --------------
; Negamax convention: a node's value is held from the point of view of the
; player to move AT that node, so the value handed back up is (1 - r).
; Returns (list rebuilt-node value-for-player-to-move-here).
(define (mcts-visit node moves-fn apply-fn c max-depth)
  (let* ((state (mcts-state node))
         (moves (moves-fn state)))
    (cond ((eq moves nil) (list (mcts-update node 0.0) 0.0))
          ((eq (mcts-children node) nil)
           (let* ((expanded (mcts-expand node moves apply-fn))
                  (r (mcts-rollout state moves-fn apply-fn 0 max-depth)))
             (list (mcts-update expanded r) r)))
          (t (let* ((kid (mcts-best-ucb node c))
                    (res (mcts-visit kid moves-fn apply-fn c max-depth))
                    (r (- 1.0 (cadr res))))
               (list (mcts-update (mcts-replace-child node (car res)) r) r))))))

(define (mcts-iterations root iterations moves-fn apply-fn c max-depth)
  (cond ((= iterations 0) root)
        (t (mcts-iterations (car (mcts-visit root moves-fn apply-fn c max-depth))
                            (- iterations 1) moves-fn apply-fn c max-depth))))

(define (mcts-best-move root)
  (foldl (lambda (best child)
    (cond ((or (eq best nil) (> (mcts-visits child) (mcts-visits best))) child) (t best)))
    nil (mcts-children root)))

(define (nim-state piles) piles)
(define (nim-terminal piles) (every (lambda (p) (= p 0)) piles))
; append one level only -- `flatmap` deep-flattens and would splice every
; (pile-index, count) move into loose integers
(define (concat-map f l) (foldr append nil (map f l)))
(define (nim-moves piles)
  (concat-map (lambda (idx)
    (let ((n (nth idx piles)))
      (cond ((= n 0) nil) (t (map (lambda (take) (list idx take)) (range 1 (+ n 1)))))))
    (range 0 (length piles))))
(define (nim-apply piles move)
  (let ((i (car move)) (c (cadr move)))
    (map (lambda (idx) (cond ((= idx i) (- (nth idx piles) c)) (t (nth idx piles)))) (range 0 (length piles)))))
(define (nim-eval piles) (nim-terminal piles))

(define init-nim (list 3 4 5))
(define root (mcts-node init-nim))
(define trained (mcts-iterations root 500 nim-moves nim-apply 1.4 30))
(define best (mcts-best-move trained))
(check "mcts-nim-finds-move" (not (eq best nil)))
(check "mcts-nim-explores" (> (mcts-visits trained) 100))

; --- invariants that hold for ANY random seed -------------------------------
(check "mcts-root-visits-equal-iterations" (= (mcts-visits trained) 500))
; the root is visited once on its own expansion, every later visit descends
(check "mcts-child-visits-account-for-root"
  (= (foldl (lambda (acc kid) (+ acc (mcts-visits kid))) 0 (mcts-children trained))
     (- (mcts-visits trained) 1)))
(check "mcts-root-has-one-child-per-move"
  (= (length (mcts-children trained)) (length (nim-moves init-nim))))
(check "mcts-best-is-a-legal-successor"
  (contains (mcts-state best) (map (lambda (m) (nim-apply init-nim m)) (nim-moves init-nim))))
; With the seed fixed at the top of the file this run is fully deterministic.
; 3 xor 4 xor 5 = 2, so the unique winning reply is 3 -> 3 xor 2 = 1, and the
; search does converge on it: most visits go to (1 4 5).
(check "mcts-picks-the-winning-nim-move" (equal (mcts-state best) (list 1 4 5)))
(check "mcts-values-are-probabilities"
  (every (lambda (kid) (and (>= (mcts-mean kid) 0.0) (<= (mcts-mean kid) 1.0))) (mcts-children trained)))

(define (two-pile-nim piles) (= (length piles) 2))
(define nim2 (list 2 2))
(define r2 (mcts-node nim2))
(define t2 (mcts-iterations r2 500 nim-moves nim-apply 1.4 30))
(define b2 (mcts-best-move t2))
(check "mcts-nim-2pile-move" (not (eq b2 nil)))
; (2 2) is a P-position: whoever must move loses under perfect play, so the
; root's win rate for the player to move must stay below one half.
(check "mcts-2pile-is-losing-for-mover" (< (mcts-mean t2) 0.5))

(define (mcts-search state moves-fn apply-fn iterations)
  (let* ((n0 (mcts-node state))
         (tree (mcts-iterations n0 iterations moves-fn apply-fn 1.4 30)))
    (mcts-best-move tree)))

(define action (mcts-search init-nim nim-moves nim-apply 200))
(check "mcts-search-returns-child" (not (eq action nil)))

(print "=== MCTS Done ===")
