; ============================================================
; name: minimax-alpha-beta
; file: algorithms/ai/minimax-alpha-beta.lsp
; subject: ai
; capability: game-tree search adversarial
; aliases: "minimax" "alpha-beta" "alpha beta pruning" "game search"
; signature: (mm-eval board depth maximizing) (ab-eval board depth
;            alpha beta maximizing) (best-move board maximizing)
; complexity: O(b^d) minimax, O(b^(d/2)) alpha-beta best case
; see-also: mcts (examples/math/classical-ai/mcts.lsp)
; derived-from: Knuth & Moore 1975 (alpha-beta); Russell & Norvig,
;               AIMA ch. 5
; status: example (tested)
; checks: 13 checks incl. exact game value, ab == minimax, and the
;         pruning-effectiveness node comparison; run: src/sisp <file>
; ============================================================
; Tic-tac-toe minimax with alpha-beta pruning.  Board = (player .
; 9 cells), cells x/o/e.  Scores: x win = 10 - depth (prefer fast
; wins), o win = depth - 10, draw = 0.  x maximizes, o minimizes.
; Alpha-beta must short-circuit, so it is an explicit scan, not a fold.

(define (mm-board-cells board) (cdr board))
(define (mm-cell board i) (nth i (cdr board)))

(define (mm-set-cell board i val)
  (cons (car board) (mm-lset (cdr board) i val)))

(define (mm-lset lst i val)
  (cond ((= i 0) (cons val (cdr lst)))
        (t (cons (car lst) (mm-lset (cdr lst) (- i 1) val)))))

(define (mm-empty-cells board)
  (foldl (lambda (acc i)
           (cond ((eq (nth i (mm-board-cells board)) (quote e))
                  (cons i acc))
                 (t acc)))
         nil (list 8 7 6 5 4 3 2 1 0)))

(define (mm-winner board)
  (mm-find-win board
    (list (list 0 1 2) (list 3 4 5) (list 6 7 8)
          (list 0 3 6) (list 1 4 7) (list 2 5 8)
          (list 0 4 8) (list 2 4 6))))

(define (mm-find-win board lines)
  (cond ((null lines) nil)
        (t (let* ((l (car lines))
                  (a (mm-cell board (car l)))
                  (b (mm-cell board (cadr l)))
                  (c (mm-cell board (caddr l))))
             (cond ((and (not (eq a (quote e))) (eq a b) (eq b c)) a)
                   (t (mm-find-win board (cdr lines))))))))

(define (mm-terminal board)
  (cond ((not (eq (mm-winner board) nil)) t)
        ((eq (mm-empty-cells board) nil) t)
        (t nil)))

(define (mm-eval-terminal board depth)
  (let ((w (mm-winner board)))
    (cond ((eq w (quote x)) (- 10 depth))
          ((eq w (quote o)) (+ depth (- 10)))
          (t 0))))

(define (mm-eval board depth maximizing)
  "minimax value of board"
  (cond ((mm-terminal board) (mm-eval-terminal board depth))
        (maximizing
         (foldl (lambda (best i)
                  (max best (mm-eval (mm-set-cell board i (quote x))
                                     (+ depth 1) nil)))
                -1000 (mm-empty-cells board)))
        (t (foldl (lambda (best i)
                    (min best (mm-eval (mm-set-cell board i (quote o))
                                       (+ depth 1) t)))
                  1000 (mm-empty-cells board)))))

(define (ab-eval board depth alpha beta maximizing)
  "alpha-beta value of board"
  (cond ((mm-terminal board) (mm-eval-terminal board depth))
        (maximizing (ab-scan board depth alpha beta (mm-empty-cells board) t))
        (t (ab-scan board depth alpha beta (mm-empty-cells board) nil))))

(define (ab-scan board depth alpha beta moves maximizing)
  (cond ((null moves) (cond (maximizing alpha) (t beta)))
        (t (let* ((player (cond (maximizing (quote x)) (t (quote o))))
                  (nb (mm-set-cell board (car moves) player))
                  (val (ab-eval nb (+ depth 1) alpha beta (not maximizing))))
             (cond (maximizing
                    (let ((na (max alpha val)))
                      (cond ((>= na beta) na)
                            (t (ab-scan board depth na beta (cdr moves) maximizing)))))
                   (t (let ((nbeta (min beta val)))
                      (cond ((<= nbeta alpha) nbeta)
                            (t (ab-scan board depth alpha nbeta (cdr moves) maximizing))))))))))

(define (best-move board maximizing)
  "the best cell index for the player to move, per alpha-beta"
  (car (best-move-walk board (mm-empty-cells board) maximizing
                       (list -1 -1000))))

(define (best-move-walk board moves maximizing best)
  (cond ((null moves) best)
        (t (let* ((player (cond (maximizing (quote x)) (t (quote o))))
                  (nb (mm-set-cell board (car moves) player))
                  (val (ab-eval nb 0 -1000 1000 (not maximizing))))
             (cond ((or (and maximizing (> val (cadr best)))
                        (and (not maximizing) (< val (cadr best))))
                    (best-move-walk board (cdr moves) maximizing
                                    (list (car moves) val)))
                   (t (best-move-walk board (cdr moves) maximizing best)))))))

; ---- instrumentation (setq counters, checks only) ----

(define mm-nodes 0)
(define ab-nodes 0)

(define (mm-counted board depth maximizing)
  (progn (setq mm-nodes (+ mm-nodes 1))
         (cond ((mm-terminal board) (mm-eval-terminal board depth))
               (maximizing
                (foldl (lambda (best i)
                         (max best (mm-counted (mm-set-cell board i (quote x))
                                               (+ depth 1) nil)))
                       -1000 (mm-empty-cells board)))
               (t (foldl (lambda (best i)
                           (min best (mm-counted (mm-set-cell board i (quote o))
                                                 (+ depth 1) t)))
                         1000 (mm-empty-cells board))))))

(define (ab-counted board depth alpha beta maximizing)
  (progn (setq ab-nodes (+ ab-nodes 1))
         (cond ((mm-terminal board) (mm-eval-terminal board depth))
               (maximizing (ab-scan-c board depth alpha beta (mm-empty-cells board) t))
               (t (ab-scan-c board depth alpha beta (mm-empty-cells board) nil)))))

(define (ab-scan-c board depth alpha beta moves maximizing)
  (cond ((null moves) (cond (maximizing alpha) (t beta)))
        (t (let* ((player (cond (maximizing (quote x)) (t (quote o))))
                  (nb (mm-set-cell board (car moves) player))
                  (val (ab-counted nb (+ depth 1) alpha beta (not maximizing))))
             (cond (maximizing
                    (let ((na (max alpha val)))
                      (cond ((>= na beta) na)
                            (t (ab-scan-c board depth na beta (cdr moves) maximizing)))))
                   (t (let ((nbeta (min beta val)))
                      (cond ((<= nbeta alpha) nbeta)
                            (t (ab-scan-c board depth alpha nbeta (cdr moves) maximizing))))))))))

(define (mm-cell-empty-p i)
  (and (>= i 0) (< i 9)
       (eq (mm-cell mm-mid i) (quote e))))

; ---- boards ----

(define mm-empty (cons (quote x) (list (quote e) (quote e) (quote e)
                                       (quote e) (quote e) (quote e)
                                       (quote e) (quote e) (quote e))))

; mid-game (7 empty cells): x to move, no threats, draw under best play
(define mm-mid
  (cons (quote x) (list (quote x) (quote o) (quote e)
                        (quote o) (quote e) (quote e)
                        (quote e) (quote e) (quote e))))

; x to move; x can win in one at cell 2
(define mm-win-in-1
  (cons (quote x) (list (quote x) (quote x) (quote e)
                        (quote o) (quote o) (quote e)
                        (quote e) (quote e) (quote e))))

; x to move; o threatens cell 2, x blocks there, draw under best play
(define mm-must-block
  (cons (quote x) (list (quote o) (quote o) (quote e)
                        (quote e) (quote x) (quote e)
                        (quote e) (quote e) (quote e))))

; ---- checks ----

(check "minimax: mid-game board is a draw under best play (value 0)"
       (= (mm-eval mm-mid 0 t) 0))

(check "alpha-beta: mid-game board is a draw under best play (value 0)"
       (= (ab-eval mm-mid 0 -1000 1000 t) 0))

(check "minimax: win in 1 scores 9 (10 - 1)"
       (= (mm-eval mm-win-in-1 0 t) 9))

(check "alpha-beta: win in 1 scores 9"
       (= (ab-eval mm-win-in-1 0 -1000 1000 t) 9))

(check "alpha-beta: equals minimax on the mid-game board"
       (= (ab-eval mm-mid 0 -1000 1000 t)
          (mm-eval mm-mid 0 t)))

(check "alpha-beta: equals minimax on a blocking board"
       (= (ab-eval mm-must-block 0 -1000 1000 t)
          (mm-eval mm-must-block 0 t)))

(check "minimax: must-block board prevents the immediate loss (value >= 0)"
       (>= (mm-eval mm-must-block 0 t) 0))

(check "best-move: takes the immediate win (cell 2)"
       (= (best-move mm-win-in-1 t) 2))

(check "best-move: blocks the immediate loss (cell 2)"
       (= (best-move mm-must-block t) 2))

(check "best-move: mid-game plays some legal move"
       (mm-cell-empty-p (best-move mm-mid t)))

(check "pruning: alpha-beta expands strictly fewer nodes than minimax"
       (let ((v1 (progn (setq mm-nodes 0) (mm-counted mm-mid 0 t)))
             (v2 (progn (setq ab-nodes 0) (ab-counted mm-mid 0 -1000 1000 t))))
         (and (= v1 v2)
              (< ab-nodes mm-nodes))))

(check "pruning: the node saving is substantial (minimax > 2x alpha-beta)"
       (let ((v1 (progn (setq mm-nodes 0) (mm-counted mm-mid 0 t)))
             (v2 (progn (setq ab-nodes 0) (ab-counted mm-mid 0 -1000 1000 t))))
         (> mm-nodes (* 2 ab-nodes))))

(check "terminal: x win in line is detected"
       (eq (mm-winner (cons (quote o) (list (quote x) (quote x) (quote x)
                                            (quote e) (quote e) (quote e)
                                            (quote e) (quote e) (quote e))))
            (quote x)))
