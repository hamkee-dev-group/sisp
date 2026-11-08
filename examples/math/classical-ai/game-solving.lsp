(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== Retrograde Analysis & Game Solving ===")

; half-open integer range [lo, hi)
(define (range lo hi) (cond ((>= lo hi) nil) (t (cons lo (range (+ lo 1) hi)))))

; association lists (not SISP builtins)
(define (alist-set al key val) (cons (cons key val) al))
(define (alist-get al key)
  (cond ((eq al nil) nil) ((equal (caar al) key) (cdar al)) (t (alist-get (cdr al) key))))

; append one level only -- `flatmap` DEEP-flattens, which would splice the
; pile-lists and move-triples we build here into a flat soup of atoms.
(define (concat-map f l) (foldr append nil (map f l)))

; mex = minimum excludant of a multiset of non-negative integers
(define (mex-inner sorted n)
  (cond ((eq sorted nil) n) ((< n (car sorted)) n) ((= n (car sorted)) (mex-inner (cdr sorted) (+ n 1))) (t (mex-inner (cdr sorted) n))))

(define (mex nums) (mex-inner (sort < nums) 0))

(define (nim-grundy piles)
  (foldl (lambda (g p) (logxor g p)) 0 piles))

(define (logxor a b)
  (cond ((= a 0) b) ((= b 0) a) (t (logxor-bit a b 0 0))))

(define (logxor-bit a b pos sum)
  (cond ((and (= a 0) (= b 0)) sum)
        (t (let ((abit (mod a 2)) (bbit (mod b 2)))
             (logxor-bit (truncate (/ a 2)) (truncate (/ b 2)) (+ pos 1)
               (+ sum (* (cond ((= abit bbit) 0) (t 1)) (^ 2 pos))))))))

(define nim-test (list 3 4 5))
(check "nim-grundy-nonzero" (not (= (nim-grundy nim-test) 0)))

(define nim2 (list 1 2 3))
(check "nim-position-grundy" (= (nim-grundy nim2) 0))

(define (nim-moves-piles piles)
  (concat-map (lambda (i)
    (let ((n (nth i piles)))
      (cond ((= n 0) nil) (t (map (lambda (c) (replace-nth piles i (- n c))) (range 1 (+ n 1)))))))
    (range 0 (length piles))))

(define (replace-nth lst i val)
  (cond ((eq lst nil) nil) ((= i 0) (cons val (cdr lst))) (t (cons (car lst) (replace-nth (cdr lst) (- i 1) val)))))

(define (nim-solve piles)
  (let ((g (nim-grundy piles)))
    (cond ((= g 0) nil) (t (find-winning-move piles piles (nim-moves-piles piles))))))

(define (find-winning-move original piles moves)
  (cond ((eq moves nil) nil)
        ((= (nim-grundy (car moves)) 0) (car moves))
        (t (find-winning-move original piles (cdr moves)))))

(define winning-move (nim-solve (list 3 4 5)))
(check "nim-has-winning-move" (not (eq winning-move nil)))
; 3^4^5 = 2, so the only winning reply is 3 -> 3^2 = 1
(check "nim-winning-move-is-1-4-5" (equal winning-move (list 1 4 5)))
(check "nim-winning-move-is-a-P-position" (= (nim-grundy winning-move) 0))

(define (domineering-board w h)
  (foldl (lambda (acc i) (cons (quote e) acc)) nil (range 0 (* w h))))

(define (dom-move-row board w h row col direction)
  (cond ((eq direction (quote h))
         (list (* row w) (+ col 1) w h))
        ((eq direction (quote v))
         (list row col row (+ col 1) w h))
        (t nil)))

(define (dom-valid board w h row col dir)
  (cond ((eq dir (quote h))
         (and (< (+ col 1) w) (eq (nth (+ (* row w) col) board) (quote e)) (eq (nth (+ (* row w) col 1) board) (quote e))))
        ((eq dir (quote v))
         (and (< (+ row 1) h) (eq (nth (+ (* row w) col) board) (quote e)) (eq (nth (+ (* (+ row 1) w) col) board) (quote e))))
        (t nil)))

(define (dom-apply board w h row col dir player)
  (cond ((eq dir (quote h))
         (lset (lset board (+ (* row w) col) player) (+ (* row w) col 1) player))
        ((eq dir (quote v))
         (lset (lset board (+ (* row w) col) player) (+ (* (+ row 1) w) col) player))
        (t board)))

(define (lset lst i val) (cond ((= i 0) (cons val (cdr lst))) (t (cons (car lst) (lset (cdr lst) (- i 1) val)))))

(define (dom-moves board w h)
  (concat-map (lambda (row)
    (concat-map (lambda (col)
      (filter (lambda (d) (not (eq d nil)))
        (list (cond ((dom-valid board w h row col (quote h)) (list (quote h) row col)) (t nil))
              (cond ((dom-valid board w h row col (quote v)) (list (quote v) row col)) (t nil)))))
      (range 0 w)))
    (range 0 h)))

(define (dom-terminal-p board w h)
  (eq (dom-moves board w h) nil))

(define (dom-grundy board w h)
  (cond ((dom-terminal-p board w h) 0)
        (t (let ((moves (dom-moves board w h)))
             (mex (map (lambda (m)
                   (let* ((dir (car m)) (row (cadr m)) (col (caddr m))
                          (nb (dom-apply board w h row col dir (quote x))))
                     (dom-grundy nb w h)))
                 moves))))))

(define dom-test (list (quote e) (quote e) (quote e) (quote e)))
(define dg (dom-grundy dom-test 2 2))
; NB both players may play either orientation here, so this is impartial Cram,
; not partizan Domineering. On 2x2 every first move leaves exactly one reply,
; so each child has Grundy value mex{0} = 1 and the root is mex{1,1,1,1} = 0.
(check "domineering2x2-grundy" (>= dg 0))
(check "domineering2x2-grundy-is-0" (= dg 0))

(define (hackenbush-simple edges)
  (foldl (lambda (g e) (logxor g e)) 0 edges))

(define (hb-nim-simple)
  (hackenbush-simple (list 1 2 1)))
(check "hackenbush-nim-value" (not (= (hb-nim-simple) 0)))

; ---- genuine retrograde analysis ------------------------------------------
; Backward induction over a topologically ordered position list: a position
; with no legal move is a LOSS for the player to move; otherwise it is a WIN
; exactly when some move reaches a position already labelled LOSS.
; `positions` must list every child before its parent -- for Nim, ordering by
; total stones ascending guarantees that (a move strictly removes stones).
(define (retrograde-label positions moves-fn)
  (foldl (lambda (tbl pos)
           (let* ((kids (moves-fn pos))
                  (won (any (lambda (c) (eq (alist-get tbl c) (quote loss))) kids)))
             (alist-set tbl pos (cond (won (quote win)) (t (quote loss))))))
         nil positions))

; all two-heap Nim positions with heaps in [0, maxh], ordered by total stones
(define (nim2-positions maxh)
  (sort (lambda (p q) (< (+ (car p) (cadr p)) (+ (car q) (cadr q))))
        (concat-map (lambda (a) (map (lambda (b) (list a b)) (range 0 (+ maxh 1))))
                    (range 0 (+ maxh 1)))))

(define nim2-all (nim2-positions 3))
(define nim2-table (retrograde-label nim2-all nim-moves-piles))
(check "nim-retrograde-solved" (not (eq nim2-table nil)))
(check "nim-retrograde-labels-all" (= (length nim2-table) (length nim2-all)))
; the empty position is a loss for whoever must move
(check "nim-retrograde-empty-is-loss" (eq (alist-get nim2-table (list 0 0)) (quote loss)))
; Sprague-Grundy cross-check: loss <=> xor of the heaps is 0 <=> equal heaps
(check "nim-retrograde-matches-xor"
  (every (lambda (pos)
           (eq (alist-get nim2-table pos)
               (cond ((= (nim-grundy pos) 0) (quote loss)) (t (quote win)))))
         nim2-all))
(check "nim-retrograde-losses-are-equal-heaps"
  (every (lambda (pos) (eq (= (car pos) (cadr pos))
                           (eq (alist-get nim2-table pos) (quote loss))))
         nim2-all))

(define (grundy-table game max-n moves-fn)
  (foldl (lambda (tbl n)
    (let* ((pos (game n))
           (children (moves-fn pos))
           (child-gs (map (lambda (c) (let ((g (alist-get tbl c))) (cond ((eq g nil) 0) (t g)))) children))
           (gval (mex child-gs)))
      (alist-set tbl pos gval)))
    nil (range 0 (+ max-n 1))))

(define nim-gt (grundy-table (lambda (n) (list n)) 10 nim-moves-piles))
(check "nim-grundy-table-built" (not (eq nim-gt nil)))
; a single Nim heap of n has Grundy value n
(check "nim-grundy-table-is-identity"
  (every (lambda (n) (= (alist-get nim-gt (list n)) n)) (range 0 11)))

(print "=== Game Solving Done ===")
