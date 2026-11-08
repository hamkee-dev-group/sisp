(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== Heuristic Search: A* and IDA* ===")

; half-open integer range [lo, hi)
(define (range lo hi) (cond ((>= lo hi) nil) (t (cons lo (range (+ lo 1) hi)))))

(define (make-puzzle tiles) tiles)
(define (puzzle-ref p i) (nth i p))
(define (puzzle-zero p)
  (index-of 0 p))

; association lists (not SISP builtins)
(define (alist-set al key val) (cons (cons key val) al))
(define (alist-get al key)
  (cond ((eq al nil) nil) ((equal (caar al) key) (cdar al)) (t (alist-get (cdr al) key))))

(define goal-puzzle (list 1 2 3 4 5 6 7 8 0))

(define (manhattan-distance puzzle)
  (foldl (lambda (sum i)
    (let ((tile (puzzle-ref puzzle i)))
      (cond ((= tile 0) sum) (t (+ sum (tile-distance tile i))))))
    0 (range 0 9)))

(define (tile-distance tile pos)
  (let ((goal-row (truncate (/ (- tile 1) 3))) (goal-col (mod (- tile 1) 3))
        (cur-row (truncate (/ pos 3))) (cur-col (mod pos 3)))
    (+ (abs (- cur-row goal-row)) (abs (- cur-col goal-col)))))

(define (misplaced-tiles puzzle)
  (foldl (lambda (cnt i)
    (let ((tile (puzzle-ref puzzle i)))
      (cond ((or (= tile 0) (= tile (nth i goal-puzzle))) cnt) (t (+ cnt 1)))))
    0 (range 0 9)))

(define (valid-moves pos)
  (let ((row (truncate (/ pos 3))) (col (mod pos 3)))
    (filter (lambda (d) (not (eq d nil)))
      (list (cond ((> row 0) (- pos 3)) (t nil))
            (cond ((< row 2) (+ pos 3)) (t nil))
            (cond ((> col 0) (- pos 1)) (t nil))
            (cond ((< col 2) (+ pos 1)) (t nil))))))

(define (swap puzzle from to)
  (map (lambda (i)
    (cond ((= i from) (puzzle-ref puzzle to))
          ((= i to) (puzzle-ref puzzle from))
          (t (puzzle-ref puzzle i))))
    (range 0 9)))

(define (astar-search start goal heuristic)
  (let* ((z (puzzle-zero start))
         (h (heuristic start)))
    (astar-loop (list (list start z h 0 nil)) nil goal heuristic)))

(define (astar-loop open closed goal heuristic)
  (cond ((eq open nil) nil)
        (t (let* ((node (astar-best open))
                  (state (car node))
                  (rest (remove-node node open)))
             (cond ((equal state goal) (reverse-path node))
                   ((in-set state closed) (astar-loop rest closed goal heuristic))
                   (t (let* ((new-closed (cons state closed))
                             (children (expand-node-astar node goal heuristic)))
                        (astar-loop (merge-open children rest) new-closed goal heuristic))))))))

(define (astar-best open)
  (foldl (lambda (best node)
    (let ((best-f (+ (nth 3 best) (nth 2 best))) (node-f (+ (nth 3 node) (nth 2 node))))
      (cond ((< node-f best-f) node) (t best))))
    (car open) (cdr open)))

(define (remove-node node open) (filter (lambda (n) (not (equal n node))) open))
(define (in-set state closed) (contains state closed))
(define (reverse-path node) (cond ((eq (nth 4 node) nil) (list (car node))) (t (append (reverse-path (nth 4 node)) (list (car node))))))

; a node is (state blank-index h g parent-node)
(define (expand-node-astar node goal heuristic)
  (let* ((state (car node))
         (z (puzzle-zero state))
         (moves (valid-moves z)))
    (map (lambda (m)
      (let* ((np (swap state z m))
             (h (heuristic np))
             (g (+ (nth 3 node) 1)))
        (list np m h g node)))
      moves)))

(define (merge-open children open)
  (foldl (lambda (acc child) (cons child acc)) open children))

(define simple-start (list 1 2 3 4 5 6 7 0 8))
(define (simple-h puzzle) 0)
(define path (astar-search simple-start goal-puzzle simple-h))
(check "astar-solves-trivial" (not (eq path nil)))
; the blank sits at index 7 and belongs at 8: exactly one move, so 2 states
(check "astar-trivial-path-length" (= (length path) 2))
(check "astar-path-starts-at-start" (equal (car path) simple-start))
(check "astar-path-ends-at-goal" (equal (last path) goal-puzzle))

; ---- IDA* -----------------------------------------------------------------
; The DFS returns either a PATH (a list) or a NUMBER: the smallest f-value that
; exceeded the current bound, which becomes the next bound. `ida-inf` means
; "nothing left to try below max-depth", which is what terminates the search.
(define ida-inf 999999)

(define (ida*-search start goal heuristic max-depth)
  (ida*-loop start goal heuristic (heuristic start) max-depth))

(define (ida*-loop state goal heuristic bound max-depth)
  (let ((result (ida*-dfs state goal heuristic 0 bound max-depth)))
    (cond ((listp result) result)
          ((>= result ida-inf) nil)
          (t (ida*-loop state goal heuristic result max-depth)))))

(define (ida*-dfs state goal heuristic g bound max-depth)
  (let ((f (+ g (heuristic state))))
    (cond ((> f bound) f)
          ((equal state goal) (list state))
          ((>= g max-depth) ida-inf)
          (t (ida*-try-moves state goal (valid-moves (puzzle-zero state)) heuristic g bound max-depth)))))

(define (ida*-try-moves state goal moves heuristic g bound max-depth)
  (cond ((eq moves nil) ida-inf)
        (t (let* ((z (puzzle-zero state))
                  (ns (swap state z (car moves)))
                  (result (ida*-dfs ns goal heuristic (+ g 1) bound max-depth)))
             (cond ((listp result) (cons state result))
                   (t (let ((remaining (ida*-try-moves state goal (cdr moves) heuristic g bound max-depth)))
                        (cond ((listp remaining) remaining)
                              ; both branches failed: the next bound is the
                              ; MINIMUM over-bound f-value, not the first one
                              (t (min result remaining))))))))))

(define simple-start-b (list 1 2 3 4 5 6 0 7 8))
(define path-b (astar-search simple-start-b goal-puzzle simple-h))
(define ida-b (ida*-search simple-start-b goal-puzzle manhattan-distance 20))
(check "astar-dijkstra-finds" (not (eq path-b nil)))
(check "ida-finds-solution" (not (eq ida-b nil)))
; the blank must travel 6 -> 7 -> 8: two moves, so 3 states
(check "astar-b-path-length" (= (length path-b) 3))
; IDA* is optimal too, so it must agree with A* on cost
(check "ida-agrees-with-astar" (= (length ida-b) (length path-b)))
(check "ida-path-ends-at-goal" (equal (last ida-b) goal-puzzle))

(define (astar-count-nodes start goal heuristic)
  (let* ((z (puzzle-zero start)) (h (heuristic start)))
    (astar-count-loop (list (list start z h 0 nil)) nil goal heuristic 0)))

(define (astar-count-loop open closed goal heuristic cnt)
  (cond ((eq open nil) cnt)
        (t (let* ((node (astar-best open))
                  (state (car node))
                  (rest (remove-node node open)))
             (cond ((equal state goal) cnt)
                   ((in-set state closed) (astar-count-loop rest closed goal heuristic cnt))
                   (t (let* ((new-closed (cons state closed))
                             (children (expand-node-astar node goal heuristic)))
                        (astar-count-loop (merge-open children rest) new-closed goal heuristic (+ cnt 1)))))))))

(define start-md (list 1 2 3 4 5 6 7 0 8))
(define path-md (astar-search start-md goal-puzzle manhattan-distance))
; admissibility: h(start) never exceeds the true solution cost
(check "manhattan-heuristic-admissible"
  (<= (manhattan-distance start-md) (- (length path-md) 1)))
; an informed heuristic must not expand MORE nodes than blind Dijkstra
(check "manhattan-expands-no-more-than-dijkstra"
  (<= (astar-count-nodes start-md goal-puzzle manhattan-distance)
      (astar-count-nodes start-md goal-puzzle simple-h)))

; heuristic VALUES on the known start (8 is one step out of place)
(check "misplaced-tiles on start-md = 1"
       (= (misplaced-tiles start-md) 1))
(check "manhattan-distance on start-md = 1"
       (= (manhattan-distance start-md) 1))
(check "manhattan dominates misplaced"
       (>= (manhattan-distance start-md) (misplaced-tiles start-md)))

(print "=== Heuristic Search Done ===")
