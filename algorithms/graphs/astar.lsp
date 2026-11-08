; ============================================================
; name: astar
; file: algorithms/graphs/astar.lsp
; subject: graph
; capability: search optimize pathfinding
; aliases: "a-star" "astar" "a* search" "heuristic pathfinding"
; signature: (search-loop) -> path, over the globals *open* *goal* *heuristic*
; complexity: O(V^2) here (linear scan of the open list); a consistent
;             heuristic lets A* expand fewer nodes than Dijkstra
; see-also: maximum-flow
; derived-from: Hart, Nilsson & Raphael 1968
; status: example (tested)
; checks: 7 - Manhattan heuristic; node-expansion compared vs Dijkstra
; ============================================================
; A* Search — Pathfinding on 5x5 Grid with Obstacles
; Manhattan-distance heuristic. Compare with Dijkstra:
; A* expands fewer nodes. Verify path length and nodes expanded.
; ============================================================

(print "=== A* Search Pathfinding ===")


; ---------- grid 5x5 with obstacles ----------
(define obstructions (list (list 0 1) (list 1 2) (list 2 3) (list 3 4)))

(define (obstacle-p r c)
  (contains (list r c) obstructions))

(define (in-bounds-p r c)
  (and (>= r 0) (<= r 4) (>= c 0) (<= c 4)))

(define (valid-cell-p r c)
  (and (in-bounds-p r c) (not (obstacle-p r c))))

(define (manhattan r1 c1 r2 c2)
  (+ (abs (- r1 r2)) (abs (- c1 c2))))

(define (neighbors-4 r c)
  (filter (lambda (p) (valid-cell-p (car p) (cadr p)))
    (list (list (- r 1) c) (list (+ r 1) c)
          (list r (- c 1)) (list r (+ c 1)))))

; ---------- alist helpers ----------
(define (alist-get al key)
  (cond ((eq al nil) nil)
        ((equal (caar al) key) (cdar al))
        (t (alist-get (cdr al) key))))

(define (alist-set al key val)
  (cons (cons key val) al))

; ---------- global state ----------
(setq *gscore* nil)
(setq *fscore* nil)
(setq *parent* nil)
(setq *expanded* 0)
(setq *open* nil)
(setq *heuristic* nil)
(setq *goal* nil)

; ---------- extract min by f-score ----------
(define (extract-min-score unvisited score-fn)
  (cond ((eq unvisited nil) nil)
        (t (let ((seed (car unvisited)))
          (foldl (lambda (best cell)
            (let ((db (score-fn best))
                  (dc (score-fn cell)))
              (cond ((< dc db) cell) (t best))))
            seed (cdr unvisited))))))

(define (f-get cell)
  (let ((v (alist-get *fscore* cell)))
    (cond ((eq v nil) 99999) (t v))))

(define (g-get cell)
  (let ((v (alist-get *gscore* cell)))
    (cond ((eq v nil) 99999) (t v))))

(define (pop-open)
  (let ((best (extract-min-score *open* (lambda (cell) (f-get cell)))))
    (setq *open* (remove (lambda (c) (equal c best)) *open*))
    best))

(define (reconstruct-path parent goal)
  (cond ((eq (alist-get parent goal) nil) (list goal))
        (t (let ((p (alist-get parent goal)))
          (append (reconstruct-path parent p) (list goal))))))

; ---------- search loop ----------
(define (search-loop)
  (cond ((eq *open* nil) nil)
        (t (let ((current (pop-open)))
          (cond ((equal current *goal*)
                 (reconstruct-path *parent* current))
                (t (setq *expanded* (+ *expanded* 1))
                   (let ((ns (neighbors-4 (car current) (cadr current))))
                     (let ((tentative (+ (alist-get *gscore* current) 1)))
                       (foreach (lambda (nbr)
                         (cond ((< tentative (g-get nbr))
                                (setq *parent* (alist-set *parent* nbr current))
                                (setq *gscore* (alist-set *gscore* nbr tentative))
                                (setq *fscore* (alist-set *fscore* nbr
                                  (+ tentative (*heuristic* nbr *goal*))))
                                (cond ((not (contains nbr *open*))
                                       (setq *open* (cons nbr *open*)))))))
                         ns)
                       (search-loop)))))))))

; ---------- run A* ----------
(define start-node (list 0 0))
(define goal-node (list 4 4))

(setq *gscore* (alist-set nil start-node 0))
(setq *fscore* (alist-set nil start-node (manhattan 0 0 4 4)))
(setq *parent* nil)
(setq *expanded* 0)
(setq *open* (list start-node))
(setq *heuristic* (lambda (n g) (manhattan (car n) (cadr n) (car g) (cadr g))))
(setq *goal* goal-node)

(define astar-path (search-loop))
(define astar-exp *expanded*)

(print "A* path:")
(print astar-path)

; ---------- run Dijkstra ----------
(setq *gscore* (alist-set nil start-node 0))
(setq *fscore* (alist-set nil start-node 0))
(setq *parent* nil)
(setq *expanded* 0)
(setq *open* (list start-node))
(setq *heuristic* (lambda (n g) 0))
(setq *goal* goal-node)

(define dij-path (search-loop))
(define dij-exp *expanded*)

(print "Dijkstra path:")
(print dij-path)

; ---------- verification ----------
(define astar-len (cond ((eq astar-path nil) 0) (t (- (length astar-path) 1))))
(define dij-len (cond ((eq dij-path nil) 0) (t (- (length dij-path) 1))))

(print "A* nodes expanded:")
(print astar-exp)
(print "Dijkstra nodes expanded:")
(print dij-exp)
(print "A* path length (steps):")
(print astar-len)
(print "Dijkstra path length (steps):")
(print dij-len)

(check "astar-finds-path" (not (eq astar-path nil)))
(check "dijkstra-finds-path" (not (eq dij-path nil)))
(check "paths-same-length" (= astar-len dij-len))
(check "path-starts-at-start" (equal (car astar-path) start-node))
(check "path-ends-at-goal" (equal (last astar-path) goal-node))

; Verify each step in path is adjacent
(define (path-valid-p p)
  (cond ((eq (cdr p) nil) t)
        ((contains (cadr p) (neighbors-4 (caar p) (cadar p)))
         (path-valid-p (cdr p)))
        (t nil)))

(check "astar-path-valid" (path-valid-p astar-path))

; On this grid A* expands strictly fewer nodes than Dijkstra
(print "=== A* vs Dijkstra comparison ===")
(check "astar-fewer-expanded" (< astar-exp dij-exp))

(print "=== A* Search Done ===")
