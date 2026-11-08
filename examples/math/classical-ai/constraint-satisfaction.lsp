(load "lib/collections/pairs.lsp")

(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== Constraint Satisfaction Problems ===")

; half-open integer range [lo, hi)
(define (range lo hi) (cond ((>= lo hi) nil) (t (cons lo (range (+ lo 1) hi)))))

(define (dom-get dom var)
  (cond ((eq dom nil) nil) ((equal (caar dom) var) (cdar dom)) (t (dom-get (cdr dom) var))))
(define (dom-set dom var vals) (cons (cons var vals) dom))
(define (dom-update dom var vals)
  (cond ((eq dom nil) nil) ((equal (caar dom) var) (cons (cons var vals) (cdr dom))) (t (cons (car dom) (dom-update (cdr dom) var vals)))))

; append one level only -- `flatmap` deep-flattens, and a closure is itself a
; cons, so flatmap would tear every constraint lambda apart.
(define (concat-map f l) (foldr append nil (map f l)))

(define (make-domain vars values)
  (map (lambda (v) (cons v values)) vars))

(define (var-domain dom var) (dom-get dom var))
(define (domain-size dom var) (length (dom-get dom var)))

(define (assignment-complete assignment vars)
  (every (lambda (v) (alist-has assignment v)) vars))

(define (alist-get al key)
  (cond ((eq al nil) nil) ((equal (caar al) key) (cdar al)) (t (alist-get (cdr al) key))))
; membership test returning a STRICT boolean: values may legitimately be 0/nil,
; so truthiness of the value cannot stand in for "is this variable assigned?"
(define (alist-has al key)
  (cond ((eq al nil) nil) ((equal (caar al) key) t) (t (alist-has (cdr al) key))))

(define (select-unassigned vars assignment dom)
  (find (lambda (v) (not (alist-has assignment v))) vars))

(define (order-domain var dom)
  (dom-get dom var))

(define (consistent assignment constraints)
  (every (lambda (c) (c assignment)) constraints))

(define (forward-check dom var val constraints vars)
  (let ((tmp (alist-set nil var val)))
    (foldl (lambda (new-dom v)
      (cond ((alist-has tmp v) new-dom)
            (t (let* ((old-vals (dom-get new-dom v))
                      (valid (filter (lambda (x)
                               (let ((tmp2 (alist-set tmp v x)))
                                 (consistent tmp2 constraints)))
                               old-vals)))
                 (cond ((eq valid nil) nil) (t (dom-update new-dom v valid)))))))
      dom vars)))

(define (backtrack assignment vars dom constraints)
  (cond ((assignment-complete assignment vars) assignment)
        (t (let* ((v (select-unassigned vars assignment dom))
                  (vals (order-domain v dom)))
             (backtrack-try assignment vars dom constraints v vals)))))

(define (backtrack-try assignment vars dom constraints var vals)
  (cond ((eq vals nil) nil)
        (t (let* ((val (car vals))
                  (new-assign (alist-set assignment var val)))
             (cond ((consistent new-assign constraints)
                     (let ((fc-dom (forward-check dom var val constraints vars)))
                       (cond ((eq fc-dom nil) (backtrack-try assignment vars dom constraints var (cdr vals)))
                             (t (let ((result (backtrack new-assign vars fc-dom constraints)))
                                  (cond ((eq result nil) (backtrack-try assignment vars dom constraints var (cdr vals)))
                                        (t result)))))))
                   (t (backtrack-try assignment vars dom constraints var (cdr vals))))))))

(define (alist-set al key val) (cons (cons key val) al))
(define (get-values-alist al) (map (lambda (p) (cdr p)) al))

(define map-vars (list (quote wa) (quote nt) (quote sa) (quote q) (quote nsw) (quote v)))
(define map-colors (list (quote r) (quote g) (quote b)))
(define map-dom (make-domain map-vars map-colors))

(define (map-constraint-c a b)
  (lambda (assign) (let ((va (alist-get assign a)) (vb (alist-get assign b))) (or (eq va nil) (eq vb nil) (not (eq va vb))))))
(define map-constraints
  (list (map-constraint-c (quote wa) (quote nt))
        (map-constraint-c (quote wa) (quote sa))
        (map-constraint-c (quote nt) (quote sa))
        (map-constraint-c (quote nt) (quote q))
        (map-constraint-c (quote sa) (quote q))
        (map-constraint-c (quote sa) (quote nsw))
        (map-constraint-c (quote sa) (quote v))
        (map-constraint-c (quote q) (quote nsw))
        (map-constraint-c (quote nsw) (quote v))))

(define map-soln (backtrack nil map-vars map-dom map-constraints))
(check "map-coloring-solved" (not (eq map-soln nil)))
(check "map-coloring-6-vars" (= (length map-soln) 6))
(check "map-coloring-consistent" (consistent map-soln map-constraints))

(define (nqueens-vars n) (map (lambda (i) i) (range 0 n)))
(define (nqueens-dom n) (make-domain (nqueens-vars n) (range 0 n)))
(define (nqueens-constraint r1 r2)
  (lambda (assign) (let ((c1 (alist-get assign r1)) (c2 (alist-get assign r2))) (or (eq c1 nil) (eq c2 nil) (and (not (= c1 c2)) (not (= (abs (- r1 r2)) (abs (- c1 c2)))))))))
(define (nqueens-constraints n)
  (concat-map (lambda (r1)
    (map (lambda (r2) (nqueens-constraint r1 r2))
      (filter (lambda (r2) (not (= r1 r2))) (range 0 n))))
    (range 0 n)))

(define nq4-soln (backtrack nil (nqueens-vars 4) (nqueens-dom 4) (nqueens-constraints 4)))
(check "nqueens4-solved" (not (eq nq4-soln nil)))
(check "nqueens4-consistent" (consistent nq4-soln (nqueens-constraints 4)))
(check "nqueens4-distinct-cols" (= (length (dedup (get-values-alist nq4-soln))) 4))

(define (sudoku-vars) (foldl (lambda (acc i) (cons i acc)) nil (reverse (range 0 16))))
(define (sudoku-dom) (make-domain (sudoku-vars) (list 1 2 3 4)))
(define (sudoku-row cell) (truncate (/ cell 4)))
(define (sudoku-col cell) (mod cell 4))
(define (sudoku-block cell) (+ (* (truncate (/ (sudoku-row cell) 2)) 2) (truncate (/ (sudoku-col cell) 2))))
; two DISTINCT cells are peers when they share a row, a column or a 2x2 block
(define (sudoku-peers-p a b)
  (and (not (= a b))
       (or (= (sudoku-row a) (sudoku-row b))
           (= (sudoku-col a) (sudoku-col b))
           (= (sudoku-block a) (sudoku-block b)))))
(define (sudoku-constraint c1 c2)
  (lambda (assign) (let ((v1 (alist-get assign c1)) (v2 (alist-get assign c2))) (or (eq v1 nil) (eq v2 nil) (not (= v1 v2))))))

(define (sudoku-constraints)
  (concat-map (lambda (c1)
    (map (lambda (c2) (sudoku-constraint c1 c2))
      (filter (lambda (c2) (and (< c1 c2) (sudoku-peers-p c1 c2))) (sudoku-vars))))
    (sudoku-vars)))

(define (sudoku-puzzle)
  (list (cons 0 1) (cons 3 4) (cons 5 3) (cons 6 2) (cons 9 4) (cons 10 1) (cons 15 2)))

(define sudoku-soln (backtrack (sudoku-puzzle) (sudoku-vars) (sudoku-dom) (sudoku-constraints)))
(check "sudoku4x4-solved" (not (eq sudoku-soln nil)))
(check "sudoku-has-16-cells" (= (length sudoku-soln) 16))
(check "sudoku-consistent" (consistent sudoku-soln (sudoku-constraints)))
; the givens must survive in the completed grid
(check "sudoku-respects-givens" (every (lambda (g) (= (alist-get sudoku-soln (car g)) (cdr g))) (sudoku-puzzle)))

; ---- AC-3 arc consistency -------------------------------------------------
; revise(vi, vj): drop every value of D(vi) that has no support in D(vj)
; under the "must differ" constraint. Returns nil on a domain wipeout.
(define (ac3-supported val vj dj)
  (any (lambda (y) (not (equal val y))) dj))

(define (ac3-revise dom vi vj)
  (let* ((di (dom-get dom vi))
         (dj (dom-get dom vj))
         (new-di (cond ((eq dj nil) di)
                       (t (filter (lambda (x) (ac3-supported x vj dj)) di)))))
    (cond ((eq new-di nil) nil)
          (t (dom-update dom vi new-di)))))

(define (ac3-main dom arcs)
  (cond ((eq arcs nil) dom)
        (t (let* ((arc (car arcs))
                  (vi (car arc))
                  (vj (cadr arc))
                  (old-size (domain-size dom vi))
                  (new-dom (ac3-revise dom vi vj)))
             (cond ((eq new-dom nil) nil)
                   ((< (domain-size new-dom vi) old-size)
                    ; D(vi) shrank: re-enqueue every arc (vk, vi), vk /= vi, vj
                    (let ((more-arcs (map (lambda (vk) (list vk vi))
                                          (filter (lambda (vk) (and (not (equal vk vj)) (not (equal vk vi))))
                                                  (get-names dom)))))
                      (ac3-main new-dom (append (cdr arcs) more-arcs))))
                   (t (ac3-main new-dom (cdr arcs))))))))

(define small-arcs (list (list (quote wa) (quote nt)) (list (quote nt) (quote wa)) (list (quote nt) (quote sa)) (list (quote sa) (quote nt)) (list (quote sa) (quote wa)) (list (quote wa) (quote sa))))
(define ac3-result (ac3-main map-dom small-arcs))
(check "ac3-map-reduces-domains" (not (eq ac3-result nil)))

; a problem where AC-3 really prunes: wa=(1 2), nt=(1), wa != nt
; forces wa to (2)  (domains are dotted pairs, like make-domain's)
(define prune-dom (list (cons 'wa (list 1 2)) (cons 'nt (list 1))))
(define prune-arcs (list (list 'wa 'nt) (list 'nt 'wa)))
(define prune-result (ac3-main prune-dom prune-arcs))
(check "ac3 prunes wa to (2) when nt = (1)"
       (equal (cdr (assoc 'wa prune-result)) (list 2)))

(print "=== CSP Done ===")
