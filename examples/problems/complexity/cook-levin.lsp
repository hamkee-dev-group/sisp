; cook-levin.lsp -- Reduce 3-COLOR to SAT via Cook-Levin theorem.
; Input: graph adjacency list. Output: CNF + DPLL satisfiability check.
; Variables: v1c1,v1c2,v1c3,... as identifiers via read-from-string.
; Verify on triangle (3-colorable->SAT) and K4 (not->UNSAT).

(define (cl-var v c)
  (car (read-from-string (cat (cat "v" (num-to-str v)) (cat "c" (num-to-str c))))))

(define (cl-colors) (seq 1 3))

(define (cl-at-least-one v)
  (list (map (lambda (c) (cl-var v c)) (cl-colors))))

(define (cl-exclusive v)
  (let ((a (cl-var v 1))
        (b (cl-var v 2))
        (c (cl-var v 3)))
    (list (list (list (quote not) a) (list (quote not) b))
          (list (list (quote not) a) (list (quote not) c))
          (list (list (quote not) b) (list (quote not) c)))))

(define (cl-edge-clauses u v)
  (list (list (list (quote not) (cl-var u 1)) (list (quote not) (cl-var v 1)))
        (list (list (quote not) (cl-var u 2)) (list (quote not) (cl-var v 2)))
        (list (list (quote not) (cl-var u 3)) (list (quote not) (cl-var v 3)))))

(define (cl-vertex-list g) (map car g))

(define (cl-edge-pairs g)
  (cond ((eq g nil) nil)
        (t (let ((u (caar g)) (nb (cadar g)))
             (append (map (lambda (w) (list u w))
                          (filter (lambda (w) (< u w)) nb))
                     (cl-edge-pairs (cdr g)))))))

(define (cl-build-cnf g)
  (let ((vs (cl-vertex-list g))
        (es (cl-edge-pairs g)))
    (append
      (foldl append nil (map cl-at-least-one vs))
      (foldl append nil (map cl-exclusive vs))
      (foldl append nil (map (lambda (e)
                                (cl-edge-clauses (car e) (cadr e)))
                              es)))))

(define (cl-lit-neg-p lit)
  (cond ((consp lit) (eq (car lit) (quote not)))
        (t nil)))

(define (cl-lit-name lit)
  (cond ((cl-lit-neg-p lit) (cadr lit))
        (t lit)))

(define (cl-clause-sat clause model)
  (any (lambda (lit)
         (let ((nm (cl-lit-name lit))
               (ng (cl-lit-neg-p lit)))
           (let ((pr (assoc nm model)))
             (cond ((eq pr nil) nil)
                   (ng (eq (cadr pr) nil))
                   (t (eq (cadr pr) t))))))
       clause))

(define (cl-vars-from-clause clause acc)
  (cond ((eq clause nil) acc)
        (t (cl-vars-from-clause (cdr clause)
             (cons (cl-lit-name (car clause)) acc)))))

(define (cl-vars-from-clauses clauses acc)
  (cond ((eq clauses nil) acc)
        (t (cl-vars-from-clauses (cdr clauses)
             (cl-vars-from-clause (car clauses) acc)))))

(define (cl-extract-vars clauses)
  (dedup (reverse (cl-vars-from-clauses clauses nil))))

(define (cl-search clauses model vars)
  (cond ((eq vars nil)
         (cond ((every (lambda (c) (cl-clause-sat c model)) clauses)
                model)
               (t nil)))
        (t (let ((v (car vars)))
             (let ((pos (cl-search clauses
                           (cons (list v t) model) (cdr vars))))
               (cond (pos pos)
                     (t (cl-search clauses
                          (cons (list v nil) model) (cdr vars)))))))))

(define (cl-solve g)
  (let ((clauses (cl-build-cnf g)))
    (let ((vars (cl-extract-vars clauses)))
      (cl-search clauses nil vars))))

; --- verify triangle (3-colorable) and K4 (not 3-colorable) ---
(let ((tri-result (cl-solve '((1 (2 3)) (2 (1 3)) (3 (1 2)))))
      (k4-result (cl-solve '((1 (2 3 4)) (2 (1 3 4)) (3 (1 2 4)) (4 (1 2 3))))))
  (list
    (list (quote triangle-3colorable) (not (eq tri-result nil)))
    (list (quote k4-unsatisfiable) (eq k4-result nil))
    (list (quote tri-clauses) (length (cl-build-cnf '((1 (2 3)) (2 (1 3)) (3 (1 2))))))
    (list (quote k4-clauses) (length (cl-build-cnf '((1 (2 3 4)) (2 (1 3 4)) (3 (1 2 4)) (4 (1 2 3))))))))
