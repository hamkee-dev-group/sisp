; ============================================================ ; 111 — Constraint Propagation: AC-3 Arc Consistency ; Map coloring: 3 regions (A,B,C), 2 colors (red, blue). ; Constraints: A≠B, B≠C. ; Show domains before/after AC-3. Backtrack with MRV to find ; all solutions. Verify count = 2. ; ============================================================
(print "=== Constraint Propagation ===")


; ---------- CSP representation ----------
(define colors (list (quote red) (quote blue)))
(define variables (list (quote A) (quote B) (quote C)))

; Domains: alist (var . (list of values))
(define (init-doms)
  (list (cons (quote A) colors)
        (cons (quote B) colors)
        (cons (quote C) colors)))

; Access
(define (dom-get d var)
  (cond ((eq d nil) nil)
        ((eq (caar d) var) (cdar d))
        (t (dom-get (cdr d) var))))

(define (dom-set d var vals)
  (cons (cons var vals) d))

; Constraints: {}(A B), (B A), (B C), (C B)
(define arcs
  (list (list (quote A) (quote B))
        (list (quote B) (quote A))
        (list (quote B) (quote C))
        (list (quote C) (quote B))))

(define (neq a b) (not (eq a b)))

; Revise: remove values from dom(A) that have no support in dom(B)
(define (revise doms vi vj)
  (let ((di (dom-get doms vi)))
    (let ((new-di (filter (lambda (x)
            (foldl (lambda (acc y)
              (or acc (neq x y)))
              nil
              (dom-get doms vj)))
            di)))
      (cond ((< (length new-di) (length di))
             (dom-set doms vi new-di))
            (t doms)))))

; AC-3: iterate arc queue until no changes
(define (ac3 doms queue)
  (cond ((eq queue nil) doms)
        (t (let* ((arc (car queue))
                  (vi (car arc))
                  (vj (cadr arc))
                  (rest (cdr queue))
                  (new-doms (revise doms vi vj)))
          (cond ((< (length (dom-get new-doms vi))
                    (length (dom-get doms vi)))
                 ; Domain shrank, re-add affected arcs
                 (let ((new-queue
                    (append rest
                      (map (lambda (vk) (list vk vi))
                        (filter (lambda (vk)
                          (and (not (eq vk vj))
                               (not (eq vk vi))))
                          variables)))))
                   (ac3 new-doms new-queue)))
                (t (ac3 new-doms rest)))))))

; ============================================================ ; Before and after AC-3 ; ============================================================
(print "--- Domains BEFORE AC-3 ---")
(define doms-before (init-doms))
(map (lambda (v)
  (print (list v (quote ->) (dom-get doms-before v)))
  nil)
  variables)

(print "--- Running AC-3 ---")
(define doms-after (ac3 doms-before arcs))
(print "--- Domains AFTER AC-3 ---")
(map (lambda (v)
  (print (list v (quote ->) (dom-get doms-after v)))
  nil)
  variables)

; Verify domains shrunk or are consistent
; With two colours and only "not-equal" constraints every value keeps a
; support, so AC-3 is a no-op here: all three domains stay {red, blue}.
(check "ac3-keeps-A-full"
  (equal (dom-get doms-after (quote A)) colors))
(check "ac3-keeps-B-full"
  (equal (dom-get doms-after (quote B)) colors))
(check "ac3-keeps-C-full"
  (equal (dom-get doms-after (quote C)) colors))

; ============================================================ ; Backtracking with MRV ; ============================================================
(print "--- Backtracking with MRV ---")

; model: alist (var . value)
(define (model-lookup m var)
  (cond ((eq m nil) nil)
        ((eq (caar m) var) (cdar m))
        (t (model-lookup (cdr m) var))))

; Check if val for var is consistent with current partial assignment
(define (consistent m var val)
  (foldl (lambda (acc a)
    (cond ((not acc) nil)
          ((eq (car a) var)
            (let ((other-val (model-lookup m (cadr a))))
              (cond ((eq other-val nil) t)
                    (t (neq val other-val)))))
          ((eq (cadr a) var)
            (let ((other-val (model-lookup m (car a))))
              (cond ((eq other-val nil) t)
                    (t (neq other-val val)))))
          (t acc)))
    t
    arcs))

; MRV: choose unassigned variable with smallest domain
(define (mrv m doms)
  (let ((unassigned (filter (lambda (v)
          (eq (model-lookup m v) nil)) variables)))
    (cond ((eq unassigned nil) nil)
          (t (foldl (lambda (best v)
            (cond ((eq best nil) v)
                  ((< (length (dom-get doms v))
                      (length (dom-get doms best)))
                   v)
                  (t best)))
            nil unassigned)))))

; Backtrack search
(define (backtrack-solve m doms)
  (let ((v (mrv m doms)))
    (cond ((eq v nil) (list m))
          (t (foldl (lambda (sols val)
              (cond ((consistent m v val)
                (let ((nm (cons (cons v val) m)))
                  (append sols (backtrack-solve nm doms))))
                (t sols)))
              nil
              (dom-get doms v))))))

(define all-solutions (backtrack-solve nil doms-after))
(print "Number of solutions:")
(print (length all-solutions))
(check "ac3-has-2-solutions" (= (length all-solutions) 2))

; Print solutions
(map (lambda (sol)
  (print (list (quote solution)
    (list (model-lookup sol (quote A))
          (model-lookup sol (quote B))
          (model-lookup sol (quote C)))))
  nil)
  all-solutions)

; Verify each solution satisfies all constraints
(define (verify-solution sol)
  (and (neq (model-lookup sol (quote A)) (model-lookup sol (quote B)))
       (neq (model-lookup sol (quote B)) (model-lookup sol (quote C)))))

(check "solution-1-valid" (verify-solution (car all-solutions)))
(check "solution-2-valid"
  (verify-solution (car (cdr all-solutions))))

(print "=== Constraint Propagation Done ===")
