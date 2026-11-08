; ============================================================
; name: hopcroft-karp
; file: algorithms/graphs/hopcroft-karp.lsp
; subject: graph
; capability: matching optimize
; aliases: "hopcroft-karp" "maximum bipartite matching" "hopcroft karp"
; signature: (hopcroft-karp) -> matching, over the globals *L* *R* *E*
; complexity: O(E sqrt V)
; see-also: bipartite-matching maximum-flow
; derived-from: Hopcroft & Karp 1973
; status: example (tested)
; checks: 7 - exhaustive cross-check against brute force on 4x4 and 3x3 graphs
; ============================================================
; Hopcroft-Karp — Maximum Bipartite Matching O(E√V)
; Left={1,2,3,4}, Right={a,b,c,d}
; Edges: (1 a) (1 b) (2 b) (2 c) (3 c) (3 d) (4 d) (4 a)
; Compare with exhaustive enumeration on small graphs (4x4 and 3x3).
; ============================================================

(print "=== Hopcroft-Karp Maximum Bipartite Matching ===")


; ---------- alist helpers ----------
(define (alist-get al key)
  (cond ((eq al nil) nil)
        ((equal (caar al) key) (cdar al))
        (t (alist-get (cdr al) key))))

(define (alist-set al key val)
  (cons (cons key val) al))

; ---------- graph data ----------
(setq *L* (list 1 2 3 4))
(setq *R* (list (quote a) (quote b) (quote c) (quote d)))
(setq *E* (list (list 1 (quote a)) (list 1 (quote b))
                (list 2 (quote b)) (list 2 (quote c))
                (list 3 (quote c)) (list 3 (quote d))
                (list 4 (quote d)) (list 4 (quote a))))

(define (left-p v) (contains v *L*))

(define (edge-in-E-p u v)
  (contains (list u v) *E*))

; ---------- matching helpers ----------
(define (mate-u u matching)
  (cond ((eq matching nil) nil)
        ((= (caar matching) u) (cadar matching))
        (t (mate-u u (cdr matching)))))

(define (mate-v v matching)
  (cond ((eq matching nil) nil)
        ((equal (cadar matching) v) (caar matching))
        (t (mate-v v (cdr matching)))))

; ---------- Hopcroft-Karp BFS ----------
; Returns t if augmenting paths exist, nil otherwise
; Sets *dist* and *queue* as globals

(define (hk-bfs matching)
  (setq *dist* nil)
  (setq *bfs-queue* nil)
  (let ((free-u (filter (lambda (u) (eq (mate-u u matching) nil)) *L*)))
    (cond ((eq free-u nil) nil)
          (t (foreach (lambda (u)
               (setq *dist* (alist-set *dist* u 0))
               (setq *bfs-queue* (append *bfs-queue* (list u))))
               free-u)
             (labels (((bfs-loop)
               (cond ((eq *bfs-queue* nil) nil)
                     (t (let ((u (car *bfs-queue*)))
                       (setq *bfs-queue* (cdr *bfs-queue*))
                       (let ((du (alist-get *dist* u)))
                         (cond ((left-p u)
                                (foreach (lambda (v)
                                  (cond ((and (edge-in-E-p u v)
                                              (eq (alist-get *dist* v) nil))
                                         (setq *dist* (alist-set *dist* v (+ du 1)))
                                         (let ((mu (mate-v v matching)))
                                           (cond ((eq mu nil) nil)
                                                 (t (setq *dist* (alist-set *dist* mu (+ du 2)))
                                                    (setq *bfs-queue* (append *bfs-queue* (list mu)))))))))
                                  (filter (lambda (v) (edge-in-E-p u v)) *R*))
                                (bfs-loop))
                               (t (bfs-loop)))))))))
               (bfs-loop))
             ; Check if any free right vertex was reached
             (foldl (lambda (acc v)
               (cond (acc t)
                     ((and (eq (mate-v v matching) nil)
                           (not (eq (alist-get *dist* v) nil)))
                      t)
                     (t nil)))
               nil *R*)))))

; ---------- Hopcroft-Karp DFS ----------
; Try to augment along layered paths for all free left vertices
; Returns new matching

(define (hk-dfs matching)
  (let ((free-u (filter (lambda (u) (eq (mate-u u matching) nil)) *L*)))
    (labels (((try-from u cur-m)
      (let ((du (alist-get *dist* u)))
        (cond ((eq du nil) cur-m)
              (t (foldl (lambda (acc-each v)
                   (cond ((not (eq acc-each cur-m)) acc-each)
                         ((and (edge-in-E-p u v)
                               (not (eq (alist-get *dist* v) nil))
                               (= (alist-get *dist* v) (+ du 1)))
                          (let ((mv (mate-v v acc-each)))
                            (cond ((eq mv nil)
                                   ; v is free: augment
                                   (cons (list u v) (remove (lambda (m) (equal (cadr m) v)) acc-each)))
                                  ((and (not (eq (alist-get *dist* mv) nil))
                                        (= (alist-get *dist* mv) (+ du 2)))
                                   (let ((new-m (try-from mv acc-each)))
                                     (cond ((eq new-m acc-each) acc-each)
                                           (t (cons (list u v) (remove (lambda (m) (equal (cadr m) v)) new-m))))))
                                  (t acc-each))))
                         (t acc-each)))
                   cur-m *R*))))))
      (foldl (lambda (acc u)
        (cond ((not (eq (mate-u u acc) nil)) acc)
              (t (try-from u acc))))
        matching free-u))))

; ---------- Hopcroft-Karp main loop ----------
(define (hopcroft-karp)
  (labels (((loop m)
    (let ((found (hk-bfs m)))
      (cond (found (loop (hk-dfs m)))
            (t m)))))
    (loop nil)))

; ---------- run ----------
(define matching (hopcroft-karp))

(print "Maximum matching (Hopcroft-Karp):")
(print matching)
(define match-size (cond ((eq matching nil) 0) (t (length matching))))
(print "Matching size:")
(print match-size)

; ---------- verification ----------
(define (valid-matching-p m)
  (and (every (lambda (e) (edge-in-E-p (car e) (cadr e))) m)
       (= (length m) (length (dedup (map car m))))
       (= (length m) (length (dedup (map cadr m))))))

(check "hk-matching-valid" (valid-matching-p matching))

; ---------- exhaustive verification ----------
(define (all-subsets lst)
  (cond ((eq lst nil) (list nil))
        (t (let ((r (all-subsets (cdr lst))))
          (append r (map (lambda (s) (cons (car lst) s)) r))))))

(define all-edge-subs (all-subsets *E*))
(define valid-subs (filter valid-matching-p all-edge-subs))
(define exh-max (foldl (lambda (a m) (let ((s (length m))) (cond ((> s a) s) (t a)))) 0 valid-subs))

(print "Exhaustive max matching size:")
(print exh-max)

(check "hk-matches-exhaustive" (= match-size exh-max))
; L={1,2,3,4}, R={a,b,c,d} with edges forming the 8-cycle
; 1-a-4-d-3-c-2-b-1, which has a perfect matching of size 4
(check "hk-matching-is-perfect" (= match-size 4))
(check "exhaustive-max-is-4" (= exh-max 4))

; ---------- second test on smaller graph ----------
(print "--- Small graph test (3x3) ---")
(setq *L* (list 1 2 3))
(setq *R* (list (quote a) (quote b) (quote c)))
(setq *E* (list (list 1 (quote a)) (list 1 (quote b))
                (list 2 (quote a)) (list 2 (quote c))
                (list 3 (quote b)) (list 3 (quote c))))

(define hk-small (hopcroft-karp))
(print "Small graph matching:")
(print hk-small)

(define all-sml-subs (all-subsets *E*))
(define valid-sml (filter valid-matching-p all-sml-subs))
(define exh-sml-max (foldl (lambda (a m) (let ((s (length m))) (cond ((> s a) s) (t a)))) 0 valid-sml))

(print "Small graph exhaustive max:")
(print exh-sml-max)

(check "hk-small-matches-exhaustive" (= (length hk-small) exh-sml-max))
(check "hk-small-matching-valid" (valid-matching-p hk-small))
; edges form the 6-cycle 1-a-2-c-3-b-1, so the matching is perfect at size 3
(check "hk-small-is-perfect" (= (length hk-small) 3))

(print "=== Hopcroft-Karp Done ===")
