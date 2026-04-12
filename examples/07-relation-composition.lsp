; ============================================================
; 07 — Relation Composition & Transitive Closure
; R∘S = {(a,c) : ∃b, (a,b)∈R ∧ (b,c)∈S}
; UNIQUE: relations as native sets of pairs
; ============================================================

(print "=== Relation Composition ===")

(define R '((1 . 2) (2 . 3) (3 . 4)))
(define S '((2 . 5) (3 . 6) (4 . 7)))
(print "R =") (print R)
(print "S =") (print S)

; Compose: for each (a,b) in R, find (b,c) in S
(define (find-in-s b slist)
  (cond ((null slist) nil)
        ((= b (car (car slist))) (cons (cdr (car slist)) (find-in-s b (cdr slist))))
        (t (find-in-s b (cdr slist)))))

(define (compose-one a b slist)
  (let ((targets (find-in-s b slist)))
    (labels (((build tgts)
      (cond ((null tgts) nil)
            (t (cons (cons a (car tgts)) (build (cdr tgts)))))))
      (build targets))))

(define (compose r s)
  (cond ((null r) nil)
        (t (let ((pairs (compose-one (car (car r)) (cdr (car r)) s)))
             (cond ((null pairs) (compose (cdr r) s))
                   (t (labels (((cat-lists a b)
                         (cond ((null a) b)
                               (t (cons (car a) (cat-lists (cdr a) b))))))
                        (cat-lists pairs (compose (cdr r) s)))))))))

(define RS (compose R S))
(print "R∘S =") (print RS)

; --- Transitive closure ---
(print "--- Transitive Closure ---")
(define edges '((1 . 2) (2 . 3) (3 . 4) (4 . 5)))
(print "Edges =") (print edges)

(define (has-pair pairs a b)
  (cond ((null pairs) nil)
        ((and (= (car (car pairs)) a) (= (cdr (car pairs)) b)) t)
        (t (has-pair (cdr pairs) a b))))

(define (merge-unique ex nw)
  (cond ((null nw) ex)
        ((has-pair ex (car (car nw)) (cdr (car nw)))
         (merge-unique ex (cdr nw)))
        (t (merge-unique (cons (car nw) ex) (cdr nw)))))

; Build closure by composing with edges
(define r1 (merge-unique edges (compose edges edges)))
(print "Step 1:") (print (ord r1)) (print "pairs")

(define r2 (merge-unique r1 (compose r1 edges)))
(print "Step 2:") (print (ord r2)) (print "pairs")

(define r3 (merge-unique r2 (compose r2 edges)))
(print "Step 3:") (print (ord r3)) (print "pairs")

(print "--- Reachability ---")
(print "1->5?") (print (has-pair r3 1 5))
(print "1->3?") (print (has-pair r3 1 3))
(print "3->1?") (print (has-pair r3 3 1))
(print "2->5?") (print (has-pair r3 2 5))

(print "=== Relation Composition Done ===")
