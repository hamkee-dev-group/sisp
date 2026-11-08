; Raft leader election. 5 nodes, timeouts, RequestVote RPC, log commit.
(print "=== Raft Leader Election ===")
(seed 0)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

; Node: (id term voted-for state log commit-index)
(define (mk-node id) (list id 0 -1 0 nil 0))
(define (node-id n) (car n))
(define (node-term n) (cadr n))
(define (node-voted n) (caddr n))
(define (node-state n) (cadddr n))
(define (node-log n) (car (cddddr n)))
(define (node-commit n) (cadr (cddddr n)))

(define (set-term n term) (set-car (cdr n) term))
(define (set-voted n v) (set-car (cddr n) v))
(define (set-state n s) (set-car (cdddr n) s))
(define (set-commit n c) (set-car (cdr (cddddr n)) c))
(define (node-set-log n l) (set-car (cddddr n) l))

; Election: node increments term, votes for self, requests votes
(define (start-election nodes id)
  (let ((n (nth id nodes)))
    (set-term n (+ (node-term n) 1))
    (set-voted n id)
    (set-state n 1)
    (let ((votes (count-votes nodes id (node-term n))))
      (cond ((> votes (/ (length nodes) 2))
             (set-state n 2)
             (print (list 'new-leader id 'term (node-term n))))
            (t nil)))))

(define (count-votes nodes candidate-id term)
  (define (iter ns acc)
    (cond ((null ns) acc)
          (t
           (let ((n (car ns)))
             (cond ((and (>= term (node-term n))
                         (or (= (node-voted n) -1) (= (node-voted n) candidate-id)))
                    (progn
                      (set-voted n candidate-id)
                      (set-term n term)
                      (iter (cdr ns) (+ acc 1))))
                   (t (iter (cdr ns) acc)))))))
  (iter nodes 0))

; Simulate: node 0 starts as leader
(define nodes (map (lambda (i) (mk-node i)) (list 0 1 2 3 4)))
(set-state (nth 0 nodes) 2)
(set-term (nth 0 nodes) 1)
(set-voted (nth 0 nodes) 0)
(print "Node 0 is initial leader (term 1)")

; Append log entries
(node-set-log (nth 0 nodes) (list (list 1 'x 5) (list 1 'y 10) (list 1 'z 15)))

; Replicate: leader sends AppendEntries to followers
(define (replicate-log nodes leader-id)
  (let* ((leader (nth leader-id nodes))
         (leader-term (node-term leader))
         (leader-log (node-log leader))
         (leader-commit (node-commit leader)))
    (foreach (lambda (n)
      (cond ((= (node-id n) leader-id) nil)
            (t (progn
                 (set-term n leader-term)
                 (set-state n 0)
                 (node-set-log n leader-log)
                 (set-commit n leader-commit)))))
      nodes)))

(replicate-log nodes 0)

; Every follower must now hold the leader's exact log, term and commit index.
(define (all-followers-match ns leader)
  (cond ((null ns) t)
        ((= (node-id (car ns)) (node-id leader)) (all-followers-match (cdr ns) leader))
        ((and (equal (node-log (car ns)) (node-log leader))
              (and (= (node-term (car ns)) (node-term leader))
                   (= (node-commit (car ns)) (node-commit leader))))
         (all-followers-match (cdr ns) leader))
        (t nil)))
(ck "replicated-to-all-followers" (all-followers-match nodes (nth 0 nodes)))
(ck "follower-log-has-3-entries" (= (length (node-log (nth 3 nodes))) 3))

; Leader fails
(set-state (nth 0 nodes) 0)
(print "Node 0 fails")

; Node 1 wins election in term 2
(start-election nodes 1)
(ck "leader-changed-on-failure" (= (node-state (nth 1 nodes)) 2))
(ck "node-0-no-longer-leader" (not (= (node-state (nth 0 nodes)) 2)))

; Commit new entry under new leader
(node-set-log (nth 1 nodes) (append (node-log (nth 1 nodes)) (list (list 2 'w 20))))
(replicate-log nodes 1)
(ck "log-continues-under-new-leader" (> (length (node-log (nth 1 nodes))) 0))

; Term monotonicity
(define leader-term-val (node-term (nth 1 nodes)))
(define (all-at-leader-term ns lt)
  (cond ((null ns) t)
        ((= (node-term (car ns)) lt) (all-at-leader-term (cdr ns) lt))
        (t nil)))
(ck "followers-updated-to-leader-term" (all-at-leader-term nodes leader-term-val))

(print "=== Raft Done ===")
