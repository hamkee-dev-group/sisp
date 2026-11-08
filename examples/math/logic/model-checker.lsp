; ============================================================
; 68 — A Model Checker: Peterson's Mutual Exclusion, Verified
; The full state space of a two-process concurrent protocol is
; enumerated as a least fixpoint over the transition relation.
; Peterson's algorithm is PROVEN safe (mutual exclusion, no
; deadlock, no starvation region), and a naive check-then-set
; lock is caught violating mutual exclusion with a reachable
; counterexample — exhaustive verification, not testing.
; ============================================================

(load "lib/collections/relations.lsp")

(print "=== Model Checking Peterson's Algorithm ===")


; ---------- protocol as a successor function ----------
; A state is (pc0 pc1 flag0 flag1 turn). Program counters:
;   0 idle  ->  set my flag                    -> 1
;   1       ->  give turn to the other process -> 2
;   2 wait  ->  enter when other-flag=0 or turn=mine -> 3
;   3 critical section -> clear my flag        -> 0

(define (st pc0 pc1 f0 f1 turn) (list pc0 pc1 f0 f1 turn))
(define (pc-of s i) (nth i s))
(define (flag-of s i) (nth (+ 2 i) s))
(define (turn-of s) (nth 4 s))

(define (with-pc s i v)
  (cond ((= i 0) (st v (nth 1 s) (nth 2 s) (nth 3 s) (nth 4 s)))
        (t (st (nth 0 s) v (nth 2 s) (nth 3 s) (nth 4 s)))))
(define (with-flag s i v)
  (cond ((= i 0) (st (nth 0 s) (nth 1 s) v (nth 3 s) (nth 4 s)))
        (t (st (nth 0 s) (nth 1 s) (nth 2 s) v (nth 4 s)))))
(define (with-turn s v)
  (st (nth 0 s) (nth 1 s) (nth 2 s) (nth 3 s) v))

; the moves process i can make from state s (nil when blocked)
(define (peterson-step s i)
  (let ((pc (pc-of s i)) (other (- 1 i)))
    (cond ((= pc 0) (list (with-pc (with-flag s i 1) i 1)))
          ((= pc 1) (list (with-pc (with-turn s other) i 2)))
          ((= pc 2)
            (cond ((or (= (flag-of s other) 0) (= (turn-of s) i))
              (list (with-pc s i 3)))
              (t nil)))                       ; busy-wait: blocked
          (t (list (with-pc (with-flag s i 0) i 0))))))

(define (successors step s)
  (append (step s 0) (step s 1)))

; ---------- reachability as a least fixpoint ----------
(define (reachable step init)
  (lfp (lambda (seen)
    (union seen
      (list-to-set (foldl (lambda (acc s) (append acc (successors step s)))
        nil (set-to-list seen)))))
    init))

(define initial (st 0 0 0 0 0))
(define states (reachable peterson-step (list-to-set (list initial))))
(print (list (quote reachable-states) (length states)))
(check "state-space-is-finite-and-small" (<= (length states) 40))

; ---------- safety: mutual exclusion ----------
(define (both-critical s) (and (= (pc-of s 0) 3) (= (pc-of s 1) 3)))
(check "mutual-exclusion"
  (equal (filter (lambda (s) (both-critical s)) states) {}))

; ---------- no deadlock: every reachable state can move ----------
(check "no-deadlock"
  (every (lambda (s)
    (not (eq (successors peterson-step s) nil))) states))

; ---------- no starvation region (CTL: AG EF critical) ----------
; backward reachability: the states from which process 0 CAN still
; reach its critical section; if that is every reachable state, no
; scheduling decision ever locks process 0 out permanently.
(define (backward-reachable step target)
  (lfp (lambda (seen)
    (union seen
      (filter (lambda (s)
        (any (lambda (n) (in n seen))
          (list-to-set (successors step s))))
        states)))
    target))

(define cs0 (filter (lambda (s) (= (pc-of s 0) 3)) states))
(define cs1 (filter (lambda (s) (= (pc-of s 1) 3)) states))
(check "process-0-reaches-critical-somewhere" (not (equal cs0 {})))
(check "critical-section-always-reachable-p0"
  (equal (backward-reachable peterson-step cs0) states))
(check "critical-section-always-reachable-p1"
  (equal (backward-reachable peterson-step cs1) states))

; ---------- the flag is load-bearing: drop the turn tie-break ----------
; Without the turn variable both processes can block each other:
; flags-only waiting deadlocks when both set flags simultaneously.
(define (flags-only-step s i)
  (let ((pc (pc-of s i)) (other (- 1 i)))
    (cond ((= pc 0) (list (with-pc (with-flag s i 1) i 1)))
          ((= pc 1) (list (with-pc s i 2)))     ; no turn assignment
          ((= pc 2)
            (cond ((= (flag-of s other) 0) (list (with-pc s i 3)))
              (t nil)))
          (t (list (with-pc (with-flag s i 0) i 0))))))
(define flags-states (reachable flags-only-step (list-to-set (list initial))))
(define flags-deadlocks
  (filter (lambda (s) (eq (successors flags-only-step s) nil))
    flags-states))
(print (list (quote flags-only-deadlock-states) flags-deadlocks))
(check "flags-only-lock-deadlocks"
  (not (equal flags-deadlocks {})))

; ---------- a naive check-then-set lock violates safety ----------
; Process i: 0 wait until other flag is 0 -> 1 set my flag -> 2
; critical -> 3 clear. The gap between checking and setting is a
; race; the checker finds a reachable state with both inside.
(define (naive-step s i)
  (let ((pc (pc-of s i)) (other (- 1 i)))
    (cond ((= pc 0)
            (cond ((= (flag-of s other) 0) (list (with-pc s i 1)))
              (t nil)))
          ((= pc 1) (list (with-pc (with-flag s i 1) i 2)))
          ((= pc 2) (list (with-pc s i 3)))     ; critical section
          (t (list (with-pc (with-flag s i 0) i 0))))))
(define naive-states (reachable naive-step (list-to-set (list initial))))
(define naive-bad
  (filter (lambda (s) (and (= (pc-of s 0) 2) (= (pc-of s 1) 2)))
    naive-states))
(print (list (quote naive-violation-states) naive-bad))
(check "naive-lock-violates-mutual-exclusion"
  (not (equal naive-bad {})))

; the checker distinguishes the protocols with the same machinery:
(check "peterson-strictly-safer-than-naive"
  (and (equal (filter (lambda (s) (both-critical s)) states) {})
       (not (equal naive-bad {}))))

(print "=== Model Checking Done ===")
