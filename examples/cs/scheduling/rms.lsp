; Rate-monotonic scheduling (RMS) — Liu-Layland.
; n tasks with period Ti, compute time Ci.
; Schedulability test: sum(Ci/Ti) <= n(2^(1/n)-1).
; Priority: shorter period = higher priority.
; Simulate 3 tasks, check deadlines met, detect missed deadlines.
; Demonstrate priority inversion with a lock.

(define (rms-utilization tasks)
  (foldl (lambda (sum tk) (+ sum (/ (car tk) (cadr tk)))) 0 tasks))

(define (rms-bound n)
  (* n (- (exp (* (/ 1 n) (log 2))) 1)))

(define (rms-schedulable-p tasks)
  (<= (rms-utilization tasks) (rms-bound (length tasks))))

(define (rms-sort tasks)
  (sort (lambda (a b) (< (cadr a) (cadr b))) tasks))

; Each job: (id remaining-time deadline period release-time); id is a
; unique task index (the old code used the compute time as id,
; colliding for equal-compute tasks)
(define (job-id j) (car j))
(define (job-rem j) (cadr j))
(define (job-deadline j) (caddr j))
(define (job-period j) (cadddr j))
(define (job-release j) (nth 4 j))

(define (rms-ready pending time)
  (filter (lambda (j) (<= (job-release j) time)) pending))

; rate-monotonic: shortest period = highest priority (the old code
; sorted by remaining time, which is not RM at all)
(define (rms-highest ready)
  (cond ((null ready) nil)
        (t (car (sort (lambda (a b) (< (job-period a) (job-period b)))
                      ready)))))

(define (rms-release tasks time)
  (rms-release-walk tasks time 0 nil))

(define (rms-release-walk tasks time idx acc)
  (cond ((null tasks) acc)
        ((= (mod time (cadar tasks)) 0)
         (rms-release-walk (cdr tasks) time (+ idx 1)
                           (cons (list idx (caar tasks)
                                       (+ time (cadar tasks))
                                       (cadar tasks)
                                       time)
                                 acc)))
        (t (rms-release-walk (cdr tasks) time (+ idx 1) acc))))

(define (rms-missed pending time)
  (filter (lambda (j) (and (> time (job-deadline j)) (> (job-rem j) 0))) pending))

(define (rms-remove-missed pending time)
  (filter (lambda (j) (not (and (> time (job-deadline j)) (> (job-rem j) 0)))) pending))

(define (rms-execute pending job)
  (map (lambda (j) (cond ((equal (job-id j) (job-id job))
                          (list (job-id j) (- (job-rem j) 1)
                                (job-deadline j) (job-period j)
                                (job-release j)))
                         (t j)))
       pending))

(define (rms-remove-completed pending job)
  (filter (lambda (j) (not (equal (job-id j) (job-id job)))) pending))

(define (rms-job-rem-after pending job)
  (let ((found (find (lambda (j) (equal (job-id j) (job-id job))) pending)))
    (cond (found (job-rem found))
          (t 0))))

(define (rms-step tasks pending log time limit)
  (cond ((>= time limit) (reverse log))
        (t (rms-tick tasks pending log time limit))))

(define (rms-tick tasks pending log time limit)
  (let ((new-jobs (rms-release tasks time)))
    (let ((all-pending (append pending new-jobs)))
      (let ((missed-jobs (rms-missed all-pending time)))
        (let ((remaining (rms-remove-missed all-pending time)))
          (let ((ready (rms-ready remaining time)))
            (let ((missed-ents (map (lambda (j) (list time (car j) 'missed)) missed-jobs)))
              (cond ((null ready)
                      (rms-step tasks remaining
                        (append log missed-ents (list (list time 'idle 'idle)))
                        (+ time 1) limit))
                    (t (rms-execute-job tasks remaining missed-ents
                         ready log time limit))))))))))

(define (rms-execute-job tasks remaining missed-ents ready log time limit)
  (let ((job (rms-highest ready)))
      (let ((new-rem (rms-execute remaining job)))
      (let ((remaining-time (rms-job-rem-after new-rem job)))
        ; complete only when the execution is fully consumed
        (let ((completed-p (= remaining-time 0)))
          (let ((new-log (append log missed-ents
                           (list (list time (job-id job)
                             (cond (completed-p 'done) (t 'run)))))))
            (rms-step tasks
              (cond (completed-p (rms-remove-completed new-rem job))
                    (t new-rem))
              new-log
              (+ time 1) limit)))))))

(define (rms-simulate tasks timelimit)
  (rms-step (rms-sort tasks) nil nil 0 timelimit))

; -- Demo: 3 tasks --

(define tasks-a '((1 3) (2 5) (1 8)))

(print (list 'tasks-a-u (rms-utilization tasks-a)))
(print (list 'rms-bound-3 (rms-bound 3)))
(print (list 'schedulable-a (rms-schedulable-p tasks-a)))

(define tasks-b '((1 4) (1 5) (1 10)))

(print (list 'tasks-b-u (rms-utilization tasks-b)))
(print (list 'schedulable-b (rms-schedulable-p tasks-b)))

(define sim-b (rms-simulate tasks-b 20))
(print (list 'timeline-b sim-b))

(define (has-missed timeline)
  (any (lambda (entry) (eq (caddr entry) 'missed)) timeline))

(print (list 'has-missed-b (has-missed (rms-simulate tasks-b 30))))

(print 'priority-inversion-demo
  '(task-low-period-8-holds-lock
    task-high-period-3-needs-lock
    task-med-period-5-preempts-low
    high-waits-for-low-blocked-by-med))

; regression checks (the three bugs: compute-time ids, wrong priority
; key, off-by-one completion)
(check "job ids are unique task indices"
       (let ((jobs (rms-release (list (list 1 4) (list 1 4)) 0)))
         (and (= (length jobs) 2)
              (not (equal (job-id (car jobs)) (job-id (cadr jobs)))))))

(check "highest priority is the shortest period"
       (= (job-period
            (rms-highest (list (list 0 1 8 8 0) (list 1 1 5 5 0)
                               (list 2 1 3 3 0))))
          3))

(check "a completed job reports done with zero work left"
       (let ((tl (rms-simulate (list (list 2 100)) 3)))
         (any (lambda (e) (equal e (list 1 0 (quote done)))) tl)))
