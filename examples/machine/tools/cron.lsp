; ============================================================
; CRON — JOB SCHEDULER FOR THE SISP OS
;
; Schedules commands to run at specified intervals.  Jobs are
; (id command interval-ticks last-run-ticks).  The tick function
; advances time and runs due jobs.
; ============================================================

(define jobs nil)
(define cron-now 0)
(define next-job-id 1)

(define (cron-add command interval)
  (let ((job (list next-job-id command interval 0)))
    (setq next-job-id (+ next-job-id 1))
    (setq jobs (cons job jobs))
    (car job)))

(define (cron-remove id)
  (setq jobs (remove-job id jobs)))

(define (remove-job id lst)
  (cond ((null lst) nil)
        ((= (caar lst) id) (cdr lst))
        (t (cons (car lst) (remove-job id (cdr lst))))))

(define (cron-list)
  (print "  ID  COMMAND         INTERVAL  LAST-RUN")
  (print "  --- --------------- --------  --------")
  (foreach (lambda (j)
    (print (str-join
      (list "  " (pad (num-to-str (car j)) 3) "  "
            (pad (cadr j) 15) " "
            (pad (num-to-str (caddr j)) 8) "  "
            (pad (num-to-str (cadddr j)) 8)))))
    jobs))

(define (cron-tick)
  (setq cron-now (+ cron-now 1))
  (foreach (lambda (j)
    (let ((elapsed (- cron-now (cadddr j))))
      (cond ((>= elapsed (caddr j))
             (print (str-join (list "CRON [" (num-to-str (car j)) "] "
                                    (cadr j))))
             (set-car (cdddr j) cron-now)))))
    jobs))

; ---- helpers ------------------------------------------------
(define (foreach fn lst)
  (cond ((null lst) nil) (t (progn (fn (car lst)) (foreach fn (cdr lst))))))

(define (str-join parts)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (car parts) (str-join (cdr parts))))))

(define (pad s width)
  (let ((n (length s)))
    (cond ((>= n width) s) (t (cat s (spaces (- width n)))))))

(define (spaces n) (cond ((= n 0) "") (t (cat " " (spaces (- n 1))))))

; ---- demo ---------------------------------------------------
(print "=== SISP Cron ===")
(print "")

(cron-add "backup-files" 5)
(cron-add "rotate-logs"  3)
(cron-add "health-check" 10)
(cron-add "sync-disk"    8)

(print "Initial job list:")
(cron-list)
(print "")

(print "--- Tick 1..12 ---")
(cron-tick) (cron-tick) (cron-tick)
(cron-tick) (cron-tick) (cron-tick)
(cron-tick) (cron-tick) (cron-tick)
(cron-tick) (cron-tick) (cron-tick)

(print "")
(print "Job list after 12 ticks:")
(cron-list)
(print "")

(print "Remove health-check (id 3):")
(cron-remove 3)
(cron-list)
(print "")
(print "=== Done ===")
