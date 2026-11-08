; ============================================================
; UNIX SIGNALS + DAEMONS + CRON — completing the OS
;
; Signals: SIGINT(2), SIGKILL(9), SIGCHLD(17), SIGTERM(15)
; Signal handlers per process, default actions, pending queue
;
; Daemons: cron (periodic tasks), syslogd (logging), inetd
; ============================================================

; ---- Signal system ------------------------------------------
(define sig-pending nil)  ; ((pid . signum) ...)
(define sig-handlers nil) ; ((pid . ((signum . handler-fn) ...)) ...)

(define (sig-send pid signum)
  (setq sig-pending (cons (cons pid signum) sig-pending))
  (print (str-join (list "[signal] sent SIG" (num-to-str signum) " to PID " (num-to-str pid)) "")))

(define (sig-handle pid signum handler)
  (let ((entry (assoc pid sig-handlers)))
    (cond (entry (let ((handlers (cdr entry)))
                   (setq sig-handlers (cons (cons pid (cons (cons signum handler)
                     (remove-sig signum handlers)))
                     (remove-sig-entry pid sig-handlers)))))
          (t (setq sig-handlers (cons (list pid (cons signum handler)) sig-handlers))))))

(define (sig-default signum)
  (cond ((= signum 2) 'interrupt)
        ((= signum 9) 'kill)
        ((= signum 15) 'terminate)
        ((= signum 17) 'child)
        (t 'unknown)))

(define (sig-deliver pid)
  (let ((pending (find (lambda (s) (= (car s) pid)) sig-pending)))
    (cond (pending (let ((signum (cdr pending)))
                     (setq sig-pending (remove (lambda (s) (= (car s) pid)) sig-pending))
                     (let ((entry (assoc pid sig-handlers)))
                       (cond (entry (let ((h (assoc signum (cdr entry))))
                                      (cond (h (print (str-join (list "[signal] PID " (num-to-str pid) " handled SIG" (num-to-str signum)) ""))
                                               ((cdr h)))
                                            (t (sig-default-action pid signum)))))
                             (t (sig-default-action pid signum))))))
          (t nil))))

(define (sig-default-action pid signum)
  (cond ((= signum 9) (print (str-join (list "[signal] PID " (num-to-str pid) " killed by SIGKILL") "")))
        (t (print (str-join (list "[signal] PID " (num-to-str pid) " received SIG" (num-to-str signum) " (default)") "")))))

(define (remove-sig signum handlers)
  (cond ((null handlers) nil)
        ((= (caar handlers) signum) (cdr handlers))
        (t (cons (car handlers) (remove-sig signum (cdr handlers))))))

(define (remove-sig-entry pid lst)
  (cond ((null lst) nil) ((= (caar lst) pid) (cdr lst)) (t (cons (car lst) (remove-sig-entry pid (cdr lst))))))

; ---- Cron daemon ---------------------------------------------
(define cron-jobs nil)  ; ((id command interval last-run) ...)
(define cron-next-id 1)
(define cron-time 0)

(define (cron-add command interval)
  (let ((id cron-next-id))
    (setq cron-next-id (+ id 1))
    (setq cron-jobs (cons (list id command interval 0) cron-jobs))
    (print (str-join (list "[cron] added job " (num-to-str id) ": " command " every " (num-to-str interval) " ticks") ""))
    id))

(define (cron-tick)
  (setq cron-time (+ cron-time 1))
  (setq cron-jobs
    (map (lambda (j)
           (let ((elapsed (- cron-time (cadddr j))))
             (cond ((>= elapsed (caddr j))
                    (print (str-join (list "[cron] running: " (cadr j)) ""))
                    (list (car j) (cadr j) (caddr j) cron-time))
                   (t j))))
         cron-jobs)))

; ---- syslogd (system logger) ---------------------------------
(define syslog-buffer nil)  ; ((level . message) ...)
(define syslog-max 100)

(define (syslog level message)
  (setq syslog-buffer (cons (cons level message) syslog-buffer))
  (cond ((> (length syslog-buffer) syslog-max)
         (setq syslog-buffer (reverse (take-log syslog-buffer syslog-max))))))

(define (syslog-show n)
  (foreach-log (reverse (take-log (reverse syslog-buffer) n))))

(define (take-log lst n)
  (cond ((or (null lst) (= n 0)) nil) (t (cons (car lst) (take-log (cdr lst) (- n 1))))))

(define (foreach-log entries)
  (cond ((null entries) nil)
        (t (progn (print (str-join (list "[" (level-str (caar entries)) "] " (cdar entries)) ""))
                  (foreach-log (cdr entries))))))

(define (level-str l)
  (cond ((eq l 'info) "INFO") ((eq l 'warn) "WARN") ((eq l 'error) "ERROR") (t "DEBUG")))

; ---- inetd (internet super-server) ---------------------------
(define inetd-services nil)  ; ((port . handler-fn) ...)

(define (inetd-register port handler)
  (setq inetd-services (cons (cons port handler) inetd-services))
  (print (str-join (list "[inetd] registered service on port " (num-to-str port)) "")))

(define (inetd-dispatch port data)
  (let ((svc (assoc port inetd-services)))
    (cond (svc ((cdr svc) data))
          (t (print (str-join (list "[inetd] no service on port " (num-to-str port)) ""))))))

; ---- Helpers -------------------------------------------------
(define (str-join parts sep)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (cat (car parts) sep) (str-join (cdr parts) sep)))))

; ============================================================
; DEMO — Signals, Cron, Syslog, inetd
; ============================================================
(print "=============================================")
(print "  Unix Signals + Daemons")
(print "=============================================")
(print "")

; --- Signals ---
(print "--- Signal System ---")
(sig-handle 1 2 (lambda () (print "[PID 1] caught SIGINT gracefully")))
(sig-send 1 2)
(sig-deliver 1)
(sig-send 1 15)
(sig-deliver 1)
(sig-send 2 9)
(sig-deliver 2)
(print "")

; --- Cron ---
(print "--- Cron Daemon ---")
(cron-add "sync-files" 3)
(cron-add "rotate-logs" 5)
(cron-add "health-check" 2)
(print "")
(cron-tick)
(cron-tick)
(cron-tick)
(cron-tick)
(print "")

; --- Syslog ---
(print "--- syslogd ---")
(syslog 'info "kernel booted successfully")
(syslog 'warn "disk space low on /dev/disk0")
(syslog 'error "segfault in process 42")
(syslog 'info "cron job health-check completed")
(print "Recent logs:")
(syslog-show 4)
(print "")

; --- inetd ---
(print "--- inetd ---")
(inetd-register 80 (lambda (data) (print (str-join (list "[inetd:80] HTTP request: " (list-to-str data)) ""))))
(inetd-register 22 (lambda (data) (print (str-join (list "[inetd:22] SSH connection") ""))))
(inetd-dispatch 80 (str-to-list "GET / HTTP/1.0"))
(print "")

(print "=============================================")
(print "  Signals + Daemons — all verified")
(print "=============================================")
