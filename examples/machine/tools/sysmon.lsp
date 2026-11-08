; ============================================================
; SYSTEM MONITOR — INSPECT RUNNING SYSTEM STATE
;
; Provides a "top"-like view of the SISP machine: running
; processes, memory usage, CPU cycles, and uptime.  Designed
; to run as a kernel service on the SISP OS.
;
; Queries:
;   (sys-ps)         — list all processes
;   (sys-meminfo)    — memory usage summary
;   (sys-cpuinfo)    — cycles executed per process
;   (sys-uptime)     — ticks since boot
;   (sys-summary)    — one-page overview
; ============================================================

; ---- global counters ----------------------------------------
(define sys-ticks 0)
(define sys-boot-time (truncate (runtime)))

(define (sys-tick) (setq sys-ticks (+ sys-ticks 1)))

; ---- process information ------------------------------------
; Process: (pid name state cycles ram-used)

(define (make-proc-entry pid name state cycles ram-used)
  (list pid name state cycles ram-used))

(define (proc-entry-pid   p) (car p))
(define (proc-entry-name  p) (cadr p))
(define (proc-entry-state p) (caddr p))
(define (proc-entry-cycles p) (cadddr p))
(define (proc-entry-ram   p) (car (cddddr p)))

(define proc-table nil)

(define (sys-proc-add pid name state ram)
  (setq proc-table (cons (make-proc-entry pid name state 0 ram) proc-table)))

(define (sys-proc-update pid state cycles)
  (setq proc-table
    (map (lambda (p)
           (cond ((= (proc-entry-pid p) pid)
                  (make-proc-entry pid (proc-entry-name p)
                    state cycles (proc-entry-ram p)))
                 (t p)))
         proc-table)))

(define (sys-proc-remove pid)
  (setq proc-table
    (filter (lambda (p) (not (= (proc-entry-pid p) pid)))
            proc-table)))

; ---- monitoring queries -------------------------------------
(define (sys-ps)
  (print "PID  NAME     STATE   CYCLES   RAM")
  (print "---  -------- ------- -------- -----")
  (foreach (lambda (p)
             (print (str-join
               (list (pad-left (num-to-str (proc-entry-pid p)) 3) "  "
                     (pad-right (proc-entry-name p) 8) " "
                     (pad-right (print-sym (proc-entry-state p)) 7) " "
                     (pad-left (num-to-str (proc-entry-cycles p)) 8) " "
                     (pad-left (num-to-str (proc-entry-ram p)) 5)))))
           proc-table))

(define (print-sym s)
  (cond ((eq s 'running) "running")
        ((eq s 'ready) "ready")
        ((eq s 'blocked) "blocked")
        ((eq s 'zombie) "zombie")
        (t "unknown")))

(define (sys-meminfo)
  (let ((total 256)
        (used (foldl (lambda (sum p) (+ sum (proc-entry-ram p)))
                     0 proc-table)))
    (print (str-join (list "Memory: " (num-to-str used) " / " (num-to-str total)
                           " words used (" (num-to-str (truncate (/ (* used 100) total))) "%)")))))

(define (sys-cpuinfo)
  (let ((total (foldl (lambda (sum p) (+ sum (proc-entry-cycles p)))
                      0 proc-table)))
    (print (str-join (list "Total cycles executed: " (num-to-str total))))
    (print (str-join (list "System ticks: " (num-to-str sys-ticks))))))

(define (sys-uptime)
  (let ((elapsed (- (truncate (runtime)) sys-boot-time)))
    (print (str-join (list "Uptime: " (num-to-str elapsed) " seconds")))))

(define (sys-summary)
  (print "=== SYSTEM SUMMARY ===")
  (sys-uptime)
  (sys-meminfo)
  (sys-cpuinfo)
  (print "--- Processes ---")
  (sys-ps))

; ---- string formatting helpers ------------------------------
(define (pad-left s width)
  (let ((n (length s)))
    (cond ((>= n width) s)
          (t (cat (spaces (- width n)) s)))))

(define (pad-right s width)
  (let ((n (length s)))
    (cond ((>= n width) s)
          (t (cat s (spaces (- width n)))))))

(define (spaces n)
  (cond ((= n 0) "")
        (t (cat " " (spaces (- n 1))))))

(define (str-join parts)
  (cond ((null parts) "")
        ((null (cdr parts)) (car parts))
        (t (cat (car parts) (str-join (cdr parts))))))

(define (foreach fn lst)
  (cond ((null lst) nil)
        (t (progn (fn (car lst)) (foreach fn (cdr lst))))))

(define (filter pred lst)
  (cond ((null lst) nil)
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (t (filter pred (cdr lst)))))

; ---- demo ---------------------------------------------------
(sys-proc-add 1 "init" 'running 12)
(sys-proc-add 2 "shell" 'ready 8)
(sys-proc-add 3 "worker" 'blocked 5)
(sys-summary)
