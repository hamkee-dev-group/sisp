; ============================================================
; METRICS MONITORING + DASHBOARD + ALERTING
;
; Time-series store: ((metric-name (timestamp value) ...) ...)
; Alert rules:        ((name threshold consecutive-intervals) ...)
; Alert state:        ((name triggered-p fire-count) ...)
; ============================================================

(define (mon-ts-make name values) (list name values))
(define (mon-ts-name entry) (car entry))
(define (mon-ts-values entry) (cadr entry))

(define (mon-get store name)
  (find (lambda (e) (equal (mon-ts-name e) name)) store))

(define (mon-set store name values)
  (cons (mon-ts-make name values)
    (remove (lambda (e) (equal (mon-ts-name e) name)) store)))

(define (mon-record store name timestamp value)
  (let ((entry (mon-get store name)))
    (cond ((eq entry nil)
           (mon-set store name (list (list timestamp value))))
          (t (mon-set store name
              (append (mon-ts-values entry) (list (list timestamp value))))))))

(define (mon-latest-value store name)
  (let ((entry (mon-get store name)))
    (cond ((eq entry nil) 0)
          ((eq (mon-ts-values entry) nil) 0)
          (t (cadr (last (mon-ts-values entry)))))))

(define (alert-make name threshold intervals) (list name threshold intervals))
(define (alert-name r) (car r))
(define (alert-threshold r) (cadr r))
(define (alert-intervals r) (caddr r))

(define (ast-make name triggered count) (list name triggered count))
(define (ast-name s) (car s))
(define (ast-triggered s) (cadr s))
(define (ast-count s) (caddr s))

(define (mon-define-alert rules name threshold intervals)
  (cons (alert-make name threshold intervals) rules))

(define (mon-find-state state name)
  (find (lambda (s) (equal (ast-name s) name)) state))

(define (mon-update-state state ast)
  (cons ast (remove (lambda (s) (equal (ast-name s) (ast-name ast))) state)))

(define (mon-check-rule store state rule)
  (let ((name (alert-name rule))
        (threshold (alert-threshold rule))
        (intervals (alert-intervals rule)))
    (let ((value (mon-latest-value store name))
          (ast (mon-find-state state name)))
      (let ((prev (cond ((eq ast nil) (ast-make name nil 0)) (t ast))))
        (cond ((> value threshold)
               (let ((nc (+ (ast-count prev) 1)))
                 (ast-make name (>= nc intervals) nc)))
              (t (ast-make name nil 0)))))))

(define (mon-check-all store rules state)
  (foldl (lambda (st rule) (mon-update-state st (mon-check-rule store st rule)))
         state rules))

(define (mon-collect-cpu i) (+ 30 (mod (* 7 i) 60)))
(define (mon-collect-mem i) (+ 40 (mod (* 3 i) 30)))
(define (mon-collect-disk i) (+ 50 (mod (* 2 i) 20)))
(define (mon-collect-procs i) (+ 50 (mod (* 5 i) 50) (mod (* 13 i) 10)))

(define (spaces n)
  (cond ((<= n 0) "")
        (t (cat " " (spaces (- n 1))))))

(define (pad-left s width)
  (let ((n (length s)))
    (cond ((>= n width) s)
          (t (cat (spaces (- width n)) s)))))

(define (pad-right s width)
  (let ((n (length s)))
    (cond ((>= n width) s)
          (t (cat s (spaces (- width n)))))))

(define (metric-label name)
  (cond ((equal name "cpu") "CPU %")
        ((equal name "mem") "Memory %")
        ((equal name "disk") "Disk %")
        ((equal name "procs") "Processes")
        (t name)))

(define (fmt-value v name)
  (cond ((equal name "procs") (num-to-str v))
        (t (cat (num-to-str v) "%"))))

(define (mon-display-metric store state name)
  (let ((v (mon-latest-value store name))
        (ast (mon-find-state state name)))
    (let ((status (cond ((eq ast nil) "OK")
                        ((ast-triggered ast) "ALERT!")
                        (t (cat "WARN(" (cat (num-to-str (ast-count ast)) ")"))))))
      (print (cat (cat (pad-right (metric-label name) 14) " ")
              (cat (pad-left (fmt-value v name) 10) (cat "  " status)))))))

(define (mon-dashboard store state)
  (print "")
  (print "==========================================")
  (print "  SYSTEM MONITORING DASHBOARD")
  (print "  METRIC         VALUE       STATUS")
  (print "  -------------- ----------  ----------")
  (mon-display-metric store state "cpu")
  (mon-display-metric store state "mem")
  (mon-display-metric store state "disk")
  (mon-display-metric store state "procs")
  (print "=========================================="))

(print "==========================================")
(print "  METRICS MONITORING + ALERT SYSTEM")
(print "==========================================")

(define store nil)
(define rules nil)
(define alert-state nil)
(define rules (mon-define-alert rules "cpu" 85 3))
(define rules (mon-define-alert rules "mem" 70 2))

(define (sim-interval i store state)
  (let ((cpu-v (mon-collect-cpu i))
        (mem-v (mon-collect-mem i))
        (disk-v (mon-collect-disk i))
        (procs-v (mon-collect-procs i)))
    (let ((s (mon-record (mon-record (mon-record
              (mon-record store "cpu" i cpu-v) "mem" i mem-v)
              "disk" i disk-v) "procs" i procs-v)))
      (let ((newstate (mon-check-all s rules state)))
        (print (cat (cat "[" (cat (num-to-str i) "] "))
                (cat "cpu=" (cat (num-to-str cpu-v)
                (cat (cat "% mem=" (num-to-str mem-v))
                (cat (cat "% disk=" (num-to-str disk-v))
                (cat "% procs=" (num-to-str procs-v))))))))
        (let ((cpu-ast (mon-find-state newstate "cpu")))
          (cond ((and (not (eq cpu-ast nil)) (> (ast-count cpu-ast) 0))
                 (print (cat "  CPU warnings: "
                        (cat (num-to-str (ast-count cpu-ast))
                         (cond ((ast-triggered cpu-ast) " *** ALERT! ***")
                               (t ""))))))))
        (let ((mem-ast (mon-find-state newstate "mem")))
          (cond ((and (not (eq mem-ast nil)) (> (ast-count mem-ast) 0))
                 (print (cat "  MEM warnings: "
                        (cat (num-to-str (ast-count mem-ast))
                         (cond ((ast-triggered mem-ast) " *** ALERT! ***")
                               (t ""))))))))
        (list s newstate)))))

(define (run-sim i max store state)
  (cond ((> i max) (list store state))
        (t (let ((res (sim-interval i store state)))
             (run-sim (+ i 1) max (car res) (cadr res))))))

(define result (run-sim 1 10 store alert-state))
(define final-store (car result))
(define final-state (cadr result))

(mon-dashboard final-store final-state)

(print "")
(print "==========================================")
(print "  Monitoring system: pass")
(print "==========================================")
