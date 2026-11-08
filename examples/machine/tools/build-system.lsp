; Build system: Make-like DAG execution with timestamps and parallel scheduling.
; Rules: (target deps commands...).  Timestamps are simulated counters.
; Builds DAG, topo-sorts, executes out-of-date targets, simulates parallelism.

(define rules
  '((a (b c) a-cmd1 a-cmd2)
    (b (d)   b-cmd1)
    (c ()    c-cmd1 c-cmd2)
    (d ()    d-cmd1)))

(define timestamps nil)
(define built-targets nil)
(define ts-clock 0)

(define (bs-now)
  (prog1 ts-clock (setq ts-clock (+ ts-clock 1))))

(define (bs-reset)
  (setq timestamps
    '((a 1 0) (b 1 0) (c 1 0) (d 1 0)))
  (setq ts-clock 10)
  (setq built-targets nil))

(define (bs-ts-entry name)
  (find (lambda (e) (equal (car e) name)) timestamps))

(define (bs-get-ts name)
  (cadr (bs-ts-entry name)))

(define (bs-get-artifact name)
  (caddr (bs-ts-entry name)))

(define (bs-set-ts name self-time artifact-time)
  (setq timestamps
    (cons (list name self-time artifact-time)
          (remove (lambda (e) (equal (car e) name))
                  timestamps))))

(define (bs-rule name)
  (find (lambda (r) (equal (car r) name)) rules))

(define (bs-deps name)
  (let ((r (bs-rule name)))
    (cond ((eq r nil) nil) (t (cadr r)))))

(define (bs-commands name)
  (let ((r (bs-rule name)))
    (cond ((eq r nil) nil) (t (cddr r)))))

(define (bs-touch name new-ts)
  (bs-set-ts name new-ts (bs-get-artifact name)))

(define (bs-stale-p name)
  (let ((self (bs-get-ts name))
        (art (bs-get-artifact name)))
    (cond ((> self art) t)
          (t (let ((deps (bs-deps name)))
               (any (lambda (d)
                      (> (bs-get-artifact d) art))
                    deps))))))

(define (bs-visit name visited order)
  (cond ((in name visited) (list visited order))
        (t (let* ((deps (bs-deps name))
                  (state (bs-visit-list deps visited order)))
             (list (cons name (car state))
                   (append (cadr state) (list name)))))))

(define (bs-visit-list names visited order)
  (cond ((null names) (list visited order))
        (t (let* ((state (bs-visit (car names) visited order)))
             (bs-visit-list (cdr names) (car state) (cadr state))))))

(define (bs-topo-sort name)
  (cadr (bs-visit name nil nil)))

(define (bs-execute name)
  (cond ((in name built-targets)
         (print (cat "  up-to-date: " (str-name name)))
         nil)
        ((bs-stale-p name)
         (prog1 t
           (print (cat "  building: " (str-name name)))
           (foreach (lambda (cmd)
                      (print (cat "    run: " (cmd-name cmd))))
                    (bs-commands name))
           (let ((now (bs-now)))
             (bs-set-ts name now now))
           (setq built-targets (cons name built-targets))))
        (t (progn
             (print (cat "  up-to-date: " (str-name name)))
             (setq built-targets (cons name built-targets))
             nil))))

(define (bs-build target)
  (let ((order (bs-topo-sort target)))
    (print (cat "Build plan: " (orders-to-str order)))
    (foldl (lambda (built-any name)
             (let ((rebuilt (bs-execute name)))
               (or built-any rebuilt)))
           nil
           order)))

(define (bs-parallel-plan target)
  (let ((order (bs-topo-sort target)))
    (bs-group-parallel order nil)))

(define (bs-group-parallel order done)
  (cond ((null order) nil)
        (t (let ((wave (bs-collect-ready order done)))
             (cond ((null wave) (cons order nil))
                   (t (cons wave
                            (bs-group-parallel
                              (bs-remove-all wave order)
                              (append done wave)))))))))

(define (bs-collect-ready order done)
  (filter (lambda (name)
            (let ((deps (bs-deps name)))
              (every (lambda (d)
                       (in d done))
                     deps)))
          order))

(define (bs-remove-all to-remove lst)
  (filter (lambda (x) (not (in x to-remove))) lst))

(define (bs-build-parallel target)
  (let ((waves (bs-parallel-plan target)))
    (print (cat "Parallel plan: " (waves-to-str waves)))
    (cond ((null waves) nil)
          (t (bs-do-wave (car waves) (cdr waves))))))

(define (bs-do-wave wave rest)
  (setq light-bs-built nil)
  (print (cat "Wave: " (wave-to-str wave)))
  (foreach (lambda (name)
             (bs-execute name))
           wave)
  (cond ((null rest) nil)
        (t (bs-do-wave (car rest) (cdr rest)))))

(define (bs-clean target)
  (let ((order (bs-topo-sort target)))
    (print "Cleaning build artifacts...")
    (foreach (lambda (name)
               (bs-set-ts name 1 0))
             order)
    (setq built-targets nil)
    (setq ts-clock 10)
    (print "Done.")))

(define (str-name sym)
  (cond ((eq sym 'a) "A") ((eq sym 'b) "B") ((eq sym 'c) "C") ((eq sym 'd) "D")
        (t "?")))

(define (cmd-name sym)
  (cond ((eq sym 'a-cmd1) "a-cmd1") ((eq sym 'a-cmd2) "a-cmd2")
        ((eq sym 'b-cmd1) "b-cmd1")
        ((eq sym 'c-cmd1) "c-cmd1") ((eq sym 'c-cmd2) "c-cmd2")
        ((eq sym 'd-cmd1) "d-cmd1")
        (t "?")))

(define (orders-to-str lst)
  (cond ((null lst) "")
        ((null (cdr lst)) (str-name (car lst)))
        (t (cat (str-name (car lst))
                (cat " -> "
                     (orders-to-str (cdr lst)))))))

(define (wave-to-str wave)
  (cond ((null wave) "[]")
        ((null (cdr wave)) (str-name (car wave)))
        (t (cat (str-name (car wave))
                (cat "," (wave-to-str (cdr wave)))))))

(define (waves-to-str waves)
  (cond ((null waves) "")
        (t (cat (cat "[" (wave-to-str (car waves)))
                (cat "] "
                     (waves-to-str (cdr waves)))))))

; ---- demonstration ----
(print "=== BUILD SYSTEM ===")

(bs-reset)

(print "")
(print "-- Initial build of A (all targets stale) --")
(bs-build 'a)

(print "")
(print "-- Rebuild A (nothing stale): --")
(setq built-targets nil)
(bs-build 'a)

(print "")
(print "-- Touch D (source changed to 100): --")
(bs-touch 'd 100)

(print "")
(print "-- Rebuild A (only D and dependents rebuilt): --")
(setq built-targets nil)
(bs-build 'a)

(print "")
(print "-- Clean --")
(bs-clean 'a)

(print "")
(print "-- Parallel build of A: --")
(bs-build-parallel 'a)
