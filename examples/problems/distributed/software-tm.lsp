; Optimistic Software Transactional Memory. Read/write sets, validate, commit.
; Atomic {x++, y--} maintaining x+y=0 invariant.
(print "=== Software Transactional Memory ===")
(seed 1)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

; Global committed state: alist of (var . value)
(define *db* (list (list 'x 10) (list 'y -10)))

(define (read-var var)
  (let ((entry (assoc var *db*)))
    (cond ((null entry) 0) (t (cadr entry)))))

(define (write-var var val)
  (let ((entry (assoc var *db*)))
    (cond ((null entry) (progn (setq *db* (cons (list var val) *db*)) val))
          (t (progn (set-car (cdr entry) val) val)))))

; A transaction: try action with read/write logging, validate then commit.
(define (run-txn action-fn max-retries)
  (try-txn action-fn max-retries))

(define (try-txn action-fn retries)
  (cond ((<= retries 0) (list nil nil))
        (t
         ; NOTE: the logs must live in mutable boxes. `setq` on a `let` binding
         ; writes the GLOBAL of that name and leaves the lexical binding alone,
         ; so a plain `(setq read-set ...)` here silently logged nothing — the
         ; write set stayed empty and every transaction committed no writes.
         (let ((rbox (list nil)) (wbox (list nil)))
           (define (txn-read var)
             (let ((val (read-var var)))
               (set-car rbox (cons (list var val) (car rbox)))
               val))
           (define (txn-write var val)
             (set-car wbox (cons (list var val) (car wbox)))
             val)
           (let ((result (action-fn txn-read txn-write)))
             (let ((valid (validate-reads (car rbox) 0)))
               (cond (valid
                      (commit-writes (reverse (car wbox)) 0)
                      (list t result))
                     (t (try-txn action-fn (- retries 1))))))))))

(define (validate-reads reads i)
  (cond ((null reads) t)
        ((= (cadr (car reads)) (read-var (caar reads)))
         (validate-reads (cdr reads) (+ i 1)))
        (t nil)))

(define (commit-writes writes i)
  (cond ((null writes) nil)
        (t (progn
             (write-var (caar writes) (cadar writes))
             (commit-writes (cdr writes) (+ i 1))))))

; --- Test: atomic {x++, y--} maintaining x+y=0 ---
(print "Initial state: x=" (read-var 'x) ", y=" (read-var 'y))

(define (inc-x-dec-y read-fn write-fn)
  (let ((xv (read-fn 'x)) (yv (read-fn 'y)))
    (write-fn 'x (+ xv 1))
    (write-fn 'y (- yv 1))
    (list 'new-x (+ xv 1) 'new-y (- yv 1))))

(define r (run-txn inc-x-dec-y 10))
(print (list 'txn-result r))
(print "After txn: x=" (read-var 'x) ", y=" (read-var 'y))

(ck "txn-committed" (car r))
(ck "x-plus-y-zero" (= (+ (read-var 'x) (read-var 'y)) 0))

; Run multiple transactions
(foreach (lambda (i) (run-txn inc-x-dec-y 10)) (seq 1 5))
(print "After 5 more txns: x=" (read-var 'x) ", y=" (read-var 'y))
(ck "invariant-maintained-multi" (= (+ (read-var 'x) (read-var 'y)) 0))

; A transaction must actually move the committed state, not just report a result.
(ck "writes-actually-committed" (= (read-var 'x) 16))
(ck "y-actually-committed" (= (read-var 'y) -16))

; --- Conflict detection: a concurrent writer invalidates our read set ---
(setq *db* (list (list 'x 0) (list 'y 0)))
(define *attempts* 0)
(define *interfered* nil)

; Reads x, then on the FIRST attempt only, an "other thread" commits x=99
; underneath us. Validation must catch the stale read and force a retry; the
; retry re-reads 99 and derives y from it.
(define (racy-txn read-fn write-fn)
  (setq *attempts* (+ *attempts* 1))
  (let ((xv (read-fn 'x)))
    (cond ((null *interfered*)
           (setq *interfered* t)
           (write-var 'x 99))
          (t nil))
    (write-fn 'y (+ xv 1))
    xv))

(define rc (run-txn racy-txn 10))
(print (list 'racy-attempts *attempts* 'x (read-var 'x) 'y (read-var 'y)))
(ck "conflict-detection-works" (and (= *attempts* 2) (equal (car rc) t)))
(ck "conflict-retry-sees-new-value" (= (read-var 'y) 100))
(ck "conflicting-write-not-clobbered" (= (read-var 'x) 99))

; With no retries left, a conflicting transaction must abort rather than commit.
(setq *db* (list (list 'x 0) (list 'y 0)))
(setq *interfered* nil)
(setq *attempts* 0)
(define ra (run-txn racy-txn 1))
(ck "conflict-aborts-when-retries-exhausted" (null (car ra)))
(ck "aborted-txn-left-no-write" (= (read-var 'y) 0))

(print "=== Software TM Done ===")
