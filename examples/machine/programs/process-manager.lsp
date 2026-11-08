; ============================================================
; PROCESS MANAGER — FORK/EXEC AND IPC FOR THE SISP OS
;
; Extends the OS process model with:
;   fork  — duplicate the current process
;   exec  — replace process image with a new program
;   ipc   — inter-process message passing via kernel mailboxes
;
; Each process gets a unique PID, a parent PID, a state
; (ready/running/blocked/zombie), and a mailbox for messages.
; ============================================================

; ---- process structure: (pid ppid state cpu mailbox) -------
(define (proc-pid     p) (car p))
(define (proc-ppid    p) (cadr p))
(define (proc-state   p) (caddr p))
(define (proc-cpu     p) (cadddr p))
(define (proc-mailbox p) (car (cddddr p)))

(define (make-proc pid ppid state cpu)
  (list pid ppid state cpu nil))

(define (set-proc-state p s)
  (list (proc-pid p) (proc-ppid p) s (proc-cpu p) (proc-mailbox p)))

; ---- process table (alist: pid -> process) ------------------
(define procs nil)   ; global process table
(define next-pid 1)

(define (proc-find pid)
  (let ((e (assoc pid procs)))
    (cond ((eq e nil) nil) (t (cdr e)))))

(define (proc-add p)
  (setq procs (cons (cons (proc-pid p) p) procs))
  p)

(define (proc-remove pid)
  (setq procs (remove-entry pid procs)))

(define (remove-entry key alist)
  (cond ((null alist) nil)
        ((= (caar alist) key) (cdr alist))
        (t (cons (car alist) (remove-entry key (cdr alist))))))

; ---- fork: duplicate the calling process --------------------
; Copies the CPU state (RAM and registers) to a new process.
; Returns (pid . child-pid) — parent gets child's PID, child gets 0.
; (Simplified: both return the child PID for demonstration.)
(define (fork-process parent)
  (let ((child (make-proc next-pid (proc-pid parent) 'ready
                 (copy-cpu (proc-cpu parent)))))
    (setq next-pid (+ next-pid 1))
    (proc-add child)
    (proc-pid child)))

; Shallow copy of a CPU (copy RAM and register matrices).
(define (copy-cpu cpu)
  (let ((old-ram  (car cpu))
        (old-regs (car (cdr cpu))))
    (list old-ram old-regs)))  ; matrices are mutable — real copy needs dup

; ---- exec: replace process image ----------------------------
; Load a new program from the filesystem into the process's RAM.
(define (exec-process p code-words entry-addr)
  (let ((cpu (proc-cpu p)))
    (load-words cpu code-words)
    (wreg cpu 8 entry-addr)   ; set PC
    (wreg cpu 9 0)            ; clear halt flag
    (set-proc-state p 'ready)))

(define (load-words cpu words)
  (labels (((go w a)
      (cond ((null w) a)
            (t (progn (wr cpu a (car w))
                      (go (cdr w) (+ a 1)))))))
    (go words 0)))

(define (wr cpu a v) (mat-set (car cpu) 0 a (mod v 16777216)))
(define (wreg cpu r v) (mat-set (car (cdr cpu)) 0 r (mod v 16777216)))

; ---- IPC: inter-process messaging via mailboxes -------------
; Blocking send and receive (simplified: just append/drain).
(define (ipc-send sender-pid receiver-pid message)
  (let ((receiver (proc-find receiver-pid)))
    (cond ((eq receiver nil)
           (print "ipc-send: receiver not found"))
          (t (let ((new-mbox (append (proc-mailbox receiver)
                                     (list (list sender-pid message)))))
               (setq procs (cons (cons receiver-pid
                     (list (proc-pid receiver) (proc-ppid receiver)
                           (proc-state receiver) (proc-cpu receiver)
                           new-mbox))
                     (remove-entry receiver-pid procs)))
               message)))))

(define (ipc-receive pid)
  (let ((p (proc-find pid)))
    (cond ((eq p nil) nil)
          ((null (proc-mailbox p)) nil)
          (t (let ((msg (car (proc-mailbox p)))
                   (rest (cdr (proc-mailbox p))))
               (setq procs (cons (cons pid
                     (list (proc-pid p) (proc-ppid p)
                           (proc-state p) (proc-cpu p) rest))
                     (remove-entry pid procs)))
               msg)))))

; ---- wait: block until a child terminates -------------------
(define (waitpid child-pid)
  (labels (((poll)
      (let ((child (proc-find child-pid)))
        (cond ((eq child nil) nil)
              ((eq (proc-state child) 'zombie)
               (progn (proc-remove child-pid) child-pid))
              (t (poll))))))
    (poll)))

; ---- demo ---------------------------------------------------
(define demo-cpu (list (make-matrix 1 256) (make-matrix 1 10)))
(define parent (make-proc 1 0 'running demo-cpu))
(proc-add parent)

(define child-pid (fork-process parent))
(ipc-send 1 child-pid "hello from parent")
(ipc-receive child-pid)  ; => (1 "hello from parent")
