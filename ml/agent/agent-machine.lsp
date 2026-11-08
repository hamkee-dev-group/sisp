; ============================================================
; AGENT-MACHINE — connect the SISP agent to the virtual machine
;
; Extends agent.lsp with tools that control the cpu, os,
; filesystem, and shell.  The agent becomes a sysadmin of its
; own virtual computer, able to write programs, compile them
; to machine code, execute on the cpu, manage files, and
; inspect running processes.
;
; New tools:
;   (agent-run FORTH-SRC)     — compile forth source, run on cpu, return result
;   (agent-shell COMMAND)     — execute a shell command on the virtual os
;   (agent-fs-read PATH)      — read file content from virtual filesystem  
;   (agent-fs-write PATH VAL) — write file to virtual filesystem
;   (agent-fs-ls PATH)        — list directory contents
;   (agent-sys-info)          — memory usage and process count
; ============================================================

; ---- CPU (compact, integrated from cpu.lsp) ------------------
(define WORD  16777216) (define MEM 256) (define NREG 11) (define PCI 8) (define HLI 9) (define SP 6)

(define (am-rd m a) (truncate (mat-ref (car m) 0 a)))
(define (am-wr m a v) (mat-set (car m) 0 a (mod v WORD)))
(define (am-gr m r) (truncate (mat-ref (car (cdr m)) 0 r)))
(define (am-sr m r v) (mat-set (car (cdr m)) 0 r (mod v WORD)))
(define (am-mk) (list (make-matrix 1 MEM) (make-matrix 1 NREG)))

(define (am-push m v) (let ((s (- (am-gr m SP) 1))) (am-sr m SP s) (am-wr m s v)))
(define (am-pop  m)   (let ((s (am-gr m SP))) (let ((v (am-rd m s))) (am-sr m SP (+ s 1)) v)))
(define (am-iw op a b) (+ (* op 65536) (* a 256) b))

(define (am-exec m w)
  (let ((op (mod (truncate (/ w 65536)) 256))
        (a  (mod (truncate (/ w 256)) 256))
        (b  (mod w 256)))
    (cond ((= op  0) (am-sr m HLI 1))
          ((= op  1) (am-sr m a b))
          ((= op  2) (am-sr m a (+ (am-gr m a) (am-gr m b))))
          ((= op  3) (am-sr m a (- (am-gr m a) (am-gr m b))))
          ((= op  4) (am-sr m a (* (am-gr m a) (am-gr m b))))
          ((= op 11) (am-gr m a))
          ((= op 14) (am-push m (am-gr m a)))
          ((= op 15) (am-sr m a (am-pop m)))
          (t nil))))

(define (am-run m budget)
  (am-sr m SP 254)
  (labels (((c n) (cond ((= (am-gr m HLI) 1) n) ((= n budget) n)
    (t (let ((p (am-gr m PCI))) (let ((w (am-rd m p))) (am-sr m PCI (+ p 1)) (am-exec m w) (c (+ n 1))))))))
    (c 0)))

(define (am-load m ws base)
  (labels (((go w a) (cond ((null w) a) (t (progn (am-wr m (+ base a) (car w)) (go (cdr w) (+ a 1)))))))
    (go ws 0)))

; ---- Forth compiler (compact) ---------------------------------
(define (am-fc ts out)
  (cond ((null ts) out)
        ((numberp (car ts)) (am-fc (cdr ts) (append out (list (am-iw 1 0 (car ts)) (am-iw 14 0 0)))))
        ((eq (car ts) '+)    (am-fc (cdr ts) (append out (list (am-iw 15 1 0) (am-iw 15 0 0) (am-iw 2 0 1) (am-iw 14 0 0)))))
        ((eq (car ts) '*)    (am-fc (cdr ts) (append out (list (am-iw 15 1 0) (am-iw 15 0 0) (am-iw 4 0 1) (am-iw 14 0 0)))))
        ((eq (car ts) '-)    (am-fc (cdr ts) (append out (list (am-iw 15 1 0) (am-iw 15 0 0) (am-iw 3 0 1) (am-iw 14 0 0)))))
        (t (am-fc (cdr ts) out))))

(define (am-compile tokens)
  (append (am-fc tokens nil) (list (am-iw 0 0 0))))

; ---- Filesystem (compact) -------------------------------------
(define am-root nil)

(define (am-ino-name ino) (car ino))
(define (am-ino-type ino) (cadr ino))
(define (am-ino-body ino) (caddr ino))
(define (am-ino-kids ino) (caddr ino))

(define (am-mk-file name body) (list name 'file body))
(define (am-mk-dir  name kids) (list name 'dir  kids))

(define (am-fs-find dir name)
  (find (lambda (k) (equal (am-ino-name k) name)) (am-ino-kids dir)))

(define (am-fs-ls dir)
  (map (lambda (k) (list (am-ino-name k) (am-ino-type k))) (am-ino-kids dir)))

(define (am-fs-put dir name ino)
  (am-mk-dir (am-ino-name dir)
    (cons ino (remove (lambda (k) (equal (am-ino-name k) name)) (am-ino-kids dir)))))

; ---- Process tracking -----------------------------------------
(define am-procs nil)
(define am-next-pid 1)

(define (am-proc-add name)
  (let ((p (list am-next-pid name 'running)))
    (setq am-next-pid (+ am-next-pid 1))
    (setq am-procs (cons p am-procs))
    (car p)))

; ---- Agent globals (must precede tool definitions) -----------
(define *agent-tool-count* 0)
(define *agent-action-recognized* nil)
(define *agent-action-kind* "none")
(define *agent-observation* "")
(define *agent-done* nil)
(define *agent-last-value* nil)
(define *agent-final-answer* nil)

(define (agent-tool-begin kind)
  (setq *agent-tool-count* (+ *agent-tool-count* 1))
  (cond ((= *agent-tool-count* 1)
    (setq *agent-action-recognized* t)
    (setq *agent-action-kind* kind)
    t)
    (t (setq *agent-action-recognized* nil)
       (setq *agent-action-kind* "multiple")
       (setq *agent-observation* "invalid=>multiple-tools")
       nil)))
(define (am-str-join parts)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (car parts) (am-str-join (cdr parts))))))

(define (am-render val)
  (cond ((eq val nil) "nil")
        ((eq val t) "t")
        ((numberp val) (num-to-str val))
        ((equal (typeof val) "string") val)
        ((consp val) (am-str-join (list "(" (num-to-str (length val)) " items)")))
        (t "<object>")))

; ---- AGENT TOOLS ----------------------------------------------

; Run a forth program on the cpu.  Input: forth source list.  Returns result string.
(define (agent-run forth-src)
  (cond ((agent-tool-begin "run")
    (let ((bin (am-compile forth-src)))
      (let ((cpu (am-mk)))
        (am-load cpu bin 0)
        (am-sr cpu PCI 0)
        (am-run cpu 50)
        (let* ((result (am-gr cpu 0))
               (text (am-str-join (list "cpu=>R0=" (num-to-str result)))))
          (setq *agent-observation* text)
          (print (list 'agent-tool 'run 'result result))
          text))))
    (t nil)))

; Execute a shell command on the virtual os.
(define (agent-shell command)
  (cond ((agent-tool-begin "shell")
    (let ((output (am-shell-dispatch command)))
      (setq *agent-observation* (cat "shell=>" (am-render output)))
      (print (list 'agent-tool 'shell command '-> output))
      output))
    (t nil)))

; Read a file from the virtual filesystem.
(define (agent-fs-read path)
  (cond ((agent-tool-begin "fs-read")
    (let ((result (am-fs-resolve am-root path)))
      (cond ((eq result nil)
              (setq *agent-observation* "fs-read=>not-found")
              nil)
            ((eq (am-ino-type result) 'file)
             (let* ((body (am-ino-body result))
                    (text (am-str-join (list "fs-read=>" (am-render body)))))
               (setq *agent-observation* text)
               (print (list 'agent-tool 'fs-read path '-> body))
               body))
            (t (setq *agent-observation* "fs-read=>is-directory") nil))))
    (t nil)))

; Write a file to the virtual filesystem.
(define (agent-fs-write path value)
  (cond ((agent-tool-begin "fs-write")
    (setq am-root (am-fs-create am-root path value))
    (setq *agent-observation* (cat "fs-write=>ok:" (am-render path)))
    (print (list 'agent-tool 'fs-write path))
    value)
    (t nil)))

; List a directory.
(define (agent-fs-ls path)
  (cond ((agent-tool-begin "fs-ls")
    (let ((dir (am-fs-resolve am-root path)))
      (cond ((eq dir nil)
              (setq *agent-observation* "fs-ls=>not-found")
              nil)
            (t (let* ((entries (am-fs-ls dir))
                      (text (am-str-join (list "fs-ls=>" (num-to-str (length entries)) " entries"))))
                 (setq *agent-observation* text)
                 (print (list 'agent-tool 'fs-ls path entries))
                 entries)))))
    (t nil)))

; System info: memory, processes.
(define (agent-sys-info)
  (cond ((agent-tool-begin "sys-info")
    (let* ((proc-count (length am-procs))
           (text (am-str-join (list "sys=>procs=" (num-to-str proc-count)))))
      (setq *agent-observation* text)
      (print (list 'agent-tool 'sys-info 'procs proc-count))
      text))
    (t nil)))

; ---- Filesystem navigation helpers ---------------------------
(define (am-fs-resolve dir path)
  (cond ((null path) dir)
        (t (let ((kid (am-fs-find dir (car path))))
             (cond ((eq kid nil) nil)
                   (t (am-fs-resolve kid (cdr path))))))))

(define (am-fs-create dir path value)
  (cond ((null (cdr path))
         (am-fs-put dir (car path) (am-mk-file (car path) value)))
        (t (let ((kid (am-fs-find dir (car path))))
             (cond ((eq kid nil)
                    (let ((sub (am-mk-dir (car path) nil)))
                      (am-fs-put dir (car path)
                        (am-fs-create sub (cdr path) value))))
                   (t (am-fs-put dir (car path)
                        (am-fs-create kid (cdr path) value))))))))

; ---- Shell dispatch (minimal) ---------------------------------
(define (am-shell-dispatch command)
  (let ((tokens (am-tokenize command)))
    (cond ((null tokens) "empty")
          ((equal (car tokens) "ls")
           (cond ((null am-root) "no-fs")
                 (t (let ((entries (am-fs-ls am-root)))
                      (am-str-join (map (lambda (e) (cat (car e) " ")) entries))))))
          ((equal (car tokens) "ps")
           (am-str-join (list (num-to-str (length am-procs)) " procs")))
          (t (am-str-join (list "unknown:" (car tokens)))))))

(define (am-tokenize input)
  (let ((chars (str-to-list input)))
    (am-tok-acc chars "" nil)))

(define (am-tok-acc chars cur tokens)
  (cond ((null chars)
         (cond ((= (length cur) 0) (am-rev tokens))
               (t (am-rev (cons cur tokens)))))
        ((equal (car chars) " ")
         (cond ((= (length cur) 0) (am-tok-acc (cdr chars) "" tokens))
               (t (am-tok-acc (cdr chars) "" (cons cur tokens)))))
        (t (am-tok-acc (cdr chars) (cat cur (car chars)) tokens))))

(define (am-rev lst)
  (labels (((go l a) (cond ((null l) a) (t (go (cdr l) (cons (car l) a)))))) (go lst nil)))

; ---- Demo: agent session using new tools ----------------------
(print "=== Agent-Machine Integration ===")
(print "")

; Setup filesystem
(setq am-root (am-mk-dir "/"
  (list (am-mk-dir "bin" nil)
        (am-mk-dir "home" (list (am-mk-file "readme" "hello agent"))))))
(am-proc-add "kernel")
(am-proc-add "shell")

; DEMO: agent uses tools
(print "--- Agent tools demo ---")

; Run a forth program
(print "1. agent-run '(3 4 +):")
(setq *agent-tool-count* 0)
(agent-run '(3 4 +))
(print (cat "   -> " *agent-observation*))
(print "")

; Read a file
(print "2. agent-fs-read (/ home readme):")
(setq *agent-tool-count* 0)
(agent-fs-read '("home" "readme"))
(print (cat "   -> " *agent-observation*))
(print "")

; List directory
(print "3. agent-fs-ls (/):")
(setq *agent-tool-count* 0)
(agent-fs-ls '("home"))
(print (cat "   -> " *agent-observation*))
(print "")

; Write a file
(print "4. agent-fs-write (/ bin hello) '(1 2 +):")
(setq *agent-tool-count* 0)
(agent-fs-write '("bin" "hello") '(1 2 +))
(print (cat "   -> " *agent-observation*))
(print "")

; System info
(print "5. agent-sys-info:")
(setq *agent-tool-count* 0)
(agent-sys-info)
(print (cat "   -> " *agent-observation*))
(print "")

; Shell command
(print "6. agent-shell ls:")
(setq *agent-tool-count* 0)
(agent-shell "ls")
(print (cat "   -> " *agent-observation*))
(print "")

; Guard: an action may invoke at most one tool.  The second tool call in
; the same action is refused and leaves the multiple-tools observation.
(print "7. guard: two tools in one action:")
(setq *agent-tool-count* 0)
(agent-sys-info)
(agent-fs-ls '("home"))
(print (cat "   -> " *agent-observation*))
(print "")

(print "=== Agent-Machine Demo Complete ===")
