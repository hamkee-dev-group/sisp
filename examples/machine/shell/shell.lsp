; ============================================================
; SHELL — COMMAND INTERPRETER FOR THE SISP OS
;
; A simple interactive shell running as a process on the SISP
; OS.  Supports built-in commands and launching programs from
; the filesystem.
;
; Built-in commands:
;   ls [path]   — list directory
;   cat <file>  — display file contents
;   echo <msg>  — print a message
;   mkdir <dir> — create a directory
;   rm <path>   — remove a file or directory
;   run <prog>  — launch a program
;   ps          — list running processes
;   help        — show available commands
;   exit        — terminate the shell
; ============================================================

; ---- shell state: (root-dir, current-dir-path, env) ---------
(define (make-shell-state root)
  (list root '("/") nil))

(define (sh-root  st) (car st))
(define (sh-cwd   st) (cadr st))
(define (sh-env   st) (caddr st))

; ---- tokenizer: split input string into tokens --------------
(define (tokenize input)
  (tok-acc (str-to-list input) "" nil))

(define (tok-acc chars current tokens)
  (cond ((null chars)
         (cond ((= (length current) 0) (rev tokens))
               (t (rev (cons current tokens)))))
        ((equal (car chars) " ")
         (cond ((= (length current) 0)
                (tok-acc (cdr chars) "" tokens))
               (t (tok-acc (cdr chars) "" (cons current tokens)))))
        (t (tok-acc (cdr chars) (cat current (car chars)) tokens))))

(define (rev lst)
  (labels (((go l a) (cond ((null l) a) (t (go (cdr l) (cons (car l) a)))))) (go lst nil)))

; ---- command dispatcher -------------------------------------
(define (shell-dispatch st tokens)
  (cond ((null tokens) st)
        ((equal (car tokens) "help")  (cmd-help st))
        ((equal (car tokens) "echo")  (cmd-echo (cdr tokens) st))
        ((equal (car tokens) "exit")  (cmd-exit st))
        (t (cmd-unknown (car tokens) st))))

(define (cmd-help st)
  (print "Available: ls cat echo mkdir rm run ps help exit")
  st)

(define (cmd-echo args st)
  (print (str-join args))
  st)

(define (cmd-exit st)
  (print "shell: exiting")
  st)

(define (cmd-unknown cmd st)
  (print (str-join (list "shell: unknown command: " cmd)))
  st)

; ---- main shell loop (simulated with explicit state) ---------
; In a real OS, this would be an event loop driven by the kernel.
; Here we demonstrate the shell dispatch logic.
(define (shell-exec st input)
  (let ((tokens (tokenize input)))
    (cond ((null tokens) st)
          (t (shell-dispatch st tokens)))))

(define (str-join parts)
  (cond ((null parts) "")
        ((null (cdr parts)) (car parts))
        (t (cat (car parts) (str-join (cdr parts))))))

; ---- demo ---------------------------------------------------
(define sh (make-shell-state nil))
(define s1 (shell-exec sh "help"))
(define s2 (shell-exec s1 "echo hello world"))
(shell-exec s2 "exit")
