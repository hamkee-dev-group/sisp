; ============================================================
; SHELL DEMO — Session trace with filesystem, ps, echo, ls
; ============================================================

(define (str-join parts)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (car parts) (str-join (cdr parts))))))

; str-join concatenates with no separator; echo needs spaces between
; its arguments.
(define (join-sp parts)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (cat (car parts) " ") (join-sp (cdr parts))))))

(define (rev lst)
  (labels (((go l a) (cond ((null l) a) (t (go (cdr l) (cons (car l) a)))))) (go lst nil)))

(define (foreach fn lst)
  (cond ((null lst) nil) (t (progn (fn (car lst)) (foreach fn (cdr lst))))))

(define (tokenize input)
  (tok (str-to-list input) "" nil))

(define (tok chars cur tokens)
  (cond ((null chars)
         (cond ((= (length cur) 0) (rev tokens)) (t (rev (cons cur tokens)))))
        ((equal (car chars) " ")
         (cond ((= (length cur) 0) (tok (cdr chars) "" tokens))
               (t (tok (cdr chars) "" (cons cur tokens)))))
        (t (tok (cdr chars) (cat cur (car chars)) tokens))))

; ---- filesystem — built step by step to avoid nesting bugs ---
(define bin-forth (list "forth" 'file "3 4 + p"))
(define bin-cpu   (list "cpu" 'file "loadi 0 42 ; out 0 ; halt"))
(define dir-bin   (list "bin" 'dir (list bin-forth bin-cpu)))
(define home-notes (list "notes" 'file "hello world"))
(define home-todo  (list "todo" 'file "build shell"))
(define dir-user   (list "user" 'dir (list home-notes home-todo)))
(define dir-home   (list "home" 'dir (list dir-user)))
(define root-dir   (list "/" 'dir (list dir-bin dir-home)))

(define (ino-name ino) (car ino))
(define (ino-type ino) (cadr ino))
(define (ino-body ino) (caddr ino))
(define (ino-kids ino) (caddr ino))

(define (fs-ls dir)
  (map (lambda (k) (list (ino-name k) (ino-type k))) (ino-kids dir)))

(define (fs-find dir name)
  (let ((kids (ino-kids dir)))
    (find (lambda (k) (equal (ino-name k) name)) kids)))

(define (fs-cd dir path)
  (cond ((null path) dir)
        ((equal (car path) "") (fs-cd dir (cdr path)))
        (t (let ((k (fs-find dir (car path))))
             (cond ((eq k nil) nil) (t (fs-cd k (cdr path))))))))

; ---- process table ------------------------------------------
(define procs (list (list 1 "init" 'running 12 3)
                     (list 2 "shell" 'running 8 4)
                     (list 3 "worker" 'blocked 0 2)))

(define (ps-show)
  (print "  PID  NAME     STATE     CYCLES  RAM")
  (print "  ---- -------- --------- ------  ---")
  (foreach (lambda (p)
    (print (str-join
      (list "  " (pad (num-to-str (car p)) 4) " "
            (pad (cadr p) 8) " "
            (pad (print-state (caddr p)) 9) " "
            (pad (num-to-str (cadddr p)) 6) "  "
            (pad (num-to-str (car (cddddr p))) 3)))))
    procs))

(define (pad s width)
  (let ((n (length s)))
    (cond ((>= n width) s) (t (cat s (spaces (- width n)))))))

(define (spaces n)
  (cond ((= n 0) "") (t (cat " " (spaces (- n 1))))))

(define (print-state s)
  (cond ((eq s 'running) "running")
        ((eq s 'blocked) "blocked")
        ((eq s 'ready) "ready")
        (t "zombie")))

; ---- memory info --------------------------------------------
(define (meminfo)
  (let ((total 256)
        (used (foldl (lambda (s p) (+ s (car (cddddr p)))) 0 procs)))
    (print (str-join
      (list "  Memory: " (num-to-str used) "/" (num-to-str total)
            " (" (num-to-str (truncate (/ (* used 100) total))) "%)")))))

; ---- commands -----------------------------------------------
(define (cmd-help)    (print "  help ls cd cat echo ps meminfo exit"))
(define (cmd-ls dir)  (foreach (lambda (e) (print (str-join (list "  " (car e)
                           (cond ((eq (cadr e) 'dir) "/") (t "")))))) (fs-ls dir)))
(define (cmd-cat file) (print (str-join (list "  " (ino-body file)))))
(define (cmd-echo args) (print (cat "  " (join-sp args))))
(define (cmd-ps) (ps-show))
(define (cmd-meminfo) (meminfo))

; ---- session -------------------------------------------------
(print "=== SISP Shell Session ===")
(print "")

(define cwd root-dir)

(print "$ help")
(cmd-help)
(print "")

(print "$ ls")
(cmd-ls cwd)
(print "")

(print "$ cd bin")
(setq cwd (fs-cd cwd (list "bin")))
(print "$ ls")
(cmd-ls cwd)
(print "")

(print "$ cat forth")
(cmd-cat (fs-find cwd "forth"))
(print "")

(print "$ cd ..")
(setq cwd root-dir)
(print "$ echo hello from the sisp shell")
(cmd-echo (list "hello" "from" "the" "sisp" "shell"))
(print "")

(print "$ ps")
(cmd-ps)
(print "")

(print "$ meminfo")
(cmd-meminfo)
(print "")

(print "--- Checks ---")

(check "tokenizer splits on runs of spaces"
       (equal (tokenize "  ls   -l  ") (list "ls" "-l")))
(check "tokenizer on empty input" (null (tokenize "   ")))
(check "echo separates its arguments with single spaces"
       (equal (join-sp (list "hello" "from" "the" "sisp" "shell"))
              "hello from the sisp shell"))
(check "ls names the root's children"
       (equal (map car (fs-ls root-dir)) (list "bin" "home")))
(check "ls reports the type of each entry"
       (equal (map cadr (fs-ls root-dir)) (list 'dir 'dir)))
(check "cd walks a multi-component path"
       (equal (ino-name (fs-cd root-dir (list "home" "user"))) "user"))
(check "cd into a nested file finds its contents"
       (equal (ino-body (fs-cd root-dir (list "home" "user" "notes")))
              "hello world"))
(check "cd on a path that does not exist yields nothing"
       (null (fs-cd root-dir (list "home" "nobody"))))
(check "cd with an empty component is a no-op"
       (equal (ino-name (fs-cd root-dir (list "" "bin"))) "bin"))
(check "pad widens to the requested column and never truncates"
       (and (equal (pad "ab" 5) "ab   ")
            (and (= (length (pad "ab" 5)) 5) (equal (pad "abcdef" 3) "abcdef"))))
(check "process states render by name"
       (and (equal (print-state 'running) "running")
            (and (equal (print-state 'blocked) "blocked")
                 (equal (print-state 'nonesuch) "zombie"))))
(check "meminfo sums the per-process RAM"
       (= (foldl (lambda (acc p) (+ acc (car (cddddr p)))) 0 procs) 9))
(print "")

(print "$ exit")
(print "=== Session ended ===")
