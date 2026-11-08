; ============================================================
; SISP/UNIX V3 — DEMO + CHECKS
; Run: src/sisp examples/machine/os/sisp-unix-demo.lsp
; ============================================================
(load "examples/machine/os/sisp-unix.lsp")

; ============================================================
; DEMO
; ============================================================
(os-boot)
(print "")
(print "--- Filesystem ---")
(print (cat "$ ls -> " (kf-list "/")))
(print "$ echo SISP/UNIX V3 > motd")
(kf-create "motd")
(kf-write "motd" (cat "SISP/UNIX V3" nl))
(print (cat "$ ls -> " (kf-list "/")))
(print (cat "$ cat motd -> " (kf-read "motd")))
(print "")
(print "--- Kernel: spawn, preempt, fork, wait, kbd ---")
(print "$ run hello")
(k-spawn-image "hello" 2)
(k-run 2000)
(print "")
(print "$ run count & run squares (concurrent)")
(k-spawn-image "count" 2)
(k-spawn-image "squares" 2)
(k-run 2000)
(print "")
(print "$ run regs (register preservation across 50 yields)")
(k-spawn-image "regs" 2)
(k-run 2000)
(print "")
(print "$ run forkdemo (fork/wait/exit)")
(k-spawn-image "forkdemo" 2)
(k-run 2000)
(print "")
(print "$ run fstest (filesystem syscalls)")
(k-spawn-image "fstest" 2)
(k-run 2000)
(print "")
(print "$ run echo (interrupt-driven keyboard input)")
(kbd-push 104) (kbd-push 105) (kbd-push 33) (kbd-push 10)   ; "hi!" + newline
(k-spawn-image "echo" 2)
(k-run 2000)
(print "")
(print "--- ps / kill ---")
(k-spawn-image "spin" 2)
(print (sh-ps))
(print "$ kill the spinner")
(print (num-to-str (k-kill (- *nextpid* 1))))
(print (sh-ps))
(k-run 2000)
(print "")
(print "--- Shell ---")
(print "$ help")    (print (sh-run "help"))
(print "$ ls")      (print (sh-run "ls"))
(print "$ echo hello world") (print (sh-run "echo hello world"))
(print "$ echo from shell > notes")
(print (sh-run "echo from shell > notes"))
(print "$ cat notes") (print (sh-run "cat notes"))
(print "$ ls | grep motd") (print (sh-run "ls | grep motd"))
(print "$ ls | grep o | wc") (print (sh-run "ls | grep o | wc"))
(print "$ rm notes") (print (sh-run "rm notes"))
(print "$ ps")     (print (sh-run "ps"))
(print "")
(print "--- Persistence: unmount + remount ---")
(setq *fs-ibm* nil) (setq *fs-dbm* nil)
(print (cond ((fs-mount) "[K] remounted: filesystem survived")
             (t "[K] REMOUNT FAILED")))
(print (cat "$ cat motd -> " (kf-read "motd")))
(print "")

; ============================================================
; CHECKS
; ============================================================
(print "--- Checks ---")

(check "kernel: MEM bumped to 8192 for process windows" (= MEM 8192))

; -- deterministic program output, via the capture port --
(define (run-captured name)
  (setq *con-capture* nil)
  (setq *con-capture-on* t)
  (k-spawn-image name 2)
  (k-run 2000)
  (setq *con-capture-on* nil)
  (list-to-str (map code-char *con-capture*)))
(check "kernel: hello prints exactly"
       (equal (run-captured "hello") (cat "hello from userland" nl)))
(check "kernel: count prints 12345"
       (equal (run-captured "count") (cat "12345" nl)))
(check "kernel: squares prints 1 4 9 16 25"
       (equal (run-captured "squares") (nl-join (list "1" "4" "9" "16" "25"))))
(check "kernel: registers survive forced rescheduling"
       (equal (run-captured "regs") (cat "ok" nl)))
(check "kernel: fork/wait delivers child then parent"
       (equal (run-captured "forkdemo") (nl-join (list "C" "P"))))
(check "kernel: fstest round-trips a file through syscalls"
       (equal (run-captured "fstest") (cat "SFS!" nl)))
(kbd-push 116) (kbd-push 101) (kbd-push 115) (kbd-push 116) (kbd-push 10)
(check "kernel: echo reads a kbd line via interrupt-driven ring"
       (equal (run-captured "echo") (cat "test" nl)))
(check "kernel: table is clean after runs (init + sh only)"
       (= (length *procs*) 2))

; -- filesystem --
(check "fs: motd survives on disk" (equal (kf-read "motd") (cat "SISP/UNIX V3" nl)))
(kf-create "scratch")
(check "fs: write/read roundtrip"
       (equal (progn (kf-write "scratch" "0123456789") (kf-read "scratch"))
              "0123456789"))
(check "fs: rewrite replaces content"
       (equal (progn (kf-write "scratch" "xy") (kf-read "scratch")) "xy"))
(check "fs: log file written by fstest is on disk"
       (equal (kf-read "log") (cat "SFS!" nl)))
(check "fs: names listed in directory order"
       (equal (kf-list "/") "motd log scratch"))
(check "fs: delete works" (= (fs-delete "scratch" FS-ROOT) 0))
(check "fs: deleted file is gone" (null (fs-read "scratch" FS-ROOT)))
; multi-block file (300 bytes = 2 blocks)
(kf-create "big")
(check "fs: two-block file round-trips"
       (equal (progn (kf-write "big" (rep "0123456789" 30)) (kf-read "big"))
              (rep "0123456789" 30)))
(fs-delete "big" FS-ROOT)
; unmount + remount proves persistence is on the platter
(setq *fs-ibm* nil) (setq *fs-dbm* nil)
(check "fs: remount after unmount" (fs-mount))
(check "fs: motd still there after remount"
       (equal (kf-read "motd") (cat "SISP/UNIX V3" nl)))

; -- hierarchy: mkdir, nested paths, cd, rmdir --
(check "fs: mkdir makes a directory" (= (fs-mkdir "/etc" FS-ROOT) 0))
(check "fs: nested file round-trips"
       (equal (progn (kf-create "/etc/motd2")
                     (kf-write "/etc/motd2" "nested")
                     (kf-read "/etc/motd2"))
              "nested"))
(check "fs: directory listing shows contents"
       (equal (kf-list "/etc") "motd2"))
(check "fs: resolution through . and .."
       (and (numberp (fs-resolve-ino "/etc/../etc/motd2" FS-ROOT))
            (numberp (fs-resolve-ino "/etc/./motd2" FS-ROOT))))
(check "fs: rmdir refuses a non-empty directory"
       (= (fs-rmdir "/etc" FS-ROOT) 1))
(check "fs: file is not a directory"
       (null (fs-is-dir "/motd" FS-ROOT)))
(check "fs: delete then rmdir works"
       (and (= (fs-delete "/etc/motd2" FS-ROOT) 0)
            (= (fs-rmdir "/etc" FS-ROOT) 0)))
(check "shell: cd/pwd and relative paths"
       (and (equal (sh-cd "/") "") (equal *sh-cwd* "/")
            (equal (progn (sh-cd "/") (sh-run "mkdir /home")) "")
            (equal (sh-cd "/home") "") (equal *sh-cwd* "/home")
            (equal (progn (sh-run "echo hi > f") (sh-run "cat f")) "hi")
            (equal (sh-cd "..") "") (equal *sh-cwd* "/")
            (equal (sh-run "rm /home/f") "removed /home/f")
            (equal (sh-run "rmdir /home") "")))

; -- shell --
(check "shell: tokenizer" (equal (tokenize "  ls   -l ") (list "ls" "-l")))
(check "shell: echo" (equal (sh-run "echo a b c") "a b c"))
(check "shell: unknown command"
       (equal (sh-run "frobnicate") "sh: frobnicate: not found"))
(kf-create "grepfood")
(kf-write "grepfood" "alpha beta gamma")
(check "shell: pipeline grep"
       (equal (sh-run "cat grepfood | grep beta") "beta"))
(check "shell: pipeline grep | wc"
       (equal (sh-run "cat grepfood | grep a | wc") "3"))
(sh-run "echo redirected data > redir")
(check "shell: redirection writes a file"
       (equal (kf-read "redir") "redirected data"))
(check "shell: rm removes" (equal (sh-run "rm redir") "removed redir"))
(check "shell: rm reports missing"
       (equal (sh-run "rm redir") "rm: redir: no such file"))
(fs-delete "grepfood" FS-ROOT)
(check "shell: ps lists init"
       (str-contains (sh-run "ps") "init"))

; -- kernel mechanics --
(define spinpid (k-spawn-image "spin" 2))
(check "kernel: kill terminates a process" (= (k-kill spinpid) 0))
(check "kernel: kill reports missing pid" (= (k-kill 99) (- WORD 1)))
(check "kernel: only init's slot stays allocated"
       (= (length (filter (lambda (s) s) *slots*)) 1))
(check "kernel: only init and sh remain in the table"
       (= (length *procs*) 2))

; -- host sisp (metacircular) --
(define genv (s-env-extend nil '(x y) '(10 20)))
(check "s-eval: variables resolve through the environment"
       (= (s-eval '(s-+ x y) genv) 30))
(check "s-eval: nested arithmetic" (= (s-eval '(s-* (s-+ 3 4) 2) genv) 14))
(check "s-eval: quote returns its argument unevaluated"
       (equal (s-eval '(s-quote (1 2 3)) nil) (list 1 2 3)))
(check "s-eval: if picks the live branch"
       (and (= (s-eval '(s-if (s-= 1 1) 7 8) nil) 7)
            (= (s-eval '(s-if (s-= 1 2) 7 8) nil) 8)))
(check "s-apply: a lambda closes over its definition environment"
       (= (s-eval '((s-lambda (x) (s-* x x)) 5) nil) 25))
(s-eval '(s-define fact
          (s-lambda (n) (s-if (s-= n 0) 1 (s-* n (fact (s-- n 1)))))) nil)
(check "s-eval: recursion through a global definition, fact 5 = 120"
       (= (s-eval '(fact 5) nil) 120))

(print "")
(print "=========================================================")
(print "  SISP/UNIX V3 — ALL SYSTEMS OPERATIONAL")
(print "=========================================================")


