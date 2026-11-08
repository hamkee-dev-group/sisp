; ============================================================
; SISP COMPUTER — INTEGRATION CHECKS
; Run: src/sisp examples/machine/os/computer-checks.lsp
; Covers: login, shell, editor, cron, packages, sysmon, build,
; debugger, persistence, and the network layer (single-process
; loopback through the real spool files).
; ============================================================
(load "examples/machine/computer.lsp")

; These checks mutate the disk (add users, make dirs, write files).  Booting
; restores any persisted disk image, so without a reset a second run would
; start from the first run's leftovers and fail ("user exists", "dir exists",
; ...).  Wipe to a pristine, freshly-seeded filesystem so every run is
; independent of whatever examples/machine/disk-ada.img happens to hold.
(fs-format)
(seed-fs)

(print "--- Computer checks ---")

; -- session ------------------------------------------------------
(check "computer: machine id from file" (equal (machine-id) "ada"))
(check "login: root logs in" (equal (login "root" "sisp") "welcome, root"))
(check "login: whoami" (equal (whoami) "root"))
(check "login: wrong password rejected"
       (equal (login "root" "nope") "login: wrong password"))
(check "login: unknown user rejected"
       (equal (login "nobody" "x") "login: nobody: no such user"))
(check "login: user-add by root"
       (equal (user-add "ada" "lovelace" "user") "user ada added (user)"))
(check "login: new user can log in" (equal (login "ada" "lovelace") "welcome, ada"))
(check "login: non-root cannot add users"
       (equal (user-add "mallory" "x" "user") "user-add: only root"))
(check "login: logout works" (equal (progn (logout) (whoami)) "nobody"))

; -- shell on the real disk ---------------------------------------
(login "root" "sisp")
(check "shell: ls / shows seeded dirs"
       (and (str-contains (sh "ls /") "etc") (str-contains (sh "ls /") "home")))
(check "shell: mkdir and cd"
       (and (equal (sh "mkdir /home/root") "") (equal (sh "cd /home/root") "")))
(check "shell: write and read a file through redirection"
       (and (equal (sh "echo it works > notes") "wrote notes")
            (equal (sh "cat notes") "it works")))
(check "shell: pwd tracks the cwd" (equal (sh "pwd") "/home/root"))
(check "shell: back to root and remove"
       (and (equal (sh "cd /") "") (equal (sh "rm /home/root/notes") "removed /home/root/notes")))
(check "shell: run a program with output"
       (equal (sh "run count") "[3] count"))
(check "shell: pipelines still work"
       (equal (sh "ls / | grep e | wc") "3"))

; -- editor ---------------------------------------------------------
(define e (ed-open "/home/root/diary"))
(check "editor: insert and print"
       (and (equal (ed-ins e 0 "line one") e)
            (equal (ed-ins e 1 "line two") e)
            (equal (ed-print e 2) "line two")))
(ed-sub e "one" "1")
(check "editor: substitute" (equal (ed-print e 1) "line 1"))
(ed-del e 2)
(check "editor: write persists to disk"
       (and (equal (ed-write e) "wrote /home/root/diary")
            (equal (kf-read "/home/root/diary") "line 1")))

; -- packages --------------------------------------------------------
(check "pm: registry has packages"
       (not (null (member "fortune" (pm-available)))))
(check "pm: install writes files"
       (and (pm-install "fortune")
            (equal (kf-read "/usr/fortune.txt")
                   (cat (cat "sisp is a small lisp." nl)
                        (cat "this machine is real." nl)))))
(check "pm: installed list" (not (null (member "fortune" (pm-list)))))
(check "pm: dependency resolution (cowsay needs fortune)"
       (and (pm-install "cowsay")
            (not (null (member "cowsay" (pm-list))))))
(check "pm: package program runs"
       (equal (sh "run moo") "[4] moo"))

; -- build + debugger -----------------------------------------------
(check "build: assemble from source text"
       (equal (build "((loadi 0 7) (loadi 2 200) (store 2 0) (halt))" "seven")
              "built seven (4 words)"))
(define dbgcpu (dbg-cpu (cadr (assoc "seven" *images* (quote equal)))))
(run1 dbgcpu 10 0)
(check "build: program stored 7 in memory" (= (rd dbgcpu 200) 7))
(check "debugger: disassembly mentions store"
       (str-contains (dbg-dis "seven") "store"))

; -- persistence ------------------------------------------------------
(check "disk: backup writes an image"
       (and (numberp (save))
            (not (null (read-file-safe (cat (cat "examples/machine/disk-" *machine-id*) ".img"))))))
(setq dsk-store nil)
(check "disk: restore brings files back"
       (and (disk-restore (cat (cat "examples/machine/disk-" *machine-id*) ".img"))
            (equal (kf-read "/home/root/diary") "line 1")))

; -- cron --------------------------------------------------------------
(check "cron: job registration"
       (= (cron-add "count" 3) 1))
; an idle machine does not tick — keep it busy with a spinner and
; capture the console to prove the job fired on schedule
(k-spawn-image "spin" 2)
(setq *con-capture* nil)
(setq *con-capture-on* t)
(cron-start)
(k-run 400)
(cron-stop)
(setq *con-capture-on* nil)
(k-kill (- *nextpid* 1))
; many counts preempt each other mid-print, so digits interleave —
; the point is the job fired and produced output at all
(check "cron: job fired on schedule"
       (> (length *con-capture*) 50))

; -- sysmon --------------------------------------------------------------
(check "sysmon: report mentions procs and blocks"
       (and (str-contains (sysmon) "procs") (str-contains (sysmon) "blocks")))

; -- network (single-process loopback through the real spool) ------------
(net-init "ada")
(netd-start)
(check "net: loopback ping"
       (progn (net-send-raw "ada" 7 "t1|ping") (netd-tick) (net-poll)
              (equal (net-take-reply "t1") "ping")))
(kf-create "/www.html")
(kf-write "/www.html" (cat "<b>loopback</b>" nl))
(check "net: loopback web"
       (progn (net-send-raw "ada" 80 "t2|GET /www.html") (netd-tick) (net-poll)
              (equal (net-take-reply "t2") (cat "200 " (cat nl "<b>loopback</b>" nl)))))
(check "net: loopback kv put/get"
       (progn (net-send-raw "ada" 42 "t3|PUT os sisp")
              (netd-tick)
              (net-send-raw "ada" 42 "t4|GET os")
              (netd-tick) (net-poll)
              (and (equal (net-take-reply "t3") "OK")
                   (equal (net-take-reply "t4") "sisp"))))
(check "net: kv survives in /var/kv.db"
       (equal (kv-local-get "os") "sisp"))

(print "--- Computer checks done ---")
