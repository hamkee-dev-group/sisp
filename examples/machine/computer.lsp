; ============================================================
; SISP COMPUTER — a real computer + OS + tools, in SISP
;
;   Run:  src/sisp examples/machine/computer.lsp
;
; Boots the V2.1 CPU + SISP/UNIX V3 kernel with a hierarchical
; persistent filesystem, users and login, a shell, real tools
; (editor, cron, packages, sysmon, build, debugger, backup),
; and a network interface that lets two machines talk.
;
; The REPL is your terminal.  After the boot banner:
;
;   : (login "root" "sisp")
;   : (sh "ls /etc")
;   : (sh "cat /etc/motd")
;   : (sh "run count")
;   : (ed-open "/home/notes")
;   : (pm-install "fortune")
;   : (web-get "bob" "/index.html")
;
; To run a second machine: edit examples/machine/MACHINE-ID in
; another terminal (say "bob"), start it the same way, and the
; two can ping / web-get / kv-* each other.
; ============================================================

(load "examples/machine/os/sisp-unix.lsp")
(load "examples/machine/os/users.lsp")
(load "examples/machine/os/tools.lsp")
(load "examples/machine/os/net.lsp")

; ---- machine identity -----------------------------------------
(define (machine-id)
  (let ((s (read-file "examples/machine/MACHINE-ID")))
    (cond ((null s) "ada")
          (t (car (split-lines s))))))

; ---- tick master: cron + netd + sysmon share the kernel hook --
(define *cron-on* nil) (define *netd-on* nil)
(define (master-tick)
  (sysmon-tick)
  (cond (*cron-on* (cron-tick)))
  (cond (*netd-on* (netd-tick))))
(define (cron-start) (setq *cron-on* t))
(define (cron-stop) (setq *cron-on* nil))
(define (netd-start)
  (net-listen 7 echo-handler)
  (net-listen 80 web-handler)
  (net-listen 42 kv-handler)
  (setq *netd-on* t))
(define (netd-stop) (setq *netd-on* nil))

; ---- shell gate ------------------------------------------------
(define (sh cmd)
  (cond ((null *session*) "sh: login first")
        (t (let ((out (sh-run cmd)))
             (save)
             out))))
(define (save)
  (disk-backup (cat (cat "examples/machine/disk-" *machine-id*) ".img")))

; ---- filesystem seed -------------------------------------------
(define (seed-file path content)
  (cond ((null (fs-resolve-ino path FS-ROOT))
         (ensure-parents path)
         (kf-create path)
         (kf-write path content))))
(define (seed-fs)
  (users-seed)
  (map (lambda (d) (cond ((null (fs-resolve-ino d FS-ROOT)) (kf-mkdir d))))
       (list "/home" "/bin" "/usr" "/var" "/etc" "/tmp"))
  (seed-file "/etc/motd"
    (cat (cat "Welcome to your SISP computer." nl)
         (cat (cat "Kernel: V2.1 CPU, 8192W RAM, preemptive multitasking." nl)
              (cat "Type (help) for commands." nl))))
  (seed-file "/etc/issue" (cat "SISP/UNIX V3 — ready." nl))
  (seed-file "/index.html"
    (cat (cat "<html><body><h1>SISP web server</h1>" nl)
         (cat "<p>served from the machine's real disk</p></body></html>" nl)))
  (seed-file "/usr/demo.s"
    (cat (cat "((loadi 6 1) (loadi 4 200)" nl)
         (cat (cat "(label lp)" nl)
         (cat (cat "(loadi 1 42) (add 1 6) (store 4 1)" nl)
         (cat (cat "(loadi 1 1) (mov 2 4) (loadi 3 1) (sys 5)" nl)
         (cat (cat "(loadi 1 1) (add 6 1)" nl)
         (cat (cat "(loadi 2 5) (cmp 6 2) (jne lp)" nl)
         (cat (cat "(loadi 1 10) (store 4 1) (loadi 1 1) (mov 2 4) (loadi 3 1) (sys 5)" nl)
              "(loadi 1 0) (sys 0))" nl))))))))
  ; packages
  (pm-register (list "fortune" 1 nil
                     (list (list "/usr/fortune.txt"
                                 (cat (cat "sisp is a small lisp." nl)
                                      (cat "this machine is real." nl))))
                     nil))
  (pm-register (list "cowsay" 1 (list "fortune")
                     (list (list "/usr/cowsay.txt"
                                 (cat "< sisp >" nl)))
                     (list (list "moo" "((loadi 1 1) (loadi 2 6) (loadi 3 4) (sys 5) (loadi 1 0) (sys 0) (word 109) (word 111) (word 111) (word 10))")))))

; ---- help -------------------------------------------------------
(define (help)
  (print "=================================================")
  (print "  SISP COMPUTER — commands")
  (print "=================================================")
  (print "  session:   (login 'root' 'sisp') (logout) (whoami)")
  (print "  shell:     (sh 'ls /etc') (sh 'cat /etc/motd')")
  (print "             (sh 'run count') (sh 'ps') (sh 'mkdir /x')")
  (print "             pipes: (sh 'ls / | grep e | wc')")
  (print "  files:     (kf-read path) (kf-write path str) (ed-open path)")
  (print "  editor:    (ed-ins s n text) (ed-print s nil) (ed-write s)")
  (print "  packages:  (pm-available) (pm-install name) (pm-list)")
  (print "  build:     (build-file '/usr/demo.s' 'demo') then (sh 'run demo')")
  (print "  debug:     (dbg-dis name) (dbg-trace name 40)")
  (print "  system:    (sysmon) (sh 'ps') (cron-add name secs)")
  (print "             (cron-start) (cron-stop)")
  (print "  net:       (ping host) (web-get host path)")
  (print "             (kv-put host k v) (kv-get host k)")
  (print "  disk:      (save)  -- write disk image to host")
  (print "  exit:      (quit)  -- disk auto-saves on (sh ...)")
  (print "================================================="))

; ---- boot --------------------------------------------------------
; (boot) uses the id from MACHINE-ID; (boot-with-id "bob") overrides.
(define (boot) (boot-with-id (machine-id)))
(define (boot-with-id id)
    (os-boot)
    (seed-fs)
    ; restore a previous disk if we have one
    (cond ((not (null (read-file-safe (cat (cat "examples/machine/disk-" id) ".img"))))
           (disk-restore (cat (cat "examples/machine/disk-" id) ".img"))
           (print (str-join (list "[boot] disk restored for machine " id) ""))))
    (net-init id)
    (netd-start)
    (setq *k-tick-hook* master-tick)
    (print "=================================================")
    (print (str-join (list "  SISP COMPUTER — machine <" id ">") ""))
    (print "=================================================")
    (print (cat (kf-read "/etc/motd")))
    (print (str-join (list "accounts: root/sisp, guest/guest") ""))
    (print (str-join (list "net: inbox " (net-inbox-path id)) ""))
    (help)
    id)
; read a file, nil instead of an abort when missing
(define (read-file-safe path)
  (append-file path "")              ; creates it empty if missing
  (let ((s (read-file path)))
    (cond ((= (length s) 0) nil) (t s))))

(boot)
