; ============================================================
; /dev + /proc + MOUNTS + TERMINAL — completing Unix VFS
;
; Device files: major/minor numbers, /dev/null, /dev/zero,
;   /dev/tty, /dev/disk0.  Devices respond to read/write.
;
; /proc filesystem: synthetic filesystem exposing process
;   info as files (/proc/PID/status, /proc/PID/cmdline).
;
; Mount points: mount a filesystem at a directory path.
;
; Terminal line discipline: cooked mode (line buffering,
;   echo, backspace) vs raw mode.
; ============================================================

; ---- Device numbers -----------------------------------------
(define (makedev major minor) (list 'dev major minor))
(define (dev-major dev) (cadr dev))
(define (dev-minor dev) (caddr dev))
(define (dev-p dev) (and (consp dev) (eq (car dev) 'dev)))

; ---- Device switch table ------------------------------------
(define dev-switch nil)  ; ((major . (read-fn write-fn)) ...)

(define (dev-register major read-fn write-fn)
  (setq dev-switch (cons (list major read-fn write-fn) dev-switch)))

(define (dev-lookup major)
  (let ((e (assoc major dev-switch))) (cond (e (cdr e)) (t nil))))

(define (dev-read dev offset count)
  (let ((drv (dev-lookup (dev-major dev))))
    (cond (drv ((car drv) (dev-minor dev) offset count))
          (t (print "[dev] no driver") nil))))

(define (dev-write dev offset data)
  (let ((drv (dev-lookup (dev-major dev))))
    (cond (drv ((cadr drv) (dev-minor dev) offset data))
          (t (print "[dev] no driver") -1))))

; ---- /dev/null -----------------------------------------------
(define (null-read minor offset count) nil)   ; always EOF
(define (null-write minor offset data) (length data))  ; discard, report success
(dev-register 1 null-read null-write)

; ---- /dev/zero ----------------------------------------------
(define (zero-read minor offset count) (make-list-dev count 0))
(define (zero-write minor offset data) (length data))
(dev-register 2 zero-read zero-write)

(define (make-list-dev n v) (cond ((= n 0) nil) (t (cons v (make-list-dev (- n 1) v)))))

; ---- /dev/tty (current terminal) ----------------------------
(define tty-input nil)   ; pending input chars
(define tty-cooked t)    ; cooked mode by default
(define tty-echo t)

(define (tty-read minor offset count)
  (cond ((null tty-input) nil)  ; would block
        (t (let ((ch (car tty-input)))
             (setq tty-input (cdr tty-input))
             (cond ((and tty-cooked (= ch 10))
                    (setq tty-input nil)  ; flush rest of line
                    (list ch))
                   (t (cond (tty-echo (print (char->str ch))) (t nil))
                      (list ch)))))))

(define (tty-write minor offset data)
  (print (list-to-str-tt data))
  (length data))

(dev-register 3 tty-read tty-write)

; Chars travel through the device layer as integer codes; code-char
; turns one back into a one-character string.
(define (char->str ch) (code-char ch))

; ---- /dev/disk0 (block device) -------------------------------
(define disk0-data nil)  ; byte list

(define (disk0-read minor offset count)
  (sublist-dev disk0-data offset count))
(define (disk0-write minor offset data)
  (setq disk0-data (append disk0-data data))
  (length data))
(dev-register 4 disk0-read disk0-write)

(define (sublist-dev lst start len)
  (cond ((or (null lst) (= len 0)) nil)
        ((> start 0) (sublist-dev (cdr lst) (- start 1) len))
        (t (cons (car lst) (sublist-dev (cdr lst) 0 (- len 1))))))

; ---- /proc filesystem ---------------------------------------
(define proc-data nil)  ; ((pid . ((file . content) ...)) ...)

; SISP string literals have no escapes, so newlines come from code-char.
(define NL (code-char 10))

(define (proc-register pid)
  (let ((status (str-join (list "Name:   process-" (num-to-str pid) NL
                                 "State:  running" NL
                                 "Pid:    " (num-to-str pid) NL
                                 "VmSize: 4096 kB" NL) ""))
        (cmdline (str-join (list "/bin/process-" (num-to-str pid)) "")))
    (setq proc-data (cons (list pid
                           (list "status" status)
                           (list "cmdline" cmdline))
                     proc-data))))

(define (proc-read pid file)
  (let ((entry (assoc pid proc-data)))
    (cond (entry (let ((f (assoc file (cdr entry))))
                   (cond (f (cadr f)) (t nil))))
          (t nil))))

; ---- Mount points --------------------------------------------
; Each mount: (device path filesystem-root)
(define mount-table nil)

(define (sys-mount device path fs-root)
  (setq mount-table (cons (list device path fs-root) mount-table))
  (print (str-join (list "[mount] " device " on " path) "")))

(define (sys-umount path)
  (setq mount-table (remove (lambda (m) (equal (cadr m) path)) mount-table)))

; VFS path resolution: walk mounts to find filesystem and inode
(define (vfs-lookup path)
  (let ((mount (find-mount path)))
    (cond (mount (vfs-inode (caddr mount) (strip-mount path (cadr mount))))
          (t nil))))  ; rootfs

; Longest-prefix match over the mount table.  Written as an explicit
; fold: `setq` inside a `let` would not update the let binding.
(define (find-mount path) (best-mount path mount-table nil 0))

(define (best-mount path ms best best-len)
  (cond ((null ms) best)
        (t (let ((mp (cadr (car ms))))
             (cond ((and (str-starts path mp) (> (length mp) best-len))
                    (best-mount path (cdr ms) (car ms) (length mp)))
                   (t (best-mount path (cdr ms) best best-len)))))))

(define (strip-mount path mount-point)
  (substr (length mount-point) (- (length path) (length mount-point)) path))

(define (str-starts s prefix)
  (and (>= (length s) (length prefix))
       (equal (substr 0 (length prefix) s) prefix)))

(define (vfs-inode fs path) (list fs path))  ; simplified

; ---- Terminal line discipline --------------------------------
; Cooked mode: line-at-a-time, echo, backspace, ^C interrupt
; Raw mode: character-at-a-time, no processing

(define tty-line-buf nil)    ; current input line being built
(define tty-line-ready nil)  ; complete lines ready for read

(define (tty-input-char ch)
  (cond (tty-cooked (tty-cooked-input ch))
        (t (tty-raw-input ch))))

(define (tty-cooked-input ch)
  (cond ((= ch 127)  ; backspace
         (cond ((null tty-line-buf) nil)
               (t (setq tty-line-buf (cdr tty-line-buf))
                  (cond (tty-echo (print (cat (code-char 8) (cat " " (code-char 8)))))))))
        ((= ch 13)   ; CR -> newline: commit the line to the read queue
         ; tty-line-buf is held newest-first, so reverse it on commit.
         (setq tty-line-ready
               (append tty-line-ready (append (reverse tty-line-buf) (list 10))))
         (setq tty-line-buf nil)
         (cond (tty-echo (print ""))))
        ((= ch 3)    ; ^C
         (setq tty-line-buf nil)
         (print "^C")
         (print "[tty] SIGINT"))
        (t (setq tty-line-buf (cons ch tty-line-buf))
           (cond (tty-echo (print (char->str-tt ch)))))))

(define (tty-raw-input ch)
  (setq tty-line-ready (append tty-line-ready (list ch))))

; One character at a time off the ready queue.
(define (tty-read-char)
  (cond ((null tty-line-ready) nil)
        (t (let ((ch (car tty-line-ready)))
             (setq tty-line-ready (cdr tty-line-ready))
             ch))))

(define (upto-nl lst) (cond ((null lst) nil) ((= (car lst) 10) nil)
                            (t (cons (car lst) (upto-nl (cdr lst))))))
(define (past-nl lst) (cond ((null lst) nil) ((= (car lst) 10) (cdr lst))
                            (t (past-nl (cdr lst)))))

; A whole cooked line (without its terminating newline), or nil if no
; complete line has been committed yet.
(define (tty-read-line)
  (cond ((not (contains 10 tty-line-ready)) nil)
        (t (let ((ln (upto-nl tty-line-ready)))
             (setq tty-line-ready (past-nl tty-line-ready))
             (list-to-str-tt ln)))))

(define (list-to-str-tt lst)
  (cond ((null lst) "") (t (cat (char->str-tt (car lst)) (list-to-str-tt (cdr lst))))))
(define (char->str-tt ch) (code-char ch))

; ---- Helpers -------------------------------------------------
(define (str-join parts sep)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (cat (car parts) sep) (str-join (cdr parts) sep)))))

; ============================================================
; DEMO
; ============================================================
(print "=============================================")
(print "  /dev + /proc + Mounts + Terminal")
(print "=============================================")
(print "")

; --- Device files ---
(print "--- /dev/null ---")
(dev-write (makedev 1 0) 0 (str-to-list "discard me"))
(print "[dev] wrote to /dev/null (silently discarded)")
(define nd (dev-read (makedev 1 0) 0 10))
(print (str-join (list "[dev] read from /dev/null -> " (cond (nd "data") (t "EOF")) "") ""))
(print "")

(print "--- /dev/zero ---")
(define zd (dev-read (makedev 2 0) 0 5))
(print (str-join (list "[dev] read " (num-to-str (length zd))
                       " bytes from /dev/zero, all zero: "
                       (cond ((every (lambda (b) (= b 0)) zd) "yes") (t "no"))) ""))
(print "")

(print "--- /dev/tty ---")
(tty-input-char 72)  ; H
(tty-input-char 101) ; e
(tty-input-char 108) ; l
(tty-input-char 108) ; l
(tty-input-char 111) ; o
(tty-input-char 13)  ; Enter
(define tc (tty-read-line))
(print (str-join (list "[tty] cooked input -> " (cond (tc tc) (t "EOF"))) ""))
(print "")

(print "--- /proc ---")
(proc-register 42)
(print (proc-read 42 "status"))
(print "")

(print "--- Mounts ---")
(sys-mount "/dev/disk0" "/home" "home-fs")
(sys-mount "/dev/disk1" "/var" "var-fs")
(print (str-join (list "[mount] " (num-to-str (length mount-table)) " filesystems mounted") ""))
(print "")

(print "--- Checks ---")

; Device switch
(check "/dev/null reads EOF and swallows writes"
       (and (null (dev-read (makedev 1 0) 0 10))
            (= (dev-write (makedev 1 0) 0 (str-to-list "discard me")) 10)))
(check "/dev/zero yields exactly count zero bytes"
       (and (= (length zd) 5) (every (lambda (b) (= b 0)) zd)))
(check "/dev/disk0 write-then-read round-trips at an offset"
       (progn (setq disk0-data nil)
              (disk0-write 0 0 (list 10 20 30 40 50))
              (equal (dev-read (makedev 4 0) 1 3) (list 20 30 40))))
(check "unknown major has no driver entry" (null (dev-lookup 99)))

; Terminal line discipline
(check "cooked mode assembles a whole line" (equal tc "Hello"))
(check "cooked mode: backspace erases the last char"
       (progn (setq tty-line-buf nil) (setq tty-line-ready nil)
              (setq tty-echo nil)
              (foreach tty-input-char (list 97 98 127 99 13))
              (equal (tty-read-line) "ac")))
(check "cooked mode: ^C discards the partial line"
       (progn (setq tty-line-buf nil) (setq tty-line-ready nil)
              (foreach tty-input-char (list 120 121 3))
              (and (null tty-line-buf) (null (tty-read-line)))))
(check "raw mode delivers chars without waiting for a newline"
       (progn (setq tty-cooked nil) (setq tty-line-ready nil)
              (foreach tty-input-char (list 113 119))
              (let ((a (tty-read-char)) (b (tty-read-char)))
                (and (= a 113) (= b 119)))))
(setq tty-cooked t) (setq tty-echo t)

; /proc
(check "/proc/42/status reports the right pid"
       (str-starts (proc-read 42 "status") "Name:   process-42"))
(check "/proc/42/cmdline is the process path"
       (equal (proc-read 42 "cmdline") "/bin/process-42"))
(check "/proc has nothing for an unregistered pid" (null (proc-read 7 "status")))

; Mounts + VFS resolution
(check "two filesystems are mounted" (= (length mount-table) 2))
(sys-mount "/dev/disk2" "/home/user" "user-fs")
(check "vfs picks the longest matching mount point"
       (equal (vfs-lookup "/home/user/notes") (list "user-fs" "/notes")))
(check "vfs falls back to the shorter mount when it must"
       (equal (vfs-lookup "/home/other") (list "home-fs" "/other")))
(check "vfs returns nothing for an unmounted path" (null (vfs-lookup "/opt/x")))
(sys-umount "/home/user")
(check "umount removes exactly one mount" (= (length mount-table) 2))
(print "")

(print "=============================================")
(print "  Devices + /proc + Mounts + Terminal — done")
(print "=============================================")
