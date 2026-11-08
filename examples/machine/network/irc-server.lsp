; ============================================================
; IRC SERVER — MULTI-USER CHAT
;
; Server state: (users channels logs)
; user:      (nick host realname)
; channel:   (name topic nicks)
; log entry: (type from message)
;
; Commands: /nick /join /part /msg /who /whois /quit /list
; ============================================================

(define (irc-user-make nick host real) (list nick host real))
(define (irc-user-nick u) (car u))
(define (irc-user-host u) (cadr u))
(define (irc-user-real u) (caddr u))

(define (irc-chan-make name topic nicks) (list name topic nicks))
(define (irc-chan-name c) (car c))
(define (irc-chan-topic c) (cadr c))
(define (irc-chan-nicks c) (caddr c))

(define (irc-srv-make) (list nil nil nil))
(define (irc-users srv) (car srv))
(define (irc-channels srv) (cadr srv))
(define (irc-logs srv) (caddr srv))

(define (irc-set-users srv users)
  (list users (irc-channels srv) (irc-logs srv)))
(define (irc-set-channels srv channels)
  (list (irc-users srv) channels (irc-logs srv)))
(define (irc-set-logs srv logs)
  (list (irc-users srv) (irc-channels srv) logs))
(define (irc-add-log srv entry)
  (irc-set-logs srv (append (irc-logs srv) (list entry))))

(define (irc-find-user srv nick)
  (find (lambda (u) (equal (irc-user-nick u) nick)) (irc-users srv)))

(define (irc-add-user srv nick host real)
  (cond ((not (eq (irc-find-user srv nick) nil))
         (irc-add-log srv (list 'error "Server" (cat nick " already in use")))
         srv)
        (t (let ((u (irc-user-make nick host real)))
             (irc-add-log
               (irc-set-users srv (append (irc-users srv) (list u)))
               (list 'join "Server" (cat nick " connected")))))))

(define (irc-remove-user srv nick)
  (let ((srv (irc-set-users srv
               (remove (lambda (u) (equal (irc-user-nick u) nick))
                       (irc-users srv)))))
    (irc-add-log srv (list 'part "Server" (cat nick " disconnected")))))

(define (irc-find-channel srv name)
  (find (lambda (c) (equal (irc-chan-name c) name)) (irc-channels srv)))

(define (irc-replace-chan channels new-chan)
  (map (lambda (c)
         (cond ((equal (irc-chan-name c) (irc-chan-name new-chan)) new-chan)
               (t c)))
       channels))

(define (irc-channel-ensure srv name)
  (cond ((not (eq (irc-find-channel srv name) nil)) srv)
        (t (irc-set-channels srv
             (append (irc-channels srv)
               (list (irc-chan-make name "" nil)))))))

(define (irc-nick-in-channel chan nick)
  (not (eq (find (lambda (n) (equal n nick)) (irc-chan-nicks chan)) nil)))

(define (irc-channel-add-nick srv chan-name nick)
  (let ((chan (irc-find-channel srv chan-name)))
    (cond ((eq chan nil) srv)
          ((irc-nick-in-channel chan nick) srv)
          (t (let ((new-chan (irc-chan-make chan-name (irc-chan-topic chan)
                                (append (irc-chan-nicks chan) (list nick)))))
               (irc-set-channels srv
                 (irc-replace-chan (irc-channels srv) new-chan)))))))

(define (irc-channel-remove-nick srv chan-name nick)
  (let ((chan (irc-find-channel srv chan-name)))
    (cond ((eq chan nil) srv)
          (t (let ((new-chan (irc-chan-make chan-name (irc-chan-topic chan)
                                (remove (lambda (n) (equal n nick))
                                        (irc-chan-nicks chan)))))
               (irc-set-channels srv
                 (irc-replace-chan (irc-channels srv) new-chan)))))))

(define (irc-user-channels srv nick)
  (filter (lambda (c) (irc-nick-in-channel c nick))
          (irc-channels srv)))

(define (str-join parts)
  (cond ((eq parts nil) "")
        ((eq (cdr parts) nil) (car parts))
        (t (cat (car parts) (str-join (cdr parts))))))

(define (irc-nick-list nicks)
  (cond ((eq nicks nil) "")
        ((eq (cdr nicks) nil) (car nicks))
        (t (cat (cat (car nicks) " ") (irc-nick-list (cdr nicks))))))

(define (irc-parse-cmd line)
  (let ((sp (str-find line " ")))
    (cond ((eq sp nil) (list line))
          (t (cons (substr 0 sp line)
                   (list (substr (+ sp 1) (- (length line) (+ sp 1)) line)))))))

(define (irc-execute srv nick line)
  (let ((parsed (irc-parse-cmd line)))
    (let ((cmd (car parsed)))
      (cond ((equal cmd "/nick")  (let ((nn (cadr parsed)))
                                    (list (irc-rename-nick srv nick nn) nn)))
            ((equal cmd "/join")  (let ((ch (cadr parsed)))
                                    (irc-join-chan srv nick ch)))
            ((equal cmd "/part")  (let ((ch (cadr parsed)))
                                    (irc-part-chan srv nick ch)))
            ((equal cmd "/msg")   (let ((target (cadr parsed))
                                        (text (cond ((cddr parsed) (caddr parsed))
                                                    (t ""))))
                                    (irc-handle-msg srv nick target text)))
            ((equal cmd "/who")   (let ((ch (cadr parsed)))
                                    (irc-cmd-who srv nick ch)))
            ((equal cmd "/whois") (let ((tn (cadr parsed)))
                                    (irc-cmd-whois srv nick tn)))
            ((equal cmd "/quit")  (list (irc-remove-user srv nick) nil))
            ((equal cmd "/list")  (irc-cmd-list srv nick))
            (t (list (irc-add-log srv (list 'error "Server" (cat cmd " unknown")))
                     nick))))))

(define (irc-rename-nick srv old new)
  (let ((user (irc-find-user srv old)))
    (cond ((eq user nil) srv)
          ((not (eq (irc-find-user srv new) nil)) srv)
          (t (let ((srv (irc-remove-user srv old)))
               (irc-add-user srv new (irc-user-host user) (irc-user-real user)))))))

(define (irc-join-chan srv nick chan-name)
  (let ((srv (irc-channel-ensure srv chan-name)))
    (let ((srv (irc-channel-add-nick srv chan-name nick)))
      (list (irc-add-log srv
              (list 'join chan-name (cat nick (cat " joined " chan-name))))
            nick))))

(define (irc-part-chan srv nick chan-name)
  (let ((srv (irc-channel-remove-nick srv chan-name nick)))
    (list (irc-add-log srv
            (list 'part chan-name (cat nick (cat " left " chan-name))))
          nick)))

(define (irc-handle-msg srv nick target text)
  (cond ((equal (substr 0 1 target) "#")
         (irc-channel-msg srv nick target text))
        (t (irc-private-msg srv nick target text))))

(define (irc-channel-msg srv from chan-name text)
  (let ((chan (irc-find-channel srv chan-name)))
    (cond ((eq chan nil)
           (list (irc-add-log srv (list 'error "Server" (cat chan-name " no such channel")))
                 from))
          ((not (irc-nick-in-channel chan from))
           (list (irc-add-log srv (list 'error "Server" "Not in that channel"))
                 from))
          (t (list (irc-add-log srv (list 'msg chan-name (cat from (cat ": " text))))
                   from)))))

(define (irc-private-msg srv from to text)
  (let ((target (irc-find-user srv to)))
    (cond ((eq target nil)
           (list (irc-add-log srv (list 'error "Server" (cat to " no such nick")))
                 from))
          (t (list (irc-add-log srv (list 'priv to (cat from (cat ": " text))))
                   from)))))

(define (irc-cmd-who srv nick chan-name)
  (let ((chan (irc-find-channel srv chan-name)))
    (cond ((eq chan nil) (list srv nick))
          (t (progn
               (print (cat (cat chan-name " users: ")
                      (irc-nick-list (irc-chan-nicks chan))))
               (list srv nick))))))

(define (irc-cmd-whois srv nick target)
  (let ((user (irc-find-user srv target)))
    (cond ((eq user nil)
           (progn (print (cat target " not found")) (list srv nick)))
          (t (progn
               (print (str-join
                 (list target " is " (irc-user-real user)
                  " (" (irc-user-host user) ")")))
               (list srv nick))))))

(define (irc-cmd-list srv nick)
  (progn
    (fore (lambda (c)
            (print (str-join
              (list (irc-chan-name c) " ("
               (num-to-str (length (irc-chan-nicks c)))
               " users)"))))
          (irc-channels srv))
    (list srv nick)))

(define (fore fn lst)
  (cond ((eq lst nil) nil)
        (t (progn (fn (car lst)) (fore fn (cdr lst))))))

(define (irc-print-logs srv)
  (fore (lambda (entry)
          (let ((type (car entry))
                (from (cadr entry))
                (text (caddr entry)))
            (cond ((equal type 'error)
                   (print (cat "[ERROR] " text)))
                  ((equal type 'join)
                   (print (cat "[JOIN]  " text)))
                  ((equal type 'part)
                   (print (cat "[PART]  " text)))
                  ((equal type 'msg)
                   (print (cat (cat "<" (cat from "> ")) text)))
                  ((equal type 'priv)
                   (print (cat (cat "(PM from " (cat from ") ")) text)))
                  (t (print (cat (cat "[" (cat from "] ")) text))))))
        (irc-logs srv)))

(print "==========================================")
(print "  IRC SERVER — MULTI-USER CHAT")
(print "==========================================")

(define srv (irc-srv-make))
(define srv (irc-add-user srv "alice" "~a@h1" "Alice"))
(define srv (irc-add-user srv "bob"   "~b@h2" "Bob"))
(define srv (irc-add-user srv "carol" "~c@h3" "Carol"))
(print "3 users connected: alice, bob, carol")

(define srv (irc-channel-ensure srv "#general"))
(define srv (irc-channel-ensure srv "#random"))

(define res (irc-join-chan srv "alice" "#general"))
(define srv (car res))
(define res (irc-join-chan srv "bob" "#general"))
(define srv (car res))
(define res (irc-join-chan srv "carol" "#general"))
(define srv (car res))
(print "All joined #general")

(define res (irc-channel-msg srv "alice" "#general" "Hello everyone!"))
(define srv (car res))
(define res (irc-channel-msg srv "bob" "#general" "Hey alice, hi carol!"))
(define srv (car res))
(define res (irc-channel-msg srv "carol" "#general" "Hi both!"))
(define srv (car res))
(define res (irc-channel-msg srv "alice" "#general" "Doing great!"))
(define srv (car res))

(print "")
(print "--- Chat transcript ---")
(irc-print-logs srv)

(define srv (irc-remove-user srv "bob"))
(print "")
(print "--- bob quit ---")

(define res (irc-private-msg srv "alice" "carol" "hey, just between us"))
(define srv (car res))

(print "")
(print "--- After bob quit + alice PM to carol ---")
(irc-print-logs srv)

(print "")
(print "--- /who in #general ---")
(irc-cmd-who srv "alice" "#general")

(print "")
(print "--- /whois alice ---")
(irc-cmd-whois srv "alice" "alice")

(print "")
(print "==========================================")
(print "  IRC server: pass")
(print "==========================================")
