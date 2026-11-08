; ============================================================
; LOGIN SYSTEM — MULTI-USER AUTH FOR THE SISP OS
;
; Manages user accounts with passwords (plaintext in this demo),
; login sessions, and a permissions system.  Users have roles:
; admin, operator, guest.  Each role has a set of allowed cmds.
; ============================================================

; ---- user database ------------------------------------------
(define users nil)   ; alist: (username . (password role))

(define (user-add name password role)
  (setq users (cons (list name password role) users)))

(define (user-find name)
  (find (lambda (u) (equal (car u) name)) users))

(define (user-del name)
  (setq users (remove-if (lambda (u) (equal (car u) name)) users)))

(define (remove-if pred lst)
  (cond ((null lst) nil)
        ((pred (car lst)) (remove-if pred (cdr lst)))
        (t (cons (car lst) (remove-if pred (cdr lst))))))

; ---- role permissions ---------------------------------------
(define roles
  (list (list 'admin   (list "ls" "cat" "edit" "run" "ps" "kill" "adduser" "shutdown"))
        (list 'operator (list "ls" "cat" "run" "ps"))
        (list 'guest    (list "ls" "cat"))))

(define (role-allowed role command)
  (let ((r (assoc role roles)))
    (cond ((eq r nil) nil)
          (t (contains command (cadr r))))))

; ---- session management -------------------------------------
(define sessions nil)  ; alist: (username . role)

(define (login username password)
  (let ((u (user-find username)))
    (cond ((eq u nil) (print "  login: user not found") nil)
          ((not (equal (cadr u) password)) (print "  login: wrong password") nil)
          (t (setq sessions (cons (list username (caddr u)) sessions))
             (print (str-join (list "  welcome, " username " (" (sym->str (caddr u)) ")")))
             (caddr u)))))

(define (logout username)
  (setq sessions (remove-if (lambda (s) (equal (car s) username)) sessions))
  (print (str-join (list "  goodbye, " username))))

(define (whoami username)
  (let ((s (find (lambda (s) (equal (car s) username)) sessions)))
    (cond ((eq s nil) nil) (t (cadr s)))))

(define (may-i username command)
  (let ((role (whoami username)))
    (cond ((eq role nil) nil)
          (t (role-allowed role command)))))

(define (sym->str s)
  (cond ((eq s 'admin) "admin")
        ((eq s 'operator) "operator")
        ((eq s 'guest) "guest")
        (t "unknown")))

; ---- helpers ------------------------------------------------
(define (str-join parts)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (car parts) (str-join (cdr parts))))))

(define (bool-str b) (cond (b "yes") (t "no")))

; ---- demo ---------------------------------------------------
(print "=== SISP Login System ===")
(print "")

; Create users
(user-add "root" "secret123" 'admin)
(user-add "alice" "alicepw"  'operator)
(user-add "bob" "bobpw"      'guest)

(print "Users: root(admin), alice(operator), bob(guest)")
(print "")

; Login as each
(print "--- Login ---")
(define role-root (login "root" "secret123"))
(define role-alice (login "alice" "alicepw"))
(define role-bob (login "bob" "bobpw"))
(login "evil" "wrong")
(print "")

; Check permissions
(print "--- Permissions ---")
(print (str-join (list "  root  can run?  " (bool-str (may-i "root" "run")))))
(print (str-join (list "  root  can kill? " (bool-str (may-i "root" "kill")))))
(print (str-join (list "  alice can kill? " (bool-str (may-i "alice" "kill")))))
(print (str-join (list "  alice can run?  " (bool-str (may-i "alice" "run")))))
(print (str-join (list "  bob   can run?  " (bool-str (may-i "bob" "run")))))
(print (str-join (list "  bob   can ls?   " (bool-str (may-i "bob" "ls")))))
(print "")

; Logout
(print "--- Logout ---")
(logout "alice")
(print (str-join (list "  alice can run? " (bool-str (may-i "alice" "run")))))
(print "")

(print "=== Done ===")
