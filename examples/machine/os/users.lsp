; ============================================================
; USERS + LOGIN — multi-user auth on the VFS
;
; Accounts live in /etc/passwd on the disk, one line per user:
;   name:password:role        (role: root | user | guest)
; A session is just the logged-in username in *session*.
; The computer's shell gates on login; sensitive operations
; (user admin, disk format, shutdown) require the root role.
; ============================================================

(define *session* nil)

; ---- small string helpers ------------------------------------
; split-on / split-lines come from sisp-unix.lsp

; ---- password database ---------------------------------------
(define (pw-parse s)
  ; -> alist (name . (pw role))
  (map (lambda (line)
         (let ((parts (split-on line ":")))
           (cons (car parts) (list (cadr parts) (caddr parts)))))
       (split-lines s)))
(define (pw-read)
  (let ((s (kf-read "/etc/passwd")))
    (cond ((null s) nil) (t (pw-parse s)))))
(define (pw-write db)
  (kf-write "/etc/passwd"
            (str-join
              (map (lambda (e)
                     (str-join (list (car e) (car (cdr e)) (cadr (cdr e))) ":"))
                   db)
              (code-char 10))))
(define (user-find name)
  (let ((e (assoc name (pw-read) (quote equal))))
    (cond ((null e) nil) (t (cdr e)))))

(define (user-add name pw role)
  (cond ((not (equal *session* "root")) "user-add: only root")
        ((user-find name) "user-add: user exists")
        (t (let ((db (pw-read)))
             (pw-write (append db (list (cons name (list pw role)))))
             (str-join (list "user " name " added (" role ")") "")))))
(define (user-del name)
  (cond ((not (equal *session* "root")) "user-del: only root")
        ((equal name "root") "user-del: cannot remove root")
        (t (pw-write (remove (lambda (e) (equal (car e) name)) (pw-read)))
           (str-join (list "user " name " removed") ""))))

; ---- session ---------------------------------------------------
(define (login name pw)
  (let ((u (user-find name)))
    (cond ((null u) (str-join (list "login: " name ": no such user") ""))
          ((not (equal (car u) pw)) "login: wrong password")
          (t (setq *session* name)
             (str-join (list "welcome, " name) "")))))
(define (logout)
  (let ((s *session*))
    (setq *session* nil)
    (cond ((null s) "not logged in")
          (t (str-join (list "goodbye, " s) "")))))
(define (whoami)
  (cond ((null *session*) "nobody") (t *session*)))
(define (role-of name)
  (let ((u (user-find name)))
    (cond ((null u) "guest") (t (cadr u)))))
(define (require-login)
  (cond ((null *session*) nil) (t t)))
(define (require-root)
  (and (require-login) (equal (role-of *session*) "root")))

; seed the password file on a fresh disk
(define (users-seed)
  (cond ((null (fs-resolve-ino "/etc" FS-ROOT)) (kf-mkdir "/etc")))
  (cond ((null (fs-resolve-ino "/etc/passwd" FS-ROOT))
         (kf-create "/etc/passwd")
         (kf-write "/etc/passwd"
                   (cat (cat "root:sisp:root" nl) (cat "guest:guest:guest" nl))))))
