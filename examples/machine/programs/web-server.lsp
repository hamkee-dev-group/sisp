; ============================================================
; HTTP/1.0 SERVER — Static files + Forth CGI
; ============================================================

; --- Simulated filesystem ----------------------------------------------------
(define (f-name f)    (nth 0 f))
(define (f-type f)    (nth 1 f))
(define (f-content f) (nth 2 f))
(define (f-kids f)    (nth 3 f))

(define (make-file name content)
  (list name 'file content nil))

(define (make-dir name kids)
  (list name 'dir "" kids))

(define (fs-find dir name)
  (find (lambda (k) (equal (f-name k) name)) (f-kids dir)))

; --- Document root -----------------------------------------------------------
(define docroot
  (make-dir "/"
    (list
      (make-file "index.html"
        "<html><body><h1>Hello</h1><p>SISP HTTP.</p></body></html>")
      (make-file "about.html"
        "<html><body><h1>About</h1></body></html>")
      (make-file "add.fs" "3 4 +")
      (make-file "math.fs" "5 6 * 2 +")
      (make-file "style.css" "body{font-family:monospace}")
      (make-dir "images"
        (list (make-file "logo.svg" "<svg></svg>"))))))

; --- String helpers -----------------------------------------------------------
(define (str-first-line s)
  (let ((pos (str-find s "\r\n")))
    (cond (pos (substr 0 pos s))
          (t s))))

(define (response-body resp)
  (let ((pos (str-find resp "\r\n\r\n")))
    (cond (pos (substr (+ pos 4) (- (length resp) (+ pos 4)) resp))
          (t ""))))

; --- Content-Type ------------------------------------------------------------
(define (file-extension path)
  (let ((dot (str-find path ".")))
    (cond (dot (substr (+ dot 1) (- (length path) (+ dot 1)) path))
          (t ""))))

(define (content-type path)
  (let ((ext (file-extension path)))
    (cond ((equal ext "html") "text/html")
          ((equal ext "css")  "text/css")
          ((equal ext "svg")  "image/svg+xml")
          ((equal ext "txt")  "text/plain")
          ((equal ext "fs")   "text/plain")
          (t "application/octet-stream"))))

; --- Path resolution ---------------------------------------------------------
(define (resolve-path root path)
  (cond ((equal path "/") root)
        (t (let ((parts (split-path path)))
             (cond (parts (resolve-parts root parts))
                   (t nil))))))

(define (split-path p)
  (cond ((equal p "") nil)
        ((equal (substr 0 1 p) "/")
         (split-path (substr 1 (- (length p) 1) p)))
        (t (let ((slash (str-find p "/")))
             (cond (slash (cons (substr 0 slash p)
                                (split-path (substr (+ slash 1)
                                              (- (length p) (+ slash 1)) p))))
                   (t (list p)))))))

(define (resolve-parts dir parts)
  (cond ((eq parts nil) dir)
        ((eq (cdr parts) nil)
         (fs-find dir (car parts)))
        (t (let ((next (fs-find dir (car parts))))
             (cond ((and next (equal (f-type next) 'dir))
                    (resolve-parts next (cdr parts)))
                   (t nil))))))

; --- HTTP parsing ------------------------------------------------------------
(define (parse-request req)
  (let ((lines (str-split req "\n")))
    (cond ((eq lines nil) (list "" "" nil))
          (t (let ((first (car lines)))
               (let ((tokens (str-split first " ")))
                 (cond ((< (length tokens) 2) (list "" "" nil))
                       (t (list (car tokens) (cadr tokens) nil)))))))))

(define (str-split s delim)
  (let ((pos (str-find s delim)))
    (cond (pos (cons (substr 0 pos s)
                     (str-split (substr (+ pos 1) (- (length s) (+ pos 1)) s)
                                delim)))
          ((equal s "") nil)
          (t (list s)))))

; --- Response building -------------------------------------------------------
(define (make-response status body ctype)
  (let ((sl (cat "HTTP/1.0 " status))
        (ct (cat "Content-Type: " ctype))
        (cl (cat "Content-Length: " (num-to-str (length body))))
        (cc "Connection: close"))
    (cat (cat (cat (cat (cat sl "\r\n") ct) (cat "\r\n" cl))
              (cat "\r\n" cc))
         (cat "\r\n\r\n" body))))

(define (resp-200 body ctype) (make-response "200 OK" body ctype))
(define (resp-404 path) (make-response "404 Not Found"
                          (cat "<html><body><h1>404</h1><p>" (cat path "</p></body></html>"))
                          "text/html"))
(define (resp-500 msg) (make-response "500 Internal Server Error"
                         (cat "<html><body><h1>500</h1><p>" (cat msg "</p></body></html>"))
                         "text/html"))

; --- Forth interpreter -------------------------------------------------------
(define (forth-tokenize s)
  (str-split s " "))

(define (forth-run source)
  (forth-eval (forth-tokenize source) nil))

(define (forth-eval toks stack)
  (cond ((eq toks nil)
         (cond ((eq stack nil) "0") (t (num-to-str (car stack)))))
        (t (let ((tok (car toks)) (r (cdr toks)))
             (let ((num (str-to-num tok)))
               (cond (num (forth-eval r (cons num stack)))
                     ((equal tok "+")
                      (forth-eval r (forth-op stack +)))
                     ((equal tok "-")
                      (forth-eval r (forth-op stack -)))
                     ((equal tok "*")
                      (forth-eval r (forth-op stack *)))
                     ((equal tok "/")
                      (forth-eval r (forth-op stack /)))
                     ((equal tok "dup")
                      (forth-eval r (cons (car stack) stack)))
                     ((equal tok "swap")
                      (forth-eval r (cons (cadr stack) (cons (car stack) (cddr stack)))))
                     ((equal tok "drop")
                      (forth-eval r (cdr stack)))
                     (t stack)))))))

(define (forth-op stack fun)
  (let ((b (car stack))
        (a (cadr stack)))
    (cons (fun a b) (cddr stack))))

; --- Handler -----------------------------------------------------------------
(define (handle-http docroot req-str)
  (let ((parsed (parse-request req-str)))
    (let ((method (car parsed))
          (path (cadr parsed)))
      (cond ((not (equal method "GET"))
             (resp-500 "Only GET supported"))
            ((or (equal path "") (equal path "/"))
             (let ((idx (fs-find docroot "index.html")))
               (cond (idx (resp-200 (f-content idx) (content-type "index.html")))
                     (t (resp-404 path)))))
            (t (let ((file (resolve-path docroot path)))
                 (cond ((eq file nil) (resp-404 path))
                       ((equal (f-type file) 'dir) (resp-404 path))
                       ((equal (file-extension (f-name file)) "fs")
                        (let ((result (forth-run (f-content file))))
                          (resp-200 result "text/plain")))
                       (t (resp-200 (f-content file) (content-type (f-name file)))))))))))

; ---- DEMO ==================================================================
(print "=============================================")
(print "  HTTP/1.0 Server with Forth CGI")
(print "=============================================")

(print "")
(print "GET /index.html:")
(print (str-first-line (handle-http docroot "GET /index.html HTTP/1.0")))

(print "")
(print "GET /missing:")
(print (str-first-line (handle-http docroot "GET /missing HTTP/1.0")))

(print "")
(print "GET /add.fs (3 4 +):")
(let ((resp (handle-http docroot "GET /add.fs HTTP/1.0")))
  (print (str-first-line resp))
  (print (cat "Body: " (response-body resp))))

(print "")
(print "GET /math.fs (5 6 * 2 +):")
(let ((resp (handle-http docroot "GET /math.fs HTTP/1.0")))
  (print (cat "Body: " (response-body resp))))

(print "")
(print "GET /about.html:")
(print (str-first-line (handle-http docroot "GET /about.html HTTP/1.0")))

(print "")
(print "GET /style.css:")
(print (str-first-line (handle-http docroot "GET /style.css HTTP/1.0")))

(print "")
(print "=============================================")
(print "  Web server demo complete")
(print "=============================================")
