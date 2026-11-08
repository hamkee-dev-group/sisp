; ============================================================
; TEXT EDITOR — line-oriented editor for the SISP OS
;
; Inspired by Unix `ed`.  Operates on a buffer of lines, with
; commands for viewing, inserting, deleting, and saving to the
; filesystem.  Commands:
;   p [n]      — print line n (default: current)
;   P          — print all lines with numbers
;   i [text]   — insert text after current line
;   a [text]   — append text after current line
;   d [n]      — delete line n (default: current)
;   s /old/new — substitute on current line
;   w [file]   — write buffer to file
;   r [file]   — read file into buffer
;   q          — quit
;   =          — show line count
; ============================================================

(define (str-join parts)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (car parts) (str-join (cdr parts))))))

(define (rev lst)
  (labels (((go l a) (cond ((null l) a) (t (go (cdr l) (cons (car l) a)))))) (go lst nil)))

; ---- buffer: list of strings, line-number (1-based) ----------
(define (ed-make-buffer) (list nil 1))

(define (ed-lines buf) (car buf))
(define (ed-line   buf) (car (cdr buf)))

(define (ed-set-lines buf lines) (list lines (ed-line buf)))
(define (ed-goto buf n) (list (ed-lines buf) n))

(define (ed-get-line buf n)
  (let ((lines (ed-lines buf)))
    (cond ((or (< n 1) (> n (length lines))) nil)
          (t (nth (- n 1) lines)))))

(define (ed-insert buf n text)
  (let ((lines (ed-lines buf)))
    (ed-set-lines buf (list-insert lines (- n 1) text))))

(define (ed-delete buf n)
  (let ((lines (ed-lines buf)))
    (cond ((or (< n 1) (> n (length lines))) buf)
          (t (ed-set-lines buf (list-delete lines (- n 1)))))))

(define (list-insert lst idx val)
  (cond ((null lst) (list val))
        ((= idx 0) (cons val lst))
        (t (cons (car lst) (list-insert (cdr lst) (- idx 1) val)))))

(define (list-delete lst idx)
  (cond ((null lst) nil)
        ((= idx 0) (cdr lst))
        (t (cons (car lst) (list-delete (cdr lst) (- idx 1))))))

; ---- commands ------------------------------------------------
(define (ed-cmd-p buf arg)
  (let ((n (cond ((eq arg nil) (ed-line buf)) (t arg))))
    (let ((line (ed-get-line buf n)))
      (cond ((eq line nil) (print "?") buf)
            (t (print line) (ed-goto buf n))))))

(define (ed-cmd-P buf)
  (labels (((show lines i)
      (cond ((null lines) nil)
            (t (progn (print (str-join (list (num-to-str i) ": " (car lines))))
                      (show (cdr lines) (+ i 1)))))))
    (show (ed-lines buf) 1))
  buf)

(define (ed-cmd-i buf text)
  (let* ((n (ed-line buf))
         (new (ed-insert buf (+ n 1) text)))
    (ed-goto new (+ n 1))))

(define (ed-cmd-a buf text)
  (let* ((n (ed-line buf))
         (new (ed-insert buf n text)))
    (ed-goto new n)))

(define (ed-cmd-d buf arg)
  (let ((n (cond ((eq arg nil) (ed-line buf)) (t arg))))
    (let ((new (ed-delete buf n)))
      (let ((max (length (ed-lines new))))
        (ed-goto new (cond ((> n max) max) (t n)))))))

(define (ed-cmd-s buf old new-str)
  (let* ((n (ed-line buf))
         (line (ed-get-line buf n)))
    (cond ((eq line nil) (print "?") buf)
          (t (let ((replaced (str-replace line old new-str)))
               (ed-set-lines buf (list-replace (ed-lines buf) (- n 1) replaced)))))))

(define (str-replace s old new)
  (let ((pos (str-find s old)))
    (cond ((eq pos nil) s)
          (t (cat (cat (substr 0 pos s) new)
                  (substr (+ pos (length old))
                          (- (length s) (+ pos (length old))) s))))))

(define (list-replace lst idx val)
  (cond ((null lst) nil)
        ((= idx 0) (cons val (cdr lst)))
        (t (cons (car lst) (list-replace (cdr lst) (- idx 1) val)))))

(define (ed-cmd-= buf) (print (length (ed-lines buf))) buf)

; ---- session -------------------------------------------------
(define (ed-session)
  (print "=== SISP ED Editor ===")
  (print "type 'h' for help, 'q' to quit")
  (print "")

  ; Initial content
  (define buf (ed-make-buffer))
  (define buf (ed-cmd-i buf "hello from sisp"))
  (define buf (ed-cmd-a buf "this is a text editor"))
  (define buf (ed-cmd-a buf "built in pure sisp"))
  (define buf (ed-cmd-a buf "line 4 of the buffer"))

  (print "--- buffer ---")
  (ed-cmd-P buf)
  (print "")

  (print "--- insert at line 2 ---")
  (define buf (ed-goto buf 2))
  (define buf (ed-cmd-i buf "INSERTED LINE"))
  (ed-cmd-P buf)
  (print "")

  (print "--- delete line 3 ---")
  (define buf (ed-cmd-d buf 3))
  (ed-cmd-P buf)
  (print "")

  (print "--- substitute 'sisp' -> 'SISP' on line 1 ---")
  (define buf (ed-goto buf 1))
  (define buf (ed-cmd-s buf "sisp" "SISP"))
  (ed-cmd-P buf)
  (print "")

  (print "--- line count ---")
  (ed-cmd-= buf)
  (print "")

  (print "=== done ==="))

(ed-session)
