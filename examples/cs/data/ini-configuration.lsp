; INI-style configuration parser handles section headers and key-value lines.
(define (ini-split line delimiter)
  (let ((index (str-find line delimiter)))
    (list (substr 0 index line)
      (substr (+ index 1) (- (length line) index 1) line))))
(define (ini-section-line-p line)
  (and (> (length line) 1) (equal (substr 0 1 line) "[")
    (equal (substr (- (length line) 1) 1 line) "]")))
(define (ini-section-name line) (substr 1 (- (length line) 2) line))
(define (ini-parse-lines lines section entries)
  (cond ((eq lines nil) (reverse entries))
        ((ini-section-line-p (car lines))
         (ini-parse-lines (cdr lines) (ini-section-name (car lines)) entries))
        (t (let ((pair (ini-split (car lines) "=")))
          (ini-parse-lines (cdr lines) section
            (cons (list section (car pair) (cadr pair)) entries))))))
(define (ini-parse lines) (ini-parse-lines lines "global" nil))
(define (ini-get config section key default)
  (let ((entry (find (lambda (item) (and (equal (car item) section)
    (equal (cadr item) key))) config)))
    (cond ((eq entry nil) default) (t (caddr entry)))))
(define ini-demo (ini-parse '("name=sisp" "[server]" "port=8080" "mode=local")))
(equal ini-demo '(("global" "name" "sisp") ("server" "port" "8080") ("server" "mode" "local"))) ; => t
(ini-get ini-demo "server" "port" "0") ; => "8080"
(ini-get ini-demo "server" "missing" "default") ; => "default"
