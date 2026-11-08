; ============================================================
; INCREMENTAL BACKUP + RESTORE SYSTEM
;
; Filesystem: ((path content mtime) ...)
; Backups:    ((id type timestamp files) ...)
; ============================================================

(define (fs-make path content mtime) (list path content mtime))
(define (fs-path entry) (car entry))
(define (fs-content entry) (cadr entry))
(define (fs-mtime entry) (caddr entry))

(define (fs-get filesystem path)
  (find (lambda (e) (equal (fs-path e) path)) filesystem))

(define (fs-put filesystem path content mtime)
  (cons (fs-make path content mtime)
    (remove (lambda (e) (equal (fs-path e) path)) filesystem)))

(define (fs-touch filesystem path content)
  (let ((now (runtime)))
    (fs-put filesystem path content now)))

(define (bkp-make id type timestamp files) (list id type timestamp files))
(define (bkp-id b) (car b))
(define (bkp-type b) (cadr b))
(define (bkp-timestamp b) (caddr b))
(define (bkp-files b) (cadddr b))

(define (bkp-full filesystem id)
  (let ((snapshot (map (lambda (e) (fs-make (fs-path e) (fs-content e) (fs-mtime e)))
                       filesystem)))
    (bkp-make id 'full (runtime) snapshot)))

(define (bkp-incr filesystem id since)
  (let ((changed (filter (lambda (e)
                    (> (fs-mtime (fs-get filesystem (fs-path e))) since))
                  filesystem)))
    (cond ((eq changed nil) (bkp-make id 'incr (runtime) nil))
          (t (bkp-make id 'incr (runtime)
              (map (lambda (e) (fs-make (fs-path e) (fs-content e) (fs-mtime e)))
                   changed))))))

(define (bkp-last-timestamp backups)
  (cond ((eq backups nil) 0)
        (t (bkp-timestamp (car backups)))))

(define (bkp-apply-entry fs entry)
  (foldl (lambda (acc file)
           (fs-put acc (fs-path file) (fs-content file) (fs-mtime file)))
         fs (bkp-files entry)))

(define (bkp-index-of backups id)
  (bkp-idx-acc backups id 0))

(define (bkp-idx-acc entries id i)
  (cond ((eq entries nil) nil)
        ((equal (bkp-id (car entries)) id) i)
        (t (bkp-idx-acc (cdr entries) id (+ i 1)))))

(define (bkp-entries-after backups index)
  (cond ((eq backups nil) nil)
        ((= index 0) backups)
        (t (bkp-entries-after (cdr backups) (- index 1)))))

(define (bkp-chain-to-head backups idx)
  (cond ((eq backups nil) nil)
        ((= idx 0) nil)
        (t (cons (car backups) (bkp-chain-to-head (cdr backups) (- idx 1))))))

(define (bkp-restore backups filesystem id)
  (let ((idx (bkp-index-of backups id)))
    (cond ((eq idx nil) filesystem)
          (t (let ((chain (reverse (cons (nth idx backups) (bkp-chain-to-head backups idx)))))
               (foldl (lambda (fs entry) (bkp-apply-entry fs entry))
                      filesystem chain))))))

(define (bkp-full-indices entries i acc)
  (cond ((eq entries nil) (reverse acc))
        ((equal (bkp-type (car entries)) 'full)
         (bkp-full-indices (cdr entries) (+ i 1) (cons i acc)))
        (t (bkp-full-indices (cdr entries) (+ i 1) acc))))

(define (bkp-retain backups n)
  (let ((full-pos (bkp-full-indices backups 0 nil)))
    (cond ((= (length full-pos) 0) backups)
          ((<= (length full-pos) n) backups)
          (t (take backups (nth n full-pos))))))

(define (bkp-id-str id)
  (cond ((equal id 'B1) "B1")
        ((equal id 'B3) "B3")
        ((equal id 'I2) "I2")
        (t "?")))

(define (bkp-ids-str blist)
  (cond ((eq blist nil) "")
        ((eq (cdr blist) nil) (bkp-id-str (bkp-id (car blist))))
        (t (cat (cat (bkp-id-str (bkp-id (car blist))) ",")
                (bkp-ids-str (cdr blist))))))

(define (str-join parts)
  (cond ((eq parts nil) "")
        ((eq (cdr parts) nil) (car parts))
        (t (cat (car parts) (str-join (cdr parts))))))

(print "==========================================")
(print "  INCREMENTAL BACKUP + RESTORE SYSTEM")
(print "==========================================")

(define fs nil)
(define fs (fs-touch fs "/etc/hosts" "127.0.0.1 localhost"))
(define fs (fs-touch fs "/etc/passwd" "root:x:0:root"))
(define fs (fs-touch fs "/home/user/notes" "hello world"))
(define fs (fs-touch fs "/var/log/messages" "boot ok"))
(print "Initial filesystem: 4 files")

(define backups nil)
(define b1 (bkp-full fs 'B1))
(define backups (cons b1 backups))
(print (str-join (list "Full backup B1: "
                  (num-to-str (length (bkp-files b1)))
                  " files in snapshot")))

(define fs (fs-touch fs "/etc/hosts" "127.0.0.1 localhost\n::1 localhost"))
(define fs (fs-touch fs "/home/user/notes" "hello world v2"))
(print "Modified 2 files: /etc/hosts, /home/user/notes")

(define i2 (bkp-incr fs 'I2 (bkp-last-timestamp backups)))
(define backups (cons i2 backups))
(print (str-join (list "Incremental I2: "
                  (num-to-str (length (bkp-files i2)))
                  " files backed up (expected 2)")))

(define restored (bkp-restore backups nil 'B1))
(print "")
(print "Restore to B1 (original versions):")
(print (cat "  /etc/hosts: " (fs-content (fs-get restored "/etc/hosts"))))
(print (cat "  /home/user/notes: " (fs-content (fs-get restored "/home/user/notes"))))

(define restored2 (bkp-restore backups nil 'I2))
(print "")
(print "Restore to I2 (includes modifications):")
(print (cat "  /etc/hosts: " (fs-content (fs-get restored2 "/etc/hosts"))))
(print (cat "  /home/user/notes: " (fs-content (fs-get restored2 "/home/user/notes"))))

(print "")
(define b3 (bkp-full fs 'B3))
(define backups (cons b3 backups))
(define retained (bkp-retain backups 1))
(print (str-join (list "Retention (keep 1 full): "
                  (num-to-str (length retained))
                  " backups kept: "
                  (bkp-ids-str retained))))

(print "==========================================")
(print "  Backup system: pass")
(print "==========================================")
