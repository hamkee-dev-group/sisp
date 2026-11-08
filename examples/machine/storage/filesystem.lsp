; ============================================================
; HIERARCHICAL FILE SYSTEM FOR THE SISP OS
;
; Builds on the OS's DISK (a flat assoc of name->value) to
; provide hierarchical directories, file creation/deletion,
; reading/writing, and listing.  Inodes store metadata.
;
; Path format: ("/" "usr" "bin" "sh") — list of directory names
; followed by the file name.  "/" is the root.
;
; Each inode: (name type content children) where
;   type is 'file or 'dir
;   content is arbitrary value (file body)
;   children is an alist of child inodes (for directories)
; ============================================================

; ---- inode and tree operations ------------------------------
(define (inode-name inode) (car inode))
(define (inode-type inode) (cadr inode))
(define (inode-body inode) (caddr inode))
(define (inode-kids inode) (cadddr inode))

(define (make-file name content)
  (list name 'file content nil))

(define (make-dir name)
  (list name 'dir nil nil))

(define (mk-root)
  (make-dir "/"))

; Look up a child by name in a directory's children alist.
(define (dir-lookup dir name)
  (assoc name (inode-kids dir)))

; Insert or replace a child inode in a directory.  Returns new dir.
(define (dir-insert dir child)
  (let ((name (inode-name child))
        (kids (inode-kids dir)))
    (list (inode-name dir) 'dir (inode-body dir)
          (alist-put name child kids))))

; Remove a child by name from a directory.
(define (dir-remove dir name)
  (let ((kids (inode-kids dir)))
    (list (inode-name dir) 'dir (inode-body dir)
          (alist-remove name kids))))

; ---- alist helpers -----------------------------------------
(define (alist-put key val alist)
  (cond ((eq alist nil) (list (list key val)))
        ((equal (caar alist) key)
         (cons (list key val) (cdr alist)))
        (t (cons (car alist) (alist-put key val (cdr alist))))))

(define (alist-remove key alist)
  (cond ((eq alist nil) nil)
        ((equal (caar alist) key) (cdr alist))
        (t (cons (car alist) (alist-remove key (cdr alist))))))

; ---- path resolution ---------------------------------------
; Follow a path from root, returning (inode . parent-dir) or nil.
; Path is ("/" "dir1" "dir2" ... "file").
(define (fs-resolve root path)
  (cond ((eq path nil) (cons root nil))
        ((equal (car path) "/")
         (fs-resolve root (cdr path)))
        (t (fs-resolve-step root path nil))))

(define (fs-resolve-step dir path parent)
  (cond ((eq (cdr path) nil)               ; last component
         (let ((child (dir-lookup dir (car path))))
           (cond ((eq child nil) nil)
                 (t (cons (cdr child) dir)))))
        (t (let ((child (dir-lookup dir (car path))))
             (cond ((eq child nil) nil)
                   ((eq (inode-type (cdr child)) 'dir)
                    (fs-resolve-step (cdr child) (cdr path) dir))
                   (t nil))))))

; ---- file system API ---------------------------------------
(define root-dir (mk-root))

; Create a file at path with content. Creates parent dirs if needed.
(define (fs-create-file root path content)
  (fs-mkfile root (clean-path path) content))

(define (clean-path p) (cond ((equal (car p) "/") (cdr p)) (t p)))

(define (fs-mkfile root path content)
  (cond ((eq (cdr path) nil)                        ; just a file name
         (dir-insert root (make-file (car path) content)))
        (t (let ((child (dir-lookup root (car path))))
             (cond ((eq child nil)                   ; mkdir if needed
                    (let ((new-dir (make-dir (car path))))
                      (let ((updated-subdir (fs-mkfile new-dir (cdr path) content)))
                        (dir-insert root updated-subdir))))
                   (t (let ((subdir (cdr child)))
                        (let ((updated-subdir (fs-mkfile subdir (cdr path) content)))
                          (dir-insert root updated-subdir)))))))))

; Read a file's content. Returns content or nil.
(define (fs-read root path)
  (let ((res (fs-resolve root (clean-path path))))
    (cond ((eq res nil) nil)
          ((eq (inode-type (car res)) 'file)
           (inode-body (car res)))
          (t nil))))

; List directory contents as a list of (name type) pairs.
(define (fs-ls root path)
  (let ((res (fs-resolve root (clean-path path))))
    (cond ((eq res nil) nil)
          (t (map (lambda (e)
                    (list (car e) (inode-type (cdr e))))
                  (inode-kids (car res)))))))

; Delete a file or empty directory.
(define (fs-delete root path)
  (let ((res (fs-resolve root (clean-path path))))
    (cond ((eq res nil) root)
          (t (let ((target (car res)) (parent (cdr res)))
               (cond ((eq parent nil) root)
                     (t (dir-remove parent (inode-name target)))))))))

; ---- demonstration ------------------------------------------
(define root (mk-root))
(define r1 (fs-create-file root '("/" "home" "user" "notes")
              "hello world"))
(define r2 (fs-create-file r1 '("/" "home" "user" "todo")
              "build a shell"))

(fs-read r2 '("/" "home" "user" "notes"))  ; => "hello world"
(fs-ls r2 '("/" "home" "user"))            ; => (("notes" file) ("todo" file))
(fs-ls r2 '("/" "home"))                   ; => (("user" dir))
