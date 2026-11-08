; Package manager: install, remove, update with dependency resolution.
; Registry entries: (name version deps files). Installed: list of names.

(define registry
  '((a 1 (b c) (a-file a-conf))
    (b 1 (d)   (b-file))
    (c 1 ()    (c-file))
    (d 1 ()    (d-file))
    (e 2 (f g) (e-file))
    (f 2 ()    (f-file))
    (g 2 (h)   (g-file))
    (h 2 ()    (h-file))))

(define installed nil)

(define (pm-find name)
  (let ((entry (find (lambda (p) (equal (car p) name)) registry)))
    entry))

(define (pm-installed-p name)
  (in name installed))

(define (pm-mark name)
  (setq installed (cons name installed)))

(define (pm-unmark name)
  (setq installed (remove (lambda (x) (equal x name)) installed)))

(define (pm-deps name)
  (let ((pkg (pm-find name)))
    (cond ((eq pkg nil) nil) (t (nth 2 pkg)))))

(define (pm-files name)
  (let ((pkg (pm-find name)))
    (cond ((eq pkg nil) nil) (t (nth 3 pkg)))))

(define (pm-version name)
  (let ((pkg (pm-find name)))
    (cond ((eq pkg nil) nil) (t (cadr pkg)))))

(define (pm-has-cycle names visited path)
  (cond ((null names) nil)
        ((in (car names) path) t)
        ((in (car names) visited) (pm-has-cycle (cdr names) visited path))
        (t (or (pm-has-cycle (pm-deps (car names))
                             (cons (car names) visited)
                             (cons (car names) path))
               (pm-has-cycle (cdr names) visited path)))))

(define (pm-topo names visited)
  (cond ((null names) nil)
        ((in (car names) visited) (pm-topo (cdr names) visited))
        (t (append (pm-topo (pm-deps (car names)) visited)
                   (cons (car names)
                         (pm-topo (cdr names) (cons (car names) visited)))))))

(define (pm-resolve-deps name)
  (cond ((eq (pm-find name) nil) nil)
        ((pm-has-cycle (list name) nil nil) nil)
        ; a package reachable by two different paths is emitted twice,
        ; so collapse to the first (earliest) occurrence of each
        (t (dedup (pm-topo (list name) nil)))))

(define (pm-download files)
  (cond ((null files) t)
        (t (progn (pm-download (cdr files)) t))))

(define (pm-install name)
  (cond ((pm-installed-p name)
         (print (cat "Already installed: " (str-name name)))
         nil)
        ((eq (pm-find name) nil)
         (print (cat "Not in registry: " (str-name name)))
         nil)
        (t (let ((order (pm-resolve-deps name)))
             (cond ((null order)
                    (print (cat "Dependency cycle: " (str-name name)))
                    nil)
                   (t (progn
                        (foreach (lambda (p)
                                   (cond ((not (pm-installed-p p))
                                          (progn (pm-download (pm-files p))
                                                 (pm-mark p)))))
                                 order)
                        (print (cat "Installed: " (str-name name)))
                        t)))))))

(define (pm-dependents-installed name)
  (let ((dependents (filter (lambda (pkg)
                              (in name (nth 2 pkg)))
                            registry)))
    (filter (lambda (dep)
              (and (pm-installed-p dep)
                   (not (equal dep name))))
            (map car dependents))))

(define (pm-remove name)
  (cond ((not (pm-installed-p name))
         (print (cat "Not installed: " (str-name name)))
         nil)
        (t (let ((blockers (pm-dependents-installed name)))
             (cond (blockers
                     (progn
                      (print (cat (cat "Cannot remove " (str-name name))
                                  ": blocked by:"))
                      (foreach (lambda (d)
                                 (print (cat "  " (str-name d))))
                               blockers)
                      nil))
                   (t (progn
                        (pm-unmark name)
                        (print (cat "Removed: " (str-name name)))
                        t)))))))

(define (pm-update name)
  (cond ((not (pm-installed-p name))
         (print (cat "Not installed: " (str-name name)))
         nil)
        ((eq (pm-find name) nil)
         (print (cat "Not in registry: " (str-name name)))
         nil)
        (t (progn
             (let ((deps (pm-deps name)))
               (pm-unmark name)
               (pm-remove-orphans deps (list name))
               (pm-install name))))))

(define (pm-remove-orphans deps keepers)
  (cond ((null deps) nil)
        (t (let ((dep (car deps)))
             (cond ((and (pm-installed-p dep)
                         (not (in dep keepers))
                         (null (pm-dependents-installed dep)))
                    (progn (pm-unmark dep)
                           (pm-remove-orphans (pm-deps dep) keepers)))))
           ; must stay inside the t-clause: as a sibling it would be read
           ; as another cond clause and the walk would stop after one dep
           (pm-remove-orphans (cdr deps) keepers))))

(define (pm-list-installed)
  (cond ((null installed) (print "No packages installed"))
        (t (progn
             (print "Installed packages:")
             (foreach (lambda (name)
                        (print (cat (cat "  " (str-name name))
                                    (cat " v" (num-to-str (pm-version name))))))
                      installed)))))

(define (str-name sym)
  (cond ((eq sym 'a) "A") ((eq sym 'b) "B") ((eq sym 'c) "C") ((eq sym 'd) "D")
        ((eq sym 'e) "E") ((eq sym 'f) "F") ((eq sym 'g) "G") ((eq sym 'h) "H")
        (t "?")))

; ---- demonstration ----
(print "=== PACKAGE MANAGER ===")

(print "-- Installing A (depends on B,C) --")
(pm-install 'a)
(pm-list-installed)

(print "")
(print "-- Installing E (depends on F,G; G depends on H) --")
(pm-install 'e)
(pm-list-installed)

(print "")
(print "-- Removing D (blocked: B depends on D, A depends on B) --")
(pm-remove 'd)

(print "")
(print "-- Removing A --")
(pm-remove 'a)

(print "")
(print "-- Removing B then D --")
(pm-remove 'b)
(pm-remove 'd)

(print "")
(print "-- Final state --")
(pm-list-installed)

(print "")
(print "--- Checks ---")

; Registry accessors
(check "registry lookup returns the whole entry"
       (equal (pm-find 'a) '(a 1 (b c) (a-file a-conf))))
(check "unknown packages are not in the registry" (null (pm-find 'zz)))
(check "deps, files and version are read off the entry"
       (and (equal (pm-deps 'a) '(b c))
            (and (equal (pm-files 'g) '(g-file)) (= (pm-version 'e) 2))))

; Dependency resolution
(check "resolution lists dependencies before dependents"
       (equal (pm-resolve-deps 'a) '(d b c a)))
(check "resolution follows transitive dependencies"
       (equal (pm-resolve-deps 'e) '(f h g e)))
(check "a leaf package resolves to just itself"
       (equal (pm-resolve-deps 'd) '(d)))
(check "resolution of an unknown package is empty"
       (null (pm-resolve-deps 'zz)))

; A diamond must not list the shared dependency twice.
(setq registry (cons '(p 1 (q r) ()) (cons '(q 1 (s) ()) (cons '(r 1 (s) ())
                (cons '(s 1 () ()) registry)))))
(check "a diamond dependency is emitted once, before both users"
       (equal (pm-resolve-deps 'p) '(s q r p)))

; Cycle detection
(setq registry (cons '(x 1 (y) ()) (cons '(y 1 (x) ()) registry)))
(check "a dependency cycle is detected" (pm-has-cycle '(x) nil nil))
(check "an acyclic package is not reported as cyclic"
       (not (pm-has-cycle '(a) nil nil)))
(check "a cyclic package refuses to resolve" (null (pm-resolve-deps 'x)))

; Install / remove, from a clean slate
(setq installed nil)
(pm-install 'a)
(check "installing pulls in the whole dependency closure"
       (and (pm-installed-p 'a)
            (and (pm-installed-p 'b) (and (pm-installed-p 'c) (pm-installed-p 'd)))))
(check "installing twice is refused" (null (pm-install 'a)))
(check "installing an unknown package is refused" (null (pm-install 'zz)))
(check "a package with installed dependents cannot be removed"
       (and (null (pm-remove 'd)) (pm-installed-p 'd)))
(check "dependents-installed names the blockers"
       (equal (pm-dependents-installed 'd) '(b)))
(check "removing a leaf dependent succeeds"
       (and (pm-remove 'a) (not (pm-installed-p 'a))))
(check "removing something not installed is refused" (null (pm-remove 'a)))

; Update walks the whole dependency list, not just its first entry.
(setq installed nil)
(pm-install 'a)
(check "update reinstalls the package and its closure"
       (and (pm-update 'a)
            (and (pm-installed-p 'a)
                 (and (pm-installed-p 'b) (and (pm-installed-p 'c) (pm-installed-p 'd))))))
(check "update of a package that is not installed is refused"
       (null (pm-update 'e)))
(print "")
(print "=== Package manager checks complete ===")
