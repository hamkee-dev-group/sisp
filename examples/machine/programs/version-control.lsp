; ============================================================
; VERSION CONTROL — Mini git: content-addressed store,
; commits, branches, checkout, diff, log.
; ============================================================

; --- Simple rolling hash based on position and char comparison -----------
(define (hash-text s)
  (let ((chars (str-to-list s))
        (h0 0) (h1 0) (h2 0) (h3 0) (h4 0) (pos 0))
    (hash-hex (hash-loop chars pos h0 h1 h2 h3 h4))))

(define (hash-loop chars pos a b c d e)
  (cond ((eq chars nil)
         (mod (+ (+ (+ (+ (* a 1000003) (* b 1000033))
                      (* c 1000037))
                   (* d 1000039))
                (* e 1000041))
              1073741824))
        (t (let ((ch (car chars))
                 (v (+ pos 1)))
             (hash-loop (cdr chars)
                        (+ pos 1)
                        (mod (+ a (* v (ch-val ch 1))) 1073741824)
                        (mod (+ b (* v (ch-val ch 3))) 1073741824)
                        (mod (+ c (* v (ch-val ch 5))) 1073741824)
                        (mod (+ d (* v (ch-val ch 7))) 1073741824)
                        (mod (+ e (* v (ch-val ch 11))) 1073741824))))))

(define (ch-val ch mul)
  (cond ((equal ch " ") (* 1 mul))
        ((equal ch "a") (* 2 mul))
        ((equal ch "b") (* 3 mul))
        ((equal ch "c") (* 5 mul))
        ((equal ch "d") (* 7 mul))
        ((equal ch "e") (* 11 mul))
        ((equal ch "f") (* 13 mul))
        ((equal ch "g") (* 17 mul))
        ((equal ch "h") (* 19 mul))
        ((equal ch "i") (* 23 mul))
        ((equal ch "j") (* 29 mul))
        ((equal ch "k") (* 31 mul))
        ((equal ch "l") (* 37 mul))
        ((equal ch "m") (* 41 mul))
        ((equal ch "n") (* 43 mul))
        ((equal ch "o") (* 47 mul))
        ((equal ch "p") (* 53 mul))
        ((equal ch "q") (* 59 mul))
        ((equal ch "r") (* 61 mul))
        ((equal ch "s") (* 67 mul))
        ((equal ch "t") (* 71 mul))
        ((equal ch "u") (* 73 mul))
        ((equal ch "v") (* 79 mul))
        ((equal ch "w") (* 83 mul))
        ((equal ch "x") (* 89 mul))
        ((equal ch "y") (* 97 mul))
        ((equal ch "z") (* 101 mul))
        ((equal ch "0") (* 103 mul))
        ((equal ch "1") (* 107 mul))
        ((equal ch "2") (* 109 mul))
        ((equal ch "3") (* 113 mul))
        ((equal ch "4") (* 127 mul))
        ((equal ch "5") (* 131 mul))
        ((equal ch "6") (* 137 mul))
        ((equal ch "7") (* 139 mul))
        ((equal ch "8") (* 149 mul))
        ((equal ch "9") (* 151 mul))
        ((equal ch ".") (* 157 mul))
        ((equal ch ",") (* 163 mul))
        ((equal ch ":") (* 167 mul))
        ((equal ch ";") (* 173 mul))
        ((equal ch "-") (* 179 mul))
        ((equal ch "_") (* 181 mul))
        ((equal ch "/") (* 191 mul))
        ((equal ch "\n") (* 193 mul))
        ((equal ch "\r") (* 197 mul))
        (t (* (length ch) mul))))

(define (hash-hex n)
  (let* ((a (mod n 16))
         (n2 (floor (/ n 16)))
         (b (mod n2 16))
         (n3 (floor (/ n2 16)))
         (c (mod n3 16))
         (n4 (floor (/ n3 16)))
         (d (mod n4 16))
         (n5 (floor (/ n4 16)))
         (e (mod n5 16))
         (n6 (floor (/ n5 16)))
         (f (mod n6 16))
         (n7 (floor (/ n6 16)))
         (g (mod n7 16))
         (n8 (floor (/ n7 16)))
         (hx (mod n8 16)))
    (list-to-str (list (hex-d hx) (hex-d g) (hex-d f) (hex-d e)
                       (hex-d d) (hex-d c) (hex-d b) (hex-d a)))))

(define (hex4 n)
  (let* ((a (mod n 16))
         (n2 (floor (/ n 16)))
         (b (mod n2 16))
         (n3 (floor (/ n2 16)))
         (c (mod n3 16))
         (n4 (floor (/ n3 16)))
         (d (mod n4 16)))
    (list-to-str (list (hex-d d) (hex-d c) (hex-d b) (hex-d a)))))

(define (hex-d n)
  (cond ((= n 0) "0") ((= n 1) "1") ((= n 2) "2") ((= n 3) "3")
        ((= n 4) "4") ((= n 5) "5") ((= n 6) "6") ((= n 7) "7")
        ((= n 8) "8") ((= n 9) "9") ((= n 10) "a") ((= n 11) "b")
        ((= n 12) "c") ((= n 13) "d") ((= n 14) "e") ((= n 15) "f")
        (t "0")))

; --- Content-addressed store ------------------------------------------------
(define vc-store nil)  ; alist: (hash . object)

(define (vc-put hash obj)
  (let ((existing (assoc hash vc-store)))
    (cond (existing nil)
          (t (setq vc-store (cons (cons hash obj) vc-store))
             hash))))

(define (vc-get hash)
  (let ((e (assoc hash vc-store)))
    (cond (e (cdr e)) (t nil))))

; --- Blob: (blob contents) ---------------------------------------------------
(define (make-blob text)
  (let ((obj (list 'blob text)))
    (let ((h (hash-text text)))
      (vc-put h obj)
      h)))

; --- Tree: (tree ((name . hash) ...)) ----------------------------------------
(define (make-tree entries)
  (let ((obj (list 'tree entries)))
    (let ((h (hash-entries entries)))
      (vc-put h obj)
      h)))

(define (hash-entries entries)
  (cond ((eq entries nil) (hash-text ""))
        (t (hash-text
             (foldl (lambda (acc e)
                      (cat (cat (cat acc (car e)) ":") (cdr e)))
                    ""
                    entries)))))

; --- Commit: (commit parent tree msg time) -----------------------------------
(define (hash-commit parent tree msg ts)
  (hash-text (cat (cat (cat msg (num-to-str ts)) tree)
                  (cond (parent parent) (t "")))))

(define (make-commit parent tree-hash msg timestamp)
  (let ((obj (list 'commit parent tree-hash msg timestamp)))
    (let ((h (hash-commit parent tree-hash msg timestamp)))
      (vc-put h obj)
      h)))

; --- Branch: (name . commit-hash) --------------------------------------------
(define vc-branches nil)
(define vc-head nil)  ; current branch name

(define (vc-branch name commit-hash)
  (setq vc-branches (cons (cons name commit-hash)
                          (remove (lambda (p) (equal (car p) name)) vc-branches)))
  (setq vc-head name)
  commit-hash)

(define (vc-branch-ref name)
  (let ((p (assoc name vc-branches)))
    (cond (p (cdr p)) (t nil))))

; --- Init --------------------------------------------------------------------
(define (vc-init)
  (setq vc-store nil)
  (setq vc-branches nil)
  (setq vc-head nil)
  (let ((root-hash (make-tree nil)))
    (let ((root-commit (make-commit nil root-hash "initial commit" 0)))
      (vc-branch "main" root-commit)
      (print (cat "Initialized repo. Commit: " (short-hash root-commit)))
      root-commit)))

(define (short-hash h)
  (substr 0 8 h))

; --- Add file (create blob + tree with file) ---------------------------------
(define (vc-add filename text)
  (let ((blob-hash (make-blob text)))
    (let ((tree-hash (make-tree (list (cons filename blob-hash)))))
      (print (cat "Added " (cat filename (cat " -> " (short-hash blob-hash)))))
      (list tree-hash blob-hash))))

; --- Commit ------------------------------------------------------------------
(define (vc-commit tree-hash message)
  (let ((parent (vc-branch-ref vc-head))
        (ts (runtime)))
    (let ((commit-hash (make-commit parent tree-hash message ts)))
      (vc-branch vc-head commit-hash)
      (print (cat "Committed [" (cat vc-head (cat "] " (short-hash commit-hash)))))
      commit-hash)))

; --- Log ---------------------------------------------------------------------
(define (vc-log)
  (let ((hash (vc-branch-ref vc-head)))
    (cond (hash (vc-log-walk hash 0))
          (t (print "No commits")))))

(define (vc-log-walk h depth)
  (let ((obj (vc-get h)))
    (cond (obj
           (let ((parent (nth 1 obj))
                 (msg (nth 3 obj)))
             (print (cat (cat (cat (dots depth) (short-hash h)) " ") msg))
             (cond (parent (vc-log-walk parent (+ depth 1)))
                   (t nil))))
          (t nil))))

(define (dots n)
  (cond ((= n 0) "")
        (t (cat "  " (dots (- n 1))))))

; --- Checkout: restore files from a commit's tree ----------------------------
(define (vc-checkout-commit hash)
  (let ((obj (vc-get hash)))
    (cond (obj
           (let ((tree-hash (nth 2 obj)))
             (cond (tree-hash
                    (let ((tree (vc-get tree-hash)))
                      (cond (tree
                             (let ((files (vc-list-tree tree-hash)))
                               (print (cat "Checked out: " (short-hash hash)))
                               files)
                             )
                            (t (print "Tree not found") nil))))
                   (t (print "Empty tree") nil))))
          (t (print "Commit not found") nil))))

(define (vc-list-tree tree-hash)
  (let ((tree (vc-get tree-hash)))
    (cond (tree
           (let ((entries (nth 1 tree)))
             (cond ((eq entries nil) nil)
                   (t entries))))
          (t nil))))

(define (vc-tree-files hash)
  (let ((tree (vc-get hash)))
    (cond (tree (nth 1 tree))
          (t nil))))

; --- Get file content from a tree by name ------------------------------------
(define (vc-get-file tree-hash name)
  (let ((entries (vc-tree-files tree-hash)))
    (cond (entries
           (let ((pair (assoc name entries)))
             (cond (pair
                    (let ((blob (vc-get (cdr pair))))
                      (cond (blob (nth 1 blob))
                            (t nil))))
                   (t (print (cat "File not found: " name)) nil))))
          (t nil))))

; --- Diff: line-by-line comparison between two files -----------------------
(define (vc-diff-files text1 text2)
  (let ((lines1 (str-split-lines text1))
        (lines2 (str-split-lines text2)))
    (print "--- diff")
    (vc-diff-lines lines1 lines2 1 1)))

(define (str-split-lines s)
  (let ((pos (str-find s "~")))
    (cond (pos (cons (substr 0 pos s)
                     (str-split-lines (substr (+ pos 1) (- (length s) (+ pos 1)) s))))
          (t (list s)))))

(define (vc-diff-lines l1 l2 n1 n2)
  (cond ((and (eq l1 nil) (eq l2 nil)) nil)
        ((eq l1 nil)
         (print (cat "+" (cat (num-to-str n2) (cat ": " (car l2)))))
         (vc-diff-lines nil (cdr l2) n1 (+ n2 1)))
        ((eq l2 nil)
         (print (cat "-" (cat (num-to-str n1) (cat ": " (car l1)))))
         (vc-diff-lines (cdr l1) nil (+ n1 1) n2))
        ((equal (car l1) (car l2))
         (vc-diff-lines (cdr l1) (cdr l2) (+ n1 1) (+ n2 1)))
        (t (print (cat "-" (cat (num-to-str n1) (cat ": " (car l1)))))
           (print (cat "+" (cat (num-to-str n2) (cat ": " (car l2)))))
           (vc-diff-lines (cdr l1) (cdr l2) (+ n1 1) (+ n2 1)))))

; --- Diff between two commits ------------------------------------------------
(define (vc-diff-commits hash1 hash2)
  (let ((tree1 (nth 2 (vc-get hash1)))
        (tree2 (nth 2 (vc-get hash2))))
    (let ((files1 (vc-tree-files tree1))
          (files2 (vc-tree-files tree2)))
      (print (cat "Diff: " (cat (short-hash hash1) (cat ".." (short-hash hash2)))))
      (vc-diff-entries files1 files2))))

(define (vc-diff-entries e1 e2)
  (cond ((and (eq e1 nil) (eq e2 nil)) nil)
        ((eq e1 nil)
         (print (cat "+ added file: " (car (car e2))))
         nil)
        ((eq e2 nil)
         (print (cat "- removed file: " (car (car e1))))
         nil)
        (t (let ((name (car (car e1))))
             (let ((pair2 (assoc name e2)))
               (cond (pair2
                      (cond ((not (equal (cdr (car e1)) (cdr pair2)))
                             (let ((t1 (nth 1 (vc-get (cdr (car e1)))))
                                   (t2 (nth 1 (vc-get (cdr pair2)))))
                               (print (cat "--- " name))
                               (vc-diff-files t1 t2)))
                            (t nil)))
                     (t (print (cat "- " name)))))
             (vc-diff-entries (cdr e1) (remove (lambda (p) (equal (car p) name)) e2))))))

; ---- DEMO ==================================================================
(print "=============================================")
(print "  Mini Version Control (Content-Addressed)")
(print "=============================================")

(print "")
(print "--- Init repository ---")
(vc-init)

(print "")
(print "--- Add file and commit ---")
(let ((result (vc-add "hello.txt" "hello world~")))
  (let ((tree-hash (car result)))
    (vc-commit tree-hash "add hello.txt")))

(print "")
(print "--- Log ---")
(vc-log)

(print "")
(print "--- Modify and commit ---")
(let ((result (vc-add "hello.txt" "hello world~with more text~")))
  (let ((tree-hash (car result)))
    (vc-commit tree-hash "update hello.txt")))

(print "")
(print "--- Log (2 commits) ---")
(vc-log)

(print "")
(print "--- Checkout first commit ---")
(let ((main (vc-branch-ref "main")))
  (cond (main
         (let ((obj (vc-get main)))
           (cond (obj
                  (let ((p (nth 1 obj)))
                    (cond (p (vc-checkout-commit p))
                          (t (print "No parent")))))
                 (t nil))))
        (t nil)))

(print "")
(print "--- Diff between commits ---")
(let ((main (vc-branch-ref "main")))
  (cond (main
         (let ((obj (vc-get main)))
           (let ((p (nth 1 obj)))
             (cond (p (vc-diff-commits p main))
                   (t nil)))))
        (t nil)))

(print "")
(print "--- Checks ---")

; Hashing is content-addressed: same bytes in, same name out.
(check "identical content hashes identically"
       (equal (hash-text "hello world") (hash-text "hello world")))
(check "different content hashes differently"
       (not (equal (hash-text "hello world") (hash-text "hello worlds"))))
(check "hashes are 8 hex digits, so short-hash is the whole name"
       (and (= (length (hash-text "anything")) 8)
            (equal (short-hash (hash-text "anything")) (hash-text "anything"))))
(check "hashing is order sensitive"
       (not (equal (hash-text "ab") (hash-text "ba"))))

; Fresh repository for the rest of the checks.
(vc-init)
(check "a fresh repo has main pointing at the initial commit"
       (and (equal vc-head "main")
            (not (null (vc-get (vc-branch-ref "main"))))))

(define b1 (make-blob "line one~line two~"))
(check "a blob round-trips through the store"
       (equal (nth 1 (vc-get b1)) "line one~line two~"))
(check "storing the same blob twice reuses the same name"
       (equal (make-blob "line one~line two~") b1))
(check "the store keeps objects tagged by kind"
       (eq (car (vc-get b1)) 'blob))

(define tr1 (make-tree (list (cons "a.txt" b1))))
(check "a tree lists its entries"
       (equal (vc-tree-files tr1) (list (cons "a.txt" b1))))
(check "a file's contents are reachable through its tree"
       (equal (vc-get-file tr1 "a.txt") "line one~line two~"))
(check "a name that is not in the tree yields nothing"
       (null (vc-get-file tr1 "missing.txt")))

; Commits chain to their parent, and branches move with them.
(define c1 (make-commit (vc-branch-ref "main") tr1 "add a.txt" 1))
(vc-branch "main" c1)
(check "a commit records its tree, message and parent"
       (and (equal (nth 2 (vc-get c1)) tr1)
            (and (equal (nth 3 (vc-get c1)) "add a.txt")
                 (not (null (nth 1 (vc-get c1)))))))
(check "the branch ref follows the newest commit"
       (equal (vc-branch-ref "main") c1))
(check "an unknown branch has no ref" (null (vc-branch-ref "nope")))

(define b2 (make-blob "line one~line two changed~"))
(define tr2 (make-tree (list (cons "a.txt" b2))))
(define c2 (make-commit c1 tr2 "edit a.txt" 2))
(vc-branch "main" c2)
(check "a second commit points back at the first"
       (equal (nth 1 (vc-get c2)) c1))
(check "committing the same content twice keeps one blob"
       (equal (make-blob "line one~line two changed~") b2))
(check "creating a branch does not disturb the other one"
       (progn (vc-branch "topic" c1)
              (and (equal (vc-branch-ref "main") c2)
                   (equal (vc-branch-ref "topic") c1))))
(check "re-pointing a branch replaces rather than duplicates it"
       (progn (vc-branch "topic" c2)
              (and (equal (vc-branch-ref "topic") c2)
                   (= (count (lambda (p) (equal (car p) "topic")) vc-branches) 1))))

; Checkout and line splitting
(check "checkout of a commit returns that commit's files"
       (equal (vc-checkout-commit c1) (list (cons "a.txt" b1))))
(check "checkout of an unknown commit reports nothing"
       (null (vc-checkout-commit "deadbeef")))
(check "text splits into lines on the record separator"
       (equal (str-split-lines "one~two~") (list "one" "two" "")))
(check "text with no separator is a single line"
       (equal (str-split-lines "solo") (list "solo")))
(print "")

(print "=============================================")
(print "  Version control demo complete")
(print "=============================================")
