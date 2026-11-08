; ============================================================
; name: aho-corasick
; subject: strings
; capability: multi-pattern search
; status: incomplete
; canonical-home: none (imperative automaton)
; notes: Global-state automaton; KMP matches are printed alongside.
; ============================================================
; ============================================================
; Aho-Corasick: Multi-Pattern String Matching
; Builds trie + failure links. Searches in O(n + total matches).
; Patterns: ["he","she","his","hers"]. Text: "ushers".
; Verify all matches found at correct positions.
; Compare with running KMP repeatedly.
; ============================================================

(print "=== Aho-Corasick ===")


; Equal-based alist lookup (assoc uses eql; str-to-list chars need equal)
(define (a-get al key)
  (cond ((eq al nil) nil)
        ((equal (caar al) key) (cdar al))
        (t (a-get (cdr al) key))))

; Global state for automaton
(setq *edges* nil)
(setq *outputs* nil)
(setq *failures* nil)
(setq *next-node* 0)

(define (new-node)
  (let ((id *next-node*))
    (setq *next-node* (+ *next-node* 1))
    (setq *edges* (cons (cons id nil) *edges*))
    (setq *outputs* (cons (cons id nil) *outputs*))
    (setq *failures* (cons (cons id nil) *failures*))
    id))

(define (get-edges node) (cdr (assoc node *edges*)))
(define (get-outputs node) (cdr (assoc node *outputs*)))
(define (get-failure node) (cdr (assoc node *failures*)))
(define (get-child node ch) (a-get (get-edges node) ch))

(define (set-outputs node val)
  (setq *outputs* (cons (cons node val) *outputs*)))
(define (set-failure node val)
  (setq *failures* (cons (cons node val) *failures*)))
(define (add-child node ch child)
  (setq *edges* (cons (cons node (cons (cons ch child) (get-edges node))) *edges*)))

(define (trie-insert node chars pat)
  (cond ((eq chars nil)
         (set-outputs node (cons pat (get-outputs node))))
        (t (let* ((ch (car chars)) (child (get-child node ch)))
             (cond ((eq child nil)
                    (let ((nc (new-node)))
                      (add-child node ch nc)
                      (trie-insert nc (cdr chars) pat)))
                   (t (trie-insert child (cdr chars) pat)))))))

(define (build-trie patterns)
  (setq *next-node* 0)
  (setq *edges* nil)
  (setq *outputs* nil)
  (setq *failures* nil)
  (let ((root (new-node)))
    (foldl (lambda (ignored pat)
             (trie-insert root (str-to-list pat) pat))
           nil patterns)
    root))

(define (find-fail-link fn ch)
  (cond ((= fn 0)
         (let ((c (get-child 0 ch)))
           (cond ((eq c nil) 0) (t c))))
        (t (let ((c (get-child fn ch)))
             (cond ((eq c nil)
                    (find-fail-link (get-failure fn) ch))
                   (t c))))))

(define (bfs-build q)
  (cond ((eq q nil) t)
        (t (let ((node (car q)) (rest (cdr q)))
             (let ((next
                     (foldl (lambda (q-acc pair)
                              (let* ((ch (car pair))
                                     (child (cdr pair))
                                     (f (find-fail-link (get-failure node) ch)))
                                (set-failure child f)
                                (set-outputs child
                                  (append (get-outputs child) (get-outputs f)))
                                (append q-acc (list child))))
                            rest (get-edges node))))
               (bfs-build next))))))

(define (build-failure-links root)
  (set-failure root 0)
  (foldl (lambda (ignored pair)
           (set-failure (cdr pair) root))
         nil (get-edges root))
  (let ((init-q (map cdr (get-edges root))))
    (bfs-build init-q))
  root)

(define (follow-link node ch)
  (let ((c (get-child node ch)))
    (cond ((eq c nil)
           (cond ((= node 0) 0)
                 (t (follow-link (get-failure node) ch))))
          (t c))))

(define (aho-search root text)
  (let ((chars (str-to-list text)))
    (reverse (aho-scan root chars 0 nil))))

(define (aho-scan node chars pos matches)
  (cond ((eq chars nil) matches)
        (t (let* ((ch (car chars))
                  (next (follow-link node ch))
                  (outs (get-outputs next)))
             (let ((new-matches
                     (foldl (lambda (acc pat)
                              (cons (list
                                (- (+ pos 1) (length (str-to-list pat)))
                                pat)
                                acc))
                            matches outs)))
               (aho-scan next (cdr chars) (+ pos 1) new-matches))))))

; ---------- KMP for verification ----------
(define (kmp-table pat)
  (kmp-t pat 2 0 (list -1 0)))
(define (kmp-t pat j k tab)
  (cond ((> j (length pat)) tab)
        ((equal (nth (- j 1) pat) (nth k pat))
         (kmp-t pat (+ j 1) (+ k 1) (append tab (list (+ k 1)))))
        ((> k 0) (kmp-t pat j (nth k tab) tab))
        (t (kmp-t pat (+ j 1) 0 (append tab (list 0))))))

(define (kmp-find-all txt pat)
  (let ((tl (str-to-list txt)) (pl (str-to-list pat)))
    (kmp-all tl pl (kmp-table pl) 0 0 nil)))

(define (kmp-all txt pat tab i j matches)
  (cond ((>= i (length txt)) (reverse matches))
        ((equal (nth i txt) (nth j pat))
         (cond ((= (+ j 1) (length pat))
                (kmp-all txt pat tab (+ i 1) 0
                         (cons (- i (- (length pat) 1)) matches)))
               (t (kmp-all txt pat tab (+ i 1) (+ j 1) matches))))
        ((= j 0) (kmp-all txt pat tab (+ i 1) 0 matches))
        (t (kmp-all txt pat tab i (nth j tab) matches))))

(define (kmp-matches-all txt pats)
  (foldl (lambda (acc pat)
           (let ((poss (kmp-find-all txt pat)))
             (foldl (lambda (a p) (cons (list p pat) a)) acc poss)))
         nil pats))

; ---------- Tests ----------
(define patterns (list "he" "she" "his" "hers"))
(define text "ushers")

(print "Patterns:") (print patterns)
(print "Text:") (print text)

(define root1 (build-trie patterns))
(setq root1 (build-failure-links root1))
(define matches (aho-search root1 text))

(print "Aho-Corasick matches:")
(print matches)

(check "three-matches" (= (length matches) 3))
(check "she-at-1"
  (any (lambda (m) (and (= (car m) 1) (equal (cadr m) "she"))) matches))
(check "he-at-2"
  (any (lambda (m) (and (= (car m) 2) (equal (cadr m) "he"))) matches))
(check "hers-at-2"
  (any (lambda (m) (and (= (car m) 2) (equal (cadr m) "hers"))) matches))

; Compare with KMP
(print "--- KMP verification ---")
(define kmp-r (kmp-matches-all text patterns))
(print "KMP matches:")
(print (sort (lambda (a b) (< (car a) (car b))) kmp-r))

; Two KMP-found positions, looked up in the AC results (the full KMP list
; above is printed for comparison, not compared programmatically)
(check "kmp-she-at-1-in-ac"
  (any (lambda (m) (and (= (car m) 1) (equal (cadr m) "she"))) matches))
(check "kmp-he-at-2-in-ac"
  (any (lambda (m) (and (= (car m) 2) (equal (cadr m) "he"))) matches))

; Second test
(define text2 "ahishers")
(define root2 (build-trie patterns))
(setq root2 (build-failure-links root2))
(define matches2 (aho-search root2 text2))
(print "--- Text: ahishers ---")
(print "AC matches:") (print matches2)
(define kmp2 (kmp-matches-all text2 patterns))
(print "KMP matches:")
(print (sort (lambda (a b) (< (car a) (car b))) kmp2))

(check "ahis-his-at-1"
  (any (lambda (m) (and (= (car m) 1) (equal (cadr m) "his"))) matches2))
(check "ahis-he-at-4"
  (any (lambda (m) (and (= (car m) 4) (equal (cadr m) "he"))) matches2))
(check "ahis-she-at-3"
  (any (lambda (m) (and (= (car m) 3) (equal (cadr m) "she"))) matches2))
(check "ahis-hers-at-4"
  (any (lambda (m) (and (= (car m) 4) (equal (cadr m) "hers"))) matches2))

(print "=== Aho-Corasick Done ===")
