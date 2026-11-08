; ============================================================
; name: b-tree
; subject: data-structure
; capability: balanced sorted map
; status: incomplete
; canonical-home: none
; notes: No delete. bt-validate checks key order, key counts (root
;        1..2t-1, other nodes t-1..2t-1) and equal leaf depth
;        (bt-same-depth-p); duplicate inserts are ignored (ins-sorted).
; ============================================================
; ============================================================
; 87 — B-trees (TAOCP Vol 3, 6.2.4)
; Minimum-degree 2 B-tree (2-3-4 tree). Insert, search, verify:
; all leaves at same depth, every non-root node has between t-1
; and 2t-1 keys, keys sorted within nodes and between children.
; Exhaustively test with all permutations of (1 2 3 4 5).
; ============================================================

(print "=== B-trees ===")

(define (test name ok)
  (cond (ok (print (list (quote pass) name)))
        (t (print (list (quote FAIL) name)))))

; ---------- B-tree node: (leaf keys children) ----------
(define bt-deg 2)
(define (bt-leaf nd) (car nd))
(define (bt-keys nd) (cadr nd))
(define (bt-children nd) (caddr nd))
(define (bt-set-keys nd v) (set-car (cdr nd) v))
(define (bt-set-children nd v) (set-car (cddr nd) v))
(define (mk-node leaf) (list leaf nil nil))

; ---------- helper: insert into sorted list (no duplicates) ----------
(define (ins-sorted keys key)
  (cond ((eq keys nil) (list key))
        ((= key (car keys)) keys)
        ((< key (car keys)) (cons key keys))
        (t (cons (car keys) (ins-sorted (cdr keys) key)))))

(define (ins-at lst pos x)
  (cond ((= pos 0) (cons x lst))
        (t (cons (car lst) (ins-at (cdr lst) (- pos 1) x)))))

; ---------- split child ----------
(define (split-child parent idx)
  (let* ((child (nth idx (bt-children parent)))
         (new-child (mk-node (bt-leaf child)))
         (keys (bt-keys child))
         (children (bt-children child))
         (mid-pos (- bt-deg 1))
         (mid-key (nth mid-pos keys)))
    (bt-set-keys child (take keys mid-pos))
    (bt-set-keys new-child (drop keys (+ mid-pos 1)))
    (cond ((not (bt-leaf child))
           (bt-set-children child (take children bt-deg))
           (bt-set-children new-child (drop children bt-deg))))
    (bt-set-children parent
      (ins-at (bt-children parent) (+ idx 1) new-child))
    (bt-set-keys parent
      (ins-at (bt-keys parent) idx mid-key))))

; ---------- find child index for a key ----------
(define (child-idx keys key idx)
  (cond ((eq keys nil) idx)
        ((< key (car keys)) idx)
        (t (child-idx (cdr keys) key (+ idx 1)))))

; ---------- insert into non-full node ----------
(define (ins-nonfull nd key)
  (cond ((bt-leaf nd) (progn (bt-set-keys nd (ins-sorted (bt-keys nd) key)) nil))
        (t (let ((idx (child-idx (bt-keys nd) key 0)))
          (let ((child (nth idx (bt-children nd))))
            (cond ((= (length (bt-keys child)) (- (* 2 bt-deg) 1))
                   (progn (split-child nd idx)
                          (ins-nonfull nd key)))
                  (t (ins-nonfull child key))))))))

; ---------- insert into tree ----------
(define (bt-insert root key)
  (cond ((eq root nil) (let ((nd (mk-node t))) (ins-nonfull nd key) nd))
        ((= (length (bt-keys root)) (- (* 2 bt-deg) 1))
         (let ((new-root (mk-node nil)))
           (bt-set-children new-root (list root))
           (split-child new-root 0)
           (ins-nonfull new-root key)
           new-root))
        (t (progn (ins-nonfull root key) root))))

; ---------- search ----------
(define (bt-search nd key)
  (cond ((eq nd nil) nil)
        (t (bt-search-kv nd (bt-keys nd) (bt-children nd) key 0))))

(define (bt-search-kv nd keys children key idx)
  (cond ((eq keys nil)
         (cond ((bt-leaf nd) nil) (t (bt-search (nth idx children) key))))
        ((= key (car keys)) (car keys))
        ((< key (car keys))
         (cond ((bt-leaf nd) nil) (t (bt-search (nth idx children) key))))
        (t (bt-search-kv nd (cdr keys) children key (+ idx 1)))))

; ---------- validation ----------
(define (bt-validate nd is-root)
  (and (keys-sorted (bt-keys nd))
       (kc-ok nd is-root)
       (cond ((bt-leaf nd) t)
             (t (and (children-ok (bt-keys nd) (bt-children nd))
                     (bt-same-depth-p nd)
                     (every (lambda (c) (bt-validate c nil))
                            (bt-children nd)))))))

(define (kc-ok nd is-root)
  (let ((n (length (bt-keys nd))))
    (cond (is-root (and (>= n 1) (<= n (- (* 2 bt-deg) 1))))
          (t (and (>= n (- bt-deg 1)) (<= n (- (* 2 bt-deg) 1)))))))

; every leaf at the same depth
(define (bt-same-depth-p nd)
  (let ((depths (bt-leaf-depths nd 0 nil)))
    (every (lambda (d) (= d (car depths))) depths)))

(define (bt-leaf-depths nd depth acc)
  (cond ((bt-leaf nd) (cons depth acc))
        (t (bt-leaf-depths-children (bt-children nd) depth acc))))

(define (bt-leaf-depths-children children depth acc)
  (cond ((null children) acc)
        (t (bt-leaf-depths-children (cdr children) depth
                                    (bt-leaf-depths (car children)
                                                    (+ depth 1) acc)))))

(define (children-ok keys children)
  (cond ((eq keys nil) t)
        (t (and (child-lt (car children) (car keys))
                (child-gt (cadr children) (car keys))
                (children-ok (cdr keys) (cdr children))))))

(define (child-lt child key)
  (cond ((eq child nil) t)
        (t (every (lambda (k) (< k key)) (all-keys child)))))

(define (child-gt child key)
  (cond ((eq child nil) t)
        (t (every (lambda (k) (> k key)) (all-keys child)))))

(define (all-keys nd)
  (cond ((bt-leaf nd) (bt-keys nd))
        (t (foldl (lambda (acc c) (append acc (all-keys c))) nil (bt-children nd)))))

(define (keys-sorted ks)
  (cond ((or (eq ks nil) (eq (cdr ks) nil)) t)
        ((> (car ks) (cadr ks)) nil)
        (t (keys-sorted (cdr ks)))))

; ---------- verify a permutation ----------
(define (check-perm perm)
  (ins-all perm nil))

(define (ins-all remaining tree)
  (cond ((eq remaining nil) t)
        (t (let ((nt (bt-insert tree (car remaining))))
          (and (bt-validate nt t)
               (ins-all (cdr remaining) nt))))))

; ---------- permutations ----------
(setq *perms* nil)

(define (build-perms)
  (setq *perms* (list (list)))
  (setq *perms* (add-all 1 *perms*))
  (setq *perms* (add-all 2 *perms*))
  (setq *perms* (add-all 3 *perms*))
  (setq *perms* (add-all 4 *perms*))
  (setq *perms* (add-all 5 *perms*))
  nil)

(define (add-all x perms)
  (add-all-acc x perms nil))

(define (add-all-acc x perms acc)
  (cond ((eq perms nil) acc)
        (t (add-all-acc x (cdr perms)
             (append (ins-everywhere x (car perms) 0 (length (car perms)) nil) acc)))))

(define (ins-everywhere x perm pos maxpos acc)
  (cond ((> pos maxpos) acc)
        (t (ins-everywhere x perm (+ pos 1) maxpos
             (cons (ins-at-perm perm pos x) acc)))))

(define (ins-at-perm lst pos x)
  (cond ((= pos 0) (cons x lst))
        (t (cons (car lst) (ins-at-perm (cdr lst) (- pos 1) x)))))

(print "building-permutations")
(build-perms)
(test "permutation-count-120" (= (length *perms*) 120))

; representative tests
(print "testing-representative")
(define r1 (check-perm (list 1 2 3 4 5)))
(define r2 (check-perm (list 5 4 3 2 1)))
(define r3 (check-perm (list 3 1 4 2 5)))
(define r4 (check-perm (list 2 4 1 5 3)))
(test "bt-sorted-ok" r1)
(test "bt-reverse-ok" r2)
(test "bt-mixed-1-ok" r3)
(test "bt-mixed-2-ok" r4)

; exhaustive
(define (check-all perms)
  (cond ((eq perms nil) t)
        ((check-perm (car perms)) (check-all (cdr perms)))
        (t nil)))

(print "checking-all-120")
(define ok (check-all *perms*))
(print (list (quote all-result) ok))
(test "all-120-btree-pass" ok)

; duplicate keys are not inserted twice
(define dup-tree (bt-insert (bt-insert (bt-insert nil 5) 5) 5))
(test "duplicate-insert-keeps-one-key"
      (= (length (all-keys dup-tree)) 1))
(test "duplicate-tree-still-valid"
      (bt-validate dup-tree t))

(print "=== B-trees Done ===")
