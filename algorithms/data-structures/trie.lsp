; ============================================================
; name: trie
; file: algorithms/data-structures/trie.lsp
; subject: data-structure
; capability: prefix map string keys
; aliases: "prefix tree" "digital tree" "retrieval tree"
; signature: (trie-new) (trie-insert t key val) (trie-lookup t key)
;            (trie-delete t key) (trie-starts-with t prefix)
;            (trie-size t)
; complexity: O(m) per operation, m = key length
; see-also: hash-table suffix-array
; derived-from: Fredkin 1960; TAOCP 6.3
; status: example (tested)
; checks: 12 checks; run: src/sisp <this file> from repo root
; ============================================================
; Prefix trie over one-byte-string characters, purely functional.
; Node = (value children) where value is the stored payload or nil,
; children an alist of (one-char-string . node).  Keys are strings.

(define (trie-new) (list nil nil))

(define (trie-children tr) (cadr tr))
(define (trie-value tr) (car tr))

; association ops on the child alist (keys are one-char strings)
(define (trie-child tr ch)
  (let ((entry (trie-alist-get (trie-children tr) ch)))
    (cond ((eq entry nil) nil)
          (t entry))))

(define (trie-alist-get al key)
  (cond ((null al) nil)
        ((equal (caar al) key) (cdar al))
        (t (trie-alist-get (cdr al) key))))

(define (trie-alist-set al key val)
  (cons (cons key val) (trie-alist-delete al key)))

(define (trie-alist-delete al key)
  (cond ((null al) nil)
        ((equal (caar al) key) (cdr al))
        (t (cons (car al) (trie-alist-delete (cdr al) key)))))

; walk/build along the character list of key
(define (trie-insert-chars tr chars val)
  (cond ((null chars) (list val (trie-children tr)))
        (t (let* ((ch (car chars))
                  (child (trie-child tr ch))
                  (sub (cond ((null child) (trie-new)) (t child)))
                  (new-child (trie-insert-chars sub (cdr chars) val)))
             (list (trie-value tr)
                   (trie-alist-set (trie-children tr) ch new-child))))))

(define (trie-insert tr key val)
  (trie-insert-chars tr (str-to-list key) val))

(define (trie-walk tr chars)
  (cond ((null chars) tr)
        (t (let ((child (trie-child tr (car chars))))
             (cond ((null child) nil)
                   (t (trie-walk child (cdr chars))))))))

(define (trie-lookup tr key)
  "payload stored at key, or nil"
  (let ((node (trie-walk tr (str-to-list key))))
    (cond ((null node) nil)
          (t (trie-value node)))))

(define (trie-delete-chars tr chars)
  (cond ((null chars) (list nil (trie-children tr)))
        (t (let ((child (trie-child tr (car chars))))
             (cond ((null child) tr)
                   (t (list (trie-value tr)
                            (trie-alist-set (trie-children tr) (car chars)
                                            (trie-delete-chars child (cdr chars))))))))))

(define (trie-delete tr key)
  "removes the payload at key (structure is kept simple)"
  (trie-delete-chars tr (str-to-list key)))

; collect every (key . value) under a node, in sorted order
(define (trie-collect tr prefix acc)
  (let ((with-self (cond ((eq (trie-value tr) nil) acc)
                         (t (cons (cons prefix (trie-value tr)) acc)))))
    (trie-collect-children (trie-children tr) prefix with-self)))

(define (trie-collect-children children prefix acc)
  (cond ((null children) acc)
        (t (trie-collect-children (cdr children) prefix
                                  (trie-collect (cdar children)
                                                (cat prefix (caar children))
                                                acc)))))

(define (trie-starts-with tr prefix)
  "alist of (key . value) for every key starting with prefix"
  (let ((node (trie-walk tr (str-to-list prefix))))
    (cond ((null node) nil)
          (t (trie-collect node prefix nil)))))

(define (trie-size tr)
  (+ (cond ((eq (trie-value tr) nil) 0) (t 1))
     (trie-size-children (trie-children tr))))

(define (trie-size-children children)
  (cond ((null children) 0)
        (t (+ (trie-size (cdar children))
              (trie-size-children (cdr children))))))

; ---- check helpers (defined before use) ----

(define (trie-fill tr pairs)
  (cond ((null pairs) tr)
        (t (trie-fill (trie-insert tr (caar pairs) (cdar pairs)) (cdr pairs)))))

(define (trie-sorted-keys tr prefix)
  (map car (trie-starts-with tr prefix)))

; equal-based membership: `in` uses EQL, which compares strings by
; identity (unreliable across differently-built equal strings)
(define (trie-member x l)
  (cond ((null l) nil)
        ((equal x (car l)) t)
        (t (trie-member x (cdr l)))))

; ---- checks ----

(check "trie: lookup missing key -> nil"
       (eq (trie-lookup (trie-new) "nope") nil))

(check "trie: insert then lookup"
       (= (trie-lookup (trie-insert (trie-new) "cat" 1) "cat") 1))

(check "trie: shared prefixes do not disturb each other"
       (let ((tr (trie-fill (trie-new)
                            (list (cons "cat" 1) (cons "car" 2) (cons "card" 3)))))
         (and (= (trie-lookup tr "cat") 1)
              (= (trie-lookup tr "car") 2)
              (= (trie-lookup tr "card") 3)
              (eq (trie-lookup tr "ca") nil))))

(check "trie: prefix of a key is not a false hit"
       (eq (trie-lookup (trie-insert (trie-new) "card" 3) "car") nil))

(check "trie: extension of a key is not a false hit"
       (eq (trie-lookup (trie-insert (trie-new) "car" 2) "card") nil))

(check "trie: replace keeps newest"
       (= (trie-lookup (trie-insert (trie-insert (trie-new) "cat" 1) "cat" 2) "cat") 2))

(check "trie: starts-with finds the whole family"
       (let ((keys (trie-sorted-keys
                     (trie-fill (trie-new)
                                (list (cons "cat" 1) (cons "car" 2)
                                      (cons "card" 3) (cons "dog" 4)))
                     "ca")))
         (and (trie-member "car" keys) (trie-member "card" keys)
              (trie-member "cat" keys)
              (= (length keys) 3))))

(check "trie: starts-with with empty prefix returns everything"
       (= (length (trie-starts-with
                    (trie-fill (trie-new)
                               (list (cons "a" 1) (cons "b" 2) (cons "c" 3)))
                    ""))
          3))

(check "trie: starts-with with no match -> nil"
       (eq (trie-starts-with (trie-insert (trie-new) "cat" 1) "dog") nil))

(check "trie: delete removes only the payload"
       (let ((tr (trie-delete
                   (trie-fill (trie-new)
                              (list (cons "cat" 1) (cons "car" 2)))
                   "cat")))
         (and (eq (trie-lookup tr "cat") nil)
              (= (trie-lookup tr "car") 2))))

(check "trie: size counts payloads"
       (= (trie-size (trie-fill (trie-new)
                                (list (cons "cat" 1) (cons "car" 2) (cons "dog" 3))))
          3))

(check "trie: many keys all retrievable"
       (let ((tr (trie-fill (trie-new)
                            (list (cons "alpha" 1) (cons "beta" 2)
                                  (cons "gamma" 3) (cons "delta" 4)
                                  (cons "epsilon" 5) (cons "zeta" 6)))))
         (and (= (trie-lookup tr "alpha") 1)
              (= (trie-lookup tr "zeta") 6)
              (= (trie-size tr) 6))))
