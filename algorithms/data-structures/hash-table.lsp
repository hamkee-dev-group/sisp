; ============================================================
; name: hash-table
; file: algorithms/data-structures/hash-table.lsp
; subject: data-structure
; capability: map lookup insert delete
; aliases: "hash map" "hashtable" "dictionary" "chaining"
; signature: (ht-make size) (ht-insert ht key val) (ht-lookup ht key)
;            (ht-delete ht key) (ht-size ht) (ht-keys ht)
; complexity: O(1) average lookup/insert, O(n) worst case
; see-also: alist bloom-filter
; status: example (tested)
; checks: 13 checks at bottom; run: src/sisp <this file> from repo root
; ============================================================
; A hash table as a vector of buckets with separate chaining, purely
; functional: the table is a list of buckets, each bucket a list of
; (key . value) pairs.  SISP has no bitwise ops (xor is boolean-only),
; so keys are hashed with djb2 (h = h*33 + byte), which needs only
; addition and multiplication.  Keys may be strings (hashed over bytes)
; or integers (mixed with a Knuth multiplicative constant).

(define (ht-djb2 str)
  (ht-djb2-walk (str-to-list str) 5381))

(define (ht-djb2-walk chars h)
  (cond ((null chars) (mod h 4294967296))
        (t (ht-djb2-walk (cdr chars)
                         (mod (+ (* h 33) (char-code (car chars)))
                              4294967296)))))

; Knuth's multiplicative mix for integer keys
(define (ht-int-hash k)
  (mod (* (mod k 4294967296) 2654435761) 4294967296))

(define (ht-hash key)
  (cond ((equal (typeof key) "string") (ht-djb2 key))
        ((equal (typeof key) "integer") (ht-int-hash key))
        (t (error "HT-HASH: UNSUPPORTED KEY TYPE" key))))

(define (ht-bucket-index key size)
  (mod (ht-hash key) size))

(define (ht-make size)
  (cond ((<= size 0) (error "HT-MAKE: SIZE MUST BE POSITIVE" size))
        (t (ht-make-buckets size nil))))

(define (ht-make-buckets n acc)
  (cond ((= n 0) acc)
        (t (ht-make-buckets (- n 1) (cons nil acc)))))

; functional list update: xs with position i replaced by v
(define (ht-list-set xs i v)
  (cond ((= i 0) (cons v (cdr xs)))
        (t (cons (car xs) (ht-list-set (cdr xs) (- i 1) v)))))

; bucket-local association ops (equal comparison)
(define (ht-bucket-lookup bucket key)
  (cond ((null bucket) nil)
        ((equal (caar bucket) key) (cdar bucket))
        (t (ht-bucket-lookup (cdr bucket) key))))

(define (ht-bucket-delete bucket key)
  (cond ((null bucket) nil)
        ((equal (caar bucket) key) (cdr bucket))
        (t (cons (car bucket) (ht-bucket-delete (cdr bucket) key)))))

(define (ht-lookup ht key)
  "value bound to key, or nil when absent"
  (ht-bucket-lookup (nth (ht-bucket-index key (length ht)) ht) key))

(define (ht-insert ht key val)
  "table with key bound to val, replacing any previous binding"
  (let* ((i (ht-bucket-index key (length ht)))
         (bucket (nth i ht)))
    (ht-list-set ht i
                 (cons (cons key val) (ht-bucket-delete bucket key)))))

(define (ht-delete ht key)
  "table with any binding for key removed"
  (let* ((i (ht-bucket-index key (length ht)))
         (bucket (nth i ht)))
    (ht-list-set ht i (ht-bucket-delete bucket key))))

(define (ht-bucket-count bucket)
  (cond ((null bucket) 0) (t (+ 1 (ht-bucket-count (cdr bucket))))))

(define (ht-size ht)
  "number of stored bindings"
  (foldl (lambda (acc bucket) (+ acc (ht-bucket-count bucket))) 0 ht))

(define (ht-keys ht)
  "all keys, bucket order"
  (foldl (lambda (acc bucket)
           (append acc (map car bucket)))
         nil ht))

; ---- check helpers (defined before use) ----

(define (ht-fill ht names i)
  (cond ((null names) ht)
        (t (ht-fill (ht-insert ht (car names) i) (cdr names) (+ i 1)))))

(define (ht-all-present ht names)
  (cond ((null names) t)
        ((eq (ht-lookup ht (car names)) nil) nil)
        (t (ht-all-present ht (cdr names)))))

(define (ht-drop-all ht names)
  (cond ((null names) ht)
        (t (ht-drop-all (ht-delete ht (car names)) (cdr names)))))

; equal-based string membership (EQL compares strings by identity)
(define (ht-member-str x l)
  (cond ((null l) nil)
        ((equal x (car l)) t)
        (t (ht-member-str x (cdr l)))))

; ---- checks ----

(check "hash-table: lookup missing key -> nil"
       (eq (ht-lookup (ht-make 8) "nope") nil))

(check "hash-table: insert then lookup"
       (= (ht-lookup (ht-insert (ht-make 8) "a" 1) "a") 1))

(check "hash-table: replace keeps newest"
       (= (ht-lookup (ht-insert (ht-insert (ht-make 8) "a" 1) "a" 2) "a") 2))

(check "hash-table: several keys in one table"
       (let ((ht (ht-insert (ht-insert (ht-insert (ht-make 4) "x" 1)
                                       "y" 2)
                            "z" 3)))
         (and (= (ht-lookup ht "x") 1)
              (= (ht-lookup ht "y") 2)
              (= (ht-lookup ht "z") 3))))

(check "hash-table: integer keys"
       (let ((ht (ht-insert (ht-insert (ht-make 8) 10 "ten") 20 "twenty")))
         (and (equal (ht-lookup ht 10) "ten")
              (equal (ht-lookup ht 20) "twenty"))))

(check "hash-table: delete removes the binding"
       (eq (ht-lookup (ht-delete (ht-insert (ht-make 8) "a" 1) "a") "a") nil))

(check "hash-table: delete of absent key is a no-op"
       (= (ht-lookup (ht-delete (ht-insert (ht-make 8) "a" 1) "b") "a") 1))

(check "hash-table: size counts bindings"
       (= (ht-size (ht-insert (ht-insert (ht-insert (ht-make 4) "a" 1)
                                         "b" 2)
                              "c" 3))
          3))

(check "hash-table: replace does not grow size"
       (= (ht-size (ht-insert (ht-insert (ht-make 4) "a" 1) "a" 2)) 1))

(check "hash-table: keys returns stored keys"
       (let ((ks (ht-keys (ht-insert (ht-insert (ht-make 4) "a" 1) "b" 2))))
         (and (ht-member-str "a" ks) (ht-member-str "b" ks)
              (= (length ks) 2))))

; collisions forced: table of size 1 puts every key in one bucket
(check "hash-table: size-1 table chains collisions correctly"
       (let ((ht (ht-insert (ht-insert (ht-insert (ht-make 1) "a" 1)
                                       "b" 2)
                            "c" 3)))
         (and (= (ht-lookup ht "a") 1)
              (= (ht-lookup ht "b") 2)
              (= (ht-lookup ht "c") 3)
              (= (ht-size ht) 3))))

(check "hash-table: many keys round-trip through delete"
       (let* ((names (list "k1" "k2" "k3" "k4" "k5" "k6" "k7" "k8"))
              (ht (ht-fill (ht-make 4) names 0)))
         (and (= (ht-size ht) 8)
              (ht-all-present ht names)
              (= (ht-size (ht-drop-all ht names)) 0))))

(check "hash-table: djb2 known value (\"a\" -> 0x2B606 = 177670)"
       (= (ht-djb2 "a") 177670))
