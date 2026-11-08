; ============================================================
; name: lru-cache
; file: algorithms/data-structures/lru-cache.lsp
; subject: data-structure
; capability: bounded cache eviction
; aliases: "lru" "least recently used cache"
; signature: (lru-make capacity) (lru-get c key) (lru-put c key val)
;            (lru-size c) (lru-keys c)
; complexity: O(n) per operation (assoc-list model; the production
;             version pairs a hash table with a doubly linked list)
; see-also: hash-table queue
; derived-from: none; examples/cs/data/persistent-cache.lsp has a
;               heavier TTL+LRU implementation for programs
; status: example (tested)
; checks: 11 checks; run: src/sisp <this file> from repo root
; ============================================================
; LRU cache as an association list ordered by recency: the most
; recently touched binding is at the front, the least at the back.
; get moves the hit to the front; put inserts or refreshes at the front
; and evicts from the back when over capacity.  Keys compared with equal.

(define (lru-make capacity)
  (cond ((<= capacity 0) (error "LRU-MAKE: CAPACITY MUST BE POSITIVE" capacity))
        (t (cons capacity nil))))

(define (lru-capacity c) (car c))
(define (lru-entries c) (cdr c))

(define (lru-find entries key)
  (cond ((null entries) nil)
        ((equal (caar entries) key) (car entries))
        (t (lru-find (cdr entries) key))))

(define (lru-remove entries key)
  (cond ((null entries) nil)
        ((equal (caar entries) key) (cdr entries))
        (t (cons (car entries) (lru-remove (cdr entries) key)))))

(define (lru-get c key)
  "returns (value new-cache) as a 2-element list; (nil c) on a miss"
  (let ((hit (lru-find (lru-entries c) key)))
    (cond ((eq hit nil) (list nil c))
          (t (list (cdr hit)
                   (cons (lru-capacity c)
                         (cons hit (lru-remove (lru-entries c) key))))))))

(define (lru-put c key val)
  "inserts or refreshes the binding; evicts least-recently-used if full"
  (let* ((without (lru-remove (lru-entries c) key))
         (entries (cons (cons key val) without)))
    (cons (lru-capacity c)
          (lru-truncate entries (lru-capacity c)))))

; keep only the front n entries (drop the least-recently-used tail)
(define (lru-truncate entries n)
  (cond ((null entries) nil)
        ((= n 0) nil)
        (t (cons (car entries) (lru-truncate (cdr entries) (- n 1))))))

(define (lru-size c) (length (lru-entries c)))

(define (lru-keys c)
  "keys from most to least recently used"
  (map car (lru-entries c)))

; ---- checks ----

(check "lru: make with zero capacity is catchable"
       (equal (car (catch (lru-make 0) (lambda (e) e))) (quote error)))

(check "lru: get on a miss returns (nil cache)"
       (let ((r (lru-get (lru-make 2) "x")))
         (and (eq (car r) nil)
              (= (lru-size (cadr r)) 0))))

(check "lru: put then get"
       (let ((r (lru-get (lru-put (lru-make 2) "a" 1) "a")))
         (= (car r) 1)))

(check "lru: eviction of the least recently used"
       (let ((c (lru-put (lru-put (lru-put (lru-make 2) "a" 1) "b" 2) "c" 3)))
         (and (eq (car (lru-get c "a")) nil)
              (= (car (lru-get c "c")) 3))))

(check "lru: get refreshes recency and saves from eviction"
       (let* ((c1 (lru-put (lru-put (lru-make 2) "a" 1) "b" 2))
              (c2 (cadr (lru-get c1 "a")))
              (c3 (lru-put c2 "c" 3)))
         (and (= (car (lru-get c3 "a")) 1)
              (eq (car (lru-get c3 "b")) nil))))

(check "lru: put of an existing key refreshes without growing"
       (let ((c (lru-put (lru-put (lru-make 2) "a" 1) "a" 2)))
         (and (= (lru-size c) 1)
              (= (car (lru-get c "a")) 2))))

(check "lru: size never exceeds capacity"
       (<= (lru-size (lru-put (lru-put (lru-put (lru-put (lru-make 2) "a" 1) "b" 2) "c" 3) "d" 4)) 2))

(check "lru: keys are ordered most-recent-first"
       (let ((c (lru-put (lru-put (lru-put (lru-make 3) "a" 1) "b" 2) "c" 3)))
         (equal (lru-keys (cadr (lru-get c "a")))
                (list "a" "c" "b"))))

(check "lru: integer keys"
       (let ((c (lru-put (lru-put (lru-make 2) 1 "one") 2 "two")))
         (equal (car (lru-get c 1)) "one")))

(check "lru: eviction order over a sequence"
       (let* ((c (lru-put (lru-put (lru-put (lru-make 3) "a" 1) "b" 2) "c" 3))
              (c2 (lru-put c "d" 4)))
         (and (eq (car (lru-get c2 "a")) nil)
              (= (car (lru-get c2 "b")) 2)
              (= (car (lru-get c2 "d")) 4))))

(check "lru: capacity-1 cache keeps only the newest"
       (let ((c (lru-put (lru-put (lru-make 1) "a" 1) "b" 2)))
         (and (eq (car (lru-get c "a")) nil)
              (= (car (lru-get c "b")) 2))))
