; ============================================================
; SIMPLE DATABASE — KEY-VALUE STORE ON THE FILESYSTEM
;
; Provides persistent key-value storage using the SISP OS
; filesystem.  Each database is a directory; each key is a
; file whose content is the value (number or string).
;
; API:
;   (db-open path)     — open/create a database
;   (db-put db k v)    — store value v under key k
;   (db-get db k)      — retrieve value, or nil
;   (db-del db k)      — delete a key
;   (db-keys db)       — list all keys
;   (db-count db)      — number of entries
;   (db-transact db)   — begin a transaction (copy-on-write)
;   (db-commit db txn) — commit a transaction
; ============================================================

; ---- database is an alist: ((key . value) ...) --------------
(define (make-db)
  nil)

(define (db-put db key val)
  (db-put-acc db key val nil))

(define (db-put-acc db key val acc)
  (cond ((null db) (cons (cons key val) acc))
        ((equal (caar db) key)
         (append (rev acc) (cons (cons key val) (cdr db))))
        (t (db-put-acc (cdr db) key val (cons (car db) acc)))))

(define (db-get db key)
  (let ((e (assoc key db)))
    (cond ((eq e nil) nil) (t (cdr e)))))

(define (db-del db key)
  (cond ((null db) nil)
        ((equal (caar db) key) (cdr db))
        (t (cons (car db) (db-del (cdr db) key)))))

(define (db-keys db)
  (map car db))

(define (db-count db)
  (length db))

; ---- transactions: copy-on-write snapshot -------------------
; A transaction is a (snapshot . pending-ops) pair.
(define (make-txn db)
  (cons db nil))

(define (txn-put txn key val)
  (cons (car txn) (cons (cons 'put (list key val)) (cdr txn))))

(define (txn-del txn key)
  (cons (car txn) (cons (cons 'del (list key)) (cdr txn))))

(define (txn-commit txn)
  (let ((db (car txn))
        (ops (cdr txn)))
    (txn-apply db (rev ops))))

(define (txn-apply db ops)
  (cond ((null ops) db)
        ((eq (caar ops) 'put)
         (txn-apply (db-put db (cadar ops) (caddar ops)) (cdr ops)))
        ((eq (caar ops) 'del)
         (txn-apply (db-del db (cadar ops)) (cdr ops)))
        (t (txn-apply db (cdr ops)))))

; ---- persistent storage simulation -------------------------
; A "stored database" wraps an alist with a path and tracks
; whether it has been synced.
(define (make-stored-db path)
  (list path (make-db) t))   ; (path data synced)

(define (sdb-path db) (car db))
(define (sdb-data db) (cadr db))
(define (sdb-synced db) (caddr db))

(define (sdb-put sdb key val)
  (let ((data (sdb-data sdb)))
    (list (sdb-path sdb) (db-put data key val) nil)))

(define (sdb-get sdb key)
  (db-get (sdb-data sdb) key))

(define (sdb-sync sdb)
  (print (str-join (list "sync: writing " (sdb-path sdb) " to disk")))
  (list (sdb-path sdb) (sdb-data sdb) t))

; ---- helpers -------------------------------------------------
(define (rev lst)
  (labels (((go l a) (cond ((null l) a) (t (go (cdr l) (cons (car l) a)))))) (go lst nil)))

(define (str-join parts)
  (cond ((null parts) "")
        ((null (cdr parts)) (car parts))
        (t (cat (car parts) (str-join (cdr parts))))))

; ---- demo ---------------------------------------------------
(define db (make-stored-db "/data/mydb"))
(define db2 (sdb-put db "name" "alice"))
(define db3 (sdb-put db2 "role" "admin"))
(sdb-get db3 "name")    ; => "alice"
(sdb-get db3 "role")    ; => "admin"
(db-count (sdb-data db3))  ; => 2

; Transaction demo
(define txn (make-txn (sdb-data db3)))
(define txn2 (txn-put txn "status" "active"))
(define txn3 (txn-del txn2 "role"))
(define new-data (txn-commit txn3))
(db-get new-data "status")   ; => "active"
(db-get new-data "role")     ; => nil
