; ============================================================
; 18 — Relational Database Algebra via Sets
; Tables as sets of tuples, SELECT/PROJECT/JOIN
; UNIQUE: relational algebra IS set theory — SISP native
; ============================================================

(print "=== Relational Algebra ===")

; Employees table: (id name dept)
(define employees '((1 alice eng) (2 bob sales) (3 carol eng)
                    (4 dave sales) (5 eve mgmt)))
(print "Employees:") (print employees)

; --- SELECT: filter rows ---
(define (db-select tbl col val)
  (cond ((null tbl) nil)
        ((eq (nth col (car tbl)) val)
         (cons (car tbl) (db-select (cdr tbl) col val)))
        (t (db-select (cdr tbl) col val))))

(define eng-team (db-select employees 3 'eng))
(print "--- SELECT dept=eng ---")
(print eng-team)

(define sales-team (db-select employees 3 'sales))
(print "--- SELECT dept=sales ---")
(print sales-team)

; --- PROJECT: extract column ---
(define (db-project tbl col)
  (cond ((null tbl) nil)
        (t (cons (nth col (car tbl))
                 (db-project (cdr tbl) col)))))

(print "--- PROJECT name ---")
(print (db-project employees 2))

(print "--- PROJECT dept ---")
(print (db-project employees 3))

; --- UNION via set operations ---
(define eng-set {alice carol})
(define sales-set {bob dave})
(define all-set (union eng-set sales-set))
(print "--- UNION eng ∪ sales ---")
(print all-set)

; --- INTERSECTION ---
; Who is in both eng-related and name-starting sets?
(define vowel-names {alice eve})
(print "--- INTERSECT vowel-names ∩ eng ---")
(print (cap vowel-names eng-set))

; --- DIFFERENCE ---
(define all-names {alice bob carol dave eve})
(print "--- DIFFERENCE: not in eng ---")
(print (diff all-names eng-set))

; --- JOIN: employees × departments ---
(define depts '((eng 100) (sales 80) (mgmt 120)))
(print "--- Departments ---")
(print depts)

(define (lookup-dept dname dlist)
  (cond ((null dlist) 0)
        ((eq (car (car dlist)) dname) (nth 2 (car dlist)))
        (t (lookup-dept dname (cdr dlist)))))

(define (join-rows tbl dtbl)
  (cond ((null tbl) nil)
        (t (let ((row (car tbl))
                 (budget (lookup-dept (nth 3 (car tbl)) dtbl)))
             (cons (list (nth 1 row) (nth 2 row) (nth 3 row) budget)
                   (join-rows (cdr tbl) dtbl))))))

(define joined (join-rows employees depts))
(print "--- JOIN employees ⋈ depts ---")
(print "(id name dept budget)")
(print joined)

(print "=== Relational Algebra Done ===")
