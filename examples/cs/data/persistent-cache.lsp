; Persistent cache combines explicit expiry times with least-recently-used eviction.
; Cache entries are (key value expires last-access); no wall-clock builtin is required.
(define (pc-live entries now)
  (filter (lambda (entry) (> (caddr entry) now)) entries))
(define (pc-oldest entries)
  (foldl (lambda (best entry)
    (cond ((or (eq best nil) (< (cadddr entry) (cadddr best))) entry) (t best))) nil entries))
(define (pc-trim entries capacity)
  (cond ((<= (length entries) capacity) entries)
        (t (pc-trim (remove (lambda (entry) (equal entry (pc-oldest entries))) entries) capacity))))
(define (pc-put cache key value now ttl)
  (let ((capacity (car cache)) (entries (pc-live (cadr cache) now)))
    (list capacity (pc-trim (cons (list key value (+ now ttl) now)
      (remove (lambda (entry) (equal (car entry) key)) entries)) capacity))))
(define (pc-get cache key now default)
  (let ((entry (find (lambda (item) (and (equal (car item) key) (> (caddr item) now)))
                 (cadr cache))))
    (cond ((eq entry nil) default) (t (cadr entry)))))
(define pc-demo (pc-put (pc-put (list 2 nil) 'a 1 0 10) 'b 2 1 10))
(pc-get pc-demo 'a 5 'missing) ; => 1
(pc-get pc-demo 'a 11 'expired) ; => expired
(map (lambda (entry) (car entry)) (cadr (pc-put pc-demo 'c 3 2 10))) ; => (c b)

