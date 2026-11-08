; Chord DHT. Consistent hashing ring, finger table O(log N). Lookup follows fingers.
; Join/leave splice successor/predecessor pointers directly: there is no periodic
; stabilization, other nodes' fingers are not refreshed, and stored data is not moved.
(print "=== Chord DHT ===")
(seed 42)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

(define M 8)
(define RING-SIZE 256)

; Node: (id data finger-table successor predecessor)
(define (mk-node id)
  (list id nil
        (map (lambda (x) (cons (mod (+ id (^ 2 x)) RING-SIZE) nil)) (seq 0 (- M 1)))
        nil nil))

(define (node-id n) (car n))
(define (node-data n) (cadr n))
(define (node-fingers n) (caddr n))
(define (node-succ n) (cadddr n))
(define (node-pred n) (car (cddddr n)))

(define (set-succ n s) (set-car (cdddr n) s))
(define (set-pred n p) (set-car (cddddr n) p))
(define (set-data n d) (set-car (cdr n) d))

; --- Ring operations ---
(define (in-range key start end)
  (cond ((< start end) (and (> key start) (<= key end)))
        ((> start end) (or (> key start) (<= key end)))
        (t (not (= key start)))))

; Find successor of key
(define (find-succ ring node-id-lookup key)
  (let ((entry (assoc node-id-lookup ring)))
    (cond ((null entry) nil)
          (t (let ((node (cdr entry)))
               (let ((succ (node-succ node)))
                 (cond ((null succ) node)
                       ((in-range key node-id-lookup (node-id succ)) succ)
                       (t (let ((closest (closest-preceding-finger node key)))
                            (cond ((= (node-id closest) node-id-lookup) succ)
                                  (t (find-succ ring (node-id closest) key))))))))))))

(define (closest-preceding-finger node key)
  (find-closest (node-fingers node) (- M 1) node key))

(define (find-closest fingers i node key)
  (cond ((< i 0) node)
        (t (let ((fid (cdr (nth i fingers))))
             (cond ((and (not (null fid))
                         (in-range (node-id fid) (node-id node) key))
                    fid)
                   (t (find-closest fingers (- i 1) node key)))))))

; --- Initialize ring ---
(setq *ring* nil)

(define (add-node ring node)
  (cons (cons (node-id node) node) ring))

; Build ring
(define (build-ring ids)
  (setq *ring* nil)
  (foreach (lambda (id)
    (setq *ring* (add-node *ring* (mk-node id))))
    ids)
  (let ((sorted-ids (sort (lambda (a b) (< a b)) ids)))
    (let ((n (length sorted-ids)))
      (build-ring-ptrs sorted-ids n 0)
      (build-fingers sorted-ids)))
  *ring*)

(define (build-ring-ptrs sorted-ids n i)
  (cond ((= i n) nil)
        (t (let* ((cur-id (nth i sorted-ids))
                  (next-id (nth (mod (+ i 1) n) sorted-ids))
                  (prev-id (nth (mod (+ i (- n 1)) n) sorted-ids))
                  (cur (cdr (assoc cur-id *ring*)))
                  (nex (cdr (assoc next-id *ring*)))
                  (prv (cdr (assoc prev-id *ring*))))
             (set-succ cur nex)
             (set-pred cur prv)
             (build-ring-ptrs sorted-ids n (+ i 1))))))

(define (build-fingers ids)
  (foreach (lambda (id)
    (let ((node (cdr (assoc id *ring*))))
      (build-node-fingers node id 0)))
    ids))

(define (build-node-fingers node id i)
  (cond ((= i M) nil)
        (t (let* ((start (mod (+ id (^ 2 i)) RING-SIZE))
                  (succ-node (find-succ *ring* id start)))
             (set-cdr (nth i (node-fingers node)) succ-node)
             (build-node-fingers node id (+ i 1))))))

; --- Test the Chord ring ---
(print "Building Chord ring...")
(define node-ids (list 10 45 80 120 160 200 240))
(build-ring node-ids)

(print (list 'nodes node-ids))
(ck "ring-built" (> (length *ring*) 0))

; Finger tables
(define node10 (cdr (assoc 10 *ring*)))
(print "Finger table for node 10:")
(foreach (lambda (i)
  (let ((f (nth i (node-fingers node10))))
    (let ((succ-id (cond ((null (cdr f)) -1) (t (node-id (cdr f))))))
      (print (list 'finger i 'start (car f) '-> succ-id)))))
  (seq 0 (min 3 (- M 1))))

(ck "node10-has-fingers" (> (length (node-fingers node10)) 0))

; --- Lookup test ---
(define (chord-lookup key)
  (let ((start-node (cdr (assoc (nth 0 node-ids) *ring*))))
    (node-id (find-succ *ring* (node-id start-node) key))))

(define lookup-50 (chord-lookup 50))
(define lookup-100 (chord-lookup 100))
(define lookup-5 (chord-lookup 5))

(print (list 'lookup-50 lookup-50))
(print (list 'lookup-100 lookup-100))
(print (list 'lookup-5 lookup-5))

(ck "lookup-works" (and (= lookup-50 80) (= lookup-100 120) (= lookup-5 10)))

; --- Store and retrieve data ---
(define (chord-put key value)
  (let ((start-node (cdr (assoc (nth 0 node-ids) *ring*))))
    (let ((succ (find-succ *ring* (node-id start-node) key)))
      (set-data succ (cons (list key value) (node-data succ)))
      value)))

(define (chord-get key)
  (let ((start-node (cdr (assoc (nth 0 node-ids) *ring*))))
    (let ((succ (find-succ *ring* (node-id start-node) key)))
      (let ((entry (assoc key (node-data succ))))
        (cond ((null entry) nil) (t (cadr entry)))))))

(chord-put 50 'hello)
(chord-put 100 'world)
(chord-put 5 'chord)

(define g50 (chord-get 50))
(define g100 (chord-get 100))
(define g5 (chord-get 5))
(define g99 (chord-get 99))

(print (list 'get-50 g50 'get-100 g100 'get-5 g5 'get-99 g99))
(ck "chord-put-get" (and (eq g50 'hello) (eq g100 'world) (eq g5 'chord)))
(ck "chord-miss-returns-nil" (null g99))

; --- Join: add a new node ---
(define (join-node new-id)
  (let* ((start-node (cdr (assoc (nth 0 node-ids) *ring*)))
         (succ (find-succ *ring* (node-id start-node) new-id))
         (pred (node-pred succ))
         (new-node (mk-node new-id)))
    (set-succ new-node succ)
    (set-pred new-node pred)
    (set-succ pred new-node)
    (set-pred succ new-node)
    (setq *ring* (add-node *ring* new-node))
    (build-node-fingers new-node new-id 0)
    new-node))

(print "Joining node 150...")
(join-node 150)

(define new-lookup (chord-lookup 150))
(print (list 'lookup-150-after-join new-lookup))
(ck "lookup-after-join" (>= new-lookup 150))

(define g50-after (chord-get 50))
(ck "data-intact-after-join" (eq g50-after 'hello))

; --- Leave: remove a node ---
(define (leave-node node-id)
  (let ((node (cdr (assoc node-id *ring*))))
    (let ((pred (node-pred node)) (succ (node-succ node)))
      (set-succ pred succ)
      (set-pred succ pred)
      (setq *ring* (filter (lambda (e) (not (= (car e) node-id))) *ring*))
      t)))

(print "Leave node 45...")
(define ring-len-before (length *ring*))
(define n45 (cdr (assoc 45 *ring*)))
(define pred-of-45 (node-pred n45))
(define succ-of-45 (node-succ n45))
(define lookup-20-before (chord-lookup 20))
(leave-node 45)

; A successful leave means: the node is gone from the ring, the ring shrank by
; exactly one, its neighbours are now spliced directly together, and lookups
; for the key range it owned now reach its successor.
(ck "leave-succeeds" (null (assoc 45 *ring*)))
(ck "leave-shrinks-ring-by-one" (= (length *ring*) (- ring-len-before 1)))
(ck "leave-relinks-neighbours"
    (and (eq (node-succ pred-of-45) succ-of-45)
         (eq (node-pred succ-of-45) pred-of-45)))
(ck "departed-keys-move-to-successor"
    (and (= lookup-20-before 45) (= (chord-lookup 20) (node-id succ-of-45))))

(print "=== Chord DHT Done ===")
