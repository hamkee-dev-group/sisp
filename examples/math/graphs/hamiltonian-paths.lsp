; ============================================================ ; 112 — Hamiltonian Paths (TAOCP Vol 4A, 7.1.4) ; Find all Hamiltonian paths in small directed graphs using ; backtracking. Graph 1: path of 4 nodes (1-2-3-4): 2 paths. ; Graph 2: 4-cycle (1-2-3-4-1): 8 directed paths. ; Also implement subset DP for n=4 and cross-check counts. ; ============================================================
(print "=== Hamiltonian Paths ===")

; ---------- adjacency ----------
; Graph represented as list of (u . v) directed edges
(define (adjacent orig dest edges)
  (cond ((eq edges nil) nil)
        ((and (equal (caar edges) orig) (equal (cdar edges) dest)) t)
        (t (adjacent orig dest (cdr edges)))))

(define (neighbors v edges)
  (foldl (lambda (acc e)
    (cond ((equal (car e) v) (cons (cdr e) acc))
          (t acc)))
    nil edges))

(define (all-vertices edges)
  (let ((vs (foldl (lambda (acc e)
            (let ((a (cons (car e) (cons (cdr e) acc))))
              (let ((b (dedup-acc a nil)))
                b)))
            nil edges)))
    vs))

(define (dedup-acc lst acc)
  (cond ((eq lst nil) acc)
        ((in (car lst) acc) (dedup-acc (cdr lst) acc))
        (t (dedup-acc (cdr lst) (cons (car lst) acc)))))

; ---------- backtracking: find all Hamiltonian paths ----------
; A Hamiltonian path visits every vertex exactly once.

(define (ham-paths edges)
  (let ((verts (all-vertices edges))
        (n (length (all-vertices edges))))
    (let ((sorted (sort-verts verts)))
      (foldl (lambda (acc start)
        (append acc (ham-from edges sorted start (list start) (- n 1))))
        nil sorted))))

(define (sort-verts vs)
  (cond ((eq vs nil) nil)
        (t (let ((m (min-v vs)))
          (cons m (sort-verts (remove-elem m vs)))))))

(define (min-v vs)
  (cond ((eq (cdr vs) nil) (car vs))
        ((< (car vs) (min-v (cdr vs))) (car vs))
        (t (min-v (cdr vs)))))

(define (remove-elem x lst)
  (cond ((eq lst nil) nil)
        ((equal (car lst) x) (cdr lst))
        (t (cons (car lst) (remove-elem x (cdr lst))))))

(define (ham-from edges all-verts current-vertex visited remaining)
  (cond ((= remaining 0) (list (reverse visited)))
        (t (foldl (lambda (acc nxt)
            (cond ((in nxt visited) acc)
                  ((adjacent current-vertex nxt edges)
                   (append acc (ham-from edges all-verts nxt
                                (cons nxt visited) (- remaining 1))))
                  (t acc)))
            nil
            all-verts))))

; ============================================================ ; Graph 1: path of 4 nodes (1-2-3-4) — directed both ways ; ============================================================
(print "--- Graph 1: Path of 4 nodes ---")
(define edge-path4
  (list (cons 1 2) (cons 2 3) (cons 3 4)
        (cons 4 3) (cons 3 2) (cons 2 1)))

(define hp1 (ham-paths edge-path4))
(print "Hamiltonian paths in path-4:")
(define hp1-count (length hp1))
(print hp1-count)
(print "Paths:")
(print hp1)
(check "path4-has-2-ham-paths" (= hp1-count 2))

; ============================================================ ; Graph 2: 4-cycle (1-2-3-4-1) directed both ways ; ============================================================
(print "--- Graph 2: 4-cycle ---")
(define edge-cycle4
  (list (cons 1 2) (cons 2 3) (cons 3 4) (cons 4 1)
        (cons 2 1) (cons 3 2) (cons 4 3) (cons 1 4)))

(define hp2 (ham-paths edge-cycle4))
(print "Hamiltonian paths in 4-cycle:")
(define hp2-count (length hp2))
(print hp2-count)
(print "Paths:")
(print hp2)
(check "cycle4-has-8-ham-paths" (= hp2-count 8))

; ============================================================ ; Subset Dynamic Programming for Hamiltonian paths ; ============================================================
; DP[mask][v] = number of Hamiltonian paths covering vertices in mask, ending at v
; n = number of vertices

; Subset DP over a global accumulator. Two SISP points make this the
; natural shape: (setq x ...) writes the GLOBAL binding and does not
; mutate a let-bound lexical, so the table must be the top-level *dp*;
; and iteration reads clearest as foreach over seq rather than a tower
; of hand-written labels loops.
;   *dp*[mask][v] = number of paths covering exactly the vertices in
;   mask and ending at vertex index v.
(define *dp* nil)
(define (bit-set mask i) (= (mod (truncate (/ mask (^ 2 i))) 2) 1))

(define (ham-dp edges)
  (let* ((verts (all-vertices edges))
         (n (length verts))
         (sv (sort-verts verts))
         (num (^ 2 n)))
    (setq *dp* (make-dp-table num n 0))
    ; base case: the single-vertex path {v} ending at v
    (foreach (lambda (k) (setq *dp* (dp-set *dp* (^ 2 k) k 1)))
             (seq 0 (- n 1)))
    ; fill: extend each path ending at u by an edge u->v
    (foreach (lambda (mask)
      (foreach (lambda (v)
        (cond ((bit-set mask v)
          (let ((prev (- mask (^ 2 v))))
            (cond ((> prev 0)
              (let ((s (foldl (lambda (acc u)
                  (cond ((and (bit-set prev u)
                              (adjacent (nth u sv) (nth v sv) edges))
                          (+ acc (dp-get *dp* prev u)))
                        (t acc)))
                  0 (seq 0 (- n 1)))))
                (setq *dp* (dp-set *dp* mask v s))))
              (t nil))))
          (t nil)))
        (seq 0 (- n 1))))
      (seq 0 (- num 1)))
    ; total over all vertices with the full mask
    (foldl (lambda (acc v) (+ acc (dp-get *dp* (- num 1) v)))
           0 (seq 0 (- n 1)))))

; a table of `states` rows, each an n-vector; cons keeps the rows
; nested (append would flatten them into one states*n list, which
; dp-get/dp-set — expecting (nth v (nth mask table)) — cannot index
(define (make-dp-table states n val)
  (labels (((build k acc)
    (cond ((= k 0) acc)
          (t (build (- k 1) (cons (make-row n val) acc))))))
    (build states nil)))

(define (make-row n val)
  (labels (((build k acc)
    (cond ((= k 0) acc)
          (t (build (- k 1) (cons val acc))))))
    (build n nil)))

(define (dp-get table mask v)
  (nth v (nth mask table)))

(define (dp-set table mask v val)
  (list-set table mask (list-set (nth mask table) v val)))

(define (list-set lst idx val)
  (cond ((= idx 0) (cons val (cdr lst)))
        (t (cons (car lst) (list-set (cdr lst) (- idx 1) val)))))

; ============================================================ ; Cross-check counts with DP ; ============================================================
(print "--- DP cross-check ---")

(define dp-path4 (ham-dp edge-path4))
(print "DP count for path-4:")
(print dp-path4)
(check "dp-path4-matches-backtrack" (= dp-path4 hp1-count))

(define dp-cycle4 (ham-dp edge-cycle4))
(print "DP count for 4-cycle:")
(print dp-cycle4)
(check "dp-cycle4-matches-backtrack" (= dp-cycle4 hp2-count))

; ============================================================ ; n=4 cross-check: verify both methods agree ; ============================================================
(check "n4-cross-check-path" (= dp-path4 hp1-count))
(check "n4-cross-check-cycle" (= dp-cycle4 hp2-count))

(print "=== Hamiltonian Paths Done ===")
