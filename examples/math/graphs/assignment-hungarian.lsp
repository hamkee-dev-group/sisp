; ============================================================
; name: hungarian-assignment
; subject: graph
; capability: assignment optimize
; status: incomplete
; canonical-home: none
; notes: Does a single line-cover adjustment (enough for this matrix, not in
;        general); N = 3 is hard-coded.
; ============================================================
; ============================================================
; 118 — Assignment Problem (Hungarian Algorithm)
; 3x3 cost matrix. Find min-cost perfect matching.
; Exhaustively verify all 6 permutations.
; Show Hungarian reduction steps.
; Cost matrix: ((4 1 3) (2 0 5) (3 2 2))
; Optimal: (0,1)=1 + (1,0)=2 + (2,2)=2 = 5
; ============================================================

(print "=== Assignment Problem (Hungarian) ===")


(define cost-matrix '((4 1 3) (2 0 5) (3 2 2)))
(define N 3)
(define (cost i j) (nth j (nth i cost-matrix)))

(print "Cost matrix:")
(print cost-matrix)

; --- Exhaustive: all 6 permutations ---
(define (perms lst)
  (cond ((equal lst nil) (list nil))
        (t (foldl (lambda (acc x)
            (append acc
              (map (lambda (p) (cons x p))
                (perms (filter (lambda (y) (not (equal y x))) lst)))))
            nil lst))))

(define all-perms (perms '(0 1 2)))
(define (total-cost assign)
  (labels (((go i perm acc)
    (cond ((equal perm nil) acc)
          (t (go (+ i 1) (cdr perm) (+ acc (cost i (car perm))))))))
    (go 0 assign 0)))

(define costs (map total-cost all-perms))
(define min-cost (foldl (lambda (a c) (cond ((< c a) c) (t a))) 1000 costs))
(print "All 6 permutation costs:")
(print (zip all-perms costs))
(print "Minimum cost (exhaustive):")
(print min-cost)
(check "min-cost-is-5" (= min-cost 5))

; --- Hungarian algorithm ---
; Step 1: Row reduction
(define (row-min row)
  (foldl (lambda (a x) (cond ((< x a) x) (t a))) 1000 row))

(define (row-reduce rows)
  (map (lambda (row)
    (let ((rm (row-min row)))
      (map (lambda (x) (- x rm)) row)))
    rows))

(define r1 (row-reduce cost-matrix))
(print "After row reduction (subtract row minima):")
(print r1)

; Step 2: Column reduction
(define (col-min-val c rows)
  (foldl (lambda (a row) (let ((x (nth c row))) (cond ((< x a) x) (t a)))) 1000 rows))

(define (col-reduce rows)
  (labels (((go c mat)
    (cond ((= c N) mat)
          (t (let ((cm (col-min-val c mat)))
            (go (+ c 1)
              (map (lambda (row)
                (list-replace row c (- (nth c row) cm)))
                mat)))))))
    (go 0 rows)))

(define (list-replace lst idx val)
  (cond ((equal lst nil) nil)
        ((= idx 0) (cons val (cdr lst)))
        (t (cons (car lst) (list-replace (cdr lst) (- idx 1) val)))))

(define r2 (col-reduce r1))
(print "After column reduction (subtract col minima):")
(print r2)

; Step 3: Find max zero assignment via backtracking
(define (at m r c) (nth c (nth r m)))

(define (has x lst)
  (cond ((equal lst nil) nil)
        ((equal (car lst) x) t)
        (t (has x (cdr lst)))))

; Find max number of independent zeros
(define (max-zero-assign m)
  (labels (((dfs row used-cols count)
    (cond ((= row N) count)
          (t (foldl (lambda (best col)
              (cond ((and (not (has col used-cols)) (= (at m row col) 0))
                     (let ((v (dfs (+ row 1) (cons col used-cols) (+ count 1))))
                       (cond ((> v best) v) (t best))))
                    (t best)))
              (dfs (+ row 1) used-cols count) ; skip row
              (seq 0 (- N 1)))))))
    (dfs 0 nil 0)))

(define max-zeros (max-zero-assign r2))
(print "Max independent zeros in reduced matrix:")
(print max-zeros)

; Step 4: If max-zeros < N, adjust via minimum line covering
; For this example, max-zeros = 2 < N = 3, so we need adjustment.
; Find minimum line cover (brute force for N=3)

(define (all-line-sets)
  ; Represent as list (rows cols)
  (let ((all-rows (all-subsets-3 '(0 1 2)))
        (all-cols (all-subsets-3 '(0 1 2))))
    (foldl (lambda (acc rws)
      (append acc
        (map (lambda (cls) (list rws cls)) all-cols)))
      nil all-rows)))

(define (all-subsets-3 lst)
  (cond ((equal lst nil) (list nil))
        (t (let ((r (all-subsets-3 (cdr lst))))
          (append r (map (lambda (s) (cons (car lst) s)) r))))))

(define (covers-all-zeros m lines)
  (let ((rows (car lines)) (cols (cadr lines)))
    (every (lambda (r)
      (every (lambda (c)
        (or (not (= (at m r c) 0))
            (has r rows)
            (has c cols)))
        (seq 0 (- N 1))))
      (seq 0 (- N 1)))))

(define (line-count lines)
  (+ (length (car lines)) (length (cadr lines))))

(define line-sets (all-line-sets))
(define covering-lines
  (filter (lambda (ls) (covers-all-zeros r2 ls)) line-sets))
(define min-lines
  (foldl (lambda (a ls) (let ((c (line-count ls))) (cond ((< c a) c) (t a)))) 1000 covering-lines))
(print "Minimum lines to cover all zeros:")
(print min-lines)

; Find a minimal cover
(define best-cover
  (find (lambda (ls) (= (line-count ls) min-lines)) covering-lines))

(print "Best line cover (rows, cols):")
(print best-cover)

; Step 5: Adjust
(define (hungarian-adjust m lines)
  (let ((mr (car lines)) (mc (cadr lines)))
    (let ((all-rows '(0 1 2)) (all-cols '(0 1 2)))
      (let ((ur (filter (lambda (r) (not (has r mr))) all-rows))
            (uc (filter (lambda (c) (not (has c mc))) all-cols)))
        ; Find min uncovered value
        (let ((minv (foldl (lambda (a r)
                     (foldl (lambda (b c) (let ((v (at m r c))) (cond ((< v b) v) (t b))))
                       a uc))
                   1000 ur)))
          (cond ((= minv 1000) m)
                (t
                 (labels (((rebuild r c acc)
                   (cond ((= r N) acc)
                         ((= c N) (rebuild (+ r 1) 0 acc))
                         ((and (has r ur) (has c uc))
                          (rebuild r (+ c 1)
                            (list-replace acc r
                              (list-replace (nth r acc) c
                                (- (at m r c) minv)))))
                         ((and (has r mr) (has c mc))
                          (rebuild r (+ c 1)
                            (list-replace acc r
                              (list-replace (nth r acc) c
                                (+ (at m r c) minv)))))
                         (t (rebuild r (+ c 1) acc)))))
                   (rebuild 0 0 m)))))))))

(define r3 (hungarian-adjust r2 best-cover))
(print "After adjustment:")
(print r3)

; Now find optimal assignment in r3
(define (find-best-assign m used-cols row acc)
  (cond ((= row N) acc)
        (t (labels (((try col)
              (cond ((= col N) nil)
                    ((and (not (has col used-cols)) (= (at m row col) 0))
                     (let ((r (find-best-assign m (cons col used-cols) (+ row 1)
                                 (cons col acc))))
                       (cond (r r) (t (try (+ col 1))))))
                    (t (try (+ col 1))))))
            (try 0)))))

(define assign (reverse (find-best-assign r3 nil 0 nil)))
(print "Optimal column assignment:")
(print assign)

; Compute cost in original matrix
(define hung-cost
  (labels (((go i cols acc)
    (cond ((equal cols nil) acc)
          (t (go (+ i 1) (cdr cols) (+ acc (cost i (car cols))))))))
    (go 0 assign 0)))

(print "Hungarian algorithm cost:")
(print hung-cost)
(check "hungarian-cost-is-5" (= hung-cost 5))

(print "=== Assignment Problem Done ===")
