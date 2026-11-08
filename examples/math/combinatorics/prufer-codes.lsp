; ============================================================
; 107 — Trees and Prufer Codes (TAOCP Vol 1, 2.3.4.4)
; Encode a labeled tree on n=4 nodes as a Prufer sequence of
; length n-2. Decode back to tree. Enumerate all 4^(4-2)=16
; possible Prufer sequences and verify each decodes to a unique
; tree. Verify Cayley's formula: 16 labelled trees on 4 nodes.
; ============================================================

(print "=== Prufer Codes ===")


; ---------- helpers ----------
(define (deg k degs)
  ; degree of vertex k in the degree vector
  (nth (- k 1) degs))

(define (set-deg k v degs)
  ; set degree of vertex k to v
  (list-set degs (- k 1) v))

(define (list-set xs i v)
  (cond ((= i 0) (cons v (cdr xs)))
        (t (cons (car xs) (list-set (cdr xs) (- i 1) v)))))

(define (vec-inc idx vec)
  ; increment element at index (0-based)
  (list-set vec idx (+ (nth idx vec) 1)))

; ---------- tree -> Prufer sequence ----------
; Input: edges as pairs (list (1 2) (2 3) (3 4)). Nodes are 1..n.
; Output: Prufer sequence of length n-2.

(define (tree-to-prufer edges n)
  (let* ((deg-vec (make-degree-vec edges n))
         (code nil))
    (prufer-loop edges deg-vec n code)))

(define (make-degree-vec edges n)
  (labels (((go es vec)
    (cond ((eq es nil) vec)
          (t (let* ((e (car es))
                    (a (car e))
                    (b (cadr e))
                    (v1 (vec-inc (- a 1) vec))
                    (v2 (vec-inc (- b 1) v1)))
               (go (cdr es) v2))))))
    (go edges (map (lambda (ignored) 0) (seq 0 (- n 1))))))

(define (prufer-loop edges degs n code)
  (cond ((= n 2) code)  ; done
        (t (let ((leaf (find-leaf degs n)))
             (let* ((neighbor (find-neighbor edges leaf))
                    (new-edges (remove-edge edges leaf neighbor))
                    (new-degs (set-deg leaf -1
                              (set-deg neighbor (- (deg neighbor degs) 1) degs)))
                    (new-code (append code (list neighbor))))
               (prufer-loop new-edges new-degs (- n 1) new-code))))))

(define (find-leaf degs n)
  (labels (((go k)
    (cond ((> k n) nil)
          ((= (deg k degs) 1) k)
          (t (go (+ k 1))))))
    (go 1)))

(define (find-neighbor edges leaf)
  (cond ((eq edges nil) nil)
        ((= (caar edges) leaf) (cadar edges))
        ((= (cadar edges) leaf) (caar edges))
        (t (find-neighbor (cdr edges) leaf))))

(define (remove-edge edges a b)
  (filter (lambda (e) (not (or (and (= (car e) a) (= (cadr e) b))
                               (and (= (car e) b) (= (cadr e) a)))))
          edges))

; ---------- Prufer sequence -> tree ----------
; Given Prufer code p of length n-2, reconstruct edges of tree.
(define (prufer-to-tree code)
  (let ((n (+ (length code) 2)))
    (prufer-decode code n)))

(define (prufer-decode code n)
  (let ((deg-vec (make-deg-from-code code n)))
    (labels (((go code-i degs edges)
      (cond ((eq code-i nil)
             ; final edge: two remaining nodes with degree 1
             (let ((last-two (find-deg-1s degs 1)))
               (cond ((= (length last-two) 2)
                      (append edges (list (list (car last-two) (cadr last-two)))))
                     (t edges))))
            (t (let* ((v (car code-i))
                      (leaf (find-deg-1 degs 1)))
                 (let ((new-degs (set-deg leaf -1
                                  (set-deg v (- (deg v degs) 1) degs))))
                   (go (cdr code-i) new-degs
                       (append edges (list (list leaf v))))))))))
      (go code deg-vec nil))))

(define (make-deg-from-code code n)
  (labels (((go i vec)
    (cond ((eq i nil) vec)
          (t (go (cdr i) (vec-inc (- (car i) 1) vec))))))
    (let ((base (map (lambda (ignored) 1) (seq 0 (- n 1)))))
      (go code base))))

(define (find-deg-1s degs k)
  (cond ((> k (length degs)) nil)
        ((= (nth (- k 1) degs) 1)
         (cons k (find-deg-1s degs (+ k 1))))
        (t (find-deg-1s degs (+ k 1)))))

(define (find-deg-1 degs k)
  (cond ((> k (length degs)) nil)
        ((= (nth (- k 1) degs) 1) k)
        (t (find-deg-1 degs (+ k 1)))))

; ============================================================
; Test on n=4
; All 16 possible Prufer sequences of length 2 over {1,2,3,4}
; ============================================================

; generate all Prufer sequences of length 2 over {1,2,3,4}
(define all-prufer
  (foldl (lambda (acc i)
    (append acc (map (lambda (j) (list i j)) (seq 1 4))))
    nil (seq 1 4)))

(check "16-prufer-sequences" (= (length all-prufer) 16))

; decode each to get edges
(define all-trees (map prufer-to-tree all-prufer))

; verify all 16 are distinct
(define (canonical-edges edges)
  (sort (lambda (a b)
    (cond ((< (car a) (car b)) t)
          ((> (car a) (car b)) nil)
          (t (< (cadr a) (cadr b)))))
    (map (lambda (e) (list (min (car e) (cadr e))
                           (max (car e) (cadr e))))
         edges)))

(define all-canon (map canonical-edges all-trees))

(check "all-16-trees-distinct" (= (length (dedup all-canon)) 16))
(check "Cayley-4-4-4-2" (= 16 (^ 4 (- 4 2))))

; Round-trip on multiple codes
(define (rto c)
  (let* ((tr (prufer-to-tree c))
         (c2 (tree-to-prufer tr 4)))
    (equal c c2)))

(check "rt-code-1-1" (rto (list 1 1)))
(check "rt-code-1-2" (rto (list 1 2)))
(check "rt-code-4-3" (rto (list 4 3)))
(check "rt-code-3-1" (rto (list 3 1)))

; Show a few examples
(print "--- example ---")
(define sample-code (list 1 1))
(define sample-tree (prufer-to-tree sample-code))
(print (list (quote code) sample-code))
(print (list (quote tree-edges) sample-tree))
(define re-encoded (tree-to-prufer sample-tree 4))
(print (list (quote re-encoded) re-encoded))
(check "sample-round-trip-ok" (equal re-encoded sample-code))

(print "=== Prufer Codes Done ===")
