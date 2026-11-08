; ============================================================
; name: optimal-bst
; subject: data-structure
; capability: static optimal bst
; status: incomplete
; canonical-home: none
; notes: Data is the CLRS 15.5 example; optimal expected cost 11/4, and the
;        tree rebuilt from the root table has the same cost.
; ============================================================
; ============================================================
; 100 — Optimal Binary Search Trees (TAOCP Vol 1, 2.3.4.5; Vol 3, 6.2.2)
; Dynamic programming with Knuth's lower bound r[i][j-1] on the root; the
; root search still runs up to j, so this is not the full O(n^2) version.
; Given p = (0.15 0.10 0.05 0.10 0.20) and q = (0.05 0.10 0.05 0.05 0.05 0.10)
; Compute optimal cost and root table. Reconstruct tree.
; Verify cost equals DP table value.
; ============================================================

(print "=== Optimal Binary Search Trees ===")


(define p (list 3/20 1/10 1/20 1/10 1/5))
(define q (list 1/20 1/10 1/20 1/20 1/20 1/10))
(define n (length p))

; Use 1-indexed table: rows 1..n+1, cols 0..n
; Flat array: entry (i,j) stored at index i*(n+1)+j
(define stride (+ n 1))
(define total-cells (* stride (+ n 2)))

(define (idx i j) (+ (* i stride) j))

(define (list-set xs idx v)
  (cond ((= idx 0) (cons v (cdr xs)))
        (t (cons (car xs) (list-set (cdr xs) (- idx 1) v)))))

(define (flat-new s val)
  (map (lambda (ignored) val) (seq 0 (- s 1))))

(define (fget arr i j) (nth (idx i j) arr))
(define (fset arr i j val) (list-set arr (idx i j) val))

; Base case via getter: e[i][i-1] = q[i-1], w[i][i-1] = q[i-1]
(define (e-get arr i j)
  (cond ((= j (- i 1)) (nth (- i 1) q))
        (t (fget arr i j))))

(define (w-get arr i j)
  (cond ((= j (- i 1)) (nth (- i 1) q))
        (t (fget arr i j))))

; Main DP
(define e-tab nil)
(define w-tab nil)
(define r-tab nil)

(define (run-dp)
  (setq e-tab (flat-new total-cells 0))
  (setq w-tab (flat-new total-cells 0))
  (setq r-tab (flat-new total-cells 0))

  ; lengths l = 0..n
  (labels (((len-loop l)
    (cond ((<= l n)
      (labels (((i-loop i)    ; i = 1..n-l+1
        (cond ((<= (+ i l) n)
          (let ((j (+ i l)))
            ; w[i][j] = w[i][j-1] + p_j + q_j
            (let ((wk (+ (w-get w-tab i (- j 1)) (nth (- j 1) p) (nth j q))))
              (setq w-tab (fset w-tab i j wk))
              (let ((r-lo (let ((v (fget r-tab i (- j 1)))) (cond ((= v 0) i) (t v))))
                    (r-hi (let ((v (fget r-tab (+ i 1) j))) (cond ((= v 0) j) (t v)))))
                (let ((best (try-roots i j wk r-lo 999999 i)))
                  (setq e-tab (fset e-tab i j (car best)))
                  (setq r-tab (fset r-tab i j (cadr best))))
                (i-loop (+ i 1))))))
          (t nil))))
        (i-loop 1))
      (len-loop (+ l 1)))
      (t nil))))
    (len-loop 0)))

(define (try-roots i j wk r best-e best-r)
  (cond ((> r j) (list best-e best-r))
        (t (let ((candidate (+ (e-get e-tab i (- r 1))
                               (e-get e-tab (+ r 1) j)
                               wk)))
          (cond ((< candidate best-e)
                 (try-roots i j wk (+ r 1) candidate r))
                (t
                 (try-roots i j wk (+ r 1) best-e best-r)))))))

(run-dp)

(define opt-cost (e-get e-tab 1 n))
(print (list (quote optimal-cost) opt-cost))

(define total-prob (+ (foldl + 0 p)
                       (foldl + 0 q)))
(check "probabilities-sum-to-1" (= total-prob 1))
(check "optimal-cost-computed" (= opt-cost (/ 11 4)))
(check "optimal-cost-is-not-just-positive" (> opt-cost 0))

; ---------- tree reconstruction ----------
(define (build-tree i j)
  (cond ((> i j) (list (quote dummy) i))
        (t (let ((r (fget r-tab i j)))
          (list (quote node)
                (list (quote key) r)
                (build-tree i (- r 1))
                (build-tree (+ r 1) j))))))

(define bst-tree (build-tree 1 n))
(print (list (quote tree) bst-tree))

; ---------- verify cost independently ----------
(define (tree-cost tr depth)
  (cond ((eq (car tr) (quote dummy))
         (let ((idx (- (cadr tr) 1)))
           (* (nth idx q) depth)))
        ((eq (car tr) (quote node))
         (let* ((key-info (cadr tr))
                (kidx (- (cadr key-info) 1))
                (left (caddr tr))
                (right (cadddr tr)))
           (+ (* (nth kidx p) depth)
              (tree-cost left (+ depth 1))
              (tree-cost right (+ depth 1)))))
        (t 0)))

(define computed-cost (tree-cost bst-tree 1))
(print (list (quote tree-computed-cost) computed-cost))
(check "tree-cost-matches-dp-table" (= computed-cost opt-cost))

; ---------- verify all keys present ----------
(define (collect-keys tr)
  (cond ((eq (car tr) (quote dummy)) nil)
        ((eq (car tr) (quote node))
         (cons (cadr (cadr tr))
               (append (collect-keys (caddr tr))
                       (collect-keys (cadddr tr)))))
        (t nil)))

(define keys-collected (collect-keys bst-tree))
(check "all-5-keys-present" (= (length keys-collected) 5))
(check "each-key-appears-once"
  (every (lambda (k)
    (= (count (lambda (x) (= x k)) keys-collected) 1))
    (seq 1 5)))

(print "=== Optimal BST Done ===")
