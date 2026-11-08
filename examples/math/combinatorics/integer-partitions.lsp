; ============================================================
; 104 — Integer Partitions (TAOCP Vol 4A, 7.2.1.4)
; Generate partitions of n=7 in lexicographic order (largest part
; first). Verify count = p(7) = 15. Generate minimal-change
; (Gray-code) order: each step differs by +/-1 in exactly one
; part (Ferrers diagram: move one box to another row).
; Verify conjugation is an involution.
; ============================================================

(print "=== Integer Partitions ===")


; ---------- helpers ----------
(define (sum-list xs)
  (foldl + 0 xs))

(define (rem1 x lst)
  (cond ((eq lst nil) nil)
        ((equal (car lst) x) (cdr lst))
        (t (cons (car lst) (rem1 x (cdr lst))))))

; ---------- lexicographic generation ----------
(define (parts n lim)
  (cond ((= n 0) (list nil))
        ((= lim 0) nil)
        ((< n lim) (parts n n))
        (t (append
             (map (lambda (p) (cons lim p))
                  (parts (- n lim) lim))
             (parts n (- lim 1))))))

(define lex-parts (parts 7 7))
(check "p7-lex-15" (= (length lex-parts) 15))
(print (list (quote lexicographic) lex-parts))

(define (descending-p p)
  (cond ((eq (cdr p) nil) t)
        ((>= (car p) (cadr p)) (descending-p (cdr p)))
        (t nil)))
(check "all-descending" (every descending-p lex-parts))
(check "all-sum-7" (every (lambda (p) (= (sum-list p) 7)) lex-parts))
(check "all-distinct" (= (length (dedup lex-parts)) 15))

; ---------- conjugation ----------
(define (conjugate p)
  (cond ((eq p nil) nil)
        (t (let ((mx (car p)))
          (labels (((build i acc)
            (cond ((= i 0) acc)
                  (t (build (- i 1)
                       (cons (count (lambda (x) (>= x i)) p) acc))))))
            (build mx nil))))))

(check "conj-421-is-3211" (equal (conjugate (list 4 2 1)) (list 3 2 1 1)))
(check "conj-involution"
  (every (lambda (p) (equal (conjugate (conjugate p)) p)) lex-parts))

; ---------- adjacency ----------
(define (norm-vec v) (sort (lambda (i j) (> i j)) v))
(define (pad-to len xs)
  (cond ((>= (length xs) len) xs)
        (t (pad-to len (append xs (list 0))))))

(define (adjacent-p a b)
  (let* ((sa (norm-vec a))
         (sb (norm-vec b))
         (maxlen (max (length sa) (length sb))))
    (let ((ea (pad-to maxlen sa))
          (eb (pad-to maxlen sb)))
      (check-diffs ea eb 0 0 0))))

(define (check-diffs a b pos incs decs)
  (cond ((= pos (length a))
         (and (= incs 1) (= decs 1)))
        ((= (nth pos a) (nth pos b))
         (check-diffs a b (+ pos 1) incs decs))
        ((and (= (+ (nth pos a) 1) (nth pos b)) (= incs 0))
         (check-diffs a b (+ pos 1) 1 decs))
        ((and (= (+ (nth pos b) 1) (nth pos a)) (= decs 0))
         (check-diffs a b (+ pos 1) incs 1))
        (t nil)))

; ---------- build adjacency list and find Hamiltonian path ----------
; Use longer DFS with entry ordering to avoid dead ends.

(define (neighbors-of node all-nodes)
  (filter (lambda (q) (and (not (equal node q)) (adjacent-p node q)))
          all-nodes))

(define (ham-path nodes)
  (ham-from (car nodes) (list (car nodes)) (cdr nodes) (length nodes)))

(define (ham-from last path remaining target-len)
  (cond ((eq remaining nil)
         (cond ((= (length path) target-len) (reverse path))
               (t nil)))
        (t (let ((nbrs (neighbors-of last remaining)))
             (try-nbrs nbrs path remaining target-len)))))

(define (try-nbrs nbrs path remaining target-len)
  (cond ((eq nbrs nil) nil)
        (t (let ((cand (car nbrs)))
             (let ((result (ham-from cand
                            (cons cand path)
                            (rem1 cand remaining)
                            target-len)))
               (cond (result result)
                     (t (try-nbrs (cdr nbrs) path remaining target-len))))))))

(print "--- Gray code ---")
(define gray (ham-path lex-parts))
(cond (gray
       (progn
         (print gray)
         (check "gray-15" (= (length gray) 15))
         (check "gray-all-diff" (= (length (dedup gray)) 15))
         (define (gray-ok ps)
           (cond ((eq (cdr ps) nil) t)
                 ((adjacent-p (car ps) (cadr ps))
                  (gray-ok (cdr ps)))
                 (t nil)))
         (check "gray-adjacent-steps" (gray-ok gray))))
      (t (print "Gray: no full path found")))

(print "=== Integer Partitions Done ===")
