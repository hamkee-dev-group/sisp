; ============================================================
; Stern-Brocot Tree
; Every positive rational appears exactly once. Implements
; path-finding (L/R sequence) to reach any fraction.
; ============================================================

(print "=== Stern-Brocot Tree ===")

(define (sub a b) (+ a (* -1 b)))

; Safe append that handles nil arguments (built-in append requires non-nil)
(define (my-append a b)
  (if (equal a nil) b
    (if (equal b nil) a
      (append a b))))

; The Stern-Brocot tree is defined by mediants.
; Starting from 0/1 and 1/0, the mediant of a/b and c/d is (a+c)/(b+d).
; The root is 1/1. Left child: mediant with left ancestor.
;                   Right child: mediant with right ancestor.

; Build one level of the Stern-Brocot tree.
; Each node is (p q) meaning fraction p/q.
; Given a row, compute the next row by inserting mediants.
; Boundaries are implicitly 0/1 (left) and 1/0 (right).
(define (mediant a-p a-q b-p b-q) (list (+ a-p b-p) (+ a-q b-q)))

; Generate Stern-Brocot sequence at given depth.
; Returns a list of (p q) pairs representing all fractions at that depth.
(define (sb-level depth)
  (labels (((build d left-p left-q right-p right-q)
    (if (= d 0)
      (list (list (+ left-p right-p) (+ left-q right-q)))
      (let* ((mid-p (+ left-p right-p))
             (mid-q (+ left-q right-q))
             (left-subtree (build (sub d 1) left-p left-q mid-p mid-q))
             (right-subtree (build (sub d 1) mid-p mid-q right-p right-q)))
        (my-append (my-append left-subtree (list (list mid-p mid-q))) right-subtree)))))
  (build depth 0 1 1 0)))

; Print fractions from a list of (p q) pairs
(define (print-fracs lst)
  (if (equal lst nil) nil
    (let* ((pair (car lst))
           (p (car pair))
           (q (car (cdr pair))))
      (progn (print (/ p q)) (print-fracs (cdr lst))))))

; --- Display first few levels ---
(print "--- Level 0 (root) ---")
(print-fracs (sb-level 0))

(print "--- Level 1 ---")
(print-fracs (sb-level 1))

(print "--- Level 2 ---")
(print-fracs (sb-level 2))

(print "--- Level 3 (all fractions with small denominators) ---")
(print-fracs (sb-level 3))

; --- Path-finding algorithm ---
; To find fraction p/q in the Stern-Brocot tree:
; Start at boundaries 0/1 (left) and 1/0 (right).
; Mediant m = (lp+rp)/(lq+rq).
; If p/q = m, done. If p/q < m, go Left. If p/q > m, go Right.
(define (sb-path p q)
  (labels (((find lp lq rp rq acc depth)
    (if (> depth 20) acc
      (let* ((mp (+ lp rp))
             (mq (+ lq rq)))
        (cond
          ((= (/ p q) (/ mp mq)) acc)
          ((< (/ p q) (/ mp mq))
            (find lp lq mp mq (my-append acc (list 'L)) (+ depth 1)))
          (t
            (find mp mq rp rq (my-append acc (list 'R)) (+ depth 1))))))))
  (find 0 1 1 0 nil 0)))

; Reconstruct fraction from path
(define (sb-from-path path)
  (labels (((walk p lp lq rp rq)
    (if (equal p nil) (/ (+ lp rp) (+ lq rq))
      (let* ((mp (+ lp rp)) (mq (+ lq rq)))
        (if (equal (car p) 'L)
          (walk (cdr p) lp lq mp mq)
          (walk (cdr p) mp mq rp rq))))))
  (walk path 0 1 1 0)))

; --- Demonstrate path-finding ---
(print "--- Path to 3/5 ---")
(print (sb-path 3 5))
(print "Reconstructed:")
(print (sb-from-path (sb-path 3 5)))

(print "--- Path to 2/3 ---")
(print (sb-path 2 3))
(print "Reconstructed:")
(print (sb-from-path (sb-path 2 3)))

(print "--- Path to 7/4 ---")
(print (sb-path 7 4))
(print "Reconstructed:")
(print (sb-from-path (sb-path 7 4)))

(print "--- Path to 1/1 (root) ---")
(print (sb-path 1 1))

(print "--- Path to 3/7 ---")
(print (sb-path 3 7))
(print "Reconstructed:")
(print (sb-from-path (sb-path 3 7)))

(print "=== Stern-Brocot Tree Done ===")
