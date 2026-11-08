; ============================================================
; 108 — Young Tableaux (TAOCP Vol 3, 5.1.4)
; Generate all standard Young tableaux of shape (3,1).
; Verify count using hook-length formula: 4!/(4*2*1*1)=3.
; List the 3 tableaux explicitly. Also test shape (2,2):
; count = 4!/(3*2*2*1)=2.
; ============================================================

(print "=== Young Tableaux ===")


; ---------- factorial ----------
(define (fact n)
  (cond ((<= n 1) 1)
        (t (* n (fact (- n 1))))))
(check "fact-4" (= (fact 4) 24))

; ---------- helpers ----------
(define (sum-list xs)
  (foldl + 0 xs))

(define (make-range n)
  (cond ((= n 0) nil)
        (t (cons (- n 1) (make-range (- n 1))))))

(define (board-get b i j)
  (nth j (nth i b)))

(define (row-set row j val)
  (cond ((= j 0) (cons val (cdr row)))
        (t (cons (car row) (row-set (cdr row) (- j 1) val)))))

(define (board-set b i j val)
  (cond ((= i 0) (cons (row-set (car b) j val) (cdr b)))
        (t (cons (car b) (board-set (cdr b) (- i 1) j val)))))

; ---------- hook-length formula ----------
(define (hook-length shape i j)
  (let ((row-len (nth i shape)))
    (+ (- row-len j)
       (col-count shape i j))))

(define (col-count shape i j)
  (let ((rows (length shape)))
    (col-count-go shape rows (+ i 1) j 0)))

(define (col-count-go shape rows k j acc)
  (cond ((>= k rows) acc)
        ((> (nth k shape) j) (col-count-go shape rows (+ k 1) j (+ acc 1)))
        (t (col-count-go shape rows (+ k 1) j acc))))

(define (hook-length-product shape)
  (hlp-row shape 0 (length shape) 1))

(define (hlp-row shape i rows acc)
  (cond ((>= i rows) acc)
        (t (hlp-col shape i 0 (nth i shape) rows acc))))

(define (hlp-col shape i j row-len rows acc)
  (cond ((>= j row-len) (hlp-row shape (+ i 1) rows acc))
        (t (hlp-col shape i (+ j 1) row-len rows
             (* acc (hook-length shape i j))))))

(define (hook-count shape)
  (let ((n (sum-list shape)))
    (/ (fact n) (hook-length-product shape))))

(check "hook-3-1" (= (hook-count (list 3 1)) 3))
(check "hook-2-2" (= (hook-count (list 2 2)) 2))

; ============================================================
; Generate all SYT of given shape by backtracking
; ============================================================
(define (gen-syt shape)
  (let ((n (sum-list shape))
        (rows (length shape)))
    (let ((board (empty-board shape)))
      (fill-board board shape n 1))))

(define (empty-board shape)
  (map (lambda (row-len) (map (lambda (ignored) 0) (make-range row-len)))
       shape))

(define (fill-board board shape n k)
  (cond ((> k n) (list board))
        (t (let ((moves (vam board shape)))
             (foldl (lambda (acc pos)
               (append acc (fill-board (board-set board (car pos) (cadr pos) k)
                                       shape n (+ k 1))))
               nil moves)))))

(define (vam board shape)
  (vam-row board shape 0 (length shape) nil))

(define (vam-row board shape i rows acc)
  (cond ((>= i rows) (reverse acc))
        (t (let ((row-len (nth i shape)))
             (vam-col board shape i 0 row-len rows acc)))))

(define (vam-col board shape i j row-len rows acc)
  (cond ((>= j row-len) (vam-row board shape (+ i 1) rows acc))
        ((and (= (board-get board i j) 0)
              (above-filled board i j)
              (left-filled board i j))
         (vam-col board shape i (+ j 1) row-len rows
           (cons (list i j) acc)))
        (t (vam-col board shape i (+ j 1) row-len rows acc))))

(define (above-filled b i j)
  (cond ((= i 0) t)
        (t (not (= (board-get b (- i 1) j) 0)))))

(define (left-filled b i j)
  (cond ((= j 0) t)
        (t (not (= (board-get b i (- j 1)) 0)))))

; ============================================================
; Test shape (3,1)
; ============================================================
(print "--- Shape (3,1) ---")
(define syt31 (gen-syt (list 3 1)))
(check "SYT-3-1-count" (= (length syt31) 3))
(print (list (quote count) (length syt31)))
(print syt31)

; Verify SYT validity with simple recursion (no nested closures)
(define (all-valid boards)
  (cond ((eq boards nil) t)
        ((valid-one (car boards)) (all-valid (cdr boards)))
        (t nil)))

(define (valid-one tab)
  (and (rows-ok tab) (cols-ok tab)))

(define (rows-ok tab)
  (cond ((eq tab nil) t)
        ((row-ok (car tab)) (rows-ok (cdr tab)))
        (t nil)))

(define (row-ok r)
  (cond ((eq (cdr r) nil) t)
        ((< (car r) (cadr r)) (row-ok (cdr r)))
        (t nil)))

(define (cols-ok tab)
  (cond ((eq (cdr tab) nil) t)
        (t (cols-ok2 (car tab) (cadr tab) 0))))

(define (cols-ok2 r1 r2 j)
  (cond ((>= j (length r2)) t)
        ((< (nth j r1) (nth j r2)) (cols-ok2 r1 r2 (+ j 1)))
        (t nil)))

(check "SYT-3-1-valid" (all-valid syt31))

; ============================================================
; Test shape (2,2)
; ============================================================
(print "--- Shape (2,2) ---")
(define syt22 (gen-syt (list 2 2)))
(check "SYT-2-2-count" (= (length syt22) 2))
(print (list (quote count) (length syt22)))
(print syt22)

(check "SYT-2-2-valid" (all-valid syt22))

(print "=== Young Tableaux Done ===")
