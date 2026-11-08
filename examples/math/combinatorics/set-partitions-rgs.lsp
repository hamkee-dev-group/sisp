; ============================================================
; 105 — Set Partitions: Restricted-Growth Strings (RGS)
; TAOCP Vol 4A, 7.2.1.5
; Generate all set partitions of {1,2,3,4} as RGS:
; a1=0, a_k <= 1 + max(a1..a_{k-1}).
; Verify count = Bell(4) = 15. Implement rank/unrank.
; ============================================================

(print "=== Set Partitions (RGS) ===")


; ---------- Stirling and Bell ----------
(define (stirling2 n k)
  (cond ((= k 0) (cond ((= n 0) 1) (t 0)))
        ((> k n) 0)
        (t (+ (stirling2 (- n 1) (- k 1))
              (* k (stirling2 (- n 1) k))))))

(define bell4 (+ (stirling2 4 1) (stirling2 4 2)
                 (stirling2 4 3) (stirling2 4 4)))
(check "Bell-4-is-15" (= bell4 15))

; ---------- list max helper ----------
(define (list-max xs)
  (cond ((eq xs nil) -1)
        (t (foldl (lambda (a b) (cond ((> a b) a) (t b)))
             (car xs) (cdr xs)))))

; ---------- generate all RGS of length n ----------
(define (all-rgs n)
  (cond ((= n 0) (list nil))
        (t (extend-rgs (all-rgs (- n 1))))))

(define (extend-rgs prev)
  (cond ((eq prev nil) nil)
        (t (let ((rgs (car prev))
                 (rest (extend-rgs (cdr prev))))
             (let ((mx (+ 1 (list-max rgs))))
               (append (map (lambda (v) (append rgs (list v)))
                            (range-to mx))
                       rest))))))

(define (range-to mx)
  (cond ((< mx 0) nil)
        ((= mx 0) (list 0))
        (t (seq 0 mx))))

(define rgs-list (all-rgs 4))
(check "RGS-count-15" (= (length rgs-list) 15))
(check "all-start-with-0" (every (lambda (r) (= (car r) 0)) rgs-list))
(print (list (quote rgs-all) rgs-list))

; ---------- RGS to partition ----------
(define (rgs-to-blocks rgs)
  (labels (((go k blocks r)
    (cond ((eq r nil) blocks)
          (t (go (+ k 1) (add-block (car r) k blocks) (cdr r))))))
    (go 1 nil rgs)))

(define (add-block lbl elem blocks)
  (cond ((eq blocks nil) (list (list lbl elem)))
        ((= (caar blocks) lbl)
         (cons (cons lbl (cons elem (cdar blocks))) (cdr blocks)))
        (t (cons (car blocks) (add-block lbl elem (cdr blocks))))))

(define (rgs-to-partition rgs)
  (map (lambda (b) (sort (lambda (i j) (< i j)) (cdr b)))
       (rgs-to-blocks rgs)))

(define all-parts (map rgs-to-partition rgs-list))
(check "parts-all-distinct" (= (length (dedup all-parts)) 15))

; ---------- rank / unrank (lexicographic index) ----------
(define (rank-rgs rgs)
  (find-index rgs rgs-list 0))

(define (find-index x lst i)
  (cond ((eq lst nil) nil)
        ((equal x (car lst)) i)
        (t (find-index x (cdr lst) (+ i 1)))))

(define (unrank-rgs k)
  (nth k rgs-list))

; ---------- round-trip ----------
(define (round-trip-ok)
  (every (lambda (r)
    (equal (unrank-rgs (rank-rgs r)) r))
    rgs-list))
(check "round-trip" (round-trip-ok))

; show examples
(define (show-rk rgs)
  (let* ((r (rank-rgs rgs))
         (u (unrank-rgs r)))
    (print (list rgs (quote rank) r (quote unrank) u
                 (quote ok) (equal u rgs)))))

(print "--- rank/unrank ---")
(show-rk (list 0 0 0 0))
(show-rk (list 0 0 0 1))
(show-rk (list 0 0 1 0))
(show-rk (list 0 0 1 2))
(show-rk (list 0 1 0 0))
(show-rk (list 0 1 2 3))

; verify all ranks cover 0..14
(define all-ranks (map rank-rgs rgs-list))
(check "ranks-unique" (= (length (dedup all-ranks)) 15))
(check "ranks-cover-0-14" (equal (sort (lambda (a b) (< a b)) all-ranks)
                                 (seq 0 14)))

(print "=== Set Partitions Done ===")
