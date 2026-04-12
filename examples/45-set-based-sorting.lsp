; ============================================================
; 45 — Functional Sorting Algorithms
; Pure functional insertion sort and merge sort
; ============================================================

(print "=== Functional Sorting ===")

; --- Insertion Sort ---
(define (insert x sorted)
  (cond ((null sorted) (cons x nil))
        ((<= x (car sorted)) (cons x sorted))
        (t (cons (car sorted) (insert x (cdr sorted))))))

(define (isort lst)
  (cond ((null lst) nil)
        (t (insert (car lst) (isort (cdr lst))))))

(print "--- Insertion Sort ---")
(define data1 '(5 3 8 1 9 2 7 4 6))
(print "Input:  ") (print data1)
(print "Sorted: ") (print (isort data1))

; --- Merge ---
(define (merge-lists a b)
  (cond ((null a) b)
        ((null b) a)
        ((<= (car a) (car b))
         (cons (car a) (merge-lists (cdr a) b)))
        (t (cons (car b) (merge-lists a (cdr b))))))

; Split a list into two halves using alternation
(define (split-odd lst)
  (cond ((null lst) nil)
        ((null (cdr lst)) (cons (car lst) nil))
        (t (cons (car lst) (split-odd (cdr (cdr lst)))))))

(define (split-even lst)
  (cond ((null lst) nil)
        ((null (cdr lst)) nil)
        (t (cons (car (cdr lst)) (split-even (cdr (cdr lst)))))))

; Merge sort
(define (msort lst)
  (cond ((null lst) nil)
        ((null (cdr lst)) lst)
        (t (merge-lists (msort (split-odd lst))
                        (msort (split-even lst))))))

(print "--- Merge Sort ---")
(define data2 '(10 4 7 1 3 9 8 2 6 5))
(print "Input:  ") (print data2)
(print "Sorted: ") (print (msort data2))

; --- Verify sorted ---
(define (sortedp lst)
  (cond ((null lst) t)
        ((null (cdr lst)) t)
        ((<= (car lst) (car (cdr lst))) (sortedp (cdr lst)))
        (t nil)))

(print "--- Verification ---")
(print "Insertion sorted?") (print (sortedp (isort data1)))
(print "Merge sorted?") (print (sortedp (msort data2)))

; --- More examples ---
(define data3 '(100 1 50 25 75 10 99))
(print "--- Large values ---")
(print "Input:  ") (print data3)
(print "Sorted: ") (print (msort data3))
(print "Sorted?") (print (sortedp (msort data3)))

; Sort a sequence
(define data4 (seq 1 15))
(print "--- Sort 1..15 ---")
(print "Already sorted?") (print (sortedp data4))

(print "=== Functional Sorting Done ===")
