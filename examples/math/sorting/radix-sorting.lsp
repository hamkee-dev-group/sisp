; ============================================================
; name: radix-sort
; subject: sorting
; capability: non-comparison sort
; status: healthy
; canonical-home: none
; notes: LSD radix sort demo.
; ============================================================
; ============================================================
; 97 — Radix Sorting (TAOCP Vol 3, 5.2.5)
; Stable LSD radix sort on integers in base 10. Sort by each
; digit from least to most significant. Verify stability:
; equal keys maintain relative order. MSD radix sort
; recursively. Compare both with builtin sort on random
; lists. Exhaustively test all permutations of 4 two-digit
; numbers.
; ============================================================

(print "=== Radix Sorting (TAOCP 5.2.5) ===")


(define (list-with xs i v)
  (cond ((= i 0) (cons v (cdr xs)))
        (t (cons (car xs) (list-with (cdr xs) (- i 1) v)))))

; ---------- digit extraction ----------
(define (digit-of n pos)
  (mod (truncate (/ n (^ 10 pos))) 10))

(define (num-digits n)
  (cond ((< n 10) 1)
        (t (+ 1 (num-digits (truncate (/ n 10)))))))

(define (max-digits xs)
  (cond ((eq xs nil) 0)
        (t (foldl (lambda (m x) (max m (num-digits x))) 0 xs))))

; ---------- stable counting sort by digit at position pos ----------
(define (concat-buckets buckets acc)
  (cond ((eq buckets nil) acc)
        (t (concat-buckets (cdr buckets) (append acc (car buckets))))))

(define (cs-collect buckets xs pos)
  (cond ((eq xs nil) (concat-buckets buckets nil))
        (t (let ((d (digit-of (car xs) pos)))
             (cs-collect
               (list-with buckets d
                 (append (nth d buckets) (list (car xs))))
               (cdr xs) pos)))))

(define (counting-sort-by-digit xs pos)
  (cs-collect (map (lambda (i) nil) (seq 0 9)) xs pos))

; ---------- LSD radix sort ----------
(define (lsd-by-digit xs pos max-pos)
  (cond ((>= pos max-pos) xs)
        (t (lsd-by-digit (counting-sort-by-digit xs pos) (+ pos 1) max-pos))))

(define (lsd-radix-sort xs)
  (lsd-by-digit xs 0 (max-digits xs)))

; ---------- MSD radix sort ----------
(define (fill-buckets sorted pos buckets)
  (cond ((eq sorted nil) buckets)
        (t (let ((d (digit-of (car sorted) pos)))
             (fill-buckets (cdr sorted) pos
               (list-with buckets d
                 (append (nth d buckets) (list (car sorted)))))))))

(define (msd-recurse buckets pos)
  (cond ((eq buckets nil) nil)
        (t (let ((bucket (car buckets)))
             (append
               (cond ((< pos 0) bucket)
                     ((eq (cdr bucket) nil) bucket)
                     (t (msd-sort bucket (- pos 1))))
               (msd-recurse (cdr buckets) pos))))))

; ---------- MSD radix sort ----------
(define (msd-sort ys pos)
  (cond ((< pos 0) ys)
        ((eq (cdr ys) nil) ys)
        (t (let ((sorted (counting-sort-by-digit ys pos)))
             (msd-recurse
               (fill-buckets sorted pos
                 (map (lambda (i) nil) (seq 0 9)))
               pos)))))

(define (msd-radix-sort xs)
  (msd-sort xs (- (max-digits xs) 1)))

; ---------- tests ----------
(define test-data (list 170 45 75 90 802 24 2 66))
(define lsd-sorted (lsd-radix-sort test-data))
(define expected-sorted (sort (lambda (x y) (< x y)) test-data))
(check "lsd-radix-sort-correct" (equal lsd-sorted expected-sorted))
(print (list (quote lsd-result) lsd-sorted))

; ---------- stability test ----------
; Use pairs where the "value" determines sort order and "id" tracks original order.
; sorted-by-key: LSD sort using key-fn for digit extraction
(define (key-cs-collect buckets xs key-fn pos)
  (cond ((eq xs nil) (concat-buckets buckets nil))
        (t (let ((d (digit-of (key-fn (car xs)) pos)))
             (key-cs-collect
               (list-with buckets d
                 (append (nth d buckets) (list (car xs))))
               (cdr xs) key-fn pos)))))

(define (key-counting-sort xs key-fn pos)
  (key-cs-collect (map (lambda (i) nil) (seq 0 9)) xs key-fn pos))

(define (key-lsd-by-digit xs key-fn pos max-pos)
  (cond ((>= pos max-pos) xs)
        (t (key-lsd-by-digit
            (key-counting-sort xs key-fn pos) key-fn (+ pos 1) max-pos))))

(define (lsd-sort-by-key xs key-fn)
  (key-lsd-by-digit xs key-fn 0 (max-digits (map key-fn xs))))

(define stability-data
  (list (list 5 1) (list 2 2) (list 5 3) (list 7 4)
        (list 5 5) (list 2 6) (list 7 7)))

(define stable-sorted
  (lsd-sort-by-key stability-data car))

(print (list (quote stability-input) stability-data))
(print (list (quote stability-output) stable-sorted))

; verify sorted by value
(define (sorted-by-first-p xs)
  (cond ((eq (cdr xs) nil) t)
        ((<= (car (car xs)) (car (cadr xs))) (sorted-by-first-p (cdr xs)))
        (t nil)))
(check "stability-sorted-by-value" (sorted-by-first-p stable-sorted))

; verify tags of equal values maintain original order
(define (tags-of-value v xs)
  (map (lambda (p) (cadr p))
       (filter (lambda (p) (= (car p) v)) xs)))

(check "stability-value-5"
  (equal (tags-of-value 5 stable-sorted) (tags-of-value 5 stability-data)))
(check "stability-value-2"
  (equal (tags-of-value 2 stable-sorted) (tags-of-value 2 stability-data)))
(check "stability-value-7"
  (equal (tags-of-value 7 stable-sorted) (tags-of-value 7 stability-data)))

(check "lsd-stable-all-values"
  (every (lambda (v)
    (equal (tags-of-value v stable-sorted) (tags-of-value v stability-data)))
    (list 2 5 7)))

; ---------- MSD radix sort tests ----------
(define msd-sorted (msd-radix-sort test-data))
(check "msd-radix-sort-correct" (equal msd-sorted expected-sorted))
(print (list (quote msd-result) msd-sorted))

; random list tests
(seed 42)
(define (random-ints n digits)
  (cond ((= n 0) nil)
        (t (cons (random (^ 10 digits)) (random-ints (- n 1) digits)))))

(define rand1 (random-ints 10 2))
(define rand2 (random-ints 15 3))

(check "lsd-matches-builtin-rand1"
  (equal (lsd-radix-sort rand1)
         (sort (lambda (x y) (< x y)) rand1)))
(check "lsd-matches-builtin-rand2"
  (equal (lsd-radix-sort rand2)
         (sort (lambda (x y) (< x y)) rand2)))
(check "msd-matches-builtin-rand1"
  (equal (msd-radix-sort rand1)
         (sort (lambda (x y) (< x y)) rand1)))
(check "msd-matches-builtin-rand2"
  (equal (msd-radix-sort rand2)
         (sort (lambda (x y) (< x y)) rand2)))

; ---------- exhaustive: all permutations of 4 two-digit numbers ----------
(define (insert-everywhere x xs)
  (cond ((eq xs nil) (list (list x)))
        (t (cons (cons x xs)
             (map (lambda (p) (cons (car xs) p))
               (insert-everywhere x (cdr xs)))))))
(define (permutations xs)
  (cond ((eq xs nil) (list nil))
        (t (foldl (lambda (acc p) (append (insert-everywhere (car xs) p) acc))
             nil (permutations (cdr xs))))))

(define test-4 (list 12 45 78 23))
(define all-perms-4 (permutations test-4))
(check "24-permutations-of-4-two-digit" (= (length all-perms-4) 24))

(check "lsd-correct-on-all-24-perms"
  (every (lambda (p)
    (equal (lsd-radix-sort p)
           (sort (lambda (x y) (< x y)) p)))
    all-perms-4))

(check "msd-correct-on-all-24-perms"
  (every (lambda (p)
    (equal (msd-radix-sort p)
           (sort (lambda (x y) (< x y)) p)))
    all-perms-4))

(check "lsd-msd-agree-on-all-24-perms"
  (every (lambda (p)
    (equal (lsd-radix-sort p) (msd-radix-sort p)))
    all-perms-4))

; edge cases
(check "lsd-empty" (equal (lsd-radix-sort nil) nil))
(check "lsd-single" (equal (lsd-radix-sort (list 5)) (list 5)))
(check "lsd-duplicates" (equal (lsd-radix-sort (list 3 3 3)) (list 3 3 3)))
(check "msd-empty" (equal (msd-radix-sort nil) nil))
(check "msd-single" (equal (msd-radix-sort (list 5)) (list 5)))
(check "msd-duplicates" (equal (msd-radix-sort (list 3 3 3)) (list 3 3 3)))
(check "lsd-preserves-sorted"
  (equal (lsd-radix-sort (list 1 2 3 4 5)) (list 1 2 3 4 5)))
(check "msd-preserves-sorted"
  (equal (msd-radix-sort (list 1 2 3 4 5)) (list 1 2 3 4 5)))

(print "=== Radix Sorting Done ===")
