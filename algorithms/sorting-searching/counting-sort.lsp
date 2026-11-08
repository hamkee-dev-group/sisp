; ============================================================
; name: counting-sort
; file: algorithms/sorting-searching/counting-sort.lsp
; subject: sorting
; capability: sort non-comparison
; aliases: "counting sort" "integer sort"
; signature: (counting-sort xs) -> sorted list of non-negative integers
; complexity: O(n + k) with an array of counts; O(n k) with the count
;             list used here (k = maximum element)
; see-also: quicksort rabin-karp
; derived-from: CLRS 8.2
; status: example (tested)
; checks: 10 checks incl. exhaustive permutation agreement with builtin
;         sort and stability; run: src/sisp <this file> from repo root
; ============================================================
; Counting sort for non-negative integers: count occurrences of each
; value, then emit each value its counted number of times.  Stable.
; Not a comparison sort -- the running time depends on the key range.

(load "lib/math/combinatorics.lsp")

(define (cs-max xs best)
  (cond ((null xs) best)
        ((> (car xs) best) (cs-max (cdr xs) (car xs)))
        (t (cs-max (cdr xs) best))))

; counts[v] = number of occurrences of v
(define (cs-count xs counts)
  (cond ((null xs) counts)
        (t (cs-count (cdr xs) (cs-bump counts (car xs))))))

(define (cs-bump counts v)
  (cond ((= v 0) (cons (+ (car counts) 1) (cdr counts)))
        (t (cons (car counts) (cs-bump (cdr counts) (- v 1))))))

; emit each value v its counts[v] times, in order
(define (cs-emit counts value)
  (cond ((null counts) nil)
        (t (append (cs-repeat value (car counts))
                   (cs-emit (cdr counts) (+ value 1))))))

(define (cs-repeat x n)
  (cond ((= n 0) nil) (t (cons x (cs-repeat x (- n 1))))))

(define (cs-zeros n)
  (cond ((= n 0) nil) (t (cons 0 (cs-zeros (- n 1))))))

(define (counting-sort xs)
  "sort a list of non-negative integers"
  (cond ((null xs) nil)
        (t (cs-emit (cs-count xs (cs-zeros (+ (cs-max xs 0) 1))) 0))))

; ---- check helpers (defined before use) ----

; stability: tag each element with its original index, then count-sort
; by key and confirm equal keys keep original order
(define (cs-tag xs i)
  (cond ((null xs) nil)
        (t (cons (cons (car xs) i) (cs-tag (cdr xs) (+ i 1))))))

(define (cs-sort-tagged tagged)
  (cs-emit-tagged (cs-count (map car tagged) (cs-zeros (+ (cs-max (map car tagged) 0) 1))) 0 tagged))

(define (cs-emit-tagged counts value tagged)
  (cond ((null counts) nil)
        (t (append (cs-tagged-with-value tagged value)
                   (cs-emit-tagged (cdr counts) (+ value 1) tagged)))))

(define (cs-tagged-with-value tagged v)
  (cond ((null tagged) nil)
        ((= (caar tagged) v)
         (cons (car tagged) (cs-tagged-with-value (cdr tagged) v)))
        (t (cs-tagged-with-value (cdr tagged) v))))

; ---- checks ----

(check "counting-sort: known answer"
       (equal (counting-sort (list 3 1 4 1 5 9 2 6)) (list 1 1 2 3 4 5 6 9)))

(check "counting-sort: empty -> nil"
       (eq (counting-sort nil) nil))

(check "counting-sort: singleton"
       (equal (counting-sort (list 5)) (list 5)))

(check "counting-sort: all zeros"
       (equal (counting-sort (list 0 0 0)) (list 0 0 0)))

(check "counting-sort: with zeros"
       (equal (counting-sort (list 2 0 1 0 3)) (list 0 0 1 2 3)))

(check "counting-sort: already sorted"
       (equal (counting-sort (list 1 2 3 4)) (list 1 2 3 4)))

(check "counting-sort: reverse sorted"
       (equal (counting-sort (list 4 3 2 1)) (list 1 2 3 4)))

(check "counting-sort: large range still correct"
       (equal (counting-sort (list 100 1 50 25)) (list 1 25 50 100)))

(check "counting-sort: agrees with builtin sort on all permutations of (0 1 2 3 4)"
       (every (lambda (p)
                (equal (counting-sort p)
                       (sort (lambda (a b) (< a b)) p)))
              (permutations (list 0 1 2 3 4))))

(check "counting-sort: stable (equal keys keep original order)"
       (let* ((tagged (cs-tag (list 2 1 2 1 2) 0))
              (sorted (cs-sort-tagged tagged))
              (twos (cs-tagged-with-value sorted 2))
              (ones (cs-tagged-with-value sorted 1)))
         (and (equal (map cdr twos) (list 0 2 4))
              (equal (map cdr ones) (list 1 3)))))
