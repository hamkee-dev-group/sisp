; ============================================================
; name: quicksort
; file: algorithms/sorting-searching/quicksort.lsp
; subject: sorting
; capability: sort comparison
; aliases: "quick sort" "partition sort"
; signature: (quicksort less xs) -> sorted list
; complexity: O(n log n) average, O(n^2) worst (adversarial input)
; see-also: merge-sort heapsort quickselect
; derived-from: Hoare 1962; CLRS 7
; status: example (tested)
; checks: 11 checks incl. model comparison vs builtin sort on all
;         permutations of 5; run: src/sisp <this file> from repo root
; ============================================================
; Quicksort with first-element pivot and a two-way list partition: split the
; rest into (smaller . not-smaller) and recurse.  This list version is
; stable: the partition keeps input order on each side, and keys equal to
; the pivot follow it.  For a
; guaranteed O(n log n) sort use merge-sort; quicksort here is the
; canonical partition algorithm, checked exhaustively.

(load "lib/math/combinatorics.lsp")

(define (qs-partition less pivot xs)
  (cond ((null xs) (cons nil nil))
        (t (let ((r (qs-partition less pivot (cdr xs))))
             (cond ((less (car xs) pivot)
                    (cons (cons (car xs) (car r)) (cdr r)))
                   (t (cons (car r) (cons (car xs) (cdr r)))))))))

(define (quicksort less xs)
  "sort xs by comparison function less (strict total order)"
  (cond ((null xs) nil)
        (t (let ((parts (qs-partition less (car xs) (cdr xs))))
             (append (quicksort less (car parts))
                     (cons (car xs)
                           (quicksort less (cdr parts))))))))

; ---- check helpers (defined before use) ----

(define (qs-sorted-p less xs)
  (cond ((or (null xs) (null (cdr xs))) t)
        (t (and (not (less (cadr xs) (car xs)))
                (qs-sorted-p less (cdr xs))))))

; ---- checks ----

(check "quicksort: known answer"
       (equal (quicksort (lambda (a b) (< a b)) (list 3 1 4 1 5 9 2 6))
              (list 1 1 2 3 4 5 6 9)))

(check "quicksort: already sorted"
       (equal (quicksort (lambda (a b) (< a b)) (list 1 2 3 4 5))
              (list 1 2 3 4 5)))

(check "quicksort: reverse sorted"
       (equal (quicksort (lambda (a b) (< a b)) (list 5 4 3 2 1))
              (list 1 2 3 4 5)))

(check "quicksort: empty -> nil"
       (eq (quicksort (lambda (a b) (< a b)) nil) nil))

(check "quicksort: singleton"
       (equal (quicksort (lambda (a b) (< a b)) (list 42)) (list 42)))

(check "quicksort: all equal"
       (equal (quicksort (lambda (a b) (< a b)) (list 7 7 7)) (list 7 7 7)))

(check "quicksort: descending comparison"
       (equal (quicksort (lambda (a b) (> a b)) (list 3 1 4 1 5))
              (list 5 4 3 1 1)))

(check "quicksort: output is sorted (property)"
       (qs-sorted-p (lambda (a b) (< a b))
                    (quicksort (lambda (a b) (< a b))
                               (list 9 3 7 1 5 8 2 6 4))))

(check "quicksort: output is a permutation of input (property)"
       (equal (sort (lambda (a b) (< a b))
                    (quicksort (lambda (a b) (< a b)) (list 5 3 9 1 7)))
              (sort (lambda (a b) (< a b)) (list 5 3 9 1 7))))

(check "quicksort: matches builtin sort on all 120 permutations of 5"
       (every (lambda (p)
                (equal (quicksort (lambda (a b) (< a b)) p)
                       (sort (lambda (a b) (< a b)) p)))
              (permutations (list 1 2 3 4 5))))

(check "quicksort: works on strings with a custom order (by length)"
       (equal (quicksort (lambda (a b) (< (length a) (length b)))
                         (list "pear" "apple" "fig"))
              (list "fig" "pear" "apple")))
