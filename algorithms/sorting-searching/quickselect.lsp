; ============================================================
; name: quickselect
; file: algorithms/sorting-searching/quickselect.lsp
; subject: searching
; capability: selection order-statistic
; aliases: "select" "kth smallest" "median-of-medians"
; signature: (quickselect less xs k) -> kth smallest element (0-based)
; complexity: O(n) average, O(n^2) worst (adversarial input)
; see-also: quicksort binary-search
; derived-from: Hoare 1961 (Algorithm 65, FIND)
; status: example (tested)
; checks: 11 checks incl. exhaustive agreement with sorted indexing on
;         all permutations of 5; run: src/sisp <this file>
; ============================================================
; Quickselect (Hoare's FIND): partition around the first element, then
; recurse only into the side that must contain the kth smallest.
; k is 0-based: k = 0 is the minimum, k = n-1 the maximum.

(define (qsel-partition less pivot xs)
  (cond ((null xs) (cons nil nil))
        (t (let ((r (qsel-partition less pivot (cdr xs))))
             (cond ((less (car xs) pivot)
                    (cons (cons (car xs) (car r)) (cdr r)))
                   (t (cons (car r) (cons (car xs) (cdr r)))))))))

(define (quickselect less xs k)
  "the kth smallest element of xs by less (0-based)"
  (cond ((null xs) (error "QUICKSELECT: EMPTY LIST" k))
        (t (let* ((parts (qsel-partition less (car xs) (cdr xs)))
                  (smaller (car parts))
                  (bigger (cdr parts))
                  (n-smaller (length smaller)))
             (cond ((< k n-smaller)
                    (quickselect less smaller k))
                   ((= k n-smaller) (car xs))
                   (t (quickselect less bigger
                                   (- k (+ n-smaller 1)))))))))

; ---- check helpers (defined before use) ----

(load "lib/math/combinatorics.lsp")

(define (qsel-oracle less xs k)
  "obviously correct answer: sort, then take the kth"
  (nth k (sort less xs)))

(define (qsel-all-k less p k)
  (cond ((= k (length p)) t)
        (t (and (= (quickselect less p k) (qsel-oracle less p k))
                (qsel-all-k less p (+ k 1))))))

; ---- checks ----

(check "quickselect: minimum (k = 0)"
       (= (quickselect (lambda (a b) (< a b)) (list 5 3 7 1 4) 0) 1))

(check "quickselect: maximum (k = n-1)"
       (= (quickselect (lambda (a b) (< a b)) (list 5 3 7 1 4) 4) 7))

(check "quickselect: median of 5"
       (= (quickselect (lambda (a b) (< a b)) (list 5 3 7 1 4) 2) 4))

(check "quickselect: first of sorted"
       (= (quickselect (lambda (a b) (< a b)) (list 1 2 3 4 5) 0) 1))

(check "quickselect: last of reverse-sorted"
       (= (quickselect (lambda (a b) (< a b)) (list 5 4 3 2 1) 4) 5))

(check "quickselect: with duplicates"
       (= (quickselect (lambda (a b) (< a b)) (list 3 1 4 1 5) 2) 3))

(check "quickselect: singleton"
       (= (quickselect (lambda (a b) (< a b)) (list 42) 0) 42))

(check "quickselect: matches sorted indexing on a spread"
       (and (= (quickselect (lambda (a b) (< a b)) (list 9 2 8 3 7) 0)
               (qsel-oracle (lambda (a b) (< a b)) (list 9 2 8 3 7) 0))
            (= (quickselect (lambda (a b) (< a b)) (list 9 2 8 3 7) 2)
               (qsel-oracle (lambda (a b) (< a b)) (list 9 2 8 3 7) 2))
            (= (quickselect (lambda (a b) (< a b)) (list 9 2 8 3 7) 4)
               (qsel-oracle (lambda (a b) (< a b)) (list 9 2 8 3 7) 4))))

(check "quickselect: every k on every permutation of 5"
       (every (lambda (p)
                (qsel-all-k (lambda (a b) (< a b)) p 0))
              (permutations (list 1 2 3 4 5))))

(check "quickselect: error on empty list is catchable"
       (equal (car (catch (quickselect (lambda (a b) (< a b)) nil 0)
                          (lambda (e) e)))
              (quote error)))

(check "quickselect: descending comparison selects kth largest"
       (= (quickselect (lambda (a b) (> a b)) (list 5 3 7 1 4) 1) 5))
