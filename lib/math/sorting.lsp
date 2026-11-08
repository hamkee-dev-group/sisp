; ============================================================
; lib/math/sorting.lsp -- sorting and searching
;
; Public:   (merge-sort less lst) -> sorted list
;           (merge less a b) -> merged sorted list
;           (binary-search cmp vec target) -> index | nil
; Private:  binary-search-range
; Depends:  none (uses prelude: take, drop, length, truncate, nth)
; Assumes:  less/cmp is a strict total order; binary-search requires sorted input
; ============================================================

(define (merge less a b)
  "merge two sorted lists a and b using comparison less, preserving stability"
  (cond ((null a) b)
        ((null b) a)
        ((less (car b) (car a))
         (cons (car b) (merge less a (cdr b))))
        (t
         (cons (car a) (merge less (cdr a) b)))))

(define (merge-sort less lst)
  "stable O(n log n) comparison-based sort"
  (cond ((or (null lst) (null (cdr lst))) lst)
        (t (let* ((n (length lst))
                  (mid (truncate (/ n 2)))
                  (left (take lst mid))
                  (right (drop lst mid)))
             (merge less
                    (merge-sort less left)
                    (merge-sort less right))))))

(define (binary-search cmp vec target)
  "return index of target in sorted list vec, or nil; cmp(v,target) means v < target"
  (binary-search-range cmp vec target 0 (- (length vec) 1)))

(define (binary-search-range cmp vec target lo hi)
  (cond ((> lo hi) nil)
        (t (let* ((mid (truncate (/ (+ lo hi) 2)))
                  (val (nth mid vec)))
             (cond ((cmp target val)
                    (binary-search-range cmp vec target lo (- mid 1)))
                   ((cmp val target)
                    (binary-search-range cmp vec target (+ mid 1) hi))
                   (t mid))))))
