; ============================================================
; name: edit-distance
; file: algorithms/strings/edit-distance.lsp
; subject: strings
; capability: dynamic-programming distance
; aliases: "levenshtein" "edit distance" "string distance"
; signature: (edit-distance s target) -> integer
; complexity: O(mn) time, O(mn) space in this DP-table version
; see-also: longest-common-subsequence string-search
; derived-from: Levenshtein 1966; Wagner & Fischer 1974
; status: example (tested)
; checks: 12 checks incl. textbook known answers; run: src/sisp <file>
; ============================================================
; Levenshtein edit distance: the minimum number of single-character
; insertions, deletions, and substitutions turning s into t.
; Bottom-up DP table; strings are compared as character lists.

; the DP cell: min of delete, insert, substitute
(define (ed-cell-cost a b)
  (cond ((equal a b) 0) (t 1)))

; iterate rows over s's characters; returns the final row
(define (ed-rows ss ts prev)
  (cond ((null ss) prev)
        (t (ed-rows (cdr ss) ts (ed-row-fresh ts (car ss) prev)))))

; one full row: row[0] = i+1 (deletions), then the recurrence
(define (ed-row-fresh ts ch prev)
  (ed-row-walk ts ch prev (list (+ (car prev) 1))))

(define (ed-row-walk ts ch prev acc)
  (cond ((null ts) (reverse acc))
        (t (let* ((j (length acc))
                  (above (nth j prev))
                  (left (car acc))
                  (diag (nth (- j 1) prev))
                  (best (min (+ above 1)
                             (min (+ left 1)
                                  (+ diag (ed-cell-cost ch (car ts)))))))
             (ed-row-walk (cdr ts) ch prev (cons best acc))))))

; first row: 0 1 2 ... m (cost of turning "" into t[0..j])
(define (ed-first-row m acc)
  (cond ((< m 0) acc)
        (t (ed-first-row (- m 1) (cons m acc)))))

(define (edit-distance s target)
  "levenshtein distance between strings s and t"
  (let ((ts (str-to-list target)))
    (car (reverse (ed-rows (str-to-list s) ts
                           (ed-first-row (length target) nil))))))

; ---- checks ----

(check "edit-distance: kitten -> sitting = 3 (textbook)"
       (= (edit-distance "kitten" "sitting") 3))

(check "edit-distance: flaw -> lawn = 2 (textbook)"
       (= (edit-distance "flaw" "lawn") 2))

(check "edit-distance: empty -> abc = 3"
       (= (edit-distance "" "abc") 3))

(check "edit-distance: abc -> empty = 3"
       (= (edit-distance "abc" "") 3))

(check "edit-distance: identical = 0"
       (= (edit-distance "algorithm" "algorithm") 0))

(check "edit-distance: one substitution = 1"
       (= (edit-distance "book" "back") 2))

(check "edit-distance: one insertion = 1"
       (= (edit-distance "abc" "abcd") 1))

(check "edit-distance: symmetric"
       (= (edit-distance "sitting" "kitten")
          (edit-distance "kitten" "sitting")))

(check "edit-distance: single characters"
       (and (= (edit-distance "a" "a") 0)
            (= (edit-distance "a" "b") 1)))

(check "edit-distance: completely different = max length"
       (= (edit-distance "aaaa" "bbbb") 4))

(check "edit-distance: gumbo -> gambol = 2 (textbook)"
       (= (edit-distance "gumbo" "gambol") 2))

(check "edit-distance: distance bounded by max length (property)"
       (<= (edit-distance "hello" "world") 5))
