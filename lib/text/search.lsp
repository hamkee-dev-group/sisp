; ============================================================
; lib/text/search.lsp -- Knuth-Morris-Pratt string search
;
; Public:   (string-search text pattern) -> index | nil
;           (string-find-all text pattern) -> list of indices
; Private:  kmp-table, kmp-t, kmp-search-chars, kmp-s,
;           kmp-find-all-chars
; Depends:  none (uses builtins str-to-list, strlen)
; Derived-from: examples/math/strings/kmp-string-matching.lsp (core table
;               and scan, verified; wrapped to strings with the
;               empty-pattern contract fixed, plus find-all from the
;               cross-checker in examples/math/aho-corasick.lsp)
; Assumes:  string-search of an empty pattern returns 0 (Python str.find
;           semantics); string-find-all reports ALL occurrences including
;           overlapping ones (CLRS semantics) and rejects an empty
;           pattern with error; the builtin str-find is naive -- this
;           module provides the linear-time guarantee
; ============================================================

; failure table for a char list pat: tab[j] = automaton fallback for
; state j; tab[0] = -1, tab[m] = border of the full pattern
(define (kmp-table pat)
  (kmp-t pat 2 0 (list -1 0)))

(define (kmp-t pat j k tab)
  (cond ((> j (length pat)) tab)
        ((equal (nth (- j 1) pat) (nth k pat))
         (kmp-t pat (+ j 1) (+ k 1) (append tab (list (+ k 1)))))
        ((> k 0) (kmp-t pat j (nth k tab) tab))
        (t (kmp-t pat (+ j 1) 0 (append tab (list 0))))))

; scan: txt and pat are char lists; returns index of first match or nil
(define (kmp-search-chars txt pat)
  (kmp-s txt pat (kmp-table pat) 0 0))

(define (kmp-s txt pat tab i j)
  (cond ((>= i (length txt)) nil)
        ((equal (nth i txt) (nth j pat))
         (cond ((= (+ j 1) (length pat)) (- i j))
               (t (kmp-s txt pat tab (+ i 1) (+ j 1)))))
        ((= j 0) (kmp-s txt pat tab (+ i 1) 0))
        (t (kmp-s txt pat tab i (nth j tab)))))

; all matches (overlapping included), left to right: same automaton,
; continuing from the failure link after each completed match
(define (kmp-find-all-chars txt pat)
  (kmp-fa txt pat (kmp-table pat) 0 0 nil))

(define (kmp-fa txt pat tab i j acc)
  (cond ((>= i (length txt)) (reverse acc))
        ((equal (nth i txt) (nth j pat))
         (cond ((= (+ j 1) (length pat))
                (kmp-fa txt pat tab (+ i 1) (nth (+ j 1) tab)
                        (cons (- i j) acc)))
               (t (kmp-fa txt pat tab (+ i 1) (+ j 1) acc))))
        ((= j 0) (kmp-fa txt pat tab (+ i 1) 0 acc))
        (t (kmp-fa txt pat tab i (nth j tab) acc))))

(define (string-search text pattern)
  "index of the first occurrence of pattern in text, or nil; empty pattern -> 0"
  (cond ((= (length pattern) 0) 0)
        (t (kmp-search-chars (str-to-list text) (str-to-list pattern)))))

(define (string-find-all text pattern)
  "indices of all occurrences of pattern in text, overlapping included"
  (cond ((= (length pattern) 0)
         (error "STRING-FIND-ALL: EMPTY PATTERN" text))
        (t (kmp-find-all-chars (str-to-list text) (str-to-list pattern)))))
