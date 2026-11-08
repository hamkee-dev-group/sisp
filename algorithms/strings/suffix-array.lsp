; ============================================================
; name: suffix-array
; file: algorithms/strings/suffix-array.lsp
; subject: strings
; capability: string index search
; aliases: "suffix array" "sa" "suffix sort"
; signature: (suffix-array s) -> index list
;            (sa-lcp s i j) -> integer
;            (sa-longest-repeated s) -> string
;            (sa-search s pattern) -> index | nil
; complexity: O(n^2 log n) construction (naive sort), O(m log n) search
; see-also: trie rabin-karp string-search
; derived-from: Manber & Myers 1993; built here by sorting the
;               suffixes with a lexicographic comparator
; status: example (tested)
; checks: 11 checks incl. the textbook banana vector;
;         run: src/sisp <this file> from repo root
; ============================================================
; Suffix array: the start indices of a string's suffixes in
; lexicographic order.  Characters are compared by char-code.

; lexicographic compare of two char lists: -1, 0, or 1
(define (sa-cmp xs ys)
  (cond ((null xs) (cond ((null ys) 0) (t -1)))
        ((null ys) 1)
        ((< (char-code (car xs)) (char-code (car ys))) -1)
        ((> (char-code (car xs)) (char-code (car ys))) 1)
        (t (sa-cmp (cdr xs) (cdr ys)))))

; all suffix start indices 0..n-1
(define (sa-indices n)
  (cond ((= n 0) nil) (t (cons (- n 1) (sa-indices (- n 1))))))

; the suffix starting at i
(define (sa-suffix chars i) (drop chars i))

(define (sa-less chars a b)
  (< (sa-cmp (sa-suffix chars a) (sa-suffix chars b)) 0))

(define (suffix-array s)
  "start indices of the suffixes of s in lexicographic order"
  (let ((chars (str-to-list s)))
    (sort (lambda (a b) (sa-less chars a b))
          (reverse (sa-indices (length s))))))

; longest common prefix length of suffixes i and j
(define (sa-lcp s i j)
  (sa-lcp-walk (str-to-list s) i j 0))

(define (sa-lcp-walk chars i j acc)
  (cond ((or (>= i (length (list-to-str chars)))
             (>= j (length (list-to-str chars)))) acc)
        (t (cond ((equal (nth i chars) (nth j chars))
                  (sa-lcp-walk chars (+ i 1) (+ j 1) (+ acc 1)))
                 (t acc)))))

; longest repeated substring (naive: max lcp over adjacent SA entries)
(define (sa-longest-repeated s)
  (let* ((sa (suffix-array s))
         (best (sa-lrs-walk (str-to-list s) sa 0 0)))
    (sa-substr (str-to-list s) (car best) (cadr best))))

(define (sa-substr chars start len)
  (list-to-str (take (drop chars start) len)))

(define (sa-lrs-walk chars sa best-i best-len)
  (cond ((or (null sa) (null (cdr sa))) (list best-i best-len))
        (t (let ((l (sa-lcp (list-to-str chars) (car sa) (cadr sa))))
             (cond ((> l best-len)
                    (sa-lrs-walk chars (cdr sa) (car sa) l))
                   (t (sa-lrs-walk chars (cdr sa) best-i best-len)))))))

; binary search for pattern in the suffix array
(define (sa-search s pattern)
  "start index of pattern in s, or nil"
  (sa-search-walk (str-to-list s) (str-to-list pattern)
                  (suffix-array s) 0 (- (length (suffix-array s)) 1)))

(define (sa-search-walk chars pat sa lo hi)
  (cond ((> lo hi) nil)
        (t (let* ((mid (truncate (/ (+ lo hi) 2)))
                  (suf-i (nth mid sa))
                  (c (sa-cmp-prefix (drop chars suf-i) pat)))
             (cond ((= c 0) suf-i)
                   ((< c 0) (sa-search-walk chars pat sa (+ mid 1) hi))
                   (t (sa-search-walk chars pat sa lo (- mid 1))))))))

; compare suffix against pattern: -1/0/1 over the pattern's length
(define (sa-cmp-prefix suf pat)
  (cond ((null pat) 0)
        ((null suf) -1)
        ((< (char-code (car suf)) (char-code (car pat))) -1)
        ((> (char-code (car suf)) (char-code (car pat))) 1)
        (t (sa-cmp-prefix (cdr suf) (cdr pat)))))

; ---- check helpers (defined before use) ----

; searching every full suffix finds SOME position whose suffix starts
; with it, and never a position after the suffix itself
(define (sa-all-found s)
  (let ((sa (suffix-array s)))
    (sa-all-found-walk s sa)))

(define (sa-all-found-walk s sa)
  (cond ((null sa) t)
        (t (let* ((chars (str-to-list s))
                  (pat-chars (drop chars (car sa)))
                  (hit (sa-search s (list-to-str pat-chars))))
             (and (not (eq hit nil))
                  (<= hit (car sa))
                  (= (sa-cmp-prefix (drop chars hit) pat-chars) 0)
                  (sa-all-found-walk s (cdr sa)))))))

; ---- checks ----

(check "suffix-array: banana -> (5 3 1 0 4 2) (textbook)"
       (equal (suffix-array "banana") (list 5 3 1 0 4 2)))

(check "suffix-array: aaaa -> (3 2 1 0)"
       (equal (suffix-array "aaaa") (list 3 2 1 0)))

(check "suffix-array: abc -> (0 1 2)"
       (equal (suffix-array "abc") (list 0 1 2)))

(check "suffix-array: single character -> (0)"
       (equal (suffix-array "x") (list 0)))

(check "suffix-array: lcp of ana and anana in banana = 3"
       (= (sa-lcp "banana" 1 3) 3))

(check "suffix-array: longest repeated substring of banana = ana"
       (equal (sa-longest-repeated "banana") "ana"))

(check "suffix-array: longest repeated of abcabc = abc"
       (equal (sa-longest-repeated "abcabc") "abc"))

(check "suffix-array: search ana in banana -> 1"
       (= (sa-search "banana" "ana") 1))

(check "suffix-array: search banana in banana -> 0"
       (= (sa-search "banana" "banana") 0))

(check "suffix-array: search absent -> nil"
       (eq (sa-search "banana" "apple") nil))

(check "suffix-array: every suffix is findable with prefix agreement"
       (sa-all-found "abracadabra"))
