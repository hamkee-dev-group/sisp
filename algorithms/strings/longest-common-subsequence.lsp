; ============================================================
; name: longest-common-subsequence
; file: algorithms/strings/longest-common-subsequence.lsp
; subject: strings
; capability: dynamic-programming subsequence
; aliases: "lcs" "longest common subsequence"
; signature: (lcs s t) -> subsequence string
;            (lcs-length s t) -> integer
; complexity: O(mn) time and space in this DP-table version
; see-also: edit-distance
; derived-from: CLRS 15.4
; status: example (tested)
; checks: 11 checks incl. textbook known answers; run: src/sisp <file>
; ============================================================
; Longest common subsequence: the longest string obtainable from both
; s and t by deleting characters (order preserved, contiguity not
; required).  Bottom-up DP of prefix lengths, then backtrack.

; DP cell value for prefixes s[0..i) t[0..j), given the previous row
(define (lcs-row ts ch prev)
  (lcs-row-walk ts ch prev (list 0)))

(define (lcs-row-walk ts ch prev acc)
  (cond ((null ts) (reverse acc))
        (t (let* ((j (length acc))
                  (cell (cond ((equal ch (car ts))
                               (+ (nth (- j 1) prev) 1))
                              (t (max (nth j prev)
                                      (car acc))))))
             (lcs-row-walk (cdr ts) ch prev (cons cell acc))))))

(define (lcs-rows ss ts prev)
  (cond ((null ss) prev)
        (t (lcs-rows (cdr ss) ts (lcs-row ts (car ss) prev)))))

(define (lcs-zero-row m)
  (cond ((= m 0) (list 0))
        (t (cons 0 (lcs-zero-row (- m 1))))))

; build the full DP table (list of rows) for backtracking
(define (lcs-table ss ts acc)
  (cond ((null ss) (reverse acc))
        (t (let ((row (lcs-row ts (car ss) (car acc))))
             (lcs-table (cdr ss) ts (cons row acc))))))

; backtrack from the bottom-right to recover the subsequence
(define (lcs-backtrack table ss ts i j acc)
  (cond ((or (= i 0) (= j 0)) acc)
        ((equal (nth (- i 1) ss) (nth (- j 1) ts))
         (lcs-backtrack table ss ts (- i 1) (- j 1)
                        (cons (nth (- i 1) ss) acc)))
        ((>= (nth j (nth (- i 1) table))
             (nth (- j 1) (nth i table)))
         (lcs-backtrack table ss ts (- i 1) j acc))
        (t (lcs-backtrack table ss ts i (- j 1) acc))))

(define (lcs s target)
  "the longest common subsequence of s and target, as a string"
  (let* ((ss (str-to-list s))
         (ts (str-to-list target))
         (table (lcs-table ss ts (list (lcs-zero-row (length target))))))
    (list-to-str (lcs-backtrack table ss ts (length s) (length target) nil))))

(define (lcs-length s target)
  "the length of the longest common subsequence"
  (length (lcs s target)))

; ---- check helpers (defined before use) ----

; is x a subsequence of y
(define (lcs-subseq-p x y)
  (lcs-subseq-walk (str-to-list x) (str-to-list y)))

(define (lcs-subseq-walk xs ys)
  (cond ((null xs) t)
        ((null ys) nil)
        ((equal (car xs) (car ys))
         (lcs-subseq-walk (cdr xs) (cdr ys)))
        (t (lcs-subseq-walk xs (cdr ys)))))

; ---- checks ----

(check "lcs: ABCBDAB vs BDCABA -> BCBA length 4 (CLRS)"
       (= (lcs-length "ABCBDAB" "BDCABA") 4))

(check "lcs: AGGTAB vs GXTXAYB -> GTAB length 4 (textbook)"
       (= (lcs-length "AGGTAB" "GXTXAYB") 4))

(check "lcs: identical strings -> whole string"
       (equal (lcs "abcde" "abcde") "abcde"))

(check "lcs: nothing in common -> empty"
       (equal (lcs "abc" "def") ""))

(check "lcs: empty left -> empty"
       (equal (lcs "" "abc") ""))

(check "lcs: empty right -> empty"
       (equal (lcs "abc" "") ""))

(check "lcs: subsequence of the result holds in both strings"
       (and (lcs-subseq-p (lcs "AGGTAB" "GXTXAYB") "AGGTAB")
            (lcs-subseq-p (lcs "AGGTAB" "GXTXAYB") "GXTXAYB")))

(check "lcs: length symmetric"
       (= (lcs-length "ABCBDAB" "BDCABA")
          (lcs-length "BDCABA" "ABCBDAB")))

(check "lcs: preserves order (XMJYAUZ -> MZJAWXU has MJAU? length 4)"
       (= (lcs-length "XMJYAUZ" "MZJAWXU") 4))

(check "lcs: result is a subsequence of both inputs (CLRS case)"
       (and (lcs-subseq-p (lcs "ABCBDAB" "BDCABA") "ABCBDAB")
            (lcs-subseq-p (lcs "ABCBDAB" "BDCABA") "BDCABA")))

(check "lcs: repeated characters (aaa vs aa -> 2)"
       (= (lcs-length "aaa" "aa") 2))
