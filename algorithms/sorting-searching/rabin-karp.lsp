; ============================================================
; name: rabin-karp
; file: algorithms/sorting-searching/rabin-karp.lsp
; subject: searching
; capability: string search rolling-hash
; aliases: "rabin karp" "rolling hash search" "fingerprint search"
; signature: (rk-search text pattern) -> index | nil
;            (rk-find-all text pattern) -> list of indices
; complexity: O(n + m) expected, O(nm) worst (hash collisions)
; see-also: string-search (KMP, lib/text/search.lsp)
; derived-from: Karp & Rabin 1987
; status: example (tested)
; checks: 12 checks incl. model comparison vs lib KMP on 12 fixed
;         small-alphabet strings; run: src/sisp <this file> from repo root
; ============================================================
; Rabin-Karp string search: hash the pattern and each length-m window
; of the text with a polynomial rolling hash (base 256, prime modulus
; 1000000007); compare character-by-character only on hash hits.
; Characters arrive as one-byte strings via str-to-list.

(load "lib/text/search.lsp")

(define rk-base 256)
(define rk-mod 1000000007)

; polynomial hash of a character list
(define (rk-hash chars)
  (rk-hash-walk chars 0))

(define (rk-hash-walk chars h)
  (cond ((null chars) h)
        (t (rk-hash-walk (cdr chars)
                         (mod (+ (* h rk-base) (char-code (car chars)))
                              rk-mod)))))

; base^(m-1) mod rk-mod (the weight of the leaving character)
(define (rk-high-power m)
  (rk-pow rk-base (- m 1)))

(define (rk-pow b e)
  (cond ((= e 0) 1)
        (t (mod (* b (rk-pow b (- e 1))) rk-mod))))

; roll the hash: remove the outgoing char, append the incoming char
(define (rk-roll h out-ch in-ch hp)
  (mod (+ (* (- h (* (char-code out-ch) hp)) rk-base)
          (char-code in-ch))
       rk-mod))

; char-by-character comparison of a window against the pattern
(define (rk-match-at txt pat i)
  (cond ((null pat) t)
        ((null txt) nil)
        ((equal (car txt) (car pat))
         (rk-match-at (cdr txt) (cdr pat) (+ i 1)))
        (t nil)))

; walk the text with the rolling hash; txt is the remaining window,
; i the window's start, m the pattern length, ph the pattern hash
(define (rk-walk txt pat i m ph h hp acc)
  (cond ((< (length txt) m) (reverse acc))
        (t (let ((hits (cond ((and (= h ph) (rk-match-at txt pat i))
                              (cons i acc))
                             (t acc))))
             (cond ((< (length txt) (+ m 1))
                    (reverse hits))
                   (t (rk-walk (cdr txt) pat (+ i 1) m ph
                               (rk-roll h (car txt) (nth m txt) hp)
                               hp hits)))))))

(define (rk-find-all text pattern)
  "start indices of every occurrence of pattern in text"
  (cond ((= (length pattern) 0)
         (error "RK-FIND-ALL: EMPTY PATTERN" text))
        (t (let ((txt (str-to-list text))
                 (pat (str-to-list pattern)))
             (cond ((> (length pattern) (length text)) nil)
                   (t (rk-walk txt pat 0 (length pattern)
                               (rk-hash pat)
                               (rk-hash (take txt (length pattern)))
                               (rk-high-power (length pattern))
                               nil)))))))

(define (rk-search text pattern)
  "index of the first occurrence of pattern in text, or nil"
  (let ((hits (rk-find-all text pattern)))
    (cond ((null hits) nil)
          (t (car hits)))))

; ---- check helpers (defined before use) ----

(define rk-seeded-cases
  (list (list "aabbaabbaab" "abb")
        (list "ababababab" "ab")
        (list "aaaaaaaaaa" "aaa")
        (list "babababab" "ba")
        (list "aabaaabaaab" "aab")
        (list "bbabbabba" "bba")
        (list "abcabcabca" "bca")
        (list "aaaaaab" "aab")
        (list "bababaaba" "aba")
        (list "aabbbbaa" "abbb")
        (list "ababbaab" "baab")
        (list "bbaaabbaaa" "aa")))

(define (rk-kmp-agree-all cases)
  (cond ((null cases) t)
        (t (and (equal (rk-search (caar cases) (cadar cases))
                       (string-search (caar cases) (cadar cases)))
                (equal (rk-find-all (caar cases) (cadar cases))
                       (string-find-all (caar cases) (cadar cases)))
                (rk-kmp-agree-all (cdr cases))))))

; ---- checks ----

(check "rabin-karp: known answer (world in hello world -> 6)"
       (= (rk-search "hello world" "world") 6))

(check "rabin-karp: match at start -> 0"
       (= (rk-search "abcdef" "abc") 0))

(check "rabin-karp: match at end"
       (= (rk-search "abcdef" "def") 3))

(check "rabin-karp: not found -> nil"
       (eq (rk-search "abcdef" "xyz") nil))

(check "rabin-karp: pattern longer than text -> nil"
       (eq (rk-search "ab" "abc") nil))

(check "rabin-karp: whole-string match -> 0"
       (= (rk-search "abc" "abc") 0))

(check "rabin-karp: find-all overlapping (aa in aaaa -> 0 1 2)"
       (equal (rk-find-all "aaaa" "aa") (list 0 1 2)))

(check "rabin-karp: find-all none -> nil"
       (eq (rk-find-all "abcdef" "xyz") nil))

(check "rabin-karp: agrees with KMP on every seeded random case"
       (rk-kmp-agree-all rk-seeded-cases))

(check "rabin-karp: hash of the empty string is 0"
       (= (rk-hash nil) 0))

(check "rabin-karp: polynomial hash known value (\"a\" -> 97)"
       (= (rk-hash (str-to-list "a")) 97))

(check "rabin-karp: empty-pattern rejection is catchable"
       (equal (car (catch (rk-find-all "abc" "") (lambda (e) e)))
              (quote error)))
