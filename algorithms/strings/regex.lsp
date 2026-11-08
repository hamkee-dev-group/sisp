; ============================================================
; name: regex
; file: algorithms/strings/regex.lsp
; subject: strings
; capability: pattern-matching regular-expressions nfa-simulation
; aliases: "regex" "regular expression" "regex engine" "pattern matching"
; signature: (rx-parse pattern)           -> ast
;            (rx-full-match-p pat text)    -> t/nil   ; whole text matches
;            (rx-find pat text)            -> (start end) | nil  ; leftmost
;            (rx-find-all pat text)        -> ((start end) ...)  ; non-overlapping
; complexity: O(m * n) states tracked per position with a depth budget of
;             n+1; parsing is O(m) over the pattern length m, matching is
;             an NFA-style set simulation over the n-character text
; see-also: lz78 suffix-array
; derived-from: Thompson 1968 (NFA simulation)
; status: example (tested)
; checks: 16 checks covering literals, dot, concat, alternation (with
;         precedence), groups, character classes, star (incl. zero-match
;         and standalone empty), plus, optional, and find/find-all offsets;
;         run: src/sisp <file>
; ============================================================
; A regular-expression parser and matcher.  The parser turns a pattern
; string into an AST; the matcher runs it as an NFA-style simulation that
; tracks a *set* of reachable text positions rather than backtracking.
; Supported syntax: literals, . (dot), (grouping), | (alternation),
; [class], * (star), + (plus), and ? (optional).
;
; Matching threads a set of end-positions through each AST node.  A
; sequence advances the whole set node by node; alternation unions two
; result sets; star computes an epsilon-closure (repeatedly re-applying
; the body until no new position appears), bounded by a depth budget of
; text-length+1 so it always terminates.

(define (rx-char-p characters wanted)
  (and (not (null characters)) (equal (car characters) wanted)))

(define (rx-sequence-ast parts)
  (cond ((null parts) '(empty))
        ((null (cdr parts)) (car parts))
        (t (cons 'seq parts))))

(define (rx-parse-class characters collected)
  (cond ((null characters) (list (list 'class (reverse collected)) nil))
        ((equal (car characters) "]")
          (list (list 'class (reverse collected)) (cdr characters)))
        (t (rx-parse-class (cdr characters) (cons (car characters) collected)))))

(define (rx-parse-atom characters)
  (cond ((null characters) (list '(empty) nil))
        ((equal (car characters) "(")
          (let ((inside (rx-parse-expression (cdr characters))))
            (cond ((rx-char-p (cadr inside) ")")
                    (list (car inside) (cdr (cadr inside))))
                  (t inside))))
        ((equal (car characters) "[")
          (rx-parse-class (cdr characters) nil))
        ((equal (car characters) ".")
          (list '(dot) (cdr characters)))
        (t (list (list 'lit (car characters)) (cdr characters)))))

(define (rx-postfix-p character)
  (or (equal character "*") (equal character "+") (equal character "?")))

(define (rx-wrap-postfix pattern character)
  (cond ((equal character "*") (list 'star pattern))
        ((equal character "+") (list 'plus pattern))
        (t (list 'optional pattern))))

(define (rx-parse-postfix pattern characters)
  (cond ((or (null characters) (not (rx-postfix-p (car characters))))
          (list pattern characters))
        (t (rx-parse-postfix (rx-wrap-postfix pattern (car characters))
             (cdr characters)))))

(define (rx-parse-repetition characters)
  (let ((atom-result (rx-parse-atom characters)))
    (rx-parse-postfix (car atom-result) (cadr atom-result))))

(define (rx-sequence-stop-p characters)
  (or (null characters) (equal (car characters) "|")
      (equal (car characters) ")")))

(define (rx-parse-sequence-walk characters parts)
  (cond ((rx-sequence-stop-p characters)
          (list (rx-sequence-ast (reverse parts)) characters))
        (t (let ((next (rx-parse-repetition characters)))
             (rx-parse-sequence-walk (cadr next) (cons (car next) parts))))))

(define (rx-parse-sequence characters)
  (rx-parse-sequence-walk characters nil))

(define (rx-parse-expression characters)
  (let ((left (rx-parse-sequence characters)))
    (cond ((rx-char-p (cadr left) "|")
            (let ((right (rx-parse-expression (cdr (cadr left)))))
              (list (list 'alt (car left) (car right)) (cadr right))))
          (t left))))

(define (rx-parse pattern)
  (car (rx-parse-expression (str-to-list pattern))))

; Position sets remove duplicate NFA states while preserving discovery order.
(define (rx-position-add positions position)
  (cond ((in position positions) positions)
        (t (append positions (list position)))))

(define (rx-position-union left right)
  (foldl (lambda (positions position) (rx-position-add positions position))
    left right))

(define (rx-character-in-p character choices)
  (any (lambda (choice) (equal character choice)) choices))

(define (rx-match-many pattern characters positions budget)
  (foldl (lambda (result position)
    (rx-position-union result (rx-match pattern characters position budget)))
    nil positions))

(define (rx-match-sequence patterns characters positions budget)
  (cond ((null patterns) positions)
        ((null positions) nil)
        (t (rx-match-sequence (cdr patterns) characters
             (rx-match-many (car patterns) characters positions budget)
             budget))))

; STAR computes the epsilon closure until another repetition reaches no new state.
(define (rx-star-closure pattern characters frontier reached budget)
  (cond ((or (= budget 0) (null frontier)) reached)
        (t (let ((next (rx-match-many pattern characters frontier (- budget 1))))
             (let ((fresh (filter (lambda (position)
                            (not (in position reached))) next)))
               (rx-star-closure pattern characters fresh
                 (rx-position-union reached fresh) (- budget 1)))))))

(define (rx-match pattern characters position budget)
  (cond ((eq (car pattern) 'empty) (list position))
        ((eq (car pattern) 'lit)
          (cond ((and (< position (length characters))
                      (equal (nth position characters) (cadr pattern)))
                  (list (+ position 1)))
                (t nil)))
        ((eq (car pattern) 'dot)
          (cond ((< position (length characters)) (list (+ position 1)))
                (t nil)))
        ((eq (car pattern) 'class)
          (cond ((and (< position (length characters))
                      (rx-character-in-p (nth position characters) (cadr pattern)))
                  (list (+ position 1)))
                (t nil)))
        ((eq (car pattern) 'seq)
          (rx-match-sequence (cdr pattern) characters (list position) budget))
        ((eq (car pattern) 'alt)
          (rx-position-union
            (rx-match (cadr pattern) characters position budget)
            (rx-match (caddr pattern) characters position budget)))
        ((eq (car pattern) 'optional)
          (rx-position-union (list position)
            (rx-match (cadr pattern) characters position budget)))
        ((eq (car pattern) 'star)
          (rx-star-closure (cadr pattern) characters (list position)
            (list position) budget))
        ((eq (car pattern) 'plus)
          (rx-match-many (list 'star (cadr pattern)) characters
            (rx-match (cadr pattern) characters position budget) budget))))

(define (rx-longest positions)
  (cond ((null positions) nil)
        (t (foldl (lambda (largest position) (max largest position))
             (car positions) (cdr positions)))))

(define (rx-search-ast pattern characters start)
  (cond ((> start (length characters)) nil)
        (t (let ((ends (rx-match pattern characters start
                         (+ (length characters) 1))))
             (cond ((null ends) (rx-search-ast pattern characters (+ start 1)))
                   (t (list start (rx-longest ends))))))))

(define (rx-find pattern text)
  (rx-search-ast (rx-parse pattern) (str-to-list text) 0))

(define (rx-full-match-p pattern text)
  (let ((characters (str-to-list text)))
    (in (length characters)
      (rx-match (rx-parse pattern) characters 0 (+ (length characters) 1)))))

(define (rx-find-all-ast pattern characters start)
  (let ((match (rx-search-ast pattern characters start)))
    (cond ((null match) nil)
          (t (cons match
               (rx-find-all-ast pattern characters
                 (cond ((= (car match) (cadr match)) (+ (cadr match) 1))
                       (t (cadr match)))))))))

(define (rx-find-all pattern text)
  (rx-find-all-ast (rx-parse pattern) (str-to-list text) 0))

; ---- check helpers (defined before use) ----

(define (rx-nomatch pattern text)
  (not (rx-full-match-p pattern text)))

; ---- checks ----

; parsing: a concat with a star produces the expected AST shape.
(check "regex: parse ab*c is a seq with a star middle"
       (equal (rx-parse "ab*c")
              (list 'seq (list 'lit "a") (list 'star (list 'lit "b"))
                    (list 'lit "c"))))

; literals and concatenation
(check "regex: literal concat matches exactly"
       (rx-full-match-p "abc" "abc"))
(check "regex: literal concat rejects a wrong tail"
       (rx-nomatch "abc" "abd"))

; dot matches any single character but must consume one
(check "regex: dot matches any single char"
       (rx-full-match-p "a.c" "axc"))
(check "regex: dot needs a character to consume"
       (rx-nomatch "a.c" "ac"))

; star: zero occurrences (empty run) and many occurrences
(check "regex: star matches zero occurrences"
       (rx-full-match-p "ab*c" "ac"))
(check "regex: star matches many occurrences"
       (rx-full-match-p "ab*c" "abbbc"))
(check "regex: standalone star matches the empty string"
       (rx-full-match-p "a*" ""))

; plus: at least one required
(check "regex: plus rejects zero occurrences"
       (rx-nomatch "ab+c" "ac"))
(check "regex: plus matches one or more"
       (rx-full-match-p "ab+c" "abbc"))

; optional: element may be absent or present
(check "regex: optional absent matches"
       (rx-full-match-p "colou?r" "color"))
(check "regex: optional present matches"
       (rx-full-match-p "colou?r" "colour"))

; alternation inside a group, starred
(check "regex: grouped alternation under star"
       (rx-full-match-p "a(b|c)*d" "abcbcd"))

; alternation precedence: | is lowest, so ab|cd is (ab)|(cd), not a(b|c)d.
(check "regex: top-level alternation takes whole alternatives"
       (and (rx-full-match-p "ab|cd" "ab")
            (and (rx-full-match-p "ab|cd" "cd")
                 (rx-nomatch "ab|cd" "abd"))))

; character class with plus
(check "regex: character class matches members and rejects non-members"
       (and (rx-full-match-p "a[xy]+" "axyx")
            (rx-nomatch "a[xy]+" "az")))

; searching: leftmost span and all non-overlapping spans
(check "regex: find and find-all report correct offsets"
       (and (equal (rx-find "c.?d" "xxcadyy") (list 2 5))
            (equal (rx-find-all "ab+" "zababbx")
                   (list (list 1 3) (list 3 6)))))
