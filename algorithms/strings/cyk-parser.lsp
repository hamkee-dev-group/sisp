; ============================================================
; name: cyk-parser
; file: algorithms/strings/cyk-parser.lsp
; subject: strings
; capability: parse recognize
; aliases: "cyk" "cocke-younger-kasami" "context-free parsing" "cnf parsing"
; signature: (cyk-in-language grammar s) -> t/nil ; (cyk-parse grammar s) -> top cell
;            (cyk-build-table grammar chars) -> dp-table
; complexity: O(n^3 * |G|)
; see-also: regex prolog
; derived-from: Kasami 1965; Younger 1967; Hopcroft & Ullman 1979, 6.3
; status: example (tested)
; checks: 12 - textbook baaba example; CNF grammar DP table
; ============================================================
; CYK algorithm for context-free grammar parsing (CNF form).
; Grammar rules: S -> AB | BC, A -> BA | a, B -> CC | b, C -> AB | a.
; CNF: each production is either A -> BC or A -> a.
; DP table P[i][len] = set of nonterminals deriving wi..wi+len-1 (1-indexed).
; Test: "baaba" in language, "abaaa" not.

(define grammar '(("S" ("A" "B"))
                  ("S" ("B" "C"))
                  ("A" ("B" "A"))
                  ("A" "a")
                  ("B" ("C" "C"))
                  ("B" "b")
                  ("C" ("A" "B"))
                  ("C" "a")))

(define (cyk-rule-nonterminal rule) (car rule))
(define (cyk-rule-rhs rule) (cadr rule))
(define (cyk-is-terminal rhs) (and (listp rhs) (= (length rhs) 1)))

(define (cyk-find-nonterminals grammar)
  (dedup (map cyk-rule-nonterminal grammar)))

(define (cyk-pair r s)
  (list r s))

(define (cyk-entry-get table i j)
  (cond ((eq table nil) nil)
        ((and (= (caar table) i) (= (cadar table) j)) (caddar table))
        (t (cyk-entry-get (cdr table) i j))))

(define (cyk-entry-set table i j val)
  (cons (list i j val) table))

(define (cyk-init table i val)
  (cyk-entry-set table i 1 val))

(define (cyk-terminal-nonterminals grammar terminal)
  (foldl (lambda (acc rule)
    (cond ((and (atom (cyk-rule-rhs rule))
                (equal (cyk-rule-rhs rule) terminal))
           (cons (cyk-rule-nonterminal rule) acc))
          (t acc)))
    nil grammar))

(define (cyk-binary-nonterminals grammar left right)
  (foldl (lambda (acc rule)
    (let ((rhs (cyk-rule-rhs rule)))
      (cond ((and (listp rhs) (= (length rhs) 2)
                  (contains (car rhs) left)
                  (contains (cadr rhs) right))
             (cons (cyk-rule-nonterminal rule) acc))
            (t acc))))
    nil grammar))

(define (cyk-add-unique nt set)
  (cond ((contains nt set) set) (t (cons nt set))))

(define (cyk-build-table grammar chars)
  (let ((n (length chars)))
    (let ((table nil))
      (let ((table (foldl (lambda (acc i)
                 (let ((nts (cyk-terminal-nonterminals grammar (list-ref chars (- i 1)))))
                   (cyk-entry-set acc i 1 nts)))
               table (range 1 (+ n 1)))))
        (let ((table (foldl (lambda (acc len)
                 (cond ((> len n) acc)
                       ; split points k = 1..len-1, so (range 1 len)
                       ; start positions i = 1..n-len+1, so (range 1 (+ (- n len) 2))
                       (t (foldl (lambda (acc2 i)
                                (let ((nts (foldl (lambda (acc3 k)
                                          (let ((left (cyk-entry-get acc2 i k))
                                                (right (cyk-entry-get acc2 (+ i k) (- len k))))
                                            (cond ((or (eq left nil) (eq right nil)) acc3)
                                                  (t (foldl (lambda (acc4 nt)
                                                       (cyk-add-unique nt acc4))
                                                     acc3
                                                     (cyk-binary-nonterminals grammar left right))))))
                                          nil (range 1 len))))
                                  (cond ((eq nts nil) acc2)
                                        (t (cyk-entry-set acc2 i len (dedup nts))))))
                            acc (range 1 (+ (- n len) 2))))))
               table (range 2 (+ n 1)))))
          table)))))

(define (list-ref l i)
  (cond ((= i 0) (car l)) (t (list-ref (cdr l) (- i 1)))))

(define (range lo hi)
  (cond ((>= lo hi) nil) (t (cons lo (range (+ lo 1) hi)))))

(define (cyk-parse grammar s)
  (let ((chars (str-to-list s)))
    (let ((table (cyk-build-table grammar chars)))
      (cyk-entry-get table 1 (length chars)))))

(define (cyk-in-language grammar s)
  (contains "S" (cyk-parse grammar s)))

(print "=== CYK Parser ===")
(print "Grammar: S->AB|BC, A->BA|a, B->CC|b, C->AB|a")
(print "Parse baaba:") (print (cyk-parse grammar "baaba"))
(print "Parse abaaa:") (print (cyk-parse grammar "abaaa"))

; Length-1 cells are just the terminal rules.
(check "cyk terminal cell for b is {B}" (equal (dedup (cyk-terminal-nonterminals grammar "b")) '("B")))
(check "cyk terminal cell for a is {C A}" (equal (dedup (cyk-terminal-nonterminals grammar "a")) '("C" "A")))

; Textbook table for baaba: the top cell is {S, A, C}.
(check "cyk top cell of baaba is {C S A}" (equal (cyk-parse grammar "baaba") '("C" "S" "A")))
(check "baaba is in the language" (cyk-in-language grammar "baaba"))
(check "abaaa is not in the language" (not (cyk-in-language grammar "abaaa")))

; Single terminals: S derives no single character in this grammar.
(check "a alone is not in the language" (not (cyk-in-language grammar "a")))
(check "b alone is not in the language" (not (cyk-in-language grammar "b")))

; ba is derivable by S->BC with B->b and C->a; ab by S->AB.
(check "ba is in the language" (cyk-in-language grammar "ba"))
(check "ab is in the language" (cyk-in-language grammar "ab"))
(check "bb is not in the language" (not (cyk-in-language grammar "bb")))

; The table respects the CYK recurrence: the start symbol is in the
; top cell exactly when the string is in the language
(check "cyk top cell contains S iff in language"
       (and (in "S" (cyk-parse grammar "baaba"))
            (not (cyk-in-language grammar "abaaa"))))
(check "cyk aa cell is {B} (B->CC, C->a)"
       (equal (cyk-entry-get (cyk-build-table grammar (str-to-list "baaba")) 2 2) '("B")))

(print "=== CYK Done ===")
