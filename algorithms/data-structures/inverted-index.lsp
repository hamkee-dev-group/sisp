; ============================================================
; name: inverted-index
; file: algorithms/data-structures/inverted-index.lsp
; subject: data-structures
; capability: index search rank information-retrieval
; aliases: "inverted index" "full-text search" "postings list" "tf ranking" "search engine"
; signature: (se-build-index documents) -> index
;            (se-postings index term) -> ((doc-id count) ...)
;            (se-search documents index query) -> ((doc-id score) ...) ranked
;            documents: ((id (token ...)) ...)
; complexity: build O(total tokens); query O(query-terms * postings length)
; see-also: suffix-array trie
; derived-from: Manning, Raghavan & Schutze 2008, ch. 1 and 6
; status: example (tested)
; checks: 10 - postings correctness (term frequency per document), ranked
;         search (correct top document, descending scores, empty result for
;         an absent term), and a hand-verified posting-count; run: src/sisp <file>
; ------------------------------------------------------------
; A full-text search engine over an inverted index.  se-build-index maps each
; term to its postings list ((doc-id term-frequency) ...); se-search scores
; documents against a query by summed tf-idf (term frequency times a smoothed
; inverse document frequency) and returns them ranked.
; ============================================================
(define (se-term-count tokens term)
  (length (filter (lambda (token) (equal token term)) tokens)))
(define (se-unique tokens)
  (foldl (lambda (result token) (cond ((in token result) result)
    (t (append result (list token))))) nil tokens))
(define (se-posting documents term)
  (foldl (lambda (postings document)
    (let ((count (se-term-count (cadr document) term)))
      (cond ((= count 0) postings) (t (append postings (list (list (car document) count)))))))
    nil documents))
(define (se-build-index documents)
  (map (lambda (term) (list term (se-posting documents term)))
    (se-unique (foldl (lambda (tokens document) (append tokens (cadr document))) nil documents))))
(define (se-postings index term)
  (let ((entry (find (lambda (item) (equal (car item) term)) index)))
    (cond ((eq entry nil) nil) (t (cadr entry)))))
(define (se-posting-count postings document-id)
  (let ((entry (find (lambda (posting) (equal (car posting) document-id)) postings)))
    (cond ((eq entry nil) 0) (t (cadr entry)))))
; Smoothed inverse document frequency prevents division by zero for absent terms.
(define (se-score index document-count document-id query)
  (foldl (lambda (score term)
    (let ((postings (se-postings index term)))
      (+ score (* (se-posting-count postings document-id)
        (+ 1 (log (/ (+ document-count 1.0) (+ (length postings) 1.0)))))))) 0.0 query))
(define (se-search documents index query)
  (sort (lambda (a b) (> (cadr a) (cadr b)))
    (filter (lambda (result) (> (cadr result) 0))
      (map (lambda (document) (list (car document)
        (se-score index (length documents) (car document) query))) documents))))

; ============================================================
; ---- checks ----
; ============================================================
(define ix-docs
  '((d1 (sisp code interpreter code))
    (d2 (cooking recipe bread))
    (d3 (lisp interpreter language))))
(define ix (se-build-index ix-docs))

(check "inverted-index: 'interpreter' posts to d1 and d3 (once each)"
  (equal (se-postings ix 'interpreter) '((d1 1) (d3 1))))
(check "inverted-index: term frequency counts repeats ('code' twice in d1)"
  (equal (se-postings ix 'code) '((d1 2))))
(check "inverted-index: 'bread' posts only to d2"
  (equal (se-postings ix 'bread) '((d2 1))))
(check "inverted-index: an absent term has empty postings"
  (eq (se-postings ix 'missing) nil))
(check "inverted-index: search 'sisp code' ranks d1 first"
  (eq (car (car (se-search ix-docs ix '(sisp code)))) 'd1))
(check "inverted-index: a query on an absent term returns no results"
  (= (length (se-search ix-docs ix '(missing))) 0))
(check "inverted-index: 'interpreter' query returns exactly d1 and d3"
  (let ((ids (map car (se-search ix-docs ix '(interpreter)))))
    (and (= (length ids) 2) (in 'd1 ids) (in 'd3 ids))))
(check "inverted-index: results are ranked by non-increasing score"
  (let ((scores (map cadr (se-search ix-docs ix '(interpreter language)))))
    (or (< (length scores) 2)
        (>= (car scores) (cadr scores)))))
(check "inverted-index: every returned score is positive"
  (every (lambda (r) (> (cadr r) 0)) (se-search ix-docs ix '(sisp code))))
(check "inverted-index: 'language' query returns d3 as the only match"
  (equal (map car (se-search ix-docs ix '(language))) '(d3)))
(print "=== inverted-index checks done ===")
