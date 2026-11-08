; ============================================================
; name: naive-bayes
; file: algorithms/ai/naive-bayes-spam.lsp
; subject: ai
; capability: classify supervised
; aliases: "naive bayes" "bayesian classifier" "spam filter"
; signature: (nb-classify model tokens) -> class ; (nb-train ...) -> model
; complexity: O(tokens) per classification; Laplace smoothing
; see-also: logistic-regression
; derived-from: McCallum & Nigam 1998 (multinomial event model)
; status: example (tested)
; checks: 3 - spam/ham discrimination, incremental doc counts
; ============================================================
; Multinomial Naive Bayes spam filter with incremental training and a portable list model.
; Model fields are tag, document counts, token totals, token counts, vocabulary, and smoothing.
(define (nb-empty smoothing) (list 'naive-bayes nil nil nil nil smoothing))
(define (nb-doc-counts model) (nth 1 model))
(define (nb-token-totals model) (nth 2 model))
(define (nb-token-counts model) (nth 3 model))
(define (nb-vocabulary model) (nth 4 model))
(define (nb-smoothing model) (nth 5 model))

(define (nb-pair-get pairs key)
  (let ((entry (find (lambda (pair) (equal (car pair) key)) pairs)))
    (cond ((eq entry nil) 0) (t (cadr entry)))))
(define (nb-pair-add pairs key amount)
  (cond ((eq pairs nil) (list (list key amount)))
        ((equal (caar pairs) key)
         (cons (list key (+ (cadar pairs) amount)) (cdr pairs)))
        (t (cons (car pairs) (nb-pair-add (cdr pairs) key amount)))))
(define (nb-token-get counts label token)
  (let ((entry (find (lambda (item)
    (and (equal (car item) label) (equal (cadr item) token))) counts)))
    (cond ((eq entry nil) 0) (t (caddr entry)))))
(define (nb-token-add counts label token amount)
  (cond ((eq counts nil) (list (list label token amount)))
        ((and (equal (caar counts) label) (equal (cadar counts) token))
         (cons (list label token (+ (caddar counts) amount)) (cdr counts)))
        (t (cons (car counts) (nb-token-add (cdr counts) label token amount)))))
(define (nb-vocab-add vocabulary token)
  (cond ((in token vocabulary) vocabulary) (t (cons token vocabulary))))

; Each repeated token contributes evidence in the multinomial event model.
(define (nb-learn-tokens tokens label counts vocabulary total)
  (cond ((eq tokens nil) (list counts vocabulary total))
        (t (nb-learn-tokens (cdr tokens) label
             (nb-token-add counts label (car tokens) 1)
             (nb-vocab-add vocabulary (car tokens)) (+ total 1)))))
(define (nb-learn model label tokens)
  (let ((learned (nb-learn-tokens tokens label (nb-token-counts model)
                   (nb-vocabulary model) 0)))
    (list 'naive-bayes
      (nb-pair-add (nb-doc-counts model) label 1)
      (nb-pair-add (nb-token-totals model) label (caddr learned))
      (car learned) (cadr learned) (nb-smoothing model))))
(define (nb-train documents smoothing)
  (foldl (lambda (model document) (nb-learn model (car document) (cadr document)))
    (nb-empty smoothing) documents))

; Log probabilities prevent underflow when a message contains many tokens.
(define (nb-document-total model)
  (foldl (lambda (sum entry) (+ sum (cadr entry))) 0 (nb-doc-counts model)))
(define (nb-token-log-probability model label token)
  (let ((alpha (nb-smoothing model)))
    (log (/ (+ (nb-token-get (nb-token-counts model) label token) alpha)
            (+ (nb-pair-get (nb-token-totals model) label)
               (* alpha (length (nb-vocabulary model))))))))
(define (nb-score model label tokens)
  (+ (log (/ (nb-pair-get (nb-doc-counts model) label) (nb-document-total model)))
     (foldl (lambda (score token) (+ score (nb-token-log-probability model label token)))
       0 tokens)))
(define (nb-labels model) (map (lambda (entry) (car entry)) (nb-doc-counts model)))
(define (nb-classify model tokens)
  (car (foldl (lambda (best label)
    (let ((candidate (list label (nb-score model label tokens))))
      (cond ((or (eq best nil) (> (cadr candidate) (cadr best))) candidate) (t best))))
    nil (nb-labels model))))

(define nb-spam-model (nb-train '((ham (meeting project tomorrow)) (ham (project schedule team)) (spam (free prize money)) (spam (win money free))) 1))
(nb-classify nb-spam-model '(free money prize)) ; => spam
(nb-classify nb-spam-model '(project team meeting)) ; => ham
(nb-pair-get (nb-doc-counts (nb-learn nb-spam-model 'spam '(prize now))) 'spam) ; => 3


; ---- checks ----
(check "naive-bayes: spam words -> spam"
       (eq (nb-classify nb-spam-model '(free money prize)) 'spam))

(check "naive-bayes: ham words -> ham"
       (eq (nb-classify nb-spam-model '(project team meeting)) 'ham))

(check "naive-bayes: incremental learning bumps spam doc count to 3"
       (= (nb-pair-get (nb-doc-counts (nb-learn nb-spam-model 'spam '(prize now))) 'spam) 3))
