; data.lsp — character vocabulary and next-token windows.

(define (llm-char-id character vocabulary)
  (let ((index (index-of character vocabulary)))
    (cond ((eq index nil) (error "character not found in vocabulary" character))
          (t index))))

(define (llm-encode text vocabulary)
  (map (lambda (character) (llm-char-id character vocabulary))
       (str-to-list text)))

(define (llm-decode token-ids vocabulary)
  (list-to-str (map (lambda (token-id) (nth token-id vocabulary)) token-ids)))

(define (llm-make-windows token-ids context-length)
  (cond ((<= (length token-ids) context-length) nil)
        (t (cons
             (list (take token-ids context-length)
                   (nth context-length token-ids))
             (llm-make-windows (cdr token-ids) context-length)))))

(define (llm-window-input window) (car window))
(define (llm-window-target window) (cadr window))

