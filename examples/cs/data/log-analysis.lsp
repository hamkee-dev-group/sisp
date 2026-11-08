; Structured-log analyzer filters records, counts levels, groups components, and finds bursts.
; Records are (timestamp level component message-code).
(define (log-filter records level)
  (filter (lambda (record) (eq (cadr record) level)) records))
(define (log-counts records)
  (map (lambda (level) (list level
    (length (filter (lambda (record) (eq (cadr record) level)) records))))
    '(debug info warn error)))
(define (log-components records)
  (foldl (lambda (result record)
    (cond ((in (caddr record) result) result)
          (t (append result (list (caddr record)))))) nil records))
(define (log-component-summary records)
  (map (lambda (component) (list component
    (length (filter (lambda (record) (eq (caddr record) component)) records))))
    (log-components records)))
(define (log-window records start end)
  (filter (lambda (record) (and (>= (car record) start) (<= (car record) end))) records))
(define (log-error-burst-p records start end threshold)
  (>= (length (log-filter (log-window records start end) 'error)) threshold))
(define log-demo '((1 info api start) (2 error db timeout) (3 warn api retry) (4 error db timeout) (8 info api done)))
(log-counts log-demo) ; => ((debug 0) (info 2) (warn 1) (error 2))
(log-component-summary log-demo) ; => ((api 3) (db 2))
(log-error-burst-p log-demo 1 5 2) ; => t

