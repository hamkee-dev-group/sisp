; A native macro generates a tagged record constructor, predicate, readers, and immutable writers.
; Names are explicit arguments because SISP intentionally has no string-to-symbol interning primitive.
(define (record2-definition tag constructor predicate first-reader second-reader
          first-writer second-writer)
  (list 'progn
    (list 'define (list constructor 'first-value 'second-value)
      (list 'list (list 'quote tag) 'first-value 'second-value))
    (list 'define (list predicate 'record)
      (list 'and (list 'consp 'record)
        (list 'equal (list 'car 'record) (list 'quote tag))))
    (list 'define (list first-reader 'record) (list 'cadr 'record))
    (list 'define (list second-reader 'record) (list 'caddr 'record))
    (list 'define (list first-writer 'record 'value)
      (list 'list (list 'quote tag) 'value (list 'caddr 'record)))
    (list 'define (list second-writer 'record 'value)
      (list 'list (list 'quote tag) (list 'cadr 'record) 'value))))

(defmacro (define-record2 tag constructor predicate first-reader second-reader
           first-writer second-writer)
  (record2-definition tag constructor predicate first-reader second-reader
    first-writer second-writer))

(define-record2 book make-book book-record-p book-title book-pages
  book-with-title book-with-pages)
(define-record2 point make-point point-record-p point-x point-y
  point-with-x point-with-y)

(define handbook (make-book "SISP Handbook" 180))
(define moved (point-with-y (point-with-x (make-point 2 3) 8) 13))

(list (book-record-p handbook) (book-title handbook) (book-pages handbook)) ; => (t "SISP Handbook" 180)
(list (point-record-p moved) (point-x moved) (point-y moved)) ; => (t 8 13)
(list (book-with-pages handbook 240) (point-record-p handbook)) ; => ((book "SISP Handbook" 240) nil)
