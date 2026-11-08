; Association-pair projections.
; Public: get-names, get-values, unpair
; Depends: none (uses the embedded prelude)

(define (get-names pairs)
  "The keys of a list of (key value) pairs.
  (get-names (quote ((a 1) (b 2)))) => (a b)
See: get-values unpair assoc"
  (cond ((eq pairs nil) nil)
        (t (cons (caar pairs) (get-names (cdr pairs))))))

(define (get-values pairs)
  "The values of a list of (key value) pairs.
  (get-values (quote ((a 1) (b 2)))) => (1 2)
See: get-names unpair"
  (cond ((eq pairs nil) nil)
        (t (cons (cadar pairs) (get-values (cdr pairs))))))

(define (unpair pairs)
  "Split a list of pairs into a list of keys and a list of values.
  (unpair (quote ((a 1)))) => ((a) (1))
See: get-names get-values zip"
  (list (get-names pairs) (get-values pairs)))
