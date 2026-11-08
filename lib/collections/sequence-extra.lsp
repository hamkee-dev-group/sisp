; Less-common sequence operations kept outside the boot prelude.
; Public: deep-flatmap, succ, pred
; Depends: none (uses the embedded prelude)

(define (deep-flatmap function sequence)
  "Map, then flatten the result completely.
  (deep-flatmap (lambda (x) (list x)) (quote (1 2))) => (1 2)
Note: Unlike flatmap this descends every level, so nested structure in the results is lost.
See: flatmap flatten"
  (flatten (map function sequence)))

(define (succ value sequence)
  "The element following a given one in a list.
  (succ (quote a) (quote (a b c))) => b
See: pred nth"
  (cond ((or (eq sequence nil) (eq nil (cdr sequence))) nil)
        ((eq (car sequence) value) (cadr sequence))
        (t (succ value (cdr sequence)))))

(define (pred value sequence)
  "The element preceding a given one in a list.
  (pred (quote b) (quote (a b c))) => a
See: succ"
  (cond ((or (eq sequence nil) (eq nil (cdr sequence))) nil)
        ((eq (cadr sequence) value) (car sequence))
        (t (pred value (cdr sequence)))))
