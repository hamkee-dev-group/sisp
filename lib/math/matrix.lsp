; Convenience construction from a nested list of equal-length rows.
; Public: matrix
; Depends: none (uses matrix primitives and the embedded prelude)

(define (matrix rows)
  "Build a matrix from a list of equal-length rows.
  (mat-ref (matrix (list (list 1 2) (list 3 4))) 1 0) => 3.0
See: matrix-from-list make-matrix mat-ref"
  (matrix-from-list (length rows) (length (car rows)) (flatten rows)))
