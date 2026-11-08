; ============================================================
; sBTC — canonical serialization to byte lists
;
; Everything hashed (transactions, block headers) is first
; turned into a flat list of bytes (0..255) so the hash input
; is unambiguous.  Integers are big-endian, fixed width.
; ============================================================

; big-endian encoding of x into exactly n bytes
(define (int->bytes x n)
  (cond ((= n 0) nil)
        (t (append (int->bytes (truncate (/ x 256)) (- n 1))
                   (list (mod x 256))))))

; concatenate a list of byte lists
(define (concat lsts) (foldr append nil lsts))
(define (concat-map f xs) (concat (map f xs)))

; a curve point (pubkey) -> 8 bytes; identity/nil -> 8 zero bytes
(define (point->bytes pt)
  (cond ((eq pt nil) (int->bytes 0 8))
        (t (append (int->bytes (car pt) 4) (int->bytes (cadr pt) 4)))))

; a signature (r s) -> 8 bytes; nil (unsigned) -> 8 zero bytes
(define (sig->bytes sig)
  (cond ((eq sig nil) (int->bytes 0 8))
        (t (append (int->bytes (car sig) 4) (int->bytes (cadr sig) 4)))))
