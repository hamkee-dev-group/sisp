; ============================================================
; sBTC — blocks and the Merkle tree
;
; header = (version prev-hash merkle-root timestamp bits nonce)
;   prev-hash   : hex string of the parent block's hash
;   merkle-root : 8-word SHA-256d commitment to the tx list
;   bits        : required number of leading zero bits (difficulty)
; block  = (header txs)   ; txs[0] is the coinbase
; block hash = SHA-256d(serialize-header)
; ============================================================

; ---- Merkle tree (SHA-256d, Bitcoin-style odd duplication) --
(define (tx-hash-words tx) (sha256d-words (serialize-tx-with tx)))
(define (merkle-combine a b) (sha256d-words (append (words->bytes a) (words->bytes b))))

(define (merkle-pairs nodes)
  (cond ((eq nodes nil) nil)
        ((eq (cdr nodes) nil) (list (merkle-combine (car nodes) (car nodes))))
        (t (cons (merkle-combine (car nodes) (cadr nodes)) (merkle-pairs (cddr nodes))))))

(define (merkle-level nodes)
  (cond ((eq (cdr nodes) nil) (car nodes))
        (t (merkle-level (merkle-pairs nodes)))))

(define (merkle-root txs)
  (cond ((eq txs nil) (sha256d-words nil))
        (t (merkle-level (map tx-hash-words txs)))))

; ---- header -------------------------------------------------
(define (make-header version prev merkle ts bits nonce)
  (list version prev merkle ts bits nonce))
(define (hdr-version h) (nth 0 h))
(define (hdr-prev h)    (nth 1 h))
(define (hdr-merkle h)  (nth 2 h))
(define (hdr-ts h)      (nth 3 h))
(define (hdr-bits h)    (nth 4 h))
(define (hdr-nonce h)   (nth 5 h))
(define (hdr-with-nonce h n)
  (list (nth 0 h) (nth 1 h) (nth 2 h) (nth 3 h) (nth 4 h) n))

(define (serialize-header h)
  (append
    (append (int->bytes (hdr-version h) 4) (str->bytes (hdr-prev h)))
    (append (words->bytes (hdr-merkle h))
      (append (int->bytes (hdr-ts h) 8)
        (append (int->bytes (hdr-bits h) 4) (int->bytes (hdr-nonce h) 8))))))

; ---- block --------------------------------------------------
(define (make-block header txs) (list header txs))
(define (block-header b) (car b))
(define (block-txs b)    (cadr b))
(define (block-hash-words b) (sha256d-words (serialize-header (block-header b))))
(define (block-hash b)  (words->hex (block-hash-words b)))
(define (block-prev b)  (hdr-prev (block-header b)))
(define (block-bits b)  (hdr-bits (block-header b)))
(define (block-ts b)    (hdr-ts (block-header b)))
(define (block-coinbase b) (car (block-txs b)))

(define (block-merkle-ok b)
  (equal (hdr-merkle (block-header b)) (merkle-root (block-txs b))))
