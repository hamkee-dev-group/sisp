; ============================================================
; sBTC — the mempool (pending, unconfirmed transactions)
; ============================================================

(define (mempool-empty) nil)

(define (contains-txid pool id) (any (lambda (tx) (equal (tx-id tx) id)) pool))
(define (mempool-has pool tx) (contains-txid pool (tx-id tx)))

(define (mempool-add pool tx)
  (cond ((mempool-has pool tx) pool) (t (cons tx pool))))

; the subset of the pool that is currently valid against a UTXO set
(define (mempool-valid pool utxo) (filter (lambda (tx) (valid-tx-p utxo tx)) pool))

; total fees for a list of txs against a UTXO set
(define (mempool-fees utxo txs)
  (foldl (lambda (a tx) (+ a (tx-fee utxo tx))) 0 txs))

; drop txs that a newly-accepted block confirmed
(define (mempool-remove pool block)
  (let ((ids (map tx-id (block-txs block))))
    (filter (lambda (tx) (not (contains (tx-id tx) ids))) pool)))
