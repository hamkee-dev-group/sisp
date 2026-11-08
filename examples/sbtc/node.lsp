; ============================================================
; sBTC — a full node
;
; A node bundles a chain (block store), a mempool, an identity
; key, its peer list, and an inbox.  Nodes are mutable objects
; (set-car on the field cell) so the peer graph can hold real
; references and relay works like a live network.
;
; node = (id key chain mempool peers inbox)
; (note: SISP identifiers can't contain '!', so mutators are
;  named plainly, e.g. node-set-chain.)
; ============================================================

(define (make-node id key)
  (list id key (chain-empty) (mempool-empty) nil nil))

(define (node-id n)      (nth 0 n))
(define (node-key n)     (nth 1 n))
(define (node-chain n)   (nth 2 n))
(define (node-mempool n) (nth 3 n))
(define (node-peers n)   (nth 4 n))
(define (node-inbox n)   (nth 5 n))
(define (node-address n) (pubkey->address (key-pub (node-key n))))

; mutate field k in place
(define (nth-cons lst k) (cond ((= k 0) lst) (t (nth-cons (cdr lst) (- k 1)))))
(define (set-field n k v) (set-car (nth-cons n k) v) n)
(define (node-set-chain n c)   (set-field n 2 c))
(define (node-set-mempool n m) (set-field n 3 m))
(define (node-set-peers n p)   (set-field n 4 p))
(define (node-set-inbox n ib)  (set-field n 5 ib))

; wire two nodes together (bidirectional link)
(define (connect a b)
  (node-set-peers a (cons b (node-peers a)))
  (node-set-peers b (cons a (node-peers b))))

; ---- local state transitions --------------------------------
; accept a block into this node; t iff it was new and valid
(define (node-accept-block n blk)
  (cond ((store-has (node-chain n) (block-hash blk)) nil)
        (t (let ((c2 (chain-add (node-chain n) blk)))
             (cond ((store-has c2 (block-hash blk))
                    (node-set-chain n c2)
                    (node-set-mempool n (mempool-remove (node-mempool n) blk))
                    t)
                   (t nil))))))

; accept a tx into the mempool; t iff new and valid against tip UTXO
(define (node-accept-tx n tx)
  (cond ((mempool-has (node-mempool n) tx) nil)
        ((valid-tx-p (chain-utxo (node-chain n)) tx)
         (node-set-mempool n (mempool-add (node-mempool n) tx)) t)
        (t nil)))

; build (mine) the next block on this node's tip, no state change
(define (node-build-block n ts bits)
  (chain-next-block (node-chain n) (node-address n)
                    (mempool-valid (node-mempool n) (chain-utxo (node-chain n)))
                    ts bits))

(define (node-balance n addr) (balance-of (chain-utxo (node-chain n)) addr))
(define (node-height n) (chain-height (node-chain n)))
