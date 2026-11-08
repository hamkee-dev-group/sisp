; ============================================================
; sBTC — command-line ledger operations
;
; Each command loads on-disk state, performs ONE action, saves.
; Output is a self-describing S-expression (symbol labels print
; unquoted; addresses/hashes are strings, shown quoted).
; Identities are names; the key is derived from the name.
; ============================================================

(define cli-bits 4)
(define (name-key name)  (make-key name))
(define (name-addr name) (pubkey->address (key-pub (make-key name))))
(define (name-sym name)  (car (read-from-string name)))   ; "alice" -> symbol alice
(define (bal store who)  (balance-of (chain-utxo store) (name-addr who)))

; mine a block crediting `who` (coinbase + any valid mempool txs);
; save and return the new store
(define (ledger-extend path who)
  (let* ((st (load-state path)) (store (state-store st)) (pool (state-pool st))
         (blk (chain-next-block store (name-addr who)
                                (mempool-valid pool (chain-utxo store))
                                (+ 1 (chain-height store)) cli-bits))
         (store2 (chain-add store blk)))
    (state-save path store2 (mempool-valid (mempool-remove pool blk) (chain-utxo store2)))
    (list store2 blk)))

; ---- commands -----------------------------------------------
(define (cli-init path miner)
  (let ((store (chain-genesis (name-addr miner) cli-bits)))
    (state-save path store (mempool-empty))
    (print (list 'init (list 'genesis-miner (name-sym miner))
                 (list 'balance (bal store miner)) (list 'ledger path)))))

; show / create a wallet (deterministic from the name)
(define (cli-new who)
  (print (list 'wallet (list 'name (name-sym who)) (list 'address (name-addr who)))))

; fictional faucet deposit: mint a block reward to `who`
(define (cli-faucet path who)
  (let* ((r (ledger-extend path who)) (store2 (car r)))
    (print (list 'deposit (list 'to (name-sym who)) (list 'amount (block-subsidy (chain-height store2)))
                 (list 'source 'faucet) (list 'balance (bal store2 who))))))

(define (cli-mine path miner)
  (let* ((r (ledger-extend path miner)) (store2 (car r)) (blk (cadr r)))
    (print (list 'mined (list 'height (chain-height store2)) (list 'miner (name-sym miner))
                 (list 'txs (length (block-txs blk))) (list 'balance (bal store2 miner))))))

(define (cli-send path from to amt)
  (let* ((st (load-state path)) (store (state-store st)) (pool (state-pool st))
         (tx (build-tx (chain-utxo store) (name-key from) (name-addr to) amt 1)))
    (cond ((eq tx nil) (print (list 'error 'insufficient-funds (list 'wallet (name-sym from)))))
          (t (state-save path store (mempool-add pool tx))
             (print (list 'queued (list 'from (name-sym from)) (list 'to (name-sym to))
                          (list 'amount amt) (list 'fee 1) (list 'txid (substr 0 16 (tx-id tx)))
                          (list 'note 'mine-to-confirm)))))))

(define (cli-balance path who)
  (print (list 'balance (list 'wallet (name-sym who))
               (list 'coins (bal (state-store (load-state path)) who)))))

(define (cli-status path)
  (let* ((st (load-state path)) (store (state-store st)) (pool (state-pool st)))
    (print (list 'status (list 'height (chain-height store))
                 (list 'blocks (length (chain-blocks store)))
                 (list 'mempool (length pool))
                 (list 'tip (substr 0 16 (chain-tip store)))))))

(define (cli-log path)
  (log-blocks (car (load-state path)) 0))
(define (log-blocks blks h)
  (cond ((eq blks nil) nil)
        (t (print (list 'block (list 'height h) (list 'hash (substr 0 16 (block-hash (car blks))))
                        (list 'txs (length (block-txs (car blks))))
                        (list 'miner (substr 0 16 (output-address (car (tx-outputs (block-coinbase (car blks)))))))))
           (log-blocks (cdr blks) (+ h 1)))))
