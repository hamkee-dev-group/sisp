; ============================================================
; sBTC — a clear user story: wallet -> deposit -> mine -> send
;
; Each step prints a self-describing result (symbol labels, so
; no quote clutter; addresses are strings, shown quoted).
; ============================================================

(define BITS 4)
(define (addr name) (pubkey->address (key-pub (make-key name))))
(define (bal store who) (balance-of (chain-utxo store) (addr who)))

; a faucet deposit / a mining reward are the same mechanic: mine a
; block whose coinbase credits `who` (this is how coins are minted)
(define (credit store who) (chain-add store (chain-next-block store (addr who) nil (+ 1 (chain-height store)) BITS)))

; send `amt` from -> to, confirmed in a block mined by `miner`
(define (transfer store from to amt miner)
  (let ((tx (build-tx (chain-utxo store) (make-key from) (addr to) amt 1)))
    (cond ((eq tx nil) store)
          (t (chain-add store (chain-next-block store (addr miner) (list tx)
                                                 (+ 1 (chain-height store)) BITS))))))

; 1) generate two wallets (an identity is a name -> key -> address)
(print (list 'wallet 'alice (list 'address (addr "alice"))))
(print (list 'wallet 'bob   (list 'address (addr "bob"))))

; 2) genesis, then a fictional faucet deposit that credits alice 50
(define s (chain-genesis (addr "faucet") BITS))
(define s (credit s "alice"))
(print (list 'deposit (list 'to 'alice) (list 'amount 50) (list 'source 'faucet)))
(print (list 'balance 'alice (bal s "alice")))

; 3) alice mines a block herself (earns the block reward)
(define s (credit s "alice"))
(print (list 'mine (list 'miner 'alice) (list 'reward 50)))
(print (list 'balance 'alice (bal s "alice")))

; 4) alice sends 30 to bob; a neutral miner confirms it in a block
(define s (transfer s "alice" "bob" 30 "miner"))
(print (list 'send (list 'from 'alice) (list 'to 'bob) (list 'amount 30) (list 'fee 1)))
(print (list 'balance 'alice (bal s "alice")))
(print (list 'balance 'bob   (bal s "bob")))

; 5) final ledger summary
(print (list 'ledger (list 'height (chain-height s))
             (list 'alice (bal s "alice")) (list 'bob (bal s "bob")) (list 'miner (bal s "miner"))))
