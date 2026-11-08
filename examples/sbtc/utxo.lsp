; ============================================================
; sBTC — the UTXO set
;
; The set of unspent transaction outputs.  An entry is
; (key output) where key = "txid:index".  A transaction spends
; existing entries (its inputs) and creates new ones (its
; outputs).  This is where double-spends are prevented: a spent
; entry is gone, so a second spend finds no UTXO to consume.
; ============================================================

(define (utxo-key txid idx) (cat (cat txid ":") (num-to-str idx)))

(define (utxo-add set key out) (cons (list key out) set))
(define (utxo-remove set key) (remove (lambda (kv) (equal (car kv) key)) set))
(define (utxo-get set key)
  (let ((kv (find (lambda (kv) (equal (car kv) key)) set)))
    (cond (kv (cadr kv)) (t nil))))
(define (utxo-has set key) (cond ((utxo-get set key) t) (t nil)))

(define (input-outpoint i) (utxo-key (input-prev i) (input-index i)))
(define (input-utxo set i) (utxo-get set (input-outpoint i)))

; ---- applying transactions / blocks -------------------------
(define (remove-inputs set inputs)
  (foldl (lambda (s i) (utxo-remove s (input-outpoint i))) set inputs))

(define (add-outputs set txid outs idx)
  (cond ((eq outs nil) set)
        (t (add-outputs (utxo-add set (utxo-key txid idx) (car outs))
                        txid (cdr outs) (+ idx 1)))))

(define (apply-tx set tx)
  (let ((s1 (cond ((coinbase-p tx) set) (t (remove-inputs set (tx-inputs tx))))))
    (add-outputs s1 (tx-id tx) (tx-outputs tx) 0)))

(define (apply-block-txs set txs) (foldl apply-tx set txs))

; ---- balances -----------------------------------------------
(define (balance-of set addr)
  (foldl (lambda (a kv)
           (cond ((equal (output-address (cadr kv)) addr) (+ a (output-amount (cadr kv))))
                 (t a)))
         0 set))

; ---- validation ---------------------------------------------
; a non-coinbase input is spendable iff the UTXO exists, the
; carried pubkey owns it (address match), and the signature is
; valid for this tx's sighash.
(define (input-spendable-p set i sh)
  (let ((u (input-utxo set i)))
    (cond ((eq u nil) nil)
          ((not (equal (pubkey->address (input-pubkey i)) (output-address u))) nil)
          (t (input-sig-valid i sh)))))

(define (tx-in-total set tx)
  (foldl (lambda (a i) (+ a (output-amount (input-utxo set i)))) 0 (tx-inputs tx)))

(define (no-dup-inputs-p tx)
  (let ((ops (map input-outpoint (tx-inputs tx))))
    (= (length ops) (length (dedup ops)))))

; validate a standalone (non-coinbase) tx against a UTXO set
(define (valid-tx-p set tx)
  (cond ((coinbase-p tx) t)
        ((not (no-dup-inputs-p tx)) nil)
        ((not (every (lambda (i) (input-spendable-p set i (tx-sighash tx))) (tx-inputs tx))) nil)
        ((< (tx-in-total set tx) (tx-out-total tx)) nil)
        (t t)))

; the fee a tx pays (inputs - outputs); assumes valid-tx-p already held
(define (tx-fee set tx) (- (tx-in-total set tx) (tx-out-total tx)))
