; ============================================================
; sBTC — wallet
;
; A wallet is just a key.  It scans a UTXO set for outputs it
; owns, selects enough of them to fund a payment, and builds a
; signed transaction with a change output back to itself.
; ============================================================

; a UTXO key is "txid:index"; txid is a fixed 64 hex chars
(define (key-txid k)  (substr 0 64 k))
(define (key-index k) (str-to-num (substr 65 (length k) k)))

; UTXO entries owned by an address
(define (wallet-utxos utxo addr)
  (filter (lambda (kv) (equal (output-address (cadr kv)) addr)) utxo))

(define (wallet-balance utxo key)
  (balance-of utxo (pubkey->address (key-pub key))))

; greedily select entries until their sum covers `need`;
; returns (selected total) or nil if funds are insufficient
(define (select-utxos entries need acc chosen)
  (cond ((>= acc need) (list chosen acc))
        ((eq entries nil) nil)
        (t (select-utxos (cdr entries) need
                         (+ acc (output-amount (cadr (car entries))))
                         (cons (car entries) chosen)))))

; build and sign a payment of `amount` (plus `fee`) from key to
; recipient, with change returned to the sender; nil if underfunded
(define (build-tx utxo key recipient amount fee)
  (let ((addr (pubkey->address (key-pub key))) (need (+ amount fee)))
    (let ((sel (select-utxos (wallet-utxos utxo addr) need 0 nil)))
      (cond ((eq sel nil) nil)
            (t (let ((chosen (car sel)) (total (cadr sel)))
                 (let ((inputs (map (lambda (kv)
                                      (make-input (key-txid (car kv)) (key-index (car kv)) (key-pub key)))
                                    chosen))
                       (change (- total need)))
                   (let ((outs (cond ((> change 0)
                                      (list (make-output amount recipient) (make-output change addr)))
                                     (t (list (make-output amount recipient))))))
                     (sign-tx (make-tx inputs outs)
                              (map (lambda (kv) (key-priv key)) chosen))))))))))
