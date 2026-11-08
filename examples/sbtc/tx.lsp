; ============================================================
; sBTC — transactions
;
; tx      = (inputs outputs)
; input   = (prev-txid out-index sig pubkey)
;   prev-txid : hex string of the funding tx (or "coinbase")
;   out-index : which output of that tx is being spent
;   sig       : (r s) ECDSA signature, or nil when unsigned
;   pubkey    : spender's public key (curve point)
; output  = (amount address)
;
; txid    = hex(SHA-256d(serialize-with-sigs))
; sighash = SHA-256d(serialize-with-sigs-blanked) — what inputs sign
; ============================================================

; ---- constructors / accessors -------------------------------
(define (make-input prev idx pubkey) (list prev idx nil pubkey))
(define (input-prev i)   (car i))
(define (input-index i)  (cadr i))
(define (input-sig i)    (caddr i))
(define (input-pubkey i) (cadddr i))
(define (set-input-sig i sig) (list (car i) (cadr i) sig (cadddr i)))

(define (make-output amt addr) (list amt addr))
(define (output-amount o)  (car o))
(define (output-address o) (cadr o))

(define (make-tx inputs outputs) (list inputs outputs))
(define (tx-inputs tx)  (car tx))
(define (tx-outputs tx) (cadr tx))

; an address is the first 16 hex chars of SHA-256d(pubkey bytes)
; (real Bitcoin uses RIPEMD160(SHA256(·)); no RIPEMD native here)
(define (pubkey->address pub)
  (substr 0 16 (words->hex (sha256d-words (point->bytes pub)))))

; ---- serialization ------------------------------------------
(define (input->bytes-with i)
  (append (append (str->bytes (input-prev i)) (int->bytes (input-index i) 4))
          (append (sig->bytes (input-sig i)) (point->bytes (input-pubkey i)))))

(define (input->bytes-blank i)
  (append (append (str->bytes (input-prev i)) (int->bytes (input-index i) 4))
          (append (sig->bytes nil) (point->bytes (input-pubkey i)))))

(define (output->bytes o)
  (append (int->bytes (output-amount o) 8) (str->bytes (output-address o))))

(define (serialize-tx-with tx)
  (append (concat-map input->bytes-with (tx-inputs tx))
          (concat-map output->bytes (tx-outputs tx))))

(define (serialize-tx-blank tx)
  (append (concat-map input->bytes-blank (tx-inputs tx))
          (concat-map output->bytes (tx-outputs tx))))

(define (tx-id tx)      (words->hex (sha256d-words (serialize-tx-with tx))))
(define (tx-sighash tx) (sha256d-words (serialize-tx-blank tx)))

; ---- coinbase -----------------------------------------------
(define (coinbase-tx height addr amount)
  (make-tx (list (list "coinbase" height nil nil))
           (list (make-output amount addr))))
(define (coinbase-p tx)
  (equal (input-prev (car (tx-inputs tx))) "coinbase"))

; ---- signing ------------------------------------------------
; sign every input of tx with the aligned private key in privs
(define (sign-tx tx privs)
  (let ((sh (tx-sighash tx)))
    (make-tx (sign-inputs (tx-inputs tx) privs sh) (tx-outputs tx))))

(define (sign-inputs inputs privs sh)
  (cond ((eq inputs nil) nil)
        (t (cons (set-input-sig (car inputs) (ecdsa-sign sh (car privs)))
                 (sign-inputs (cdr inputs) (cdr privs) sh)))))

; verify one input's signature against a sighash and the pubkey it carries
(define (input-sig-valid i sh)
  (cond ((eq (input-sig i) nil) nil)
        (t (ecdsa-verify sh (input-pubkey i) (input-sig i)))))

; total output value
(define (tx-out-total tx) (foldl (lambda (a o) (+ a (output-amount o))) 0 (tx-outputs tx)))
