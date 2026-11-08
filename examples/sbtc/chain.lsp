; ============================================================
; sBTC — the blockchain, consensus, and reorgs
;
; The chain state is a block store: an assoc from block-hash to
; block, holding every valid block including side branches.  The
; active chain is whichever tip has the most cumulative work
; (sum of 2^bits over its ancestry) — Nakamoto consensus.  UTXO
; state and height for any tip are derived by walking its branch
; back to genesis, so a heavier fork transparently reorgs.
; ============================================================

(define genesis-prev
  "0000000000000000000000000000000000000000000000000000000000000000")

; ---- block store --------------------------------------------
(define (chain-empty) nil)
(define (store-get store h)
  (let ((kv (find (lambda (kv) (equal (car kv) h)) store)))
    (cond (kv (cadr kv)) (t nil))))
(define (store-put store h b) (cons (list h b) store))
(define (store-has store h) (cond ((store-get store h) t) (t nil)))
; oldest-first, so best-tip keeps the first-seen block on a work tie
(define (all-hashes store) (reverse (map car store)))

; ---- branch walking -----------------------------------------
; blocks from genesis up to (and including) hash h, genesis first
(define (branch-to store h)
  (let ((b (store-get store h)))
    (cond ((eq b nil) nil)
          ((equal (block-prev b) genesis-prev) (list b))
          (t (append (branch-to store (block-prev b)) (list b))))))

(define (height-of store h) (- (length (branch-to store h)) 1))
(define (work-of store h)
  (foldl (lambda (a b) (+ a (pow2 (block-bits b)))) 0 (branch-to store h)))
(define (utxo-upto store h)
  (foldl (lambda (u b) (apply-block-txs u (block-txs b))) nil (branch-to store h)))

; ---- best tip (most cumulative work) ------------------------
(define (best-tip store)
  (foldl (lambda (best h)
           (cond ((eq best nil) h)
                 ((> (work-of store h) (work-of store best)) h)
                 (t best)))
         nil (all-hashes store)))

(define (chain-tip store) (best-tip store))
(define (chain-tip-block store) (store-get store (best-tip store)))
(define (chain-height store)
  (cond ((eq store nil) -1) (t (height-of store (best-tip store)))))
(define (chain-utxo store)
  (cond ((eq store nil) nil) (t (utxo-upto store (best-tip store)))))
(define (chain-blocks store)
  (cond ((eq store nil) nil) (t (branch-to store (best-tip store)))))

; ---- validation ---------------------------------------------
; validate every non-coinbase tx against a progressively-updated
; UTXO snapshot; return (ok total-fees)
(define (check-txs txs utxo fees)
  (cond ((eq txs nil) (list t fees))
        ((not (valid-tx-p utxo (car txs))) (list nil 0))
        (t (check-txs (cdr txs) (apply-tx utxo (car txs))
                      (+ fees (tx-fee utxo (car txs)))))))

(define (valid-block-txs b utxo height)
  (let ((txs (block-txs b)))
    (cond ((eq txs nil) nil)
          ((not (coinbase-p (car txs))) nil)
          (t (let ((res (check-txs (cdr txs) utxo fees0)))
               (cond ((eq (car res) nil) nil)
                     (t (<= (tx-out-total (car txs))
                            (+ (block-subsidy height) (cadr res))))))))))
(define fees0 0)

(define (valid-new-block store b)
  (let ((prev (block-prev b)))
    (cond ((not (block-merkle-ok b)) nil)
          ((not (block-pow-ok b)) nil)
          ((and (not (equal prev genesis-prev)) (not (store-has store prev))) nil)
          (t (let ((utxo   (cond ((equal prev genesis-prev) nil) (t (utxo-upto store prev))))
                   (height (cond ((equal prev genesis-prev) 0) (t (+ 1 (height-of store prev))))))
               (valid-block-txs b utxo height))))))

; add a block; silently keep the store unchanged if invalid or dup
(define (chain-add store b)
  (cond ((store-has store (block-hash b)) store)
        ((valid-new-block store b) (store-put store (block-hash b) b))
        (t store)))

; ---- helpers for building on the chain ----------------------
(define (make-genesis addr bits)
  (mine-block genesis-prev (list (coinbase-tx 0 addr (block-subsidy 0))) 0 bits))
(define (chain-genesis addr bits) (chain-add (chain-empty) (make-genesis addr bits)))

; mine the next block on the current best tip, paying the miner
; subsidy + fees and including the given (already-valid) txs
(define (chain-next-block store addr txs ts bits)
  (let ((tip    (cond ((eq (best-tip store) nil) genesis-prev) (t (best-tip store))))
        (height (+ 1 (chain-height store)))
        (utxo   (chain-utxo store)))
    (let ((cb (coinbase-tx height addr (+ (block-subsidy height) (mempool-fees utxo txs)))))
      (mine-block tip (cons cb txs) ts bits))))
