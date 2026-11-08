; ============================================================
; sBTC — self tests (PASS/FAIL)
;
; Run after all modules, e.g.:
;   ./src/sisp examples/sbtc/hash.lsp ... examples/sbtc/tests.lsp < /dev/null
; ============================================================

(define test-fails 0)
(define (check label ok)
  (cond (ok (print (cat "PASS: " label)))
        (t (setq test-fails (+ test-fails 1)) (print (cat "FAIL: " label)))))

(print "== crypto: SHA-256 ==")
(check "sha256 empty-string vector"
       (equal (sha256-hex "") "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
(check "sha256 abc vector"
       (equal (sha256-hex "abc") "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))

(print "== crypto: EC + ECDSA ==")
(check "generator on curve" (on-curve-p ec-g))
(check "n*G = identity" (eq (pt-mul ec-n ec-g) nil))
(define ka (make-key "alice"))
(define kb (make-key "bob"))
(define d1 (msg-digest "pay bob 10"))
(define sig (ecdsa-sign d1 (key-priv ka)))
(check "sign/verify roundtrip" (ecdsa-verify d1 (key-pub ka) sig))
(check "tampered message rejected" (not (ecdsa-verify (msg-digest "pay bob 1000000") (key-pub ka) sig)))
(check "wrong pubkey rejected" (not (ecdsa-verify d1 (key-pub kb) sig)))
(check "forged signature rejected" (not (ecdsa-verify d1 (key-pub ka) (list (+ 1 (car sig)) (cadr sig)))))

(print "== transactions + UTXO ==")
(define aaddr (pubkey->address (key-pub ka)))
(define baddr (pubkey->address (key-pub kb)))
(define cb (coinbase-tx 1 aaddr 50))
(check "coinbase recognised" (coinbase-p cb))
(check "txid deterministic" (equal (tx-id cb) (tx-id cb)))
(define set0 (apply-block-txs nil (list cb)))
(check "coinbase credits balance" (= (balance-of set0 aaddr) 50))
(define spend (sign-tx (make-tx (list (make-input (tx-id cb) 0 (key-pub ka)))
                                (list (make-output 50 baddr))) (list (key-priv ka))))
(check "valid spend accepted" (valid-tx-p set0 spend))
(define set1 (apply-tx set0 spend))
(check "balances move on spend" (and (= (balance-of set1 aaddr) 0) (= (balance-of set1 baddr) 50)))
(check "double-spend rejected" (not (valid-tx-p set1 spend)))
(define km (make-key "mallory"))
(define theft (sign-tx (make-tx (list (make-input (tx-id cb) 0 (key-pub km)))
                                (list (make-output 50 (pubkey->address (key-pub km))))) (list (key-priv km))))
(check "theft (wrong owner) rejected" (not (valid-tx-p set0 theft)))
(define over (sign-tx (make-tx (list (make-input (tx-id cb) 0 (key-pub ka)))
                               (list (make-output 999 baddr))) (list (key-priv ka))))
(check "overspend rejected" (not (valid-tx-p set0 over)))

(print "== blocks + proof of work ==")
(define blk (mine-block "00genesis" (list (coinbase-tx 0 aaddr 50)) 1 8))
(check "mined block meets PoW" (block-pow-ok blk))
(check "merkle root valid" (block-merkle-ok blk))
(check "tampered nonce breaks PoW"
       (not (block-pow-ok (make-block (hdr-with-nonce (block-header blk) (+ 1 (hdr-nonce (block-header blk)))) (block-txs blk)))))
(check "subsidy halving" (and (= (block-subsidy 0) 50) (= (block-subsidy 20) 25) (= (block-subsidy 120) 0)))

(print "== chain + consensus (reorg) ==")
(define s (chain-genesis aaddr 4))
(define g (chain-tip s))
(define A1 (mine-block g (list (coinbase-tx 1 aaddr (block-subsidy 1))) 10 4))
(define s (chain-add s A1))
(check "extends active chain" (equal (chain-tip s) (block-hash A1)))
(define B1 (mine-block g (list (coinbase-tx 1 baddr (block-subsidy 1))) 20 4))
(define s (chain-add s B1))
(check "first-seen kept on work tie" (equal (chain-tip s) (block-hash A1)))
(define B2 (mine-block (block-hash B1) (list (coinbase-tx 2 baddr (block-subsidy 2))) 30 4))
(define s (chain-add s B2))
(check "reorg to heavier fork" (equal (chain-tip s) (block-hash B2)))
(check "invalid block rejected"
       (not (store-has (chain-add s (make-block (hdr-with-nonce (block-header B2) 987654) (block-txs B2)))
                       (block-hash (make-block (hdr-with-nonce (block-header B2) 987654) (block-txs B2))))))

(print "== P2P network ==")
(define nA (make-node "A" (make-key "A")))
(define nB (make-node "B" (make-key "B")))
(define nC (make-node "C" (make-key "C")))
(connect nA nB) (connect nB nC)
(define world (list nA nB nC))
(net-mine world nA 1 4)
(check "block relays across topology (C converges)"
       (and (all-same-tip-p world) (= (node-height nC) 0)))

(print "== wallet ==")
(net-mine world nA 2 4)
(define wtx (build-tx (chain-utxo (node-chain nA)) (node-key nA) (node-address nC) 30 5))
(check "wallet builds tx with change" (and (not (eq wtx nil)) (= (length (tx-outputs wtx)) 2)))
(check "wallet underfunded returns nil"
       (not (build-tx (chain-utxo (node-chain nA)) (node-key nA) (node-address nC) 999999 1)))

(print "============================================")
(cond ((= test-fails 0) (print "ALL TESTS PASSED"))
      (t (print (cat "TESTS FAILED: " (num-to-str test-fails)))))
