; ============================================================
; sBTC — end-to-end demo (narrated)
;
; Boots a small network, mines real proof-of-work blocks that
; gossip and converge, moves coins with signed transactions,
; bounces a double-spend, and survives a chain reorganization.
; Run last, after all modules; see run.sh.
; ============================================================

(define BITS 4)
(define (line s) (print s))
(define (rule) (print "------------------------------------------------------------"))

(define (show-balances world label)
  (line label)
  (foreach (lambda (n)
             (line (cat (cat "  " (node-id n))
                        (cat ": height " (cat (num-to-str (node-height n))
                             (cat "  balance " (num-to-str (node-balance n (node-address n)))))))))
           world))

(rule)
(line "sBTC — a real proof-of-work cryptocurrency in pure SISP")
(rule)

; ---- boot the network ---------------------------------------
(define alice (make-node "alice  " (make-key "alice-secret")))
(define bob   (make-node "bob    " (make-key "bob-secret")))
(define carol (make-node "carol  " (make-key "carol-secret")))
(connect alice bob)
(connect bob carol)              ; topology: alice <-> bob <-> carol
(define world (list alice bob carol))
(line "Booted 3 nodes.  Topology:  alice <-> bob <-> carol")
(line "(carol only hears the chain by relay through bob)")

; ---- genesis + mining ---------------------------------------
(rule)
(line "alice mines the genesis block and two more; blocks gossip out...")
(net-mine world alice 1 BITS)
(net-mine world alice 2 BITS)
(net-mine world alice 3 BITS)
(line (cat "genesis..tip propagated; all nodes agree on tip: "
           (cond ((all-same-tip-p world) "YES") (t "NO"))))
(show-balances world "Balances after alice mines 3 blocks:")

; ---- a real payment -----------------------------------------
(rule)
(line "alice pays carol 40 coins (fee 5).  Wallet selects a coin,")
(line "signs it, and broadcasts the transaction...")
(define pay (build-tx (chain-utxo (node-chain alice)) (node-key alice) (node-address carol) 40 5))
(net-submit-tx world alice pay)
(line (cat "  tx reached carol's mempool via relay: "
           (cond ((mempool-has (node-mempool carol) pay) "YES") (t "NO"))))
(line "bob mines a block that confirms the payment (and earns the fee)...")
(net-mine world bob 4 BITS)
(show-balances world "Balances after the confirmed payment:")
(line (cat "carol's confirmed balance: "
           (num-to-str (node-balance carol (node-address carol))) ))

; ---- double-spend attempt -----------------------------------
(rule)
(line "alice tries to DOUBLE-SPEND the coin she already gave carol")
(line "by re-spending the same outpoint back to herself...")
(define spent-outpoint (car (tx-inputs pay)))
(define fraud (sign-tx (make-tx (list (make-input (input-prev spent-outpoint) (input-index spent-outpoint) (key-pub (node-key alice))))
                                (list (make-output 40 (node-address alice))))
                       (list (key-priv (node-key alice)))))
(line (cat "  network accepts the double-spend? "
           (cond ((net-submit-tx world alice fraud) "YES (BUG!)") (t "NO -- rejected, coin already spent"))))

; ---- 51%-style reorg ----------------------------------------
(rule)
(line "A hidden miner secretly builds a HEAVIER competing chain")
(line "from genesis, then releases it.  The network must reorg...")
(define honest-tip (chain-tip (node-chain alice)))
(define honest-height (node-height alice))
(define kx (make-key "attacker"))
(define xaddr (pubkey->address (key-pub kx)))
; mine a private branch off genesis, one block longer than the honest chain
(define gblk (car (chain-blocks (node-chain alice))))     ; shared genesis block
(define gh (block-hash gblk))
(define (mine-priv prev height ts) (mine-block prev (list (coinbase-tx height xaddr (block-subsidy height))) ts BITS))
(define priv nil)
(define prevh gh)
(define hh 1)
(define tt 100)
; build (honest-height + 1) private blocks on top of genesis
(define (build-priv k)
  (cond ((= k 0) nil)
        (t (let ((b (mine-priv prevh hh tt)))
             (setq priv (append priv (list b)))
             (setq prevh (block-hash b))
             (setq hh (+ hh 1))
             (setq tt (+ tt 1))
             (build-priv (- k 1))))))
(build-priv (+ honest-height 1))
(line (cat (cat "  attacker secretly mined " (num-to-str (length priv))) " blocks; now releasing them."))
(foreach (lambda (b) (net-inject-block world alice b)) priv)
(line (cat "  tip changed by reorg: "
           (cond ((equal (chain-tip (node-chain alice)) honest-tip) "no") (t "YES"))))
(line (cat "  all nodes converged on the new chain: "
           (cond ((all-same-tip-p world) "YES") (t "NO"))))
(show-balances world "Balances after the reorg (attacker's chain won):")
(line "Note: carol's payment lived on the abandoned branch -- this is")
(line "exactly the 51% double-spend risk.  Security comes from honest")
(line "hashpower holding the majority; here key/PoW sizes are tiny.")

(rule)
(line "Demo complete.  Every block above met a real SHA-256d target,")
(line "every transfer carried a real ECDSA signature, and consensus")
(line "emerged from cumulative work -- all in pure SISP.")
(rule)
