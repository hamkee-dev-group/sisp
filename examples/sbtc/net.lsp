; ============================================================
; sBTC — simulated P2P gossip network
;
; The "network" is in-process (like examples/machine/network/tcp-ip.lsp):
; each node has an inbox; a message pushed to a peer is delivered
; and, if it carries something new, relayed onward — real flood
; gossip over a peer topology, so a block mined at one node
; propagates hop by hop until every node converges.
;
; message = (type payload) with type "block" | "tx"
; ============================================================

(define net-log nil)
(define (net-say s) (setq net-log (cons s net-log)) (print s))

; ---- delivery ----------------------------------------------
(define (deliver n msg) (node-set-inbox n (append (node-inbox n) (list msg))))
(define (gossip from msg) (foreach (lambda (p) (deliver p msg)) (node-peers from)))

; ---- inbox processing (with relay) --------------------------
(define (handle n msg)
  (let ((type (car msg)) (payload (cadr msg)))
    (cond ((equal type "block")
           (cond ((node-accept-block n payload) (gossip n msg)) (t nil)))
          ((equal type "tx")
           (cond ((node-accept-tx n payload) (gossip n msg)) (t nil)))
          (t nil))))

(define (process-node n)
  (let ((msgs (node-inbox n)))
    (node-set-inbox n nil)
    (foreach (lambda (m) (handle n m)) msgs)))

(define (any-inbox-p world) (any (lambda (n) (not (eq (node-inbox n) nil))) world))

; run the network until it goes quiet (all blocks/txs propagated)
(define (net-run world)
  (cond ((any-inbox-p world) (foreach process-node world) (net-run world))
        (t world)))

; ---- originating actions -----------------------------------
; node n mines a block, accepts it locally, and gossips it out
(define (net-mine world n ts bits)
  (let ((blk (node-build-block n ts bits)))
    (node-accept-block n blk)
    (node-set-mempool n (mempool-remove (node-mempool n) blk))
    (gossip n (list "block" blk))
    (net-run world)
    blk))

; node n originates a transaction into the network
(define (net-submit-tx world n tx)
  (cond ((node-accept-tx n tx)
         (gossip n (list "tx" tx))
         (net-run world) t)
        (t nil)))

; inject an already-mined block at node n (used to stage forks)
(define (net-inject-block world n blk)
  (cond ((node-accept-block n blk) (gossip n (list "block" blk)) (net-run world) t)
        (t nil)))

(define (all-same-tip-p world)
  (let ((h (chain-tip (node-chain (car world)))))
    (every (lambda (n) (equal (chain-tip (node-chain n)) h)) world)))
