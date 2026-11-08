; ============================================================
; DISTRIBUTED KV STORE — Raft-replicated with leader election
; 3-node cluster with randomized timeouts.
; ============================================================

; node encoding: (id role term votedFor log store commitIdx votes clock timeout)
; index:          0   1    2     3      4    5      6       7    8      9
(define (n-id n)       (nth 0 n))
(define (n-role n)     (nth 1 n))
(define (n-term n)     (nth 2 n))
(define (n-voted n)    (nth 3 n))
(define (n-log n)      (nth 4 n))
(define (n-store n)    (nth 5 n))
(define (n-commit n)   (nth 6 n))
(define (n-votes n)    (nth 7 n))
(define (n-clock n)    (nth 8 n))
(define (n-timeout n)  (nth 9 n))

(define (make-node id)
  (let ((to (+ 4 (mod (* id 7) 6))))
    (list id 'follower 0 nil nil nil 0 0 0 to)))

(define (n-set-at n idx v)
  (cond ((= idx 0) (set-car n v))
        ((= idx 1) (set-car (cdr n) v))
        ((= idx 2) (set-car (cddr n) v))
        ((= idx 3) (set-car (cdddr n) v))
        ((= idx 4) (set-car (cddddr n) v))
        ((= idx 5) (set-car (cdr (cddddr n)) v))
        ((= idx 6) (set-car (cddr (cddddr n)) v))
        ((= idx 7) (set-car (cdddr (cddddr n)) v))
        ((= idx 8) (set-car (cddddr (cddddr n)) v))
        ((= idx 9) (set-car (cdr (cddddr (cddddr n))) v))
        (t nil))
  n)

(define (n-set-role n r)   (n-set-at n 1 r))
(define (n-set-term n trm) (n-set-at n 2 trm))
(define (n-set-voted n v)  (n-set-at n 3 v))
(define (n-set-log n l)    (n-set-at n 4 l))
(define (n-set-store n s)  (n-set-at n 5 s))
(define (n-set-commit n c) (n-set-at n 6 c))
(define (n-set-votes n v)  (n-set-at n 7 v))
(define (n-set-clock n c)  (n-set-at n 8 c))
(define (n-set-timeout n to)(n-set-at n 9 to))

; --- global cluster state --------------------------------------------------
(define nodes nil)
(define messages nil)
(define heartbeat-gap 3)

; --- RPC messages: (type from to term ...data) ----------------------------
(define (rpc-type m)   (nth 0 m))
(define (rpc-from m)   (nth 1 m))
(define (rpc-to m)     (nth 2 m))
(define (rpc-term m)   (nth 3 m))

; --- KV entry: (key value term log-index) ----------------------------------
(define (e-key e)   (nth 0 e))
(define (e-val e)   (nth 1 e))

(define (mk-entry key value term idx)
  (list key value term idx))

; --- log helpers ------------------------------------------------------------
(define (log-last-index log)
  (cond ((eq log nil) 0)
        (t (nth 3 (last log)))))

; --- Node lookup ------------------------------------------------------------
(define (node-by-id id)
  (find (lambda (n) (eq (n-id n) id)) nodes))

; --- Message passing --------------------------------------------------------
(define (send m)
  (setq messages (append messages (list m)))
  m)

(define (deliver-all)
  (cond ((eq messages nil) nil)
        (t (let ((m (car messages)))
          (setq messages (cdr messages))
          (let ((target (node-by-id (rpc-to m))))
            (cond (target (handle-rpc target m))
                  (t nil)))
          (deliver-all)))))

; --- RPC handler ------------------------------------------------------------
(define (handle-rpc node msg)
  (let ((term (rpc-term msg)))
    (cond ((> term (n-term node))
           (n-set-term node term)
           (n-set-role node 'follower)
           (n-set-voted node nil))))
  (let ((type (rpc-type msg))
        (from (rpc-from msg)))
    (cond ((equal type 'request-vote) (handle-request-vote node msg from))
          ((equal type 'request-vote-reply) (handle-vote-reply node msg))
          ((equal type 'append-entries) (handle-append node msg))
          ((equal type 'append-reply) (handle-append-reply node msg))
          (t nil))))

; --- Election ----------------------------------------------------------------
(define (start-election node)
  (let ((new-term (+ (n-term node) 1)))
    (n-set-term node new-term)
    (n-set-role node 'candidate)
    (n-set-voted node (n-id node))
    (n-set-votes node 1)
    (foreach (lambda (peer)
        (cond ((not (eq (n-id peer) (n-id node)))
               (send (list 'request-vote (n-id node) (n-id peer)
                           new-term (log-last-index (n-log node)))))))
      nodes)))

(define (handle-request-vote node msg from)
  (let ((term (rpc-term msg))
        (last-idx (log-last-index (n-log node)))
        (cand-idx (nth 4 msg)))
    (cond ((< term (n-term node))
           (send (list 'request-vote-reply (n-id node) from (n-term node) nil))
           nil)
          (t
           (cond ((or (eq (n-voted node) nil) (eq (n-voted node) from))
                  (cond ((>= cand-idx last-idx)
                         (n-set-voted node from)
                         (send (list 'request-vote-reply (n-id node) from term t))
                         t)
                        (t (send (list 'request-vote-reply (n-id node) from term nil))
                           nil)))
                 (t (send (list 'request-vote-reply (n-id node) from term nil))
                    nil))))))

(define (handle-vote-reply node msg)
  (let ((granted (nth 4 msg)))
    (cond ((and (equal (n-role node) 'candidate)
                (eq (n-term node) (rpc-term msg))
                (eq granted t))
           (n-set-votes node (+ (n-votes node) 1))
           (cond ((>= (n-votes node) 2)
                  (become-leader node))
                 (t nil)))
          (t nil))))

(define (become-leader node)
  (n-set-role node 'leader)
  (n-set-clock node 0)
  (send-heartbeats node))

(define (send-heartbeats leader)
  (foreach (lambda (peer)
      (cond ((not (eq (n-id peer) (n-id leader)))
             (send (list 'append-entries (n-id leader) (n-id peer)
                         (n-term leader) (log-last-index (n-log leader))
                         0 nil)))))
    nodes))

; --- AppendEntries ----------------------------------------------------------
(define (handle-append node msg)
  (cond ((< (rpc-term msg) (n-term node))
         (send (list 'append-reply (n-id node) (rpc-from msg) (n-term node) nil)))
        (t
         (n-set-clock node 0)
         (cond ((equal (n-role node) 'candidate)
                (n-set-role node 'follower)))
         (let ((entries (nth 5 msg)))
           (cond ((and (not (eq entries nil)) (consp entries))
                  (apply-entries node entries))
                 (t nil)))
         (send (list 'append-reply (n-id node) (rpc-from msg) (n-term node) t)))))

(define (apply-entries node entries)
  (cond ((eq entries nil) nil)
        ((not (consp entries)) nil)
        (t (let ((e (car entries)))
             (n-set-log node (append (n-log node) (list e)))
             (n-set-store node (apply-entry (n-store node) e))
             (n-set-commit node (log-last-index (n-log node)))
             (apply-entries node (cdr entries))))))

(define (handle-append-reply node msg)
  (cond ((and (equal (n-role node) 'leader)
              (eq (n-term node) (rpc-term msg))
              (eq (nth 4 msg) t))
         (let ((rep-count (+ 1 (n-votes node))))
           (n-set-votes node rep-count)
           (cond ((>= rep-count 2)
                  (try-commit node))
                 (t nil))))
        (t nil)))

(define (try-commit leader)
  (n-set-commit leader (log-last-index (n-log leader))))

; --- Client operations -------------------------------------------------------
(define (kv-put leader key value)
  (cond ((not (equal (n-role leader) 'leader))
         (print "ERROR: not leader"))
        (t
         (let ((idx (+ (log-last-index (n-log leader)) 1)))
           (let ((entry (mk-entry key value (n-term leader) idx)))
             (n-set-log leader (append (n-log leader) (list entry)))
             (n-set-store leader (apply-entry (n-store leader) entry))
             (n-set-commit leader idx)
             (foreach (lambda (peer)
                 (cond ((not (eq (n-id peer) (n-id leader)))
                        (send (list 'append-entries (n-id leader) (n-id peer)
                                    (n-term leader) idx (list entry))))))
               nodes)
             (deliver-all)
             (print (cat (cat "PUT " key) (cat " = " (num-to-str value))))
             value)))))

(define (kv-get node key)
  (let ((pair (assoc key (n-store node))))
    (cond (pair (let ((v (e-val (cdr pair))))
                  (print (cat (cat "GET " key) (cat " = " (num-to-str v))))
                  v))
          (t (print (cat "GET " key " = nil")) nil))))

(define (apply-entry store entry)
  (cons (cons (e-key entry) entry)
        (rem-key (e-key entry) store)))

(define (rem-key key store)
  (cond ((eq store nil) nil)
        ((equal key (e-key (car store))) (cdr store))
        (t (cons (car store) (rem-key key (cdr store))))))

; --- Cluster management -----------------------------------------------------
(define (init-cluster)
  (setq nodes
    (list (make-node 1) (make-node 2) (make-node 3))))

(define (cluster-tick)
  (deliver-all)
  (foreach (lambda (n)
      (n-set-clock n (+ (n-clock n) 1))
      (cond ((equal (n-role n) 'leader)
             (cond ((>= (n-clock n) heartbeat-gap)
                    (n-set-clock n 0)
                    (send-heartbeats n))))
            (t
             (cond ((>= (n-clock n) (n-timeout n))
                    (n-set-clock n 0)
                    (start-election n))))))
    nodes)
  (deliver-all))

(define (run-ticks n)
  (cond ((> n 0)
         (cluster-tick)
         (run-ticks (- n 1)))
        (t nil)))

(define (kill-node nid)
  (let ((target (find (lambda (n) (eq (n-id n) nid)) nodes)))
    (cond (target
           (setq nodes (remove (lambda (n) (eq (n-id n) nid)) nodes))
           (print (cat "Node " (cat (num-to-str nid) " killed")))
           t)
          (t nil))))

; ---- DEMO ==================================================================
(print "=============================================")
(print "  Distributed KV Store (Raft Consensus)")
(print "=============================================")

(init-cluster)
(print "Cluster initialized: 3 nodes")

(print "")
(print "--- Phase 1: Elect a leader ---")
(run-ticks 20)

(let ((leader (find (lambda (n) (equal (n-role n) 'leader)) nodes)))
  (cond (leader
         (print (cat "Leader: Node " (num-to-str (n-id leader))))
         (let ((lid (n-id leader)))
           (print "")
           (print "--- Phase 2: PUT x = 42 ---")
           (kv-put leader "x" 42)
           (print "")
           (print "--- Phase 3: Kill leader and elect new ---")
           (kill-node lid)
           (run-ticks 20)
           (let ((new-leader (find (lambda (n) (equal (n-role n) 'leader)) nodes)))
             (cond (new-leader
                    (print (cat "New leader: Node " (num-to-str (n-id new-leader))))
                    (print "")
                    (print "--- Phase 4: GET x on each node ---")
                    (foreach (lambda (n)
                        (print (cat "Node " (cat (num-to-str (n-id n)) ":")))
                        (kv-get n "x"))
                      nodes))
                   (t (print "No new leader elected"))))))
        (t (print "No leader elected in phase 1"))))

(print "")
(print "=============================================")
(print "  Demo complete")
(print "=============================================")
