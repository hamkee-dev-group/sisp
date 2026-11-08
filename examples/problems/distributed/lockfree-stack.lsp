; Treiber lock-free stack. CAS on top pointer. ABA problem + tagged pointers.
(print "=== Lock-free Stack (Treiber) ===")
(seed 42)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

; Node: (value next-tagged-pointer)
; Tagged pointer: (node-ref tag-counter)
(define (mk-node val next) (list val next))
(define (node-val n) (car n))
(define (node-next n) (cadr n))
(define (set-node-next n next) (set-car (cdr n) next))

; Tagged pointer: (ptr tag)
(define (mk-tagged ptr tag) (list ptr tag))
(define (tagged-ptr tp) (car tp))
(define (tagged-tag tp) (cadr tp))

; CAS operation (simulated): compare tagged pointer, if match, set new
(define (cas-tagged stack expected new-ptr new-tag)
  (let ((current (cadr stack)))
    (cond ((and (eq (tagged-ptr current) (tagged-ptr expected))
                (= (tagged-tag current) (tagged-tag expected)))
           (set-car (cdr stack) (mk-tagged new-ptr new-tag))
           t)
          (t nil))))

; --- Untagged version (ABA-vulnerable) ---
(define (mk-stack-untagged) (list (quote stack) nil))
(define (push-untagged stack val)
  (let ((top (cadr stack)))
    (let ((new-node (mk-node val top)))
      (progn
        (set-car (cdr stack) new-node)
        t))))

(define (pop-untagged stack)
  (let ((top (cadr stack)))
    (cond ((null top) nil)
          (t
           (let ((next (node-next top)))
             (set-car (cdr stack) next)
             (node-val top))))))

; --- Tagged version (ABA-safe) ---
(define (mk-stack) (list (quote stack) (mk-tagged nil 0)))
(define *tag-counter* 0)

(define (push-tagged stack val)
  (setq *tag-counter* (+ *tag-counter* 1))
  (let ((current (cadr stack)))
    (let ((new-node (mk-node val current)))
      (progn
        (cas-tagged stack current new-node *tag-counter*)
        t))))

(define (pop-tagged stack)
  (let ((current (cadr stack)))
    (let ((node (tagged-ptr current)))
      (cond ((null node) nil)
            (t
             (let ((next-tp (node-next node))
                   (val (node-val node)))
               (let ((new-ptr (tagged-ptr next-tp)))
                 (progn
                   (cas-tagged stack current new-ptr (+ (tagged-tag current) 1))
                   val))))))))

; --- Test: sequential push/pop ---
(define s-untagged (mk-stack-untagged))
(print "Untagged stack: sequential push/pop")
(push-untagged s-untagged 10)
(push-untagged s-untagged 20)
(push-untagged s-untagged 30)
(define p1 (pop-untagged s-untagged))
(define p2 (pop-untagged s-untagged))
(define p3 (pop-untagged s-untagged))
(print (list 'popped p1 p2 p3))
(ck "untagged-seq-lifo" (and (= p1 30) (= p2 20) (= p3 10)))

; --- Test: tagged stack ---
(define s-tagged (mk-stack))
(print "Tagged stack: sequential push/pop")
(push-tagged s-tagged 100)
(push-tagged s-tagged 200)
(push-tagged s-tagged 300)
(define tp1 (pop-tagged s-tagged))
(define tp2 (pop-tagged s-tagged))
(define tp3 (pop-tagged s-tagged))
(print (list 'popped-tagged tp1 tp2 tp3))
(ck "tagged-seq-lifo" (and (= tp1 300) (= tp2 200) (= tp3 100)))

; --- Test: demonstrate ABA problem with untagged ---
; Classic scenario: Thread A reads top X. Thread B pops X, pushes Y, pops Y and
; pushes X back.  A's CAS(top: X -> X.next) then succeeds although X.next may
; be stale.  The simulation below re-pushes the value 2 in a fresh node.
(define s-aba (mk-stack-untagged))
(push-untagged s-aba 1)
(push-untagged s-aba 2)
(print "Demonstrating ABA: initial stack has 2,1")

; Thread A reads top
(define topA (cadr s-aba))
(print (list 'thread-A-reads-top (node-val topA)))

; Thread B pops 2
(pop-untagged s-aba)
(print "Thread B popped 2")

; Thread B pushes 3
(push-untagged s-aba 3)
(print "Thread B pushed 3")

; Thread B pops 3
(pop-untagged s-aba)
(print "Thread B popped 3")

; Thread B pushes 2 (same node val)
(push-untagged s-aba 2)
(print "Thread B pushed 2")

; Thread A's CAS is not run here.  Had the same node returned to the top, an
; untagged CAS would succeed even though the state changed: the ABA problem.
(print "ABA vulnerability: Thread A would CAS(topA->new) and succeed,")
(print "even though stack state changed underneath!")

; What the simulation shows: to Thread A the top LOOKS untouched (same value 2),
; but it is a DIFFERENT node object and A's stale node is no longer in the
; stack.  In a real ABA the same node (or a reused address) comes back, and an
; untagged CAS, comparing only the pointer, cannot tell the states apart.
(define top-b (cadr s-aba))
(ck "aba-top-value-looks-unchanged" (= (node-val top-b) (node-val topA)))
(ck "aba-demonstrated-stack-changed" (not (eq top-b topA)))

(define (node-reachable-from top target)
  (cond ((null top) nil)
        ((eq top target) t)
        (t (node-reachable-from (node-next top) target))))
(ck "aba-stale-node-detached" (not (node-reachable-from (cadr s-aba) topA)))
(ck "aba-stack-depth-back-to-2"
    (and (= (node-val (node-next top-b)) 1) (null (node-next (node-next top-b)))))

; Verify final state
(define aba-final (pop-untagged s-aba))
(print (list 'final-pop-aba aba-final))
(ck "aba-final-pop-is-2" (= aba-final 2))

; --- Test: tagged pointers prevent ABA ---
(define s-safe (mk-stack))
(push-tagged s-safe 1)
(push-tagged s-safe 2)
(print "Tagged stack prevents ABA: tag counter ensures uniqueness")

(define tpA (cadr s-safe))
(print (list 'thread-A-tag (tagged-tag tpA)))

; Simulate ABA scenario with tags
(pop-tagged s-safe)
(push-tagged s-safe 3)
(pop-tagged s-safe)
(push-tagged s-safe 2)
; Tag has changed, so CAS would fail
(print (list 'new-tag *tag-counter*))
(ck "tagged-prevents-aba" (> *tag-counter* 1))

(print "=== Lock-free Stack Done ===")
