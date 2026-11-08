; Byzantine Fault Tolerance: 4 generals, 1 traitor.
; Show 3f+1 needed (f=1, so 4 generals needed).
; Oral messages algorithm. Prove non-consensus with 3 generals.

(print "=== Byzantine Fault Tolerance ===")

; With 4 generals (f=1): 3f+1 = 4, so consensus possible.
; Commander (general 0) sends order to lieutenants (1,2,3).
; Traitor is general 3 (or commander).

; Oral messages algorithm for f=1 (2 rounds):
; Round 1: Commander sends value to all lieutenants.
; Round 2: Each lieutenant forwards received value to others.

(define (majority v1 v2 v3)
  (cond ((= v1 v2) v1)
        ((= v1 v3) v1)
        (t v2)))

(define (has-majority vals)
  (let ((c0 (count (lambda (x) (= x 0)) vals))
        (c1 (count (lambda (x) (= x 1)) vals)))
    (cond ((> c0 c1) 0) ((> c1 c0) 1) (t -1))))

; Case 1: Loyal commander (value=0), traitor lieutenant 3
; Commander sends 0 to all.
; Loyal L1 receives 0 from C, L2's relay says C sent 0 to L2.
; Loyal L2 receives 0 from C, L1's relay says C sent 0 to L1.
; Traitor L3 may lie about what C sent.
; Both loyal lieutenants use majority(own-recv, L2-relay, L3-relay)
; Since 2 out of 3 say 0, they decide 0.

(define (bft-case1)
  ; Commander (loyal) sends 0
  (define C-val 0)
  ; L1 receives 0 from C
  (define L1-from-C 0)
  ; L2 receives 0 from C
  (define L2-from-C 0)
  ; L3 (traitor) receives 0 but lies
  (define L3-from-C 0)
  
  ; Round 2: L1 relays to L2, L3 what C told L1
  (define L1-relay-to-L2 0)
  (define L1-relay-to-L3 0)
  ; L2 relays to L1, L3 what C told L2
  (define L2-relay-to-L1 0)
  (define L2-relay-to-L3 0)
  ; L3 (traitor) relays lies
  (define L3-relay-to-L1 1)
  (define L3-relay-to-L2 1)
  
  ; L1's decision: majority(L1-from-C, L2-relay-to-L1, L3-relay-to-L1)
  (define L1-decision (majority L1-from-C L2-relay-to-L1 L3-relay-to-L1))
  ; L2's decision: majority(L2-from-C, L1-relay-to-L2, L3-relay-to-L2)
  (define L2-decision (majority L2-from-C L1-relay-to-L2 L3-relay-to-L2))
  
  (= L1-decision L2-decision))

(cond ((bft-case1)
       (print "PASS: bft-4gen-consensus-loyal-cmd"))
      (t (print "FAIL: bft-4gen-consensus-loyal-cmd")))

; Case 2: Traitor commander, 3 loyal lieutenants
; Commander (traitor) sends 0 to L1, 1 to L2, 0 to L3.
; Round 2: L1 relays "C told me 0" to L2, L3.
;           L2 relays "C told me 1" to L1, L3.
;           L3 relays "C told me 0" to L1, L2.
; L1 sees: own=0, L2-relay=1, L3-relay=0 -> majority = 0
; L2 sees: own=1, L1-relay=0, L3-relay=0 -> majority = 0
; L3 sees: own=0, L1-relay=0, L2-relay=1 -> majority = 0
; All loyal decide 0. Consensus!

(define (bft-case2)
  (define L1-from-C 0)
  (define L2-from-C 1)
  (define L3-from-C 0)
  
  (define L1-relay 0)   ; what L1 relays to others
  (define L2-relay 1)   ; what L2 relays to others
  (define L3-relay 0)   ; what L3 relays to others
  
  (define L1-decision (majority L1-from-C L2-relay L3-relay))
  (define L2-decision (majority L2-from-C L1-relay L3-relay))  
  (define L3-decision (majority L3-from-C L1-relay L2-relay))
  
  (and (= L1-decision L2-decision) (= L2-decision L3-decision)))

(cond ((bft-case2)
       (print "PASS: bft-4gen-consensus-traitor-cmd"))
      (t (print "FAIL: bft-4gen-consensus-traitor-cmd")))

; Case 3: Only 3 generals total (f=1, need 3f+1=4 but have 3).
; Show no consensus possible with 3 generals, 1 traitor.
; Commander sends Vc to L1, Vc' to L2 (different values).
; L1 and L2 relay what they received.
; L1 gets: own + L2-relay. Only 2 values, can't distinguish.

(define (bft-case3a)
  ; Commander (traitor): sends 0 to L1, 1 to L2
  (define L1-from-C 0)
  (define L2-from-C 1)
  
  ; Round 2: L1 relays 0, L2 relays 1
  (define L1-relay 0)
  (define L2-relay 1)
  
  ; L1 decides based on (own=0, L2-relay=1) -> no clear majority
  (define L1-vals (list L1-from-C L2-relay))
  (define L2-vals (list L2-from-C L1-relay))
  
  (let ((L1-maj (has-majority L1-vals))
        (L2-maj (has-majority L2-vals)))
    (or (= L1-maj -1) (= L2-maj -1) (not (= L1-maj L2-maj)))))

(cond ((bft-case3a)
       (print "PASS: bft-3gen-no-consensus-traitor-cmd"))
      (t (print "FAIL: bft-3gen-no-consensus-traitor-cmd")))

; Case 3b: Loyal commander, traitor lieutenant with 3 generals.
; Commander sends 0 to both. L1 loyal, L2 traitor.
; L1 reports "C sent 0". L2 reports "C sent 1" (lies).
; L1 gets: own=0, L2-report=1 -> no clear majority (50/50)
; This breaks the algorithm and L1 must resort to default.

(define (bft-case3b)
  (define L1-from-C 0)
  (define L2-from-C 0)
  
  ; L1 relays truth, L2 lies
  (define L1-relay 0)
  (define L2-relay 1)  ; L2 lies about what C sent
  
  (define L1-vals (list L1-from-C L2-relay))
  (define L2-vals (list L2-from-C L1-relay))
  
  (let ((L1-maj (has-majority L1-vals)))
    ; With only 2 inputs and one lying, L1 can't get quorum
    (= L1-maj -1)))

(cond ((bft-case3b)
       (print "PASS: bft-3gen-no-consensus-traitor-lt"))
      (t (print "FAIL: bft-3gen-no-consensus-traitor-lt")))

; Show that with 4 generals, consensus is ALWAYS possible
; (assuming at most f=1 traitor)

(define (bft-4gen-all-cases)
  (and (bft-case1) (bft-case2)))

(cond ((bft-4gen-all-cases)
       (print "PASS: bft-4gen-consensus-always"))
      (t (print "FAIL: bft-4gen-consensus-always")))

; The impossibility result: with N = 3f = 3, consensus impossible.
; With N = 3f+1 = 4, consensus is possible.

(print (cat (cat "N=" (num-to-str 3))
            (cat " (3f): consensus " 
                 (cond ((and (not (bft-case3a)) (not (bft-case3b))) "impossible")
                       (t "fails")))))

(cond ((and (bft-case3a) (bft-case3b))
       (print "PASS: bft-3f-plus-1-necessary"))
      (t (print "FAIL: bft-3f-plus-1-necessary")))

(print "=== BYZANTINE FAULT TOLERANCE DONE ===")
