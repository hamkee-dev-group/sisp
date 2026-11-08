; Multi-proposer Paxos. Safety: only one value chosen. Liveness: eventually chosen.
(print "=== Multi-Proposer Paxos ===")
(seed 0)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

; Each acceptor tracks: (highest-prepare-ballot highest-accepted-ballot accepted-value)
(define (mk-acceptor) (list -1 -1 nil))

; Each proposer tracks: (id round proposed-value)
; Ballot = round * N + id
(define N 3)
(define (ballot id round) (+ (* round N) id))

; Phase 1: proposer sends prepare(b) to all acceptors
; Acceptor: if b > highest-prepare, update and respond with promise(highest-accepted)
; Returns new acceptor state
(define (prepare-acceptor ac b)
  (let ((highest-prep (car ac)))
    (cond ((> b highest-prep)
           (list b (cadr ac) (caddr ac)))
          (t ac))))

; Phase 2: proposer collects promises, picks value from highest-accepted, sends accept(b,v)
; Acceptor: if b >= highest-prepare, accept and respond
(define (accept-acceptor ac b val)
  (let ((highest-prep (car ac)))
    (cond ((>= b highest-prep)
           (list b b val))
          (t ac))))

(define (highest-accepted-value acceptors best-ballot best-value)
  (cond ((null acceptors) best-value)
        ((and (not (null (caddr (car acceptors))))
              (> (cadr (car acceptors)) best-ballot))
         (highest-accepted-value (cdr acceptors)
                                 (cadr (car acceptors))
                                 (caddr (car acceptors))))
        (t (highest-accepted-value (cdr acceptors) best-ballot best-value))))

; Run one proposer round. Returns (acceptor-states chosen-value)
(define (proposer-round acceptors id round val)
  (let* ((b (ballot id round))
         (acceptors1 (map (lambda (ac) (prepare-acceptor ac b)) acceptors))
         (prior-value (highest-accepted-value acceptors1 -1 nil))
         (chosen-value (cond ((null prior-value) val) (t prior-value)))
         (acceptors2 (map (lambda (ac) (accept-acceptor ac b chosen-value)) acceptors1)))
    acceptors2))

; Run until a value is chosen (majority accepts same value)
(define (run-proposals acceptors max-rounds round)
  (cond ((>= round max-rounds) (list acceptors nil))
        (t
         (let* ((new-acceptors (proposer-round acceptors (mod round N) round (+ round 100))))
           (cond ((chosen-p new-acceptors) (list new-acceptors (chosen-val new-acceptors)))
                 (t (run-proposals new-acceptors max-rounds (+ round 1))))))))

; a value is chosen when a MAJORITY of all acceptors accepted it
; (the old code checked only the first three of five)
(define (count-value v vals)
  (cond ((null vals) 0)
        ((equal (car vals) v) (+ 1 (count-value v (cdr vals))))
        (t (count-value v (cdr vals)))))

(define (majority-threshold n)
  (+ 1 (truncate (/ n 2))))

(define (chosen-p acceptors)
  (let ((vals (map caddr acceptors)))
    (any (lambda (v)
           (and (not (null v))
                (>= (count-value v vals)
                    (majority-threshold (length vals)))))
         vals)))

(define (chosen-val acceptors)
  (let ((vals (map caddr acceptors)))
    (chosen-val-walk vals vals)))

(define (chosen-val-walk candidates vals)
  (cond ((null candidates) nil)
        ((and (not (null (car candidates)))
              (>= (count-value (car candidates) vals)
                  (majority-threshold (length vals))))
         (car candidates))
        (t (chosen-val-walk (cdr candidates) vals))))

; --- Test safety: different proposers, same chosen value ---
(define acceptors0 (list (mk-acceptor) (mk-acceptor) (mk-acceptor)))
(define (test-safety pros rounds)
  (let ((r (run-proposals acceptors0 rounds 0)))
    (let ((val (cadr r)))
      (cond ((null val) nil)
            (t val)))))

(print "Test 1: single-proposer liveness (3 rounds)")
(define r1 (run-proposals acceptors0 3 0))
(define chosen1 (cadr r1))
(print (list 'chosen chosen1))
(ck "paxos-value-chosen" (not (null chosen1)))

(print "Test 2: multi-proposer eventual choice")
(define acceptors-a (list (mk-acceptor) (mk-acceptor) (mk-acceptor) (mk-acceptor) (mk-acceptor)))
(define r2 (run-proposals acceptors-a 8 0))
(define chosen2 (cadr r2))
(print (list 'chosen chosen2))
(ck "paxos-multi-chosen" (not (null chosen2)))

; Test 3: verify that when value is chosen, no other value can be chosen later
(define acceptors-b (list (mk-acceptor) (mk-acceptor) (mk-acceptor)))
(define (run-two-phases)
  (let* ((a1 (proposer-round acceptors-b 0 0 42))
         (a2 (proposer-round a1 0 0 42))
         (a3 (proposer-round a2 0 0 42))
         (val1 (caddr (car a3)))
         (a4 (proposer-round a3 1 1 99))
         (a5 (proposer-round a4 1 1 99))
         (a6 (proposer-round a5 1 1 99))
         (val2 (caddr (car a6))))
    (list val1 val2)))

(define both (run-two-phases))
(print (list 'phase1-val (car both) 'phase2-val (cadr both)))
(ck "paxos-safety-same-value" (equal (car both) (cadr both)))

(print "=== Paxos Done ===")
