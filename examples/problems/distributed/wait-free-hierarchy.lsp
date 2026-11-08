; Wait-free hierarchy. Consensus numbers: reg=1, TAS=2, CAS=infinity. Universal construction.
(print "=== Wait-Free Hierarchy ===")
(seed 0)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

; --- Atomic register (consensus number 1) ---
(define (mk-reg) (list (quote reg) nil))
(define (reg-read r) (cadr r))
(define (reg-write r v) (set-car (cdr r) v) v)

; 1-process consensus (trivial)
(define (one-proc-consensus val)
  (let ((r (mk-reg)))
    (reg-write r val)
    (reg-read r)))

(print "Atomic register: consensus number 1")
(print (list 'one-proc-consensus (one-proc-consensus 42)))
(ck "register-consensus-1-works" (= (one-proc-consensus 42) 42))

; --- Test-and-set (consensus number 2) ---
(define (mk-tas) (list (quote tas) 0))

(define (tas-op tas val)
  (let ((old (cadr tas)))
    (cond ((= old 0) (progn (set-car (cdr tas) val) old))
          (t old))))

; 2-process consensus with TAS.  Proposals are tagged (+100 for process 0,
; +200 for process 1) so the decision also names the winner: 107 = P0 with 7.
(define (two-proc-consensus val0 val1)
  (let ((tas (mk-tas)))
    (let ((r0 (tas-op tas (+ val0 100))))
      (cond ((= r0 0) (+ val0 100))
            (t (tas-op tas (+ val1 200)))))))

(print "Test-and-set: consensus number 2")
(define twopc (two-proc-consensus 7 9))
(print (list 'two-proc-tas-consensus twopc))
(ck "tas-consensus-decided" (= twopc 107))

; --- Compare-and-swap (consensus number infinity) ---
(define (mk-cas init) (list (quote cas) init))
(define (cas-op cas expected new)
  (let ((current (cadr cas)))
    (cond ((equal current expected)
           (progn (set-car (cdr cas) new) current))
          (t current))))

; N-process consensus with CAS
(define (n-proc-consensus vals)
  (let ((obj (mk-cas nil)))
    (n-proc-consensus-aux obj vals)))

(define (n-proc-consensus-aux obj vals)
  (cond ((null vals) nil)
        (t (let ((r (cas-op obj nil (car vals))))
             (cond ((null r) (n-proc-consensus-aux obj (cdr vals)))
                   (t r))))))

(print "CAS: consensus number infinity")
(define ncons (n-proc-consensus (list 10 20 30 40 50)))
(print (list 'n-proc-cas-consensus ncons))
(ck "cas-consensus-n-works" (in ncons (list 10 20 30 40 50)))

; --- CAS retry loop ---
; Read, compute f(old), CAS, retry on failure: the lock-free pattern behind
; universal constructions (not Herlihy's wait-free construction itself).
(define (mk-ufc) (mk-cas 0))

(define (ufc-inc ufc)
  (let* ((old (cadr ufc))
         (result (cas-op ufc old (+ old 1))))
    (cond ((= result old) (+ old 1))
          (t (ufc-inc ufc)))))

(define (ufc-dbl ufc)
  (let* ((old (cadr ufc))
         (result (cas-op ufc old (* old 2))))
    (cond ((= result old) (* old 2))
          (t (ufc-dbl ufc)))))

(define ufc (mk-ufc))
(print "CAS universal construction: applying ops")
(ufc-inc ufc)
(ufc-inc ufc)
(ufc-dbl ufc)
(print (list 'final-ufc-state (cadr ufc)))
(ck "universal-construction-state" (= (cadr ufc) 4))

; A CAS object holds a value like a register (written directly here)
(define creg (mk-cas 0))
(set-car (cdr creg) 99)
(ck "cas-simulates-register" (= (cadr creg) 99))

(print "=== Wait-Free Hierarchy Done ===")
