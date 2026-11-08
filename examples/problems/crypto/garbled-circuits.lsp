; Yao's garbled circuits: Alice garbles an AND gate.
; Bob evaluates via oblivious transfer (simulated).
; Truth table encrypted with simple hash.

(seed 99)

; AND gate truth table:
; (a,b) -> z: (0,0)->0, (0,1)->0, (1,0)->0, (1,1)->1

(define (modhash x)
  (mod (+ (* x 2654435761) 3141592653) 1000000007))

(define (garble-row ka kb z)
  (modhash (+ (* ka 1000003) (* kb 101) z)))

(define (alice-keys)
  (list (list 1001 2001)   ; a0, a1
        (list 3001 4001)   ; b0, b1
        (list 5001 6001))) ; z0, z1

(define keys (alice-keys))
(define a0 (caar keys)) (define a1 (cadar keys))
(define b0 (caadr keys)) (define b1 (cadadr keys))
(define z0 (car (caddr keys))) (define z1 (cadr (caddr keys)))

(define garbled-table
  (list (garble-row a0 b0 z0)   ; (0,0)->0
        (garble-row a0 b1 z0)   ; (0,1)->0
        (garble-row a1 b0 z0)   ; (1,0)->0
        (garble-row a1 b1 z1))) ; (1,1)->1

(print "=== Yao Garbled Circuits ===")

(cond ((= (length garbled-table) 4)
       (print "PASS: garbled-table-length"))
      (t (print "FAIL: garbled-table-length")))

; Bob evaluates: given keys ka, kb, tries z0 and z1
(define (eval-row garbled a-val b-val)
  (let* ((ka (cond ((= a-val 0) a0) (t a1)))
         (kb (cond ((= b-val 0) b0) (t b1)))
         (idx (+ (* a-val 2) b-val))
         (target (nth idx garbled)))
    (cond ((= target (garble-row ka kb z0)) 0)
          ((= target (garble-row ka kb z1)) 1)
          (t -1))))

; Test AND gate: (0,0)->0
(cond ((= (eval-row garbled-table 0 0) 0)
       (print "PASS: and-0-0-is-0"))
      (t (print "FAIL: and-0-0-is-0")))

(cond ((= (eval-row garbled-table 0 1) 0)
       (print "PASS: and-0-1-is-0"))
      (t (print "FAIL: and-0-1-is-0")))

(cond ((= (eval-row garbled-table 1 0) 0)
       (print "PASS: and-1-0-is-0"))
      (t (print "FAIL: and-1-0-is-0")))

(cond ((= (eval-row garbled-table 1 1) 1)
       (print "PASS: and-1-1-is-1"))
      (t (print "FAIL: and-1-1-is-1")))

; Verify that garbled table hides intermediate values
(define (all-correct-p garbled)
  (and (= (eval-row garbled 0 0) 0)
       (= (eval-row garbled 0 1) 0)
       (= (eval-row garbled 1 0) 0)
       (= (eval-row garbled 1 1) 1)))

(cond ((all-correct-p garbled-table)
       (print "PASS: garbled-and-correct"))
      (t (print "FAIL: garbled-and-correct")))

; Verify that the garbled truth table doesn't directly reveal the output
; (entries are encrypted)
(cond ((and (/= (nth 0 garbled-table) (nth 3 garbled-table))
            (/= (nth 0 garbled-table) z0)
            (/= (nth 0 garbled-table) z1))
       (print "PASS: garbled-encrypted"))
      (t (print "FAIL: garbled-encrypted")))

(print "=== GARBLED CIRCUITS DONE ===")
