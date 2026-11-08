; ============================================================
; Quantum algorithm tests (PASS/FAIL)
; ============================================================
(define test-fails 0)
(define (check label ok)
  (cond (ok (print (cat "PASS: " label)))
        (t (setq test-fails (+ test-fails 1)) (print (cat "FAIL: " label)))))
(define (approx a b) (< (abs (- a b)) 0.0001))

(print "== entanglement ==")
(let ((p (probs (bell))))
  (check "Bell: |00> and |11> each 1/2"
         (and (approx (nth 0 p) 0.5) (approx (nth 3 p) 0.5))))
(let ((p (probs (ghz 3))))
  (check "GHZ3: |000> and |111> each 1/2"
         (and (approx (nth 0 p) 0.5) (approx (nth 7 p) 0.5))))

(print "== Bernstein-Vazirani (recover hidden string, 1 query) ==")
(check "BV recovers 101" (equal (bv-recover (list 1 0 1) 3) (list 1 0 1)))
(check "BV recovers 1101" (equal (bv-recover (list 1 1 0 1) 4) (list 1 1 0 1)))

(print "== Deutsch-Jozsa (constant vs balanced, 1 query) ==")
(check "DJ constant -> constant" (dj-constant-p (dj-oracle-constant 3) 3))
(check "DJ balanced -> balanced" (not (dj-constant-p (dj-oracle-balanced 3) 3)))

(print "== Grover (search) ==")
(check "Grover finds 5 of 8 (n=3)" (= (grover-find 5 3 2) 5))
(check "Grover finds 2 of 8 (n=3)" (= (grover-find 2 3 2) 2))

(print "== Quantum Fourier Transform ==")
(let ((p (probs (qft-on (qalloc 3) 3))))
  (check "QFT|000> is uniform (all 1/8)" (every (lambda (x) (approx x 0.125)) p)))

(print "== teleportation ==")
(check "teleport |1> -> qubit2 reads 1"
       (approx (prob-qubit-1 (teleport (list (list "x" 0))) 2 3) 1.0))
(check "teleport |+> -> H on qubit2 gives |0>"
       (approx (prob-qubit-1 (run-circuit (teleport (list (list "h" 0))) (list (list "h" 2)) 3) 2 3) 0.0))

(print "== superdense coding (2 bits via 1 qubit) ==")
(check "superdense 00" (equal (int-bits (superdense 0 0) 2) (list 0 0)))
(check "superdense 01" (equal (int-bits (superdense 0 1) 2) (list 0 1)))
(check "superdense 10" (equal (int-bits (superdense 1 0) 2) (list 1 0)))
(check "superdense 11" (equal (int-bits (superdense 1 1) 2) (list 1 1)))

(print "============================================")
(cond ((= test-fails 0) (print "ALL QUANTUM TESTS PASSED"))
      (t (print (cat "TESTS FAILED: " (num-to-str test-fails)))))
