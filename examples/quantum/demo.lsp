; ============================================================
; Quantum algorithms — one clear, self-describing result each.
;
; SISP quotes strings but prints symbols/numbers/lists raw, so
; each demo prints a labelled S-expression you can read directly:
;   (grover (target (1 0 1)) (measured (1 0 1)) (prob-pct 94) (found t))
; ============================================================

(define (pct p) (truncate (+ 0.5 (* 100.0 p))))
(define (bits i n) (int-bits i n))

; 1. ENTANGLEMENT — a Bell pair: only |00> and |11>, 50% each
(print (list 'entanglement-bell
             (list 'nonzero-states (list (bits 0 2) (bits 3 2)))
             (list 'each-prob-pct (pct (nth 0 (probs (bell)))))))

; 2. GHZ — three qubits entangled: only |000> and |111>
(print (list 'entanglement-ghz
             (list 'nonzero-states (list (bits 0 3) (bits 7 3)))
             (list 'each-prob-pct (pct (nth 0 (probs (ghz 3)))))))

; 3. BERNSTEIN-VAZIRANI — find a hidden 4-bit string in ONE query
(print (list 'bernstein-vazirani
             (list 'hidden '(1 0 1 1))
             (list 'recovered (bv-recover (list 1 0 1 1) 4))
             (list 'queries 1)))

; 4. DEUTSCH-JOZSA — constant vs balanced in ONE query
(print (list 'deutsch-jozsa
             (list 'constant-oracle (cond ((dj-constant-p (dj-oracle-constant 3) 3) 'constant) (t 'balanced)))
             (list 'balanced-oracle (cond ((dj-constant-p (dj-oracle-balanced 3) 3) 'constant) (t 'balanced)))
             (list 'queries 1)))

; 5. GROVER — search 8 items for the marked |101>
(let ((st (grover-run 5 3 2)))
  (print (list 'grover
               (list 'space 8)
               (list 'target (bits 5 3))
               (list 'iterations 2)
               (list 'measured (bits (most-likely st 3) 3))
               (list 'prob-pct (pct (nth 5 (probs st))))
               (list 'found (= (most-likely st 3) 5)))))

; 6. QFT — |000> becomes a uniform superposition of all 8 states
(let ((p (probs (qft-on (qalloc 3) 3))))
  (print (list 'qft
               (list 'input (bits 0 3))
               (list 'output 'uniform)
               (list 'each-prob-pct (pct (nth 0 p))))))

; 7. TELEPORTATION — move |1> from qubit 0 to qubit 2
(print (list 'teleportation
             (list 'sent 1)
             (list 'received-prob-pct (pct (prob-qubit-1 (teleport (list (list "x" 0))) 2 3)))))

; 8. SUPERDENSE CODING — 2 classical bits through 1 qubit
(print (list 'superdense-coding
             (list 'sent '(1 0)) (list 'decoded (int-bits (superdense 1 0) 2))
             (list 'qubits-sent 1) (list 'bits-conveyed 2)))
