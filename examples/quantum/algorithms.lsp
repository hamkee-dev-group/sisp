; ============================================================
; Real quantum algorithms, as gate circuits on the QVM.
;
; Each is the genuine textbook circuit (not a shortcut): it runs
; on the simulated state vector and produces the correct quantum
; result.  Verified in tests.lsp.
; ============================================================

(define (h-all lo hi) (map (lambda (q) (list "h" q)) (qrange lo hi)))

; ---- entanglement: Bell and GHZ states ----------------------
(define (bell) (run-circuit (qalloc 2) (list (list "h" 0) (list "cnot" 0 1)) 2))
(define (ghz n)
  (run-circuit (qalloc n)
    (cons (list "h" 0) (map (lambda (q) (list "cnot" (- q 1) q)) (qrange 1 n))) n))

; ---- Bernstein-Vazirani: recover hidden string s in 1 query -
; s is a list of n bits; qubit n is the ancilla.
(define (bv-oracle s i n)
  (cond ((= i n) nil)
        ((= (nth i s) 1) (cons (list "cnot" i n) (bv-oracle s (+ i 1) n)))
        (t (bv-oracle s (+ i 1) n))))
(define (bv-run s n)
  (run-circuit (qalloc (+ n 1))
    (append (list (list "x" n))
      (append (h-all 0 (+ n 1))
        (append (bv-oracle s 0 n) (h-all 0 n))))
    (+ n 1)))
; measured input register (low n bits of the most likely outcome)
(define (bv-recover s n) (int-bits (most-likely (bv-run s n) (+ n 1)) n))

; ---- Deutsch-Jozsa: constant vs balanced in 1 query ---------
; oracle = list of gate instructions on n+1 qubits (ancilla = n)
(define (dj-run oracle n)
  (run-circuit (qalloc (+ n 1))
    (append (list (list "x" n))
      (append (h-all 0 (+ n 1))
        (append oracle (h-all 0 n))))
    (+ n 1)))
; probability the input register reads all-zero
(define (dj-prob-zero st n)
  (foldl (lambda (a i)
           (cond ((= (mod i (qpow2 n)) 0) (+ a (c-abs2 (vget st i)))) (t a)))
         0.0 (qrange 0 (qpow2 (+ n 1)))))
; t = constant, nil = balanced
(define (dj-constant-p oracle n) (> (dj-prob-zero (dj-run oracle n) n) 0.99))
; sample oracles
(define (dj-oracle-constant n) nil)                          ; f(x)=0
(define (dj-oracle-balanced n)                               ; f(x)=parity(x)
  (map (lambda (q) (list "cnot" q n)) (qrange 0 n)))

; ---- Grover: search a marked item among 2^n -----------------
(define (phase-flip st idx n)
  (build-state n (lambda (i) (cond ((= i idx) (c-scale -1 (vget st i))) (t (vget st i))))))
(define (reflect-zero st n)
  (build-state n (lambda (i) (cond ((= i 0) (vget st i)) (t (c-scale -1 (vget st i)))))))
(define (grover-diffusion st n)
  (run-circuit (reflect-zero (run-circuit st (h-all 0 n) n) n) (h-all 0 n) n))
(define (grover-step st w n) (grover-diffusion (phase-flip st w n) n))
(define (grover-iter st w n k)
  (cond ((= k 0) st) (t (grover-iter (grover-step st w n) w n (- k 1)))))
(define (grover-run w n iters)
  (grover-iter (run-circuit (qalloc n) (h-all 0 n) n) w n iters))
(define (grover-find w n iters) (most-likely (grover-run w n iters) n))

; ---- Quantum Fourier Transform ------------------------------
(define (qft-rots j k n)
  (cond ((>= k n) nil)
        (t (cons (list "cphase" k j (/ qpi (qpow2 (- k j)))) (qft-rots j (+ k 1) n)))))
(define (qft-build j n)
  (cond ((>= j n) (qft-swaps 0 (- n 1)))
        (t (append (cons (list "h" j) (qft-rots j (+ j 1) n)) (qft-build (+ j 1) n)))))
(define (qft-swaps a b)
  (cond ((>= a b) nil) (t (cons (list "swap" a b) (qft-swaps (+ a 1) (- b 1))))))
(define (qft-circuit n) (qft-build 0 n))
(define (qft-on st n) (run-circuit st (qft-circuit n) n))

; ---- Quantum teleportation (deferred-measurement form) ------
; teleport qubit 0's state to qubit 2 using a Bell pair (1,2).
; prep = instructions preparing the state on qubit 0.
(define (teleport prep)
  (run-circuit (qalloc 3)
    (append prep
      (list (list "h" 1) (list "cnot" 1 2)      ; Bell pair on 1,2
            (list "cnot" 0 1) (list "h" 0)       ; Alice's basis change
            (list "cnot" 1 2) (list "cz" 0 2)))  ; corrections (deferred)
    3))

; ---- Superdense coding --------------------------------------
; send 2 classical bits (a b) through 1 qubit + shared entanglement
(define (superdense a b)
  (let ((enc (append (cond ((= b 1) (list (list "x" 0))) (t nil))
                     (cond ((= a 1) (list (list "z" 0))) (t nil)))))
    (most-likely
      (run-circuit (qalloc 2)
        (append (list (list "h" 0) (list "cnot" 0 1))     ; shared Bell pair
          (append enc                                      ; Alice encodes
                  (list (list "cnot" 0 1) (list "h" 0))))  ; Bob decodes
        2)
      2)))
