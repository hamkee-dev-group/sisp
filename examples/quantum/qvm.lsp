; ============================================================
; A quantum computer, simulated in pure SISP
;
; State = a vector of 2^n complex amplitudes (one per basis
; state).  Gates are unitary operations applied to the vector.
; This is genuine quantum linear algebra -- real superposition,
; entanglement, interference, and measurement -- but it is a
; SIMULATION: cost is 2^n, so it gets no quantum speedup and is
; practical only for a handful of qubits.  A real quantum
; computer needs physical qubits; no program can be one.
;
; A complex number is (re im).  A qubit index q addresses bit q
; of a basis-state integer (qubit 0 = least significant bit).
; ============================================================

; ---- complex arithmetic -------------------------------------
(define (c-re z) (car z))
(define (c-im z) (cadr z))
(define (cx re im) (list re im))
(define (c-add a b) (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))
(define (c-sub a b) (list (- (car a) (car b)) (- (cadr a) (cadr b))))
(define (c-mul a b)
  (list (- (* (car a) (car b)) (* (cadr a) (cadr b)))
        (+ (* (car a) (cadr b)) (* (cadr a) (car b)))))
(define (c-scale s z) (list (* s (car z)) (* s (cadr z))))
(define (c-abs2 z) (+ (* (car z) (car z)) (* (cadr z) (cadr z))))

; ---- basis-state bit helpers --------------------------------
(define qpow2-tab
  (list 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536))
(define (qpow2 n) (nth n qpow2-tab))
(define (basis-bit i q) (mod (truncate (/ i (qpow2 q))) 2))
(define (flip-bit i q)
  (cond ((= (basis-bit i q) 1) (- i (qpow2 q))) (t (+ i (qpow2 q)))))
(define (qrange a b) (cond ((>= a b) nil) (t (cons a (qrange (+ a 1) b)))))

; ---- state vectors ------------------------------------------
(define (vget st i) (nth i st))
; |00..0>: amplitude 1 at index 0, else 0
(define (qalloc n)
  (cons (cx 1 0) (map (lambda (i) (cx 0 0)) (qrange 1 (qpow2 n)))))
(define (build-state n fn) (map fn (qrange 0 (qpow2 n))))

; ---- single-qubit gate (2x2: u00 u01 u10 u11) ---------------
(define (apply-1q st q g n)
  (build-state n
    (lambda (i)
      (let ((j (flip-bit i q)))
        (cond ((= (basis-bit i q) 0)
               (c-add (c-mul (nth 0 g) (vget st i)) (c-mul (nth 1 g) (vget st j))))
              (t
               (c-add (c-mul (nth 2 g) (vget st j)) (c-mul (nth 3 g) (vget st i)))))))))

; ---- CNOT / controlled-Z / SWAP -----------------------------
(define (apply-cnot st c tgt n)
  (build-state n
    (lambda (i)
      (cond ((= (basis-bit i c) 1) (vget st (flip-bit i tgt))) (t (vget st i))))))

(define (apply-cz st c tgt n)
  (build-state n
    (lambda (i)
      (cond ((and (= (basis-bit i c) 1) (= (basis-bit i tgt) 1))
             (c-scale -1 (vget st i)))
            (t (vget st i))))))

(define (apply-swap st a b n)
  (build-state n
    (lambda (i)
      (cond ((= (basis-bit i a) (basis-bit i b)) (vget st i))
            (t (vget st (flip-bit (flip-bit i a) b)))))))

; Toffoli (CCX): flip tgt when both controls are 1
(define (apply-ccx st c1 c2 tgt n)
  (build-state n
    (lambda (i)
      (cond ((and (= (basis-bit i c1) 1) (= (basis-bit i c2) 1))
             (vget st (flip-bit i tgt)))
            (t (vget st i))))))

; controlled phase: multiply by e^(i theta) when both qubits are 1
(define (apply-cphase st c tgt theta n)
  (build-state n
    (lambda (i)
      (cond ((and (= (basis-bit i c) 1) (= (basis-bit i tgt) 1))
             (c-mul (cx (cos theta) (sin theta)) (vget st i)))
            (t (vget st i))))))

; ---- gate matrices ------------------------------------------
(define qpi 3.141592653589793)
(define rt2 (/ 1.0 (sqrt 2.0)))
(define gate-h (list (cx rt2 0) (cx rt2 0) (cx rt2 0) (cx (- 0.0 rt2) 0)))
(define gate-x (list (cx 0 0) (cx 1 0) (cx 1 0) (cx 0 0)))
(define gate-y (list (cx 0 0) (cx 0 -1) (cx 0 1) (cx 0 0)))
(define gate-z (list (cx 1 0) (cx 0 0) (cx 0 0) (cx -1 0)))
(define gate-s (list (cx 1 0) (cx 0 0) (cx 0 0) (cx 0 1)))
(define gate-t (list (cx 1 0) (cx 0 0) (cx 0 0) (cx (cos (/ qpi 4)) (sin (/ qpi 4)))))
(define (gate-phase theta) (list (cx 1 0) (cx 0 0) (cx 0 0) (cx (cos theta) (sin theta))))

; ---- circuit runner (the "assembly" the quantum OS executes)-
; instruction forms: (h q) (x q) (y q) (z q) (s q) (t q)
;   (phase q theta) (cnot c t) (cz c t) (swap a b) (ccx c1 c2 t)
(define (apply-instr st ins n)
  (let ((op (car ins)))
    (cond ((equal op "h")     (apply-1q st (cadr ins) gate-h n))
          ((equal op "x")     (apply-1q st (cadr ins) gate-x n))
          ((equal op "y")     (apply-1q st (cadr ins) gate-y n))
          ((equal op "z")     (apply-1q st (cadr ins) gate-z n))
          ((equal op "s")     (apply-1q st (cadr ins) gate-s n))
          ((equal op "t")     (apply-1q st (cadr ins) gate-t n))
          ((equal op "phase") (apply-1q st (cadr ins) (gate-phase (caddr ins)) n))
          ((equal op "cnot")  (apply-cnot st (cadr ins) (caddr ins) n))
          ((equal op "cz")    (apply-cz st (cadr ins) (caddr ins) n))
          ((equal op "cphase") (apply-cphase st (cadr ins) (caddr ins) (cadddr ins) n))
          ((equal op "swap")  (apply-swap st (cadr ins) (caddr ins) n))
          ((equal op "ccx")   (apply-ccx st (cadr ins) (caddr ins) (cadddr ins) n))
          (t st))))
(define (run-circuit st prog n) (foldl (lambda (s ins) (apply-instr s ins n)) st prog))

; ---- measurement / inspection -------------------------------
(define (probs st) (map c-abs2 st))
; probability that qubit q reads 1
(define (prob-qubit-1 st q n)
  (foldl (lambda (a i) (cond ((= (basis-bit i q) 1) (+ a (c-abs2 (vget st i))) ) (t a)))
         0.0 (qrange 0 (qpow2 n))))
; index of the most probable basis state
(define (most-likely st n) (argmax-idx (probs st)))
(define (argmax-idx ps) (argmax-acc ps 0 0 -1.0))
(define (argmax-acc ps i best bestp)
  (cond ((eq ps nil) best)
        ((> (car ps) bestp) (argmax-acc (cdr ps) (+ i 1) i (car ps)))
        (t (argmax-acc (cdr ps) (+ i 1) best bestp))))
; total probability (should stay ~1: unitarity check)
(define (norm st) (foldl + 0.0 (probs st)))
; read the n-bit outcome integer as a bit list (qubit 0 first)
(define (int-bits x n) (map (lambda (q) (basis-bit x q)) (qrange 0 n)))
