; qec-bitflip.lsp -- 3-qubit bit-flip error correction code.
; Encode |0> -> |000>. X error on qubit 2 flips it to |010>.
; Syndromes Z1Z2 and Z2Z3 detect which qubit flipped.
; Correct by flipping identified qubit. Decode back to |0>.
; Verify: encode, error, correct, decode -> recovers |0>.

(define (qec-init-state) (list 0 0 0))

(define (qec-bit state i)
  (cond ((= i 0) (car state))
        ((= i 1) (cadr state))
        (t (caddr state))))

(define (qec-x state q)
  (cond ((= q 0) (list (- 1 (car state)) (cadr state) (caddr state)))
        ((= q 1) (list (car state) (- 1 (cadr state)) (caddr state)))
        (t (list (car state) (cadr state) (- 1 (caddr state))))))

(define (qec-xor a b)
  (cond ((= a b) 0) (t 1)))

(define (qec-syndrome state)
  (let ((q0 (car state)) (q1 (cadr state)) (q2 (caddr state)))
    (list (qec-xor q0 q1) (qec-xor q1 q2))))

(define (qec-diagnose synd)
  (cond ((equal synd '(0 0)) -1)
        ((equal synd '(1 0)) 0)
        ((equal synd '(1 1)) 1)
        ((equal synd '(0 1)) 2)
        (t -1)))

(define (qec-correct state)
  (let ((synd (qec-syndrome state)))
    (let ((qb (qec-diagnose synd)))
      (cond ((= qb -1) state)
            (t (qec-x state qb))))))

(define (qec-decode state)
  (car state))

; --- demonstration ---
(define qec-encoded (qec-init-state))
(define qec-errored (qec-x qec-encoded 1))
(define qec-synd-val (qec-syndrome qec-errored))
(define qec-fixed (qec-correct qec-errored))
(define qec-decoded (qec-decode qec-fixed))

(list
  (list (quote encoded) qec-encoded)
  (list (quote after-X-on-q1) qec-errored)
  (list (quote syndrome) qec-synd-val)
  (list (quote corrected) qec-fixed)
  (list (quote decoded) qec-decoded)
  (list (quote success) (= qec-decoded 0)))
