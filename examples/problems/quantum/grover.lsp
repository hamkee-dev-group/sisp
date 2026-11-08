; grover.lsp -- Grover's quantum search algorithm simulation (N=8).
; Amplitude vector: N entries, initially uniform 1/sqrt(N).
; Oracle: flips sign of marked item's amplitude.
; Grover iteration: H (oracle then reflection about mean).
; After ~sqrt(N)*pi/4 iterations, marked item has probability near 1.
; sqrt(8)*pi/4 ≈ 2.22 -> use 3 iterations.

(define (gr-init N)
  (let ((amp (/ 1.0 (sqrt N))))
    (gr-fill-vec N amp)))

(define (gr-fill-vec n amp)
  (cond ((= n 0) nil)
        (t (cons amp (gr-fill-vec (- n 1) amp)))))

(define (gr-mean amps)
  (/ (foldl + 0.0 amps) (length amps)))

(define (gr-oracle-step amps idx mark acc)
  (cond ((eq amps nil) (reverse acc))
        ((= idx mark)
         (gr-oracle-step (cdr amps) (+ idx 1) mark
           (cons (- 0.0 (car amps)) acc)))
        (t (gr-oracle-step (cdr amps) (+ idx 1) mark
             (cons (car amps) acc)))))

(define (gr-oracle amps mark)
  (gr-oracle-step amps 0 mark nil))

(define (gr-reflect amps)
  (let ((m (gr-mean amps)))
    (map (lambda (a) (- (* 2.0 m) a)) amps)))

(define (gr-iterate amps mark)
  (gr-reflect (gr-oracle amps mark)))

(define (gr-repeat amps mark remaining)
  (cond ((<= remaining 0) amps)
        (t (gr-repeat (gr-iterate amps mark) mark (- remaining 1)))))

(define (gr-search N mark iters)
  (gr-repeat (gr-init N) mark iters))

(define (gr-measure amps)
  (gr-best amps 0 0.0 0))

(define (gr-best amps idx best-val best-idx)
  (cond ((eq amps nil) best-idx)
        ((> (abs (car amps)) (abs best-val))
         (gr-best (cdr amps) (+ idx 1) (car amps) idx))
        (t (gr-best (cdr amps) (+ idx 1) best-val best-idx))))

(define (gr-prob amps mark)
  (let ((a (nth mark amps)))
    (* a a)))

; --- demonstration ---
(let ((N 8) (mark 5) (iters 3))
  (let ((amps (gr-search N mark iters)))
    (let ((m (gr-measure amps))
          (p (gr-prob amps mark)))
      (list
        (list (quote items) N)
        (list (quote target) mark)
        (list (quote iterations) iters)
        (list (quote measured) m)
        (list (quote found-target) (= m mark))
        (list (quote prob-of-target) p)))))
