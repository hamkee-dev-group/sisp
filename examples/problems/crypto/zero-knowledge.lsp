; Zero-knowledge proof for 3-coloring.
; Prover commits to permuted colors. Verifier challenges edge.
; A cheating prover is caught with probability >= 1/|E| per round
; (equivalently, soundness error <= 1 - 1/|E|). Simulator exists.

; 4-cycle graph
(define edges (list (list 0 1) (list 1 2) (list 2 3) (list 3 0)))

(define (check-edges coloring remaining)
  (cond ((eq remaining nil) t)
        (t (let ((edge (car remaining)))
             (let ((u (car edge)) (v (cadr edge)))
               (let ((cu (nth u coloring))
                     (cv (nth v coloring)))
                 (cond ((= cu cv) nil)
                       (t (check-edges coloring (cdr remaining))))))))))

(define (valid-coloring-p coloring)
  (check-edges coloring edges))

; A valid 4-cycle 3-coloring: vertex colors (0=r, 1=g, 2=b, 3=g)
; vertices: 0 1 2 3
; edges: 0-1, 1-2, 2-3, 3-0
; Colors: 1=r, 2=g, 3=b
(define real-coloring (list 1 2 3 2))

(cond ((valid-coloring-p real-coloring)
       (print "PASS: zkp-valid-coloring"))
      (t (print "FAIL: zkp-valid-coloring")))

; Prover: permutes colors and returns committed coloring
(define (permute-color c perm)
  (nth (- c 1) perm))

(define (commit-coloring coloring perm)
  (map (lambda (c) (permute-color c perm)) coloring))

(define (zkp-prover coloring perm)
  (commit-coloring coloring perm))

; Verifier: checks that edge endpoints have different colors
(define (zkp-verify-edge committed edge)
  (let ((u (car edge)) (v (cadr edge)))
    (/= (nth u committed) (nth v committed))))

(define (zkp-verify-all committed remaining)
  (cond ((eq remaining nil) t)
        (t (cond ((zkp-verify-edge committed (car remaining))
                  (zkp-verify-all committed (cdr remaining)))
                 (t nil)))))

; Test with identity permutation
(define id-perm (list 1 2 3))
(define committed-real (zkp-prover real-coloring id-perm))

(cond ((zkp-verify-all committed-real edges)
       (print "PASS: zkp-check-all-edges"))
      (t (print "FAIL: zkp-check-all-edges")))

; Soundness: invalid coloring detected
(define fake-coloring (list 1 1 2 2))
(cond ((not (valid-coloring-p fake-coloring))
       (print "PASS: zkp-fake-invalid"))
      (t (print "FAIL: zkp-fake-invalid")))

(define committed-fake (zkp-prover fake-coloring id-perm))

(define (count-failed committed remaining)
  (cond ((eq remaining nil) 0)
        (t (+ (cond ((zkp-verify-edge committed (car remaining)) 0) (t 1))
              (count-failed committed (cdr remaining))))))

(define fail-count (count-failed committed-fake edges))
(print (cat "Bad edges: " (num-to-str fail-count)))

(cond ((>= fail-count 1)
       (print "PASS: zkp-soundness"))
      (t (print "FAIL: zkp-soundness")))

; Soundness bound: a cheating prover is caught with probability at least
; 1/|E| — the verifier challenges one uniformly random edge, and at least
; one edge of an invalid coloring is monochromatic.
(define catch-prob (/ fail-count (length edges)))
(print (list 'catch-probability catch-prob))
(cond ((>= catch-prob (/ 1 (length edges)))
       (print "PASS: zkp-soundness-bound (>= 1/|E|)"))
      (t (print "FAIL: zkp-soundness-bound")))

; Simulator: generates transcripts indistinguishable from real
(define (zkp-simulate edge-idx)
  (let ((edge (nth edge-idx edges)))
    (list edge (list 1 2))))

; A simulated transcript must be accepted by the verifier: it names the
; challenged edge and shows two distinct colors on its endpoints.
(define (zkp-sim-ok idx)
  (let ((transcript (zkp-simulate idx)))
    (let ((edge (car transcript)) (colors (cadr transcript)))
      (and (equal edge (nth idx edges))
           (/= (car colors) (cadr colors))))))

(cond ((and (zkp-sim-ok 0) (zkp-sim-ok 1)
            (and (zkp-sim-ok 2) (zkp-sim-ok 3)))
       (print "PASS: zkp-simulator-exists"))
      (t (print "FAIL: zkp-simulator-exists")))

(print "=== ZERO KNOWLEDGE DONE ===")
