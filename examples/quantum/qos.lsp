; ============================================================
; A tiny "quantum OS" over the QVM
;
; Provides what an OS around a quantum device would: allocate a
; register, run a program, read the state out, and perform a
; probabilistic measurement that collapses superposition (real
; sampling from |amplitude|^2, via the RNG).
; ============================================================

; ---- pretty printing of basis states -----------------------
; bit string, qubit n-1 on the left ... qubit 0 on the right
(define (bits-str i n) (bits-acc i (- n 1) ""))
(define (bits-acc i q acc)
  (cond ((< q 0) acc)
        (t (bits-acc i (- q 1) (cat acc (num-to-str (basis-bit i q)))))))
(define (ket i n) (cat (cat "|" (bits-str i n)) ">"))

; ---- show the state (nonzero amplitudes only) ---------------
(define (show-state st n)
  (show-amps st 0 (qpow2 n) n))
(define (show-amps st i d n)
  (cond ((>= i d) nil)
        (t (cond ((> (c-abs2 (vget st i)) 0.000001)
                  (print (cat (cat "    " (ket i n))
                              (cat "  amp=" (cat (amp-str (vget st i))
                                   (cat "  p=" (num-to-str (c-abs2 (vget st i)))))))))
                 (t nil))
           (show-amps st (+ i 1) d n))))
(define (amp-str z)
  (cat (cat "(" (cat (num-to-str (c-re z)) (cat " " (cat (num-to-str (c-im z)) "i")))) ")"))

; ---- measurement (collapse by sampling |amp|^2) ------------
(define meas-res 1000000)
(define (sample-state st n)
  (sample-walk (probs st) 0 0.0 (/ (random meas-res) (* 1.0 meas-res))))
(define (sample-walk ps i acc r)
  (cond ((eq (cdr ps) nil) i)
        ((< r (+ acc (car ps))) i)
        (t (sample-walk (cdr ps) (+ i 1) (+ acc (car ps)) r))))

; ---- job report: what an OS would show for a run -----------
(define (qos-report label st n)
  (print (cat "  [" (cat label "]")))
  (let ((ml (most-likely st n)))
    (print (cat "    most likely:  " (cat (ket ml n) (cat "   p=" (num-to-str (nth ml (probs st)))))))
    (print (cat "    measured:     " (ket (sample-state st n) n)))
    (print (cat "    norm (unitary check): " (num-to-str (norm st))))))
