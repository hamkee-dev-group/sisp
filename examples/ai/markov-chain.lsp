; A first-order Markov model learns transition counts and deterministic predictions.
(define (mk-count transitions from to)
  (let ((entry (find (lambda (item)
    (and (equal (car item) from) (equal (cadr item) to))) transitions)))
    (cond ((eq entry nil) 0) (t (caddr entry)))))
(define (mk-add transitions from to)
  (cond ((eq transitions nil) (list (list from to 1)))
        ((and (equal (caar transitions) from) (equal (cadar transitions) to))
         (cons (list from to (+ 1 (caddar transitions))) (cdr transitions)))
        (t (cons (car transitions) (mk-add (cdr transitions) from to)))))
(define (markov-train sequence transitions)
  (cond ((or (eq sequence nil) (eq (cdr sequence) nil)) transitions)
        (t (markov-train (cdr sequence)
             (mk-add transitions (car sequence) (cadr sequence))))))
(define (mk-outgoing transitions state)
  (filter (lambda (entry) (equal (car entry) state)) transitions))
(define (markov-probability transitions from to)
  (let ((outgoing (mk-outgoing transitions from)))
    (/ (mk-count transitions from to)
       (foldl (lambda (sum entry) (+ sum (caddr entry))) 0 outgoing))))
; Generation chooses the maximum-likelihood successor at each step.
(define (markov-next transitions state)
  (cadr (foldl (lambda (best entry)
    (cond ((or (eq best nil) (> (caddr entry) (caddr best))) entry) (t best)))
    nil (mk-outgoing transitions state))))
(define (markov-generate transitions state remaining)
  (cond ((= remaining 0) nil)
        (t (cons state (markov-generate transitions (markov-next transitions state)
                         (- remaining 1))))))
(define mk-weather (markov-train '(sun rain sun rain rain sun rain) nil))
(markov-probability mk-weather 'rain 'sun) ; => 2/3
(markov-generate mk-weather 'sun 4) ; => (sun rain sun rain)

; ---- checks ----
(check "markov-chain: P(rain -> sun) = 2/3"
       (equal (markov-probability mk-weather 'rain 'sun) (/ 2 3)))

(check "markov-chain: generated sequence matches most likely transitions"
       (equal (markov-generate mk-weather 'sun 4) '(sun rain sun rain)))
