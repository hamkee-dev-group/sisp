; The HMM forward algorithm sums probabilities of every hidden-state path.
(define (hf-pair-get pairs key)
  (cadr (find (lambda (entry) (equal (car entry) key)) pairs)))
(define (hf-triple-get triples first second)
  (caddr (find (lambda (entry)
    (and (equal (car entry) first) (equal (cadr entry) second))) triples)))
(define (hf-start states initial emissions observation)
  (map (lambda (state) (list state
    (* (hf-pair-get initial state) (hf-triple-get emissions state observation)))) states))
(define (hf-step states previous transitions emissions observation)
  (map (lambda (state) (list state
    (* (hf-triple-get emissions state observation)
       (foldl (lambda (sum prior)
         (+ sum (* (cadr prior) (hf-triple-get transitions (car prior) state))))
         0 previous)))) states))
(define (hf-run states transitions emissions observations alpha)
  (cond ((eq observations nil) alpha)
        (t (hf-run states transitions emissions (cdr observations)
             (hf-step states alpha transitions emissions (car observations))))))
(define (hmm-forward states initial transitions emissions observations)
  (let ((alpha (hf-start states initial emissions (car observations))))
    (foldl (lambda (sum entry) (+ sum (cadr entry))) 0
      (hf-run states transitions emissions (cdr observations) alpha))))
(hmm-forward '(rainy sunny) '((rainy 3/5) (sunny 2/5)) '((rainy rainy 7/10) (rainy sunny 3/10) (sunny rainy 2/5) (sunny sunny 3/5)) '((rainy walk 1/10) (rainy shop 2/5) (sunny walk 3/5) (sunny shop 3/10)) '(walk)) ; => 3/10
(< 0 (hmm-forward '(rainy sunny) '((rainy 3/5) (sunny 2/5)) '((rainy rainy 7/10) (rainy sunny 3/10) (sunny rainy 2/5) (sunny sunny 3/5)) '((rainy walk 1/10) (rainy shop 2/5) (sunny walk 3/5) (sunny shop 3/10)) '(walk shop))) ; => t


; ---- checks ----
(check "hmm-forward: weather walk observation = 3/10"
       (equal (hmm-forward '(rainy sunny) '((rainy 3/5) (sunny 2/5))
               '((rainy rainy 7/10) (rainy sunny 3/10)
                 (sunny rainy 2/5) (sunny sunny 3/5))
               '((rainy walk 1/10) (rainy shop 2/5)
                 (sunny walk 3/5) (sunny shop 3/10))
               '(walk))
              (/ 3 10)))

(check "hmm-forward: two observations are positive"
       (> (hmm-forward '(rainy sunny) '((rainy 3/5) (sunny 2/5))
            '((rainy rainy 7/10) (rainy sunny 3/10)
              (sunny rainy 2/5) (sunny sunny 3/5))
            '((rainy walk 1/10) (rainy shop 2/5)
              (sunny walk 3/5) (sunny shop 3/10))
            '(walk shop))
          0))
