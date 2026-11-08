; natural-proofs.lsp -- Simulate natural lower bound proof attempt.
; Natural property P: constructive, large, useful against circuits.
; Razborov-Rudich: if P is natural (large=holds for random, useful=no small
; circuit has it, constructive=decidable), then it breaks pseudorandomness.
; Simulation: property "fraction of 1s in truth table between 0.2 and 0.8".
; Simple functions (AND, OR) fail this property (too few or many 1s).
; Random functions pass it. PRFs simulate as random -> natural proofs fail.

(define (np-all-inputs n)
  (cond ((= n 0) (list nil))
        (t (let ((r (np-all-inputs (- n 1))))
             (append (map (lambda (x) (cons 0 x)) r)
                     (map (lambda (x) (cons 1 x)) r))))))

(define (np-two-pow n)
  (cond ((= n 0) 1) (t (* 2 (np-two-pow (- n 1))))))

(define (np-eval-table f n)
  (map f (np-all-inputs n)))

(define (np-one-fraction table)
  (/ (foldl + 0 table) (length table)))

(define (np-rand-table n)
  (let ((sz (np-two-pow n)))
    (np-rand-fill sz)))

(define (np-rand-fill k)
  (cond ((= k 0) nil)
        (t (cons (cond ((< (random) 0.5) 1) (t 0))
                 (np-rand-fill (- k 1))))))

(define (np-and-func n)
  (lambda (x) (cond ((every (lambda (b) (= b 1)) x) 1) (t 0))))

(define (np-or-func n)
  (lambda (x) (cond ((any (lambda (b) (= b 1)) x) 1) (t 0))))

(define (np-parity n)
  (lambda (x) (mod (foldl + 0 x) 2)))

(define (np-majority n)
  (lambda (x) (let ((ones (foldl + 0 x)))
                (cond ((> ones (/ (length x) 2)) 1) (t 0)))))

(define (np-demo n)
  (let ((and-tbl (np-eval-table (np-and-func n) n))
        (or-tbl (np-eval-table (np-or-func n) n))
        (par-tbl (np-eval-table (np-parity n) n))
        (rand-tbl (np-rand-table n)))
    (list
      (list (quote n) n)
      (list (quote AND-frac) (np-one-fraction and-tbl)
            (quote natural) (and (> (np-one-fraction and-tbl) 0.2)
                                (< (np-one-fraction and-tbl) 0.8)))
      (list (quote OR-frac) (np-one-fraction or-tbl)
            (quote natural) (and (> (np-one-fraction or-tbl) 0.2)
                                (< (np-one-fraction or-tbl) 0.8)))
      (list (quote PAR-frac) (np-one-fraction par-tbl)
            (quote natural) (and (> (np-one-fraction par-tbl) 0.2)
                                (< (np-one-fraction par-tbl) 0.8)))
      (list (quote RAND-frac) (np-one-fraction rand-tbl)
            (quote natural) (and (> (np-one-fraction rand-tbl) 0.2)
                                (< (np-one-fraction rand-tbl) 0.8)))
      (list (quote conclusion)
            (quote natural-proofs-fail-vs-random-because-random-passes-property)))))

(list
  (np-demo 2)
  (np-demo 3))
