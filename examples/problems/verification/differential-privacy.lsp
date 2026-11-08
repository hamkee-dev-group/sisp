; Laplace mechanism. M(D)=f(D)+Lap(Delta_f/eps). Epsilon budget. Composition: eps_total=eps1+eps2.
(print "=== Differential Privacy: Laplace Mechanism ===")
(seed 42)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

; --- Laplace distribution sampling ---
(define (laplace-sample b)
  (let ((u (max 0.000001 (min 0.999999 (random)))))
    (let ((shifted (- u 0.5)))
      (cond ((> shifted 0) (* (- 0.0 b) (log (- 1.0 (* 2.0 shifted)))))
            ((< shifted 0) (* b (log (+ 1.0 (* 2.0 shifted)))))
            (t 0.0)))))

; --- Counting query ---
(define (count-where db pred)
  (count-where-aux db pred 0))

(define (count-where-aux db pred acc)
  (cond ((null db) acc)
        ((pred (car db)) (count-where-aux (cdr db) pred (+ acc 1)))
        (t (count-where-aux (cdr db) pred acc))))

(define (laplace-mechanism db pred epsilon)
  (let* ((true-count (count-where db pred))
         (sensitivity 1.0)
         (b (/ sensitivity epsilon)))
    (+ true-count (laplace-sample b))))

; --- Test database: 20 rows, ~50% age > 50 ---
(define (mk-db n)
  (map (lambda (i) (mod (+ (* i 7) 13) 100)) (seq 1 n)))

(setq *db* (mk-db 20))
(define (age-gt-50-p age) (> age 50))

(setq *true-count* (count-where *db* age-gt-50-p))
(print (list 'true-count *true-count*))

; --- Privacy loss (epsilon budget) ---
(define eps1 0.5)
(define eps2 0.3)
(define eps-total (+ eps1 eps2))

(define (run-queries database)
  (list (laplace-mechanism database age-gt-50-p eps1)
        (laplace-mechanism database (lambda (age) (> age 75)) eps2)
        eps-total))

(define results (run-queries *db*))
(define noisy-count1 (car results))
(define noisy-count2 (cadr results))
(define total-eps (caddr results))

(print (list 'query1-noisy noisy-count1 'eps1 eps1))
(print (list 'query2-noisy noisy-count2 'eps2 eps2))
(print (list 'total-eps-budget total-eps))

(ck "eps-budget-tracking" (= total-eps (+ eps1 eps2)))

; --- Higher epsilon means a smaller Laplace noise scale ---
(define (laplace-scale epsilon) (/ 1.0 epsilon))
(define low-epsilon-scale (laplace-scale 0.1))
(define high-epsilon-scale (laplace-scale 2.0))
(print (list 'noise-scale-low-epsilon low-epsilon-scale
             'noise-scale-high-epsilon high-epsilon-scale))
(ck "higher-eps-less-noise" (< high-epsilon-scale low-epsilon-scale))

; --- Privacy budget exhaustion ---
(define (track-budget queries budget)
  (track-budget-aux queries budget))

(define (track-budget-aux queries remain)
  (cond ((null queries) (list t remain))
        ((< remain (car queries)) (list nil remain))
        (t (track-budget-aux (cdr queries) (- remain (car queries))))))

(define budget-result (track-budget (list 0.3 0.2 0.4) 1.0))
(print (list 'budget-remaining budget-result))
(ck "budget-not-exceeded" (car budget-result))

(define budget-exceeded (track-budget (list 0.6 0.6) 1.0))
(print (list 'budget-exceeded budget-exceeded))
(ck "budget-case-exceeded" (not (car budget-exceeded)))

(print "=== Differential Privacy Done ===")
