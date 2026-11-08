; Code coverage analysis.
; Wrap expressions with counters to track evaluation.
; Statement coverage: which expressions were evaluated?
; Branch coverage: which cond branches were taken?
; Report: coverage percentage, uncovered expressions.

(define *cov-counter* 0)
(define *cov-hits* nil)

(define (cov-reset)
  (setq *cov-counter* 0)
  (setq *cov-hits* nil))

(define (cov-mark id expr)
  (cond ((not (in id *cov-hits*))
         (setq *cov-hits* (cons id *cov-hits*))))
  expr)

(define (cov-fresh-id)
  (setq *cov-counter* (+ *cov-counter* 1))
  *cov-counter*)

(define (cov-instr expr)
  (cond ((atom expr)
          (let ((id (cov-fresh-id)))
            (list 'cov-mark (list 'quote id) expr)))
        ((eq (car expr) 'quote) expr)
        ((eq (car expr) 'cond)
          (let ((id (cov-fresh-id)))
            (list 'cov-mark (list 'quote id)
              (cons 'cond
                (map (lambda (clause)
                       (let ((bid (cov-fresh-id)))
                         (list (list 'cov-mark (list 'quote bid)
                                 (cov-instr (car clause)))
                               (cov-instr (cadr clause)))))
                     (cdr expr))))))
        (t (let ((id (cov-fresh-id)))
             (list 'cov-mark (list 'quote id)
               (cons (car expr) (map cov-instr (cdr expr))))))))

(define (cov-run expr)
  (cov-reset)
  (let ((instr (cov-instr expr)))
    (let ((result (eval instr)))
      (list 'total *cov-counter*
            'covered (length *cov-hits*)
            'pct (cond ((= *cov-counter* 0) 0)
                       (t (* 100 (/ (length *cov-hits*) *cov-counter*))))
            'result result))))

; -- Demo: fact and sign with manual instrumented wrappers --

(define (cov-fact n)
  (cov-mark 1
    (cond ((cov-mark 2 (<= n 1))
           (cov-mark 3 1))
          (t (cov-mark 4
              (* (cov-mark 5 n)
                 (cov-mark 6 (cov-fact (cov-mark 7 (- n 1))))))))))

(cov-reset)
(print (list 'cov-fact-result (cov-fact 5)))
(print (list 'cov-fact-coverage
             'total *cov-counter*
             'covered (length *cov-hits*)))

(define (cov-sign n)
  (cov-mark 1
    (cond ((cov-mark 2 (> n 0)) (cov-mark 3 'positive))
          ((cov-mark 4 (< n 0)) (cov-mark 5 'negative))
          (t (cov-mark 6 'zero)))))

; Branch coverage: accumulate hits across multiple inputs

(cov-reset)
(cov-sign 5)
(define after-pos (cons *cov-counter* *cov-hits*))
(cov-reset)
(cov-sign 0)
(define after-zero (cons *cov-counter* *cov-hits*))
(cov-reset)
(cov-sign -3)
(define after-neg (cons *cov-counter* *cov-hits*))

; Combined: all three runs
; Combined: all three runs
(cov-reset)
(cov-sign 5)
(cov-sign 0)
(cov-sign -3)
; cov-sign has 6 instrumented points (marks 1..6); the combined run
; executes all of them
(print (list 'sign-coverage
             'total 6
             'covered (length *cov-hits*)
             'pct (* 100 (/ (length *cov-hits*) 6))))

(check "all 6 branches covered across the three runs"
       (= (length *cov-hits*) 6))

(check "positive run hits its branch"
       (and (in 2 after-pos) (in 3 after-pos)))

(check "zero run hits its branch"
       (and (in 1 after-zero) (in 4 after-zero) (in 6 after-zero)))

(check "negative run hits its branch"
       (and (in 1 after-neg) (in 2 after-neg) (in 5 after-neg)))

(check "pct reaches 100 when every branch fires"
       (= (* 100 (/ (length *cov-hits*) 6)) 100))
