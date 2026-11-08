; Simple profiler.
; Wrap functions with timing: (profile f name) returns instrumented version.
; Global profile table: ((name . (calls total-ms)) ...).
; (profile-report) — print sorted by total time.
; Demo: profile factorial vs iterative, quicksort vs insertion sort.

(define *profile-table* nil)

(define (prof-reset)
  (setq *profile-table* nil))

(define (prof-record name elapsed)
  (let ((entry (find (lambda (e) (equal (car e) name)) *profile-table*)))
    (cond (entry
            (let ((old (cdr entry)))
              (setq *profile-table*
                (cons (cons name (list (+ (car old) 1) (+ (cadr old) elapsed)))
                      (filter (lambda (e) (not (equal (car e) name))) *profile-table*)))))
          (t (setq *profile-table*
               (cons (cons name (list 1 elapsed)) *profile-table*))))))

(define (profile f name)
  (lambda (x)
    (let ((start (runtime)))
      (let ((result (f x)))
        (prof-record name (* 1000 (- (runtime) start)))
        result))))

(define (profile-report)
  (let ((sorted (sort (lambda (a b) (> (cadr (cdr a)) (cadr (cdr b)))) *profile-table*)))
    (print '---PROFILE-REPORT---)
    (foldl (lambda (acc entry)
             (let ((name (car entry))
                   (calls (cadr entry))
                   (total-ms (cadr (cdr entry))))
               (print (list name 'calls calls 'total-ms total-ms
                            'avg-ms (rationalize (/ total-ms calls)))))
             nil)
           nil sorted)))

; -- Demo functions --

(define (fact-rec n)
  (cond ((<= n 1) 1)
        (t (* n (fact-rec (- n 1))))))

(define (fact-iter n)
  (define (loop i acc)
    (cond ((> i n) acc)
          (t (loop (+ i 1) (* acc i)))))
  (loop 1 1))

(define (quicksort xs)
  (cond ((null xs) nil)
        (t (let ((pivot (car xs)))
             (append (quicksort (filter (lambda (x) (< x pivot)) (cdr xs)))
                     (cons pivot (quicksort (filter (lambda (x) (>= x pivot)) (cdr xs)))))))))

(define (cmp x y) (<= x y))

(define (is-insert x sorted)
  (cond ((null sorted) (list x))
        ((cmp x (car sorted)) (cons x sorted))
        (t (cons (car sorted) (is-insert x (cdr sorted))))))

(define (insertion-sort xs)
  (cond ((null xs) nil)
        (t (is-insert (car xs) (insertion-sort (cdr xs))))))

(define (make-int-list n)
  (cond ((= n 0) nil)
        (t (cons (random 1000) (make-int-list (- n 1))))))

; -- Profile factorial --

(prof-reset)
(seed 42)
(define p-fact-rec (profile fact-rec 'fact-rec))
(define p-fact-iter (profile fact-iter 'fact-iter))

(p-fact-rec 20)
(p-fact-iter 20)
(p-fact-rec 20)
(p-fact-iter 20)

(profile-report)

; -- Profile sorting --

(prof-reset)
(seed 42)
(define p-quicksort (profile quicksort 'quicksort))
(define p-insert (profile insertion-sort 'insertion-sort))

(define small-list (make-int-list 10))
(define big-list (make-int-list 100))

(p-quicksort small-list)
(p-insert small-list)
(p-quicksort big-list)
(p-insert big-list)

(profile-report)
