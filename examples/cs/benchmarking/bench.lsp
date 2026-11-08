; Empirical algorithm analysis.
; (time fn input n) — run fn on input n times, return mean ms.
; (growth fn start end step) — measure fn for input sizes start..end.
; (classify times) — heuristic: O(1), O(log n), O(n), O(n log n), O(n^2)?
; ASCII plot: runtime vs input size.
; Demo: classify quicksort vs bubblesort.

(define (lg2 x) (/ (log x) (log 2)))

; ---- Timing ----

(define (time-fn fn input n)
  (let ((start (runtime)))
    (time-loop fn input n start 0)))

(define (time-loop fn input n start i)
  (cond ((< i n) (progn (fn input) (time-loop fn input n start (+ i 1))))
        (t (* 1000 (- (runtime) start)))))

(define (time-ms fn input)
  (/ (time-fn fn input 100) 100))

; ---- Input generation ----

(define (int-list n)
  (cond ((= n 0) nil)
        (t (cons (random 1000) (int-list (- n 1))))))

; ---- Benchmarking ----

(define (growth measure-fn start end step)
  (cond ((> start end) nil)
        (t (let ((input (int-list start)))
             (let ((ms (time-ms measure-fn input)))
               (cons (list start ms)
                     (growth measure-fn (+ start step) end step)))))))

; ---- Sorting algorithms ----

(define (quicksort xs)
  (cond ((null xs) nil)
        (t (let ((pivot (car xs)))
             (append (quicksort (filter (lambda (x) (< x pivot)) (cdr xs)))
                     (cons pivot (quicksort (filter (lambda (x) (>= x pivot)) (cdr xs)))))))))

(define (bs-pass ys swapped)
  (cond ((null (cdr ys)) (list ys swapped))
        (t (let ((a (car ys)) (b (cadr ys)))
             (cond ((> a b)
                     (let ((rest (bs-pass (cons a (cddr ys)) t)))
                       (list (cons b (car rest)) (cadr rest))))
                   (t (let ((rest (bs-pass (cdr ys) swapped)))
                        (list (cons a (car rest)) (cadr rest)))))))))

(define (bubblesort xs)
  (cond ((<= (length xs) 1) xs)
        (t (let ((result (bs-pass xs nil)))
             (cond ((cadr result) (bubblesort (car result)))
                   (t (car result)))))))

; ---- Classification ----

(define (range start end)
  (cond ((> start end) nil)
        (t (cons start (range (+ start 1) end)))))

(define (classify measurements)
  (let* ((sizes (map car measurements))
         (times (map cadr measurements))
         (n (length sizes)))
    (cond ((< n 3) 'unknown)
          (t
            (let* ((t-ratios (map (lambda (i)
                                    (cond ((= (nth (- i 1) times) 0) 0)
                                          (t (/ (nth i times) (nth (- i 1) times)))))
                                  (range 1 (- n 1))))
                   (s-ratios (map (lambda (i)
                                    (cond ((= (nth (- i 1) sizes) 0) 0)
                                          (t (/ (nth i sizes) (nth (- i 1) sizes)))))
                                  (range 1 (- n 1))))
                   (avg-t-ratio (/ (foldl + 0 t-ratios) (length t-ratios)))
                   (avg-s-ratio (/ (foldl + 0 s-ratios) (length s-ratios))))
              (cond ((< avg-t-ratio 1.3) 'constant)
                    ((< avg-t-ratio (* avg-s-ratio 0.5)) 'logarithmic)
                    ((< avg-t-ratio (* avg-s-ratio 1.3)) 'linear)
                    ((< avg-t-ratio (* avg-s-ratio (lg2 avg-s-ratio)))
                     'n-log-n)
                    (t 'quadratic)))))))

; ---- ASCII Plot ----

(define (bar-chars n)
  (cond ((= n 0) "")
        (t (cat "#" (bar-chars (- n 1))))))

(define (bar-plot measurements)
  (let* ((times (map cadr measurements))
         (biggest (foldl max 0 times))
         (scale (cond ((= biggest 0) 1) (t (/ biggest 20)))))
    (foldl (lambda (acc m)
             (let* ((size (car m))
                    (ms (cadr m))
                    (bars (cond ((= scale 0) 0) (t (truncate (/ ms scale))))))
               (append acc (list (list 'n size 'ms ms 'bar (bar-chars bars))))))
           nil measurements)))

; ---- Demo ----

(seed 42)
(define qs-data (growth quicksort 10 100 10))
(print (list 'quicksort-class (classify qs-data)))
(print (bar-plot qs-data))

(seed 42)
(define bs-data (growth bubblesort 10 100 10))
(print (list 'bubblesort-class (classify bs-data)))
(print (bar-plot bs-data))
