; Property-based testing (QuickCheck-style).
; Generator: (gen-int), (gen-list gen n), (gen-pair ga gb).
; (for-all prop gen n) — run n random trials.
; (for-all prop gen) — keep running until counterexample found.
; Shrinker: given a counterexample, find minimal failing input.
; Demo: test (lambda (xs) (= (length (reverse xs)) (length xs))) for lists up to length 20.

(seed 42)

(define (gen-int)
  (- (random 200) 100))

(define (gen-list gen n)
  (cond ((= n 0) nil)
        (t (cons (gen) (gen-list gen (- n 1))))))

(define (gen-pair ga gb)
  (cons (ga) (gb)))

(define (gen-int-list n)
  (gen-list gen-int n))

; Run n trials, return (t) or (nil counterexample)
(define (pb-trials prop gen n i)
  (cond ((= i n) (list t))
        (t (let ((val (gen)))
             (cond ((prop val) (pb-trials prop gen n (+ i 1)))
                   (t (list nil val)))))))

(define (for-all-n prop gen n)
  (pb-trials prop gen n 0))

(define (for-all prop gen)
  (for-all-n prop gen 100))

; Shrinker: try removing from list or reducing numbers
(define (shrink-list xs)
  (cond ((null xs) nil)
        ((null (cdr xs)) nil)
        (t (cons (car xs) (shrink-list (cdr xs))))))

(define (shrink-int x)
  (cond ((= x 0) nil)
        ((> x 0) (list (truncate (/ x 2)) 0))
        (t (list (truncate (/ x 2)) 0))))

(define (shrink val)
  (cond ((consp val)
          (let ((shrunk-cdr (cond ((null (cdr val)) nil)
                                  (t (list (cdr val))))))
            (let ((shrunk-car (cond ((numberp (car val))
                                     (map (lambda (s) (cons s (cdr val))) (shrink-int (car val))))
                                    (t nil))))
              (append shrunk-cdr shrunk-car))))
        ((numberp val) (shrink-int val))
        (t nil)))

(define (pb-minimize prop shrink-fn val best-attempts)
  (cond ((= best-attempts 0) val)
        (t (let ((candidates (shrink-fn val)))
             (cond ((null candidates) val)
                   (t (let ((found (find (lambda (c) (not (prop c))) candidates)))
                        (cond (found (pb-minimize prop shrink-fn found (- best-attempts 1)))
                              (t val)))))))))

(define (property-find prop gen max-trials)
  (let ((result (pb-trials prop gen max-trials 0)))
    (cond ((car result) (list 'passed max-trials))
          (t (let ((counter (cadr result)))
               (list 'counterexample counter
                     'shrunk (pb-minimize prop shrink counter 10)))))))

; -- Demo: test list reversal preserves length --

(define (reverse-prop xs)
  (= (length (reverse xs)) (length xs)))

(define (random-gen-list)
  (let ((len (random 21)))
    (gen-int-list len)))

; Test with n=500 trials
(print (property-find reverse-prop random-gen-list 500))

; Property that should fail: list reversal preserves first element
(define (bad-reverse-prop xs)
  (cond ((null xs) t)
        (t (equal (car xs) (car (reverse xs))))))

(print (property-find bad-reverse-prop random-gen-list 500))

; Property: double reverse equals identity
(define (double-reverse-prop xs)
  (equal (reverse (reverse xs)) xs))

(print (property-find double-reverse-prop random-gen-list 500))

; Property: append length equals sum of lengths
(define (append-length-prop p)
  (let ((xs (car p)) (ys (cdr p)))
    (= (+ (length xs) (length ys)) (length (append xs ys)))))

(define (gen-pair-of-lists)
  (cons (random-gen-list) (random-gen-list)))

(print (property-find append-length-prop gen-pair-of-lists 500))
