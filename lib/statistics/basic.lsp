; ============================================================
; lib/statistics/basic.lsp -- descriptive statistics
;
; Public:   (mean xs) -> number | nil
;           (variance xs) -> sample variance (n-1 divisor) | nil
;           (pvariance xs) -> population variance (n divisor) | nil
;           (stddev xs) -> sample standard deviation | nil
;           (median xs) -> number | nil
;           (quantile xs p) -> number | nil  (p in [0 1])
;           (covariance xs ys) -> sample covariance | nil
;           (correlation xs ys) -> Pearson r | nil
; Private:  stats-var, stats-sorted-mid, stats-interp, stats-cov-walk
; Depends:  lib/math/sorting.lsp
; Derived-from: none (written fresh from standard definitions; expected
;               values in tests computed by CPython's statistics module)
; Assumes:  non-empty numeric lists; mean/median/quantile return nil on
;           empty input; variance/stddev/covariance return nil when the
;           sample size makes them undefined (< 2 elements, unequal
;           lengths); quantile uses the linear-interpolation method
;           (position = p*(n-1), numpy's default); correlation is nil
;           when either side has zero variance
;
; ============================================================
(load "lib/math/sorting.lsp")

(define (mean xs)
  "arithmetic mean of xs, or nil when empty"
  (cond ((null xs) nil)
        (t (/ (foldl + 0 xs) (length xs)))))

(define (stats-var xs n divisor)
  (let ((m (mean xs)))
    (/ (foldl (lambda (acc x) (+ acc (* (- x m) (- x m)))) 0 xs)
       divisor)))

(define (variance xs)
  "sample variance (n-1 divisor), or nil when fewer than 2 elements"
  (cond ((or (null xs) (null (cdr xs))) nil)
        (t (stats-var xs (length xs) (- (length xs) 1)))))

(define (pvariance xs)
  "population variance (n divisor), or nil when empty"
  (cond ((null xs) nil)
        (t (stats-var xs (length xs) (length xs)))))

(define (stddev xs)
  "sample standard deviation, or nil when fewer than 2 elements"
  (let ((v (variance xs)))
    (cond ((eq v nil) nil)
          (t (sqrt v)))))

; ---- order statistics ----

(define (stats-sorted-mid xs n)
  (let ((s (merge-sort (lambda (a b) (< a b)) xs)))
    (cond ((= (mod n 2) 1) (nth (truncate (/ n 2)) s))
          (t (/ (+ (nth (- (truncate (/ n 2)) 1) s)
                   (nth (truncate (/ n 2)) s))
                2)))))

(define (median xs)
  "middle value, or mean of the two middle values; nil when empty"
  (cond ((null xs) nil)
        (t (stats-sorted-mid xs (length xs)))))

(define (stats-interp s lo frac)
  (cond ((null (cdr s)) (car s))
        (t (+ (car s) (* frac (- (cadr s) (car s)))))))

(define (quantile xs p)
  "p-quantile by linear interpolation on position p*(n-1); nil when empty"
  (cond ((null xs) nil)
        (t (let* ((s (merge-sort (lambda (a b) (< a b)) xs))
                  (pos (* p (- (length xs) 1)))
                  (lo (floor pos)))
             (stats-interp (drop s lo) lo (- pos lo))))))

; ---- covariance and correlation ----

(define (stats-cov-walk xs ys mx my acc)
  (cond ((null xs) acc)
        (t (stats-cov-walk (cdr xs) (cdr ys) mx my
                           (+ acc (* (- (car xs) mx) (- (car ys) my)))))))

(define (covariance xs ys)
  "sample covariance of paired samples, or nil when undefined"
  (cond ((or (null xs) (null (cdr xs)) (not (= (length xs) (length ys)))) nil)
        (t (/ (stats-cov-walk xs ys (mean xs) (mean ys) 0)
              (- (length xs) 1)))))

(define (correlation xs ys)
  "Pearson product-moment correlation; nil when undefined"
  (let ((sx (stddev xs)) (sy (stddev ys)))
    (cond ((or (eq sx nil) (eq sy nil) (= sx 0) (= sy 0)) nil)
          (t (/ (covariance xs ys) (* sx sy))))))
