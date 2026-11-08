; ============================================================
; 89 — Mixed-radix Arithmetic (TAOCP Vol 2, 4.1)
; Convert between mixed-radix digit lists (LSD-first) and
; integers. The most significant digit is unbounded. Addition
; with carry propagation. Example: time (weeks, days, hours,
; minutes, seconds) with bases (60, 60, 24, 7) LSD-first.
; Verify round-trip conversions.
; ============================================================

(print "=== Mixed-radix Arithmetic ===")


; digits LSD-first, last digit is unbounded MSD.
; bases: finite bases LSD-first (not including the unbounded one).
; Running multiplier starts at 1 and multiplies by each base after use.
(define (radix-to-int digits bases)
  (labels
    (((walk ds bs multiplier acc)
      (cond ((eq bs nil) (+ acc (* (car ds) multiplier)))
            (t (walk (cdr ds) (cdr bs)
                     (* multiplier (car bs))
                     (+ acc (* (car ds) multiplier)))))))
    (walk digits bases 1 0)))

; Convert integer to mixed-radix digits (LSD-first, MSD unbounded).
(define (int-to-radix value bases)
  (labels
    (((walk v bs)
      (cond ((eq bs nil) (list v))
            (t (cons (mod v (car bs))
                     (walk (truncate (/ v (car bs))) (cdr bs)))))))
    (walk value bases)))

; Add two mixed-radix numbers. Returns digits with carry.
(define (radix-add a b bases)
  (labels
    (((walk xs ys bs carry)
      (cond ((and (eq xs nil) (eq ys nil))
             (cond ((= carry 0) nil) (t (list carry))))
            ((eq bs nil)
             (let ((sum (+ carry
                           (cond ((eq xs nil) 0) (t (car xs)))
                           (cond ((eq ys nil) 0) (t (car ys))))))
               (list sum)))
            (t
             (let ((sum (+ carry
                           (cond ((eq xs nil) 0) (t (car xs)))
                           (cond ((eq ys nil) 0) (t (car ys)))))
                   (base (car bs)))
               (cons (mod sum base)
                     (walk (cond ((eq xs nil) nil) (t (cdr xs)))
                           (cond ((eq ys nil) nil) (t (cdr ys)))
                           (cdr bs)
                           (truncate (/ sum base)))))))))
    (walk a b bases 0)))

; Round-trip check.
(define (roundtrip v bases)
  (= (radix-to-int (int-to-radix v bases) bases) v))

; Time: bases LSD-first = (seconds:60, minutes:60, hours:24, days:7).
(define bases-time (list 60 60 24 7))
(check "roundtrip-0"       (roundtrip 0 bases-time))
(check "roundtrip-1sec"    (roundtrip 1 bases-time))
(check "roundtrip-1min"    (roundtrip 60 bases-time))
(check "roundtrip-1hour"   (roundtrip 3600 bases-time))
(check "roundtrip-1day"    (roundtrip 86400 bases-time))
(check "roundtrip-1week"   (roundtrip 604800 bases-time))
(check "roundtrip-54321"   (roundtrip 54321 bases-time))

(check "roundtrip-battery"
  (every (lambda (v) (roundtrip v bases-time))
    (list 0 1 59 60 61 3599 3600 3601 86399 86400 86401)))

; Time addition: 1w 2d 3h 4m 5s + 4d 5h 6m 7s.
(define sec-v1 (+ 604800 (* 2 86400) (* 3 3600) (* 4 60) 5))
(define sec-v2 (+ (* 4 86400) (* 5 3600) (* 6 60) 7))
(define d1 (int-to-radix sec-v1 bases-time))
(define d2 (int-to-radix sec-v2 bases-time))
(define ds (radix-add d1 d2 bases-time))
(define s1 (radix-to-int d1 bases-time))
(define s2 (radix-to-int d2 bases-time))
(define ss (radix-to-int ds bases-time))
(check "time-roundtrip-v1" (= s1 sec-v1))
(check "time-roundtrip-v2" (= s2 sec-v2))
(check "time-addition"     (= ss (+ s1 s2)))
(print (list 'a-digits d1 'b-digits d2 'sum-digits ds))

; Decimal: bases (10 10 10 10).
(define bases-dec (list 10 10 10 10))
(check "decimal-0"    (roundtrip 0 bases-dec))
(check "decimal-9"    (roundtrip 9 bases-dec))
(check "decimal-42"   (roundtrip 42 bases-dec))
(check "decimal-9999" (roundtrip 9999 bases-dec))

; Factorial number system: bases (1 2 3 4 5).
(define bases-fact (list 1 2 3 4 5))
(define f-digits (int-to-radix 119 bases-fact))
(check "factorial-119" (= (radix-to-int f-digits bases-fact) 119))
(check "factorial-roundtrip"
  (every (lambda (v) (roundtrip v bases-fact))
    (list 0 1 5 23 119)))
(print (list 'factorial-digits-of-119 f-digits))

(print "=== Mixed-radix Arithmetic Done ===")
