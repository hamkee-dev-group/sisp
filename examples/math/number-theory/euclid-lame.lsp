; ============================================================
; 75 — Euclid's Algorithm and Lamé's Theorem (TAOCP Vol 2, 4.5.3)
; The oldest nontrivial algorithm, analyzed the Knuth way: step
; counts over ALL input pairs up to 233 show the worst case is
; achieved exactly by consecutive Fibonacci numbers; Lamé's
; digit bound holds everywhere; the extended algorithm's Bezout
; certificates are verified; and gcd(F(m),F(n)) = F(gcd(m,n)).
; ============================================================

(print "=== Euclid and Lame ===")


; ---------- the algorithm, instrumented ----------
; division steps until the remainder vanishes; a >= b >= 1.
(define (euclid-steps a b n)
  (cond ((= b 0) n)
        (t (euclid-steps b (mod a b) (+ n 1)))))

(define (euclid-gcd a b)
  (cond ((= b 0) a) (t (euclid-gcd b (mod a b)))))
(check "euclid-agrees-with-builtin"
  (every (lambda (a)
    (every (lambda (b) (= (euclid-gcd a b) (gcd a b)))
      (list 12 35 64 81 1)))
    (list 18 30 77 100 233)))

; ---------- the worst case, by exhaustion ----------
; over every pair 1 <= b < a <= 233, the maximum number of steps
; is reached ONLY at (233, 144) — consecutive Fibonaccis.
(define fibs (list 1 2 3 5 8 13 21 34 55 89 144 233))

(define *worst-steps* 0)
(define *worst-pairs* nil)
(define (consider a b)
  (let ((s (euclid-steps a b 0)))
    (cond ((> s *worst-steps*)
      (setq *worst-steps* s)
      (setq *worst-pairs* (list (list a b))))
      ((= s *worst-steps*)
        (setq *worst-pairs* (cons (list a b) *worst-pairs*)))
      (t nil))))
(define (scan-b a b)
  (cond ((= b a) nil)
        (t (consider a b) (scan-b a (+ b 1)))))
(define (scan-a a)
  (cond ((> a 233) nil)
        (t (scan-b a 1) (scan-a (+ a 1)))))
(scan-a 2)

(print (list (quote worst-steps) *worst-steps*
             (quote worst-pairs) *worst-pairs*))
(check "unique-worst-pair-is-fibonacci"
  (equal *worst-pairs* (list (list 233 144))))
(check "consecutive-fibonaccis-are-hardest"
  (and (contains 233 fibs) (contains 144 fibs)))
; Euclid on consecutive Fibonaccis produces quotient 1 at every
; step but the last — the slowest possible descent.
(define (euclid-quotients a b)
  (cond ((= b 0) nil)
        (t (cons (truncate (/ a b)) (euclid-quotients b (mod a b))))))
(print (list (quote fibonacci-quotients) (euclid-quotients 233 144)))
(check "fibonacci-quotients-are-all-ones-then-two"
  (equal (euclid-quotients 233 144)
         (list 1 1 1 1 1 1 1 1 1 1 2)))

; ---------- Lame's theorem ----------
; the number of steps is at most five times the number of decimal
; digits of the smaller argument — verified over the whole range.
(define (digit-count n)
  (cond ((< n 10) 1) (t (+ 1 (digit-count (truncate (/ n 10)))))))
(define *lame-violations* 0)
(define (lame-b a b)
  (cond ((= b a) nil)
    (t (cond ((> (euclid-steps a b 0) (* 5 (digit-count b)))
        (setq *lame-violations* (+ *lame-violations* 1))) (t nil))
      (lame-b a (+ b 1)))))
(define (lame-a a)
  (cond ((> a 233) nil)
        (t (lame-b a 1) (lame-a (+ a 1)))))
(lame-a 2)
(check "lame-bound-holds-for-all-26796-pairs" (= *lame-violations* 0))

; ---------- the extended algorithm: Bezout certificates ----------
; returns (g x y) with a*x + b*y = g; the certificate makes the
; gcd VERIFIABLE without trusting the computation.
(define (ext-euclid a b)
  (cond ((= b 0) (list a 1 0))
    (t (let ((r (ext-euclid b (mod a b))))
      (list (car r) (caddr r)
        (- (cadr r) (* (truncate (/ a b)) (caddr r))))))))

(define (bezout-ok a b)
  (let ((r (ext-euclid a b)))
    (and (= (car r) (gcd a b))
         (= (+ (* a (cadr r)) (* b (caddr r))) (car r)))))
(check "bezout-certificates-verify"
  (every (lambda (a)
    (every (lambda (b) (bezout-ok a b)) (seq 1 60)))
    (seq 1 60)))
(print (list (quote bezout-233-144) (ext-euclid 233 144)))
(check "fibonacci-bezout-uses-fibonacci-coefficients"
  (equal (ext-euclid 233 144) (list 1 -55 89)))

; ---------- gcd on Fibonacci numbers is Fibonacci on gcd ----------
(define (fib n)
  (cond ((= n 1) 1) ((= n 2) 1)
        (t (+ (fib (- n 1)) (fib (- n 2))))))
(check "gcd-of-fibs-is-fib-of-gcd"
  (every (lambda (m)
    (every (lambda (n)
      (= (gcd (fib m) (fib n)) (fib (gcd m n))))
      (seq 2 18)))
    (seq 2 18)))

(print "=== Euclid and Lame Done ===")
