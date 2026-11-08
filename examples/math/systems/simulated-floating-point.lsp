; ============================================================
; 90 — Simulated Floating Point (TAOCP Vol 2, 4.2)
; Tiny IEEE-style float: 1 sign bit, 3 exponent bits (bias=3),
; 2 mantissa bits — 6 bits total, 64 patterns.
; Normalize, add, multiply with round-to-nearest-even.
; Exhaustively verify all 64x64 pairs for add/mul commutativity.
; ============================================================

(print "=== Simulated Floating Point ===")


; ---------- 6-bit float: sign(1) exp(3 bias=3) mantissa(2) ----------
(define bias 3)

(define (unpack raw)
  (let ((s (truncate (/ raw 32)))
        (e (mod (truncate (/ raw 4)) 8))
        (m (mod raw 4)))
    (list s e m)))

(define (is-zero flt) (and (= (cadr flt) 0) (= (caddr flt) 0)))
(define zero (list 0 0 0))
(define (is-inf flt)  (and (= (cadr flt) 7) (= (caddr flt) 0)))
(define (is-nan flt)  (and (= (cadr flt) 7) (> (caddr flt) 0)))
(define (is-norm flt) (and (>= (cadr flt) 1) (<= (cadr flt) 6)))
(define (is-sub flt)  (and (= (cadr flt) 0) (> (caddr flt) 0)))

(define (ext-mant flt)
  (cond ((is-norm flt) (+ 4 (caddr flt)))
        (t (caddr flt))))

(define (adj-exp flt)
  (cond ((is-sub flt) 1)
        (t (cadr flt))))

(define (round2 m3)
  (let ((guard (mod m3 2)) (r (truncate (/ m3 2))))
    (cond ((= guard 0) r)
          ((= (mod r 2) 1) (+ r 1))
          (t r))))

(define (normalize sig exp)
  (cond ((>= sig 8)  (normalize (truncate (/ sig 2)) (+ exp 1)))
        ((< sig 4)   (cond ((> exp 0) (normalize (* sig 2) (- exp 1)))
                           (t (list sig exp))))
        (t (list sig exp))))

(define (encode sign sig exp)
  (cond ((<= sig 0) (list 0 0 0))
        (t
         (let ((normed (normalize sig exp)))
           (let ((mn (car normed)) (en (cadr normed)))
             (cond ((<= en 0)
                    (cond ((>= en -2)
                           (let ((sd (truncate (/ mn (^ 2 (+ 1 (- en)))))))
                             (cond ((>= sd 4) (list sign 4 0))
                                   (t (list sign 0 sd)))))
                          (t (list sign 0 0))))
                   ((>= en 7) (list sign 7 0))
                   (t
                    (let ((mb (round2 (- mn 4))))
                      (cond ((= mb 4)
                             (cond ((>= (+ en 1) 7) (list sign 7 0))
                                   (t (list sign (+ en 1) 0))))
                            (t (list sign en mb)))))))))))

(define (fadd a b)
  (cond ((is-nan a) a) ((is-nan b) b)
        ((is-inf a)
         (cond ((is-inf b)
                (cond ((= (car a) (car b)) a) (t (list 0 7 1))))
               (t a)))
        ((is-inf b) b)
        ((is-zero a) (cond ((is-zero b) zero) (t b)))
        ((is-zero b) a)
        (t
         (let ((sa (car a)) (sb (car b))
               (ma (ext-mant a)) (mb (ext-mant b))
               (ea (adj-exp a)) (eb (adj-exp b)))
           (cond ((> ea eb) (fadd-align sa ma ea sb mb eb))
                 (t (fadd-align sb mb eb sa ma ea)))))))

(define (fadd-align s1 m1 e1 s2 m2 e2)
  (let ((m2s (truncate (/ m2 (^ 2 (- e1 e2))))))
    (cond ((= s1 s2) (encode s1 (+ m1 m2s) e1))
          ((>= m1 m2s) (encode s1 (- m1 m2s) e1))
          (t (encode s2 (- m2s m1) e1)))))

(define (fmul a b)
  (cond ((is-nan a) a) ((is-nan b) b)
        ((is-zero a) (list (mod (+ (car a) (car b)) 2) 0 0))
        ((is-zero b) (list (mod (+ (car a) (car b)) 2) 0 0))
        ((is-inf a)
         (cond ((is-zero b) (list 0 7 1))
               (t (list (mod (+ (car a) (car b)) 2) 7 0))))
        ((is-inf b)
         (cond ((is-zero a) (list 0 7 1))
               (t (list (mod (+ (car a) (car b)) 2) 7 0))))
        (t
         (let ((ma (ext-mant a)) (mb (ext-mant b))
               (sign (mod (+ (car a) (car b)) 2))
               (exp (+ (adj-exp a) (adj-exp b) (- bias))))
           (encode sign (* ma mb) exp)))))

; ---------- Value (for sign checks) ----------
(define (fval flt)
  (let ((s (car flt)) (e (cadr flt)) (m (caddr flt)))
    (cond ((is-zero flt) 0)
          ((is-inf flt) (cond ((= s 0) 999) (t -999)))
          ((is-nan flt) 0)
          ((is-norm flt)
           (* (cond ((= s 0) 1) (t -1))
              (/ (+ 4 m) 4)
              (^ 2 (- e bias))))
          ((is-sub flt)
           (* (cond ((= s 0) 1) (t -1))
              (/ m 4)
              (/ 1 4))))))

; ---------- Generate all 64 patterns ----------
(define (enumerate lo hi)
  (cond ((> lo hi) nil)
        (t (cons lo (enumerate (+ lo 1) hi)))))

(define all-raw (enumerate 0 63))

; ---------- Tests ----------
; raw values: raw = s*32 + e*4 + m
; 0=(0 0 0) 4=(0 1 0) 8=(0 2 0) 12=(0 3 0)=one  16=(0 4 0)=two
; 20=(0 5 0) 24=(0 6 0) 28=(0 7 0)=+inf
; 32=(1 0 0) 36=(1 1 0) 40=(1 2 0) 44=(1 3 0) 48=(1 4 0) 52=(1 5 0) 56=(1 6 0) 60=(1 7 0)=-inf

(define one (unpack 12))
(define two (unpack 16))

(check "one-plus-one-is-two"
  (equal (fadd one one) two))

(check "zero-add-identity"
  (let ((r (fadd one zero)))
    (or (equal r one) (and (is-zero r) (is-zero one)))))

(check "zero-mul-annihilates"
  (is-zero (fmul one (list 0 0 0))))

(check "add-commutative-sample"
  (let ((samps (map unpack (list 0 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))))
    (every (lambda (a)
      (every (lambda (b)
        (cond ((or (is-nan a) (is-nan b)) t)
              (t (equal (fadd a b) (fadd b a)))))
        samps))
      samps)))

(check "mul-commutative-sample"
  (let ((samps (map unpack (list 0 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))))
    (every (lambda (a)
      (every (lambda (b)
        (cond ((or (is-nan a) (is-nan b)) t)
              (t (equal (fmul a b) (fmul b a)))))
        samps))
      samps)))

; ---------- Exhaustive verification ----------
(check "exhaustive-add-commutative"
  (every (lambda (i)
    (every (lambda (j)
      (let ((a (unpack i)) (b (unpack j)))
        (cond ((or (is-nan a) (is-nan b)) t)
              (t (equal (fadd a b) (fadd b a))))))
      all-raw))
    all-raw))

(check "exhaustive-mul-commutative"
  (every (lambda (i)
    (every (lambda (j)
      (let ((a (unpack i)) (b (unpack j)))
        (cond ((or (is-nan a) (is-nan b)) t)
              (t (equal (fmul a b) (fmul b a))))))
      all-raw))
    all-raw))

(check "exhaustive-zero-add-identity"
  (every (lambda (i)
    (let ((a (unpack i)) (r (fadd (unpack i) zero)))
      (or (equal r a) (and (is-zero r) (is-zero a)))))
    all-raw))

(print "=== Simulated Floating Point Done ===")
