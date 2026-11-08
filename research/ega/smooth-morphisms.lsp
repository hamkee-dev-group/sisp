; ============================================================
; EGA IV — Smooth Morphisms
; Smooth = flat + geometrically regular fibers.
; Jacobian criterion: F_q[x]/(f) smooth iff gcd(f,f')=1.
; Verified on explicit examples over F_2 and F_7.
; ============================================================

(print "=== EGA: Smooth Morphisms ===")

; ---------- Polynomial evaluation ----------
(define (f-eval coeffs x modq)
  (labels (((go cs pow acc)
    (cond ((eq cs nil) (mod acc modq))
          (t (go (cdr cs) (* pow x) (mod (+ acc (* (car cs) pow)) modq))))))
    (go coeffs 1 0)))

; ---------- F_2 tests ----------
; f = x^3+x+1 over F_2: f' = x^2+1 mod 2. Check common roots.
(check "x3+x+1-over-F2-smooth"
  (let ((f (list 1 1 0 1))
        (fp (list 1 0 1))
        (vals (set-to-list {0 1})))
    (not (any (lambda (x)
      (and (= (f-eval f x 2) 0) (= (f-eval fp x 2) 0))) vals))))

(check "x2+x+1-over-F2-smooth"
  (let ((f (list 1 1 1))
        (fp (list 1 0)))
    (not (any (lambda (x)
      (and (= (f-eval f x 2) 0) (= (f-eval fp x 2) 0)))
      (set-to-list {0 1})))))

(check "x2+1-over-F2-NOT-smooth"
  (let ((f (list 1 0 1))
        (fp (list 0 0)))
    (any (lambda (x)
      (and (= (f-eval f x 2) 0) (= (f-eval fp x 2) 0)))
      (set-to-list {0 1}))))

(check "x3+x-over-F2-NOT-smooth"
  (let ((f (list 0 1 0 1))
        (fp (list 1 0 1)))
    (any (lambda (x)
      (and (= (f-eval f x 2) 0) (= (f-eval fp x 2) 0)))
      (set-to-list {0 1}))))

(check "x4+x+1-over-F2-smooth"
  (let ((f (list 1 1 0 0 1))
        (fp (list 1 0 0 0)))
    (not (any (lambda (x)
      (and (= (f-eval f x 2) 0) (= (f-eval fp x 2) 0)))
      (set-to-list {0 1})))))

(check "x5+x2+1-over-F2-smooth"
  ; f = x^5+x^2+1, f' = 5x^4+2x = x^4 mod 2
  (let ((f (list 1 0 1 0 0 1))
        (fp (list 0 0 0 0 1)))
    (not (any (lambda (x)
      (and (= (f-eval f x 2) 0) (= (f-eval fp x 2) 0)))
      (set-to-list {0 1})))))

; ---------- F_7 tests ----------
(print "--- Smoothness over F_7 ---")

(check "x2+1-over-F7-smooth"
  (let ((f (list 1 0 1))
        (fp (list 0 2)))
    (not (any (lambda (x)
      (and (= (f-eval f x 7) 0) (= (f-eval fp x 7) 0)))
      (set-to-list (seq 0 6))))))

(check "x2-2x+1-over-F7-NOT-smooth"
  (let ((f (list 1 -2 1))
        (fp (list -2 2)))
    (any (lambda (x)
      (and (= (f-eval f x 7) 0) (= (f-eval fp x 7) 0)))
      (set-to-list (seq 0 6)))))

(check "x3-x-over-F7-smooth"
  (let ((f (list 0 -1 0 1))
        (fp (list -1 0 3)))
    (not (any (lambda (x)
      (and (= (f-eval f x 7) 0) (= (f-eval fp x 7) 0)))
      (set-to-list (seq 0 6))))))

(print "--- Smooth = flat + geometrically regular = separable over perfect fields ---")

; ---------- The Jacobian criterion via gcd(f, f') ----------
; F_q[x]/(f) is smooth over F_q iff f is separable iff gcd(f,f') is a unit.
; Polynomials are coefficient lists, low degree first.

(define (fld-inv a modp)
  (labels (((go k)
    (cond ((>= k modp) 0)
          ((= (mod (* a k) modp) 1) k)
          (t (go (+ k 1))))))
    (go 1)))

; strip leading (high-degree) zero coefficients and reduce mod p
(define (poly-norm cs modp)
  (labels (((trim l)
    (cond ((endp l) nil)
          (t (let ((rest (trim (cdr l))))
               (cond ((and (= (car l) 0) (endp rest)) nil)
                     (t (cons (car l) rest))))))))
    (trim (map (lambda (c) (mod c modp)) cs))))

(define (poly-deg a) (- (length a) 1))          ; the zero polynomial gets -1
(define (poly-lead a) (last a))

; derivative, computed independently of the fp lists used above
(define (poly-deriv a modp)
  (labels (((go l k)
    (cond ((endp l) nil)
          (t (cons (* k (car l)) (go (cdr l) (+ k 1)))))))
    (cond ((endp a) nil)
          (t (poly-norm (go (cdr a) 1) modp)))))

(define (poly-shift a k)
  (cond ((= k 0) a) (t (poly-shift (cons 0 a) (- k 1)))))

(define (poly-sub a b modp)
  (poly-norm
    (labels (((go x y)
      (cond ((and (endp x) (endp y)) nil)
            ((endp x) (cons (- 0 (car y)) (go nil (cdr y))))
            ((endp y) (cons (car x) (go (cdr x) nil)))
            (t (cons (- (car x) (car y)) (go (cdr x) (cdr y)))))))
      (go a b))
    modp))

; one long-division step: a - (lead(a)/lead(b))·x^(deg a - deg b)·b
(define (poly-rem a b modp)
  (cond ((endp b) a)
        ((< (poly-deg a) (poly-deg b)) a)
        (t (let ((sc (mod (* (poly-lead a) (fld-inv (poly-lead b) modp)) modp)))
             (poly-rem
               (poly-sub a
                 (poly-shift (map (lambda (c) (* sc c)) b)
                             (- (poly-deg a) (poly-deg b)))
                 modp)
               b modp)))))

(define (poly-gcd a b modp)
  (cond ((endp b) a)
        (t (poly-gcd b (poly-rem a b modp) modp))))

; f is separable iff gcd(f,f') has degree 0 (a nonzero constant)
(define (gcd-smooth-p cs modp)
  (let ((g (poly-gcd (poly-norm cs modp) (poly-deriv cs modp) modp)))
    (= (poly-deg g) 0)))

; the root-based test used above: no common root of f and f' in F_q
(define (root-smooth-p cs modp)
  (let ((dd (poly-deriv cs modp)))
    (not (any (lambda (x)
      (and (= (f-eval cs x modp) 0) (= (f-eval dd x modp) 0)))
      (set-to-list (seq 0 (- modp 1)))))))

; every polynomial tested above, with the verdict its check name asserts
(define smoothness-cases
  (list (list (list 1 1 0 1)     2 t)      ; x^3+x+1   smooth
        (list (list 1 1 1)       2 t)      ; x^2+x+1   smooth
        (list (list 1 0 1)       2 nil)    ; x^2+1     NOT smooth
        (list (list 0 1 0 1)     2 nil)    ; x^3+x     NOT smooth
        (list (list 1 1 0 0 1)   2 t)      ; x^4+x+1   smooth
        (list (list 1 0 1 0 0 1) 2 t)      ; x^5+x^2+1 smooth
        (list (list 1 0 1)       7 t)      ; x^2+1     smooth
        (list (list 1 -2 1)      7 nil)    ; x^2-2x+1  NOT smooth
        (list (list 0 -1 0 1)    7 t)))    ; x^3-x     smooth

(check "smooth-equiv-gcd-1"
  (every (lambda (c)
    (let ((cs (car c)) (modp (cadr c)) (want (caddr c)))
      (and (eq (gcd-smooth-p cs modp) want)
           (eq (root-smooth-p cs modp) want))))
    smoothness-cases))

(print "=== Smooth Morphisms Done ===")
