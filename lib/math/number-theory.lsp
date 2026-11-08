; ============================================================
; lib/math/number-theory.lsp -- modular arithmetic and number theory
;
; Public:   (modpow base exp modu) -> integer
;           (modinv a modu) -> integer | nil
;           (nt-egcd a b) -> (g x y) where ax + by = g = gcd(a,b), for a >= 0
;           (primes-up-to n) -> list of primes <= n
;           (prime-p n) -> t | nil
;           (factorize n) -> alist of (prime . exponent) pairs
;           (totient n) -> Euler's totient phi(n)
; Private:  nt-mulmod, nt-sieve, nt-remove-multiples, nt-contains,
;           nt-trial-division, nt-factor, nt-factor-extract,
;           nt-exact-pow, nt-totient
; Depends:  none (uses builtins)
; Derived-from: examples/math/number-theory/sieve-eratosthenes.lsp (sieve)
; Assumes:  modu > 0 for modpow/modinv; modu < 2^62 = 4611686018427387904
;           (nt-mulmod keeps every intermediate sum below 2^63 only when
;           modu < 2^62); prime-p uses trial division (deterministic for
;           the whole 64-bit range); factorize/totient assume n >= 1;
;           nt-egcd assumes a >= 0 -- see the note on it below
; ============================================================

; (a * b) mod modu by Russian-peasant doubling.  Every intermediate is
; reduced mod modu, so the largest sum ever formed is below 2*modu; this
; never overflows a signed 64-bit word while modu < 2^62.
(define (nt-mulmod a b modu)
  (labels (((walk x y r)
             (cond ((= y 0) r)
                   ((= (mod y 2) 1)
                    (walk (mod (+ x x) modu) (truncate (/ y 2)) (mod (+ r x) modu)))
                   (t (walk (mod (+ x x) modu) (truncate (/ y 2)) r)))))
    (walk a b 0)))

(define (modpow base exp modu)
  (labels (((walk b e r)
             (cond ((= e 0) r)
                   ((= e 1) (nt-mulmod r b modu))
                   ((= (mod e 2) 1)
                    (walk (nt-mulmod b b modu) (truncate (/ e 2))
                          (nt-mulmod r b modu)))
                   (t (walk (nt-mulmod b b modu) (truncate (/ e 2)) r)))))
    (walk (mod base modu) exp 1)))

; Extended Euclid, for a >= 0 only.  The base case returns (|a| 1 0), so a
; negative a would report g = |a| with x = 1 -- and a*1 = a, not |a|, i.e. the
; Bezout identity would not hold.  Narrowing the contract rather than handling
; it: the only caller is modinv, which passes (mod a modu) and so is already
; non-negative, and no caller in the tree wants the negative case.
(define (nt-egcd a b)
  (cond ((<= b 0) (list (abs a) 1 0))
        (t (let* ((rec (nt-egcd b (mod a b))) (g (car rec))
                  (x (cadr rec)) (y (caddr rec)))
             (list g y (- x (* (truncate (/ a b)) y)))))))

(define (modinv a modu)
  (let* ((result (nt-egcd (mod a modu) modu)) (g (car result)) (x (cadr result)))
    (cond ((= g 1) (mod (+ (mod x modu) modu) modu)) (t nil))))

; ---- primality and factorization ----

(define (primes-up-to n)
  (cond ((< n 2) nil)
        (t (nt-sieve (seq 2 n) 2 (truncate (sqrt n)) n))))

(define (nt-sieve candidates p limit maxn)
  (cond ((> p limit) candidates)
        ((nt-contains p candidates)
         (nt-sieve (nt-remove-multiples candidates p maxn) (+ p 1) limit maxn))
        (t (nt-sieve candidates (+ p 1) limit maxn))))

(define (nt-remove-multiples lst p maxn)
  (cond ((null lst) nil)
        ((and (> (car lst) p) (= (mod (car lst) p) 0))
         (nt-remove-multiples (cdr lst) p maxn))
        (t (cons (car lst) (nt-remove-multiples (cdr lst) p maxn)))))

(define (nt-contains x lst)
  (cond ((null lst) nil) ((= x (car lst)) t) (t (nt-contains x (cdr lst)))))

(define (prime-p n)
  (cond ((< n 2) nil) ((<= n 3) t) ((= (mod n 2) 0) nil) ((= (mod n 3) 0) nil)
        (t (nt-trial-division n 5))))

(define (nt-trial-division n d)
  (cond ((> (* d d) n) t) ((= (mod n d) 0) nil)
        ((= (mod n (+ d 2)) 0) nil) (t (nt-trial-division n (+ d 6)))))

(define (factorize n)
  (nt-factor n 2 nil))

(define (nt-factor n d acc)
  (cond ((= n 1) (reverse acc))
        ((> (* d d) n) (reverse (cons (cons n 1) acc)))
        ((= (mod n d) 0) (nt-factor-extract n d 0 acc))
        (t (nt-factor n (+ d (cond ((= d 2) 1) (t 2))) acc))))

(define (nt-factor-extract n d count acc)
  (cond ((= (mod n d) 0) (nt-factor-extract (truncate (/ n d)) d (+ count 1) acc))
        (t (nt-factor n (+ d (cond ((= d 2) 1) (t 2))) (cons (cons d count) acc)))))

(define (totient n)
  (cond ((= n 1) 1) (t (nt-totient (factorize n)))))

; p^e by exact repeated multiplication.  Callers guarantee p^e divides
; the original n, so every intermediate power is <= n and cannot overflow.
(define (nt-exact-pow p e)
  (cond ((= e 0) 1) (t (* p (nt-exact-pow p (- e 1))))))

; phi(p^e) = (p - 1) * p^(e-1); totient is multiplicative over the factorization
(define (nt-totient factors)
  (cond ((null factors) 1)
        (t (let ((p (caar factors)) (e (cdar factors)))
             (* (* (- p 1) (nt-exact-pow p (- e 1)))
                (nt-totient (cdr factors)))))))
