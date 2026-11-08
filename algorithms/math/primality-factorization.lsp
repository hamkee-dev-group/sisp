; ============================================================
; name: primality-factorization
; file: algorithms/math/primality-factorization.lsp
; subject: number-theory
; capability: primality factorize
; aliases: "miller-rabin" "pollard rho" "primality test" "integer factorization"
; signature: (primep-mr n) -> t/nil ; (pollard-rho n) -> factor
; complexity: Miller-Rabin O(k log^3 n); Pollard rho ~O(n^1/4)
; see-also: groebner-basis
; derived-from: Miller 1976; Rabin 1980; Pollard 1975
; status: example (tested)
; checks: 8 - deterministic MR {2,3,5,7} vs trial division; Pollard 8051=83*97
; ============================================================
; Primality and Factorization
; Trial division up to sqrt(n). Miller-Rabin {2,3,5,7} for n<2^31.
; Exhaustive verification up to 1000. Pollard rho: 8051 = 83*97.
; ============================================================

(print "=== Primality and Factorization ===")


; --- Trial division primality ---
(define (primep n)
  (cond ((<= n 1) nil)
        ((= n 2) t)
        ((= (mod n 2) 0) nil)
        (t (trial-div n 3))))

(define (trial-div n d)
  (cond ((> (* d d) n) t)
        ((= (mod n d) 0) nil)
        (t (trial-div n (+ d 2)))))

; --- Modular exponentiation ---
(define (modpow base exp modu)
  (modpow-iter base exp modu 1))

(define (modpow-iter base exp modu result)
  (cond ((= exp 0) result)
        ((= (mod exp 2) 0)
         (modpow-iter (mod (* base base) modu) (truncate (/ exp 2)) modu result))
        (t (modpow-iter base (- exp 1) modu (mod (* result base) modu)))))

; --- Miller-Rabin ---
(define (mr-test n a)
  (cond ((<= n 1) nil)
        ((= n 2) t)
        ((= (mod n 2) 0) nil)
        (t (let* ((decomp (factor2 (- n 1)))
                  (d (car decomp))
                  (s (cadr decomp)))
          (mr-witness n a d s)))))

(define (factor2 n)
  (factor2-iter n 0))

(define (factor2-iter n ss)
  (cond ((= (mod n 2) 1) (list n ss))
        (t (factor2-iter (truncate (/ n 2)) (+ ss 1)))))

(define (mr-witness n a d s)
  (let ((x (modpow a d n)))
    (cond ((= x 1) t)
          ((= x (- n 1)) t)
          (t (mr-check-loop n x s)))))

(define (mr-check-loop n x remaining-s)
  (cond ((<= remaining-s 1) nil)
        (t (let ((nx (mod (* x x) n)))
          (cond ((= nx (- n 1)) t)
                (t (mr-check-loop n nx (- remaining-s 1))))))))

; Deterministic MR with witnesses {2,3,5,7}
(define (primep-mr n)
  (cond ((<= n 1) nil)
        ((= n 2) t)
        ((= n 3) t)
        ((= n 5) t)
        ((= n 7) t)
        ((= (mod n 2) 0) nil)
        (t (and (mr-test n 2)
                (mr-test n 3)
                (mr-test n 5)
                (mr-test n 7)))))

; --- Exhaustive test up to 1000 ---
(print "--- Exhaustive test up to 1000 ---")

(define (verify-up-to limit)
  (verify-iter 2 limit 0))

(define (verify-iter n limit errors)
  (cond ((> n limit) errors)
        (t (let ((td (primep n)) (mr (primep-mr n)))
          (cond ((equal td mr)
                 (verify-iter (+ n 1) limit errors))
                (t (verify-iter (+ n 1) limit (+ errors 1))))))))

(define errors (verify-up-to 1000))
(print "Mismatches trial-div vs MR up to 1000:")
(print errors)
(check "mr-matches-td-up-to-1000" (= errors 0))

; --- Spot-check known primes/composites ---
(print "--- Spot checks ---")
(define known-primes '(7919 9829 104729 99991))
(print "Primes:") (print known-primes)

(define (all-primep-mr nums)
  (cond ((equal nums nil) t)
        ((primep-mr (car nums)) (all-primep-mr (cdr nums)))
        (t nil)))

(define (top k lst) (take lst k))
; print some results
(print "7919-MR:") (print (primep-mr 7919))
(print "9829-MR:") (print (primep-mr 9829))

(check "known-primes-pass" (all-primep-mr known-primes))

(define known-comps '(7917 9828 100000))
(check "known-composites-fail"
  (not (any primep-mr known-comps)))

; --- Pollard rho factorization of 8051 ---
(print "--- Pollard rho: factor 8051 ---")

(define (my-abs x)
  (cond ((< x 0) (- x)) (t x)))

(define (pollard-rho n)
  (rho-iter n 2 2 1 0))

(define (rho-iter n x y d steps)
  (cond ((> steps 200) nil)
        ((and (> d 1) (< d n)) d)
        (t (let* ((nx (mod (+ (* x x) 1) n))
                  (ny1 (mod (+ (* y y) 1) n))
                  (ny2 (mod (+ (* ny1 ny1) 1) n))
                  (nd (gcd (my-abs (- nx ny2)) n)))
            (rho-iter n nx ny2 nd (+ steps 1))))))

(define target 8051)
(define fact (pollard-rho target))
(print "Factor found:")
(print fact)
(check "pollard-rho-works" (not (equal fact nil)))

(define other-fact (truncate (/ target fact)))
(print "Other factor:")
(print other-fact)
(print "Product check:")
(print (* fact other-fact))

(check "8051=83*97" (and (= (* fact other-fact) target)
                           (or (and (= fact 83) (= other-fact 97))
                               (and (= fact 97) (= other-fact 83)))))

(check "83-is-prime" (primep 83))
(check "97-is-prime" (primep 97))

; Trial division factorization
(print "--- Trial division factorization of 8051 ---")
(define (trial-factor n)
  (trial-factor-iter n 2))

(define (trial-factor-iter n d)
  (cond ((> (* d d) n) (list n))
        ((= (mod n d) 0)
         (cons d (trial-factor-iter (truncate (/ n d)) d)))
        (t (trial-factor-iter n (+ d (if (= d 2) 1 2))))))

(define tfact (trial-factor target))
(print "Trial division factors of 8051:")
(print tfact)
(check "trial-div-factors-match" (equal tfact '(83 97)))

(print "=== Primality and Factorization Done ===")
