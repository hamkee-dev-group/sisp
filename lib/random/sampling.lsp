; ============================================================
; lib/random/sampling.lsp -- shuffles and random sampling
;
; Public:   (shuffle xs) -> random permutation of xs
;           (reservoir-sample xs k) -> list of k random elements
; Private:  rs-list-set, rs-swap, rs-fy-loop, rs-reservoir-walk
; Depends:  none (uses builtins random, seed, nth)
; Derived-from: none (written fresh from CLRS 5.3 and Knuth Algorithm R;
;               the uniformity-test approach follows
;               examples/math/random/random-sampling.lsp)
; Assumes:  shuffle returns a new list (input is not modified);
;           reservoir-sample preserves no order guarantee; k <= length
;           is required (checked, else error); randomness comes from the
;           interpreter's xoshiro256** -- (seed n) gives deterministic
;           streams for reproducible tests
; ============================================================

; functional list update: xs with position i replaced by v
(define (rs-list-set xs i v)
  (cond ((= i 0) (cons v (cdr xs)))
        (t (cons (car xs) (rs-list-set (cdr xs) (- i 1) v)))))

(define (rs-swap xs i j)
  (let ((xi (nth i xs)) (xj (nth j xs)))
    (rs-list-set (rs-list-set xs j xi) i xj)))

; for i = n-1 down to 1: j = random(i+1); swap a[i] a[j]
(define (rs-fy-loop xs i)
  (cond ((= i 0) xs)
        (t (rs-fy-loop (rs-swap xs i (random (+ i 1))) (- i 1)))))

(define (shuffle xs)
  "uniformly random permutation of xs (Fisher-Yates), as a new list"
  (cond ((null xs) nil)
        (t (rs-fy-loop xs (- (length xs) 1)))))

; Algorithm R: keep the first k, then replace with probability k/i
(define (rs-reservoir-walk xs i k acc)
  (cond ((null xs) acc)
        (t (let ((j (random (+ i 1))))
             (cond ((< j k)
                    (rs-reservoir-walk (cdr xs) (+ i 1) k
                                       (rs-list-set acc j (car xs))))
                   (t (rs-reservoir-walk (cdr xs) (+ i 1) k acc)))))))

(define (reservoir-sample xs k)
  "uniformly random sample of k elements of xs, without replacement"
  (cond ((> k (length xs))
         (error "RESERVOIR-SAMPLE: K EXCEEDS LENGTH" k))
        (t (rs-reservoir-walk (drop xs k) k k (take xs k)))))
