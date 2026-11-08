; ============================================================
; name: shamir-secret-sharing
; file: algorithms/crypto/shamir-secret-sharing.lsp
; subject: crypto
; capability: secret-sharing threshold
; aliases: "shamir" "sss" "threshold scheme"
; signature: (shamir-shares coeffs n p) -> ((x y) ...)
;            (shamir-recover shares k p) -> secret
; complexity: O(k^2) recovery (Lagrange at 0)
; see-also: rsa diffie-hellman
; derived-from: Shamir 1979; the finite-field example in Wikipedia's
;               Shamir's Secret Sharing article
; status: example (tested, teaching-grade: prime field < 2^62)
; checks: 10 checks incl. any-k-of-n recovery and k-1 insufficiency;
;         run: src/sisp <this file> from repo root
; ============================================================
; Shamir's (k, n) threshold scheme (1979): the secret is the constant
; term of a random degree k-1 polynomial over GF(p); each share is a
; point (x, y) with x != 0.  Any k shares recover the secret by
; Lagrange interpolation at x = 0; k-1 shares reveal nothing
; (information-theoretically, since every constant term fits).

(load "lib/math/number-theory.lsp")

(define (sss-eval coeffs x p)
  "evaluate the polynomial at x in GF(p) (Horner)"
  (sss-horner (reverse coeffs) x p 0))

(define (sss-horner rc x p acc)
  (cond ((null rc) acc)
        (t (sss-horner (cdr rc) x p
                       (mod (+ (mod (* acc x) p) (car rc)) p)))))

(define (shamir-shares coeffs n p)
  "shares (x_i, y_i) for i = 1..n; coeffs = (secret a1 ... a_{k-1})"
  (map (lambda (x) (list x (sss-eval coeffs x p)))
       (seq 1 n)))

; Lagrange interpolation at x = 0 over the given shares
(define (sss-lagrange0 shares p)
  (sss-sum shares shares p 0))

(define (sss-sum shares all p acc)
  (cond ((null shares) acc)
        (t (sss-sum (cdr shares) all p
                    (mod (+ acc
                            (* (cadar shares)
                               (sss-basis0 (caar shares)
                                           (sss-others (caar shares) all)
                                           p)))
                         p)))))

(define (sss-others x all)
  (cond ((null all) nil)
        ((= (caar all) x) (sss-others x (cdr all)))
        (t (cons (caar all) (sss-others x (cdr all))))))

; product of (-xj) / (xi - xj) over the other shares, in GF(p)
(define (sss-basis0 xi others p)
  (cond ((null others) 1)
        (t (let* ((xj (car others))
                  (num (mod (- 0 xj) p))
                  (den (mod (- xi xj) p))
                  (inv (modinv den p)))
               (mod (* (sss-basis0 xi (cdr others) p)
                       (mod (* num inv) p))
                    p)))))

(define (shamir-recover shares k p)
  "the secret, from any k shares"
  (sss-lagrange0 shares p))

; ---- check helpers (defined before use) ----

(define (sss-all-on-curve shares)
  (cond ((null shares) t)
        (t (and (= (sss-eval sss-coeffs (caar shares) sss-p)
                   (cadr (car shares)))
                (sss-all-on-curve (cdr shares))))))

; ---- checks (p=1613, secret 1234, coefficients 166 and 94) ----

(define sss-p 1613)
(define sss-coeffs (list 1234 166 94))
(define sss-all (shamir-shares sss-coeffs 5 sss-p))

(check "shamir: known share values (verified by execution)"
       (and (= (cadr (nth 0 sss-all)) 1494)
            (= (cadr (nth 1 sss-all)) 329)
            (= (cadr (nth 2 sss-all)) 965)))

(check "shamir: recover from shares 1,2,3"
       (= (shamir-recover (list (nth 0 sss-all) (nth 1 sss-all) (nth 2 sss-all))
                          3 sss-p)
          1234))

(check "shamir: recover from shares 1,3,5"
       (= (shamir-recover (list (nth 0 sss-all) (nth 2 sss-all) (nth 4 sss-all))
                          3 sss-p)
          1234))

(check "shamir: recover from shares 2,4,5"
       (= (shamir-recover (list (nth 1 sss-all) (nth 3 sss-all) (nth 4 sss-all))
                          3 sss-p)
          1234))

(check "shamir: recover from all 5 shares"
       (= (shamir-recover sss-all 3 sss-p) 1234))

(check "shamir: 2 of 3 shares do NOT recover (wrong value)"
       (not (= (shamir-recover (list (nth 0 sss-all) (nth 1 sss-all)) 3 sss-p)
               1234)))

(check "shamir: every share lies on the polynomial"
       (sss-all-on-curve sss-all))

(check "shamir: degree-1 scheme (line): 2 of 4 recover"
       (let ((sh (shamir-shares (list 42 7) 4 sss-p)))
         (and (= (shamir-recover (list (nth 0 sh) (nth 3 sh)) 2 sss-p) 42)
              (= (shamir-recover (list (nth 1 sh) (nth 2 sh)) 2 sss-p) 42))))

(check "shamir: degree-3 scheme: any 4 of 5 recover"
       (let ((sh (shamir-shares (list 7 1 2 3) 5 sss-p)))
         (= (shamir-recover (list (nth 0 sh) (nth 2 sh) (nth 3 sh) (nth 4 sh))
                            4 sss-p)
            7)))

(check "shamir: different polynomial, different secret, roundtrip"
       (let ((sh (shamir-shares (list 999 5 5) 4 sss-p)))
         (= (shamir-recover (list (nth 0 sh) (nth 1 sh) (nth 2 sh)) 3 sss-p)
            999)))
