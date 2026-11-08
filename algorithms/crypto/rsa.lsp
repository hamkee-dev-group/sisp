; ============================================================
; name: rsa
; file: algorithms/crypto/rsa.lsp
; subject: crypto
; capability: public-key encryption signature
; aliases: "rsa" "rivest shamir adleman" "public key"
; signature: (rsa-keygen p q e) -> (n e d)
;            (rsa-encrypt m n e) (rsa-decrypt c n d)
;            (rsa-sign m n d) (rsa-verify m sig n e)
; complexity: O(log e) encrypt, O(log d) decrypt (modpow)
; see-also: diffie-hellman shamir-secret-sharing
; derived-from: Rivest, Shamir & Adleman 1978; the worked example in
;               Wikipedia's RSA article
; status: example (tested, teaching-grade: no bignums means real key
;         sizes are impossible; safe for moduli < 2^62)
; checks: 10 checks incl. the Wikipedia vector (65 -> 2790);
;         run: src/sisp <this file> from repo root
; ============================================================
; RSA (Rivest-Shamir-Adleman 1977), teaching-grade.
; Key: n = pq, e coprime with phi(n), d = e^-1 mod phi(n).
; Encrypt: c = m^e mod n.  Decrypt: m = c^d mod n.
; Sign: s = m^d mod n.  Verify: m = s^e mod n.

(load "lib/math/number-theory.lsp")

(define (rsa-keygen p q e)
  "public/private key (n e d) from primes p q and public exponent e;
   nil when e is not invertible mod phi(n)"
  (let* ((n (* p q))
         (phi (totient n))
         (d (modinv e phi)))
    (cond ((eq d nil) nil)
          (t (list n e d)))))

(define (rsa-encrypt m n e) (modpow m e n))
(define (rsa-decrypt c n d) (modpow c d n))
(define (rsa-sign m n d) (modpow m d n))
(define (rsa-verify m sig n e) (equal m (modpow sig e n)))

; ---- check helpers (defined before use) ----

(define (rsa-roundtrip-all ms)
  (cond ((null ms) t)
        (t (and (= (rsa-decrypt (rsa-encrypt (car ms) rsa-wiki-n rsa-wiki-e)
                                 rsa-wiki-n rsa-wiki-d)
                   (car ms))
                (rsa-roundtrip-all (cdr ms))))))

; ---- checks (Wikipedia's worked example: p=61, q=53, e=17, d=2753) ----

(define rsa-wiki-n 3233)
(define rsa-wiki-e 17)
(define rsa-wiki-d 2753)

(check "rsa: Wikipedia key derivation (d = 2753)"
       (equal (rsa-keygen 61 53 17) (list 3233 17 2753)))

(check "rsa: Wikipedia encrypt 65 -> 2790"
       (= (rsa-encrypt 65 rsa-wiki-n rsa-wiki-e) 2790))

(check "rsa: Wikipedia decrypt 2790 -> 65"
       (= (rsa-decrypt 2790 rsa-wiki-n rsa-wiki-d) 65))

(check "rsa: ed = 1 mod phi(n)"
       (= (mod (* rsa-wiki-e rsa-wiki-d) (totient rsa-wiki-n)) 1))

(check "rsa: roundtrip over many messages"
       (rsa-roundtrip-all (list 1 2 42 100 500 1000 2000 3000 3232)))

(check "rsa: sign then verify"
       (rsa-verify 42 (rsa-sign 42 rsa-wiki-n rsa-wiki-d)
                   rsa-wiki-n rsa-wiki-e))

(check "rsa: Wikipedia signature vector (42 -> 3065)"
       (= (rsa-sign 42 rsa-wiki-n rsa-wiki-d) 3065))

(check "rsa: verify rejects a tampered message"
       (not (rsa-verify 43 (rsa-sign 42 rsa-wiki-n rsa-wiki-d)
                        rsa-wiki-n rsa-wiki-e)))

(check "rsa: keygen with non-coprime e -> nil"
       (eq (rsa-keygen 61 53 8) nil))

(check "rsa: fresh keygen roundtrips (e = 17, coprime to phi)"
       (let ((key (rsa-keygen 101 113 17)))
         (and (not (eq key nil))
              (= (rsa-decrypt (rsa-encrypt 1234 (car key) (cadr key))
                              (car key) (caddr key))
                 1234))))
