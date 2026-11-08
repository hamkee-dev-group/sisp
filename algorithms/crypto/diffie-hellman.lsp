; ============================================================
; name: diffie-hellman
; file: algorithms/crypto/diffie-hellman.lsp
; subject: crypto
; capability: key-exchange public-key
; aliases: "dh" "diffie hellman" "key agreement" "elgamal"
; signature: (dh-public g a p) (dh-shared other-public secret p)
;            (elgamal-keygen x g p) (elgamal-encrypt m k h g p)
;            (elgamal-decrypt ct x p)
; complexity: O(log p) per operation (modpow)
; see-also: rsa shamir-secret-sharing
; derived-from: Diffie & Hellman 1976; ElGamal 1985; the worked
;               example in Wikipedia's Diffie-Hellman article
; status: example (tested, teaching-grade: prime < 2^62)
; checks: 10 checks incl. the Wikipedia vector (shared secret 2);
;         run: src/sisp <this file> from repo root
; ============================================================
; Diffie-Hellman key agreement (1976) and ElGamal encryption (1985).
; Public: prime p, generator g.  Alice: A = g^a mod p.  Bob: B = g^b.
; Shared: s = B^a = A^b = g^(ab) mod p.
; ElGamal: key (x, h = g^x); encrypt m with ephemeral k:
; (c1, c2) = (g^k, m * h^k); decrypt: m = c2 * (c1^x)^-1.

(load "lib/math/number-theory.lsp")

(define (dh-public g a p)
  "your public value g^a mod p"
  (modpow g a p))

(define (dh-shared other-public secret p)
  "the shared secret from the other party's public value"
  (modpow other-public secret p))

(define (elgamal-keygen x g p)
  "public key h = g^x mod p for private key x"
  (dh-public g x p))

(define (elgamal-encrypt m k h g p)
  "ciphertext (c1 c2) using ephemeral key k"
  (list (modpow g k p)
        (mod (* m (modpow h k p)) p)))

(define (elgamal-decrypt ct x p)
  "recover m: c2 * (c1^x)^-1 mod p"
  (let ((s (modpow (car ct) x p)))
    (mod (* (cadr ct) (modinv s p)) p)))

; ---- check helpers (defined before use) ----

(define (elgamal-roundtrip-all ms)
  (cond ((null ms) t)
        (t (let ((key (elgamal-keygen 15 dh-g dh-p)))
             (and (= (elgamal-decrypt
                       (elgamal-encrypt (car ms) 3 key dh-g dh-p) 15 dh-p)
                     (car ms))
                  (elgamal-roundtrip-all (cdr ms)))))))

; ---- checks (Wikipedia's published vector: p=23, g=5, a=6, b=15) ----

(define dh-p 23)
(define dh-g 5)
(define dh-alice-secret 6)
(define dh-bob-secret 15)

(check "dh: Wikipedia public values (A = 8, B = 19)"
       (and (= (dh-public dh-g dh-alice-secret dh-p) 8)
            (= (dh-public dh-g dh-bob-secret dh-p) 19)))

(check "dh: Wikipedia shared secret = 2"
       (and (= (dh-shared 19 dh-alice-secret dh-p) 2)
            (= (dh-shared 8 dh-bob-secret dh-p) 2)))

(check "dh: both sides compute the same secret (fresh secrets)"
       (let* ((sec-a 9)
              (sec-b 4)
              (pub-a (dh-public dh-g sec-a dh-p))
              (pub-b (dh-public dh-g sec-b dh-p)))
         (equal (dh-shared pub-b sec-a dh-p)
                (dh-shared pub-a sec-b dh-p))))

(check "dh: shared = g^(ab) directly"
       (= (dh-shared (dh-public dh-g 6 dh-p) 15 dh-p)
          (modpow dh-g (* 6 15) dh-p)))

; ---- ElGamal (fixed vector: x=15, m=7, k=3 -> (10 12)) ----

(check "elgamal: keygen (h = 19)"
       (= (elgamal-keygen 15 dh-g dh-p) 19))

(check "elgamal: corpus encrypt vector (m=7, k=3) -> (10 12)"
       (equal (elgamal-encrypt 7 3 19 dh-g dh-p) (list 10 12)))

(check "elgamal: decrypt recovers m"
       (= (elgamal-decrypt (elgamal-encrypt 7 3 19 dh-g dh-p) 15 dh-p) 7))

(check "elgamal: roundtrip over many messages (all < p)"
       (elgamal-roundtrip-all (list 0 1 2 5 7 11 13 17 19 22)))

(check "elgamal: multiplicative homomorphism (E(3) * E(5) decrypts to 15)"
       (let* ((key (elgamal-keygen 15 dh-g dh-p))
              (e1 (elgamal-encrypt 3 2 key dh-g dh-p))
              (e2 (elgamal-encrypt 5 3 key dh-g dh-p))
              (combined (list (mod (* (car e1) (car e2)) dh-p)
                              (mod (* (cadr e1) (cadr e2)) dh-p))))
         (= (elgamal-decrypt combined 15 dh-p) (mod (* 3 5) dh-p))))

(check "elgamal: different ephemeral keys give different ciphertexts"
       (not (equal (elgamal-encrypt 7 3 19 dh-g dh-p)
                   (elgamal-encrypt 7 4 19 dh-g dh-p))))
