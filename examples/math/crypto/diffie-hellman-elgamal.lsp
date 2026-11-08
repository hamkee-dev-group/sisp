; ============================================================
; name: diffie-hellman-elgamal
; subject: crypto
; capability: key-exchange public-key
; status: healthy
; canonical-home: algorithms/crypto/diffie-hellman.lsp
; notes: Wikipedia DH vector; homomorphic ElGamal check.
; ============================================================
; ============================================================
; Diffie-Hellman Key Exchange and ElGamal Encryption
; p=23, g=5. Alice a=6, Bob b=15. Shared secret.
; ElGamal encryption using the same parameters.
; ============================================================

(print "=== Diffie-Hellman and ElGamal ===")

(define (ck name ok)
  (cond (ok (print (list 'pass name)))
        (t (print (list 'FAIL name)))))

(define (modpow base exp modu)
  (labels
    (((walk b e r)
      (cond ((= e 0) r)
            ((= (mod e 2) 1)
             (walk (mod (* b b) modu) (truncate (/ e 2)) (mod (* r b) modu)))
            (t
             (walk (mod (* b b) modu) (truncate (/ e 2)) r)))))
    (walk base exp 1)))

(define modinv
  (lambda (a p) (modpow (mod a p) (- p 2) p)))

(define dh-p 23)
(define dh-g 5)
(define dh-a 6)
(define dh-b 15)

(define A (modpow dh-g dh-a dh-p))
(define B (modpow dh-g dh-b dh-p))

(define shared-alice (modpow B dh-a dh-p))
(define shared-bob   (modpow A dh-b dh-p))

(print "p =") (print dh-p)
(print "g =") (print dh-g)
(print "Alice a =") (print dh-a)
(print "Bob b =") (print dh-b)
(print "A = g^a mod p =") (print A)
(print "B = g^b mod p =") (print B)
(print "Alice shared = B^a mod p =") (print shared-alice)
(print "Bob shared = A^b mod p =") (print shared-bob)
(ck "shared-agrees" (= shared-alice shared-bob))
(ck "shared-equals-direct" (= shared-alice (modpow dh-g (* dh-a dh-b) dh-p)))

; --- ElGamal Encryption ---
(define elg-p dh-p)
(define elg-g dh-g)
(define elg-x dh-b)
(define elg-h (modpow elg-g elg-x elg-p))

(define (elgamal-encrypt m k)
  (let* ((c1 (modpow elg-g k elg-p))
         (hk (modpow elg-h k elg-p))
         (c2 (mod (* m hk) elg-p)))
    (list c1 c2)))

(define (elgamal-decrypt ct)
  (let* ((c1 (car ct))
         (c2 (cadr ct))
         (c1x (modpow c1 elg-x elg-p))
         (c1xinv (modinv c1x elg-p))
         (msg (mod (* c2 c1xinv) elg-p)))
    msg))

(print "--- ElGamal Encryption ---")
(print "h = g^x mod p =") (print elg-h)

(define test-msg 7)
(define test-k 3)
(define elg-ct (elgamal-encrypt test-msg test-k))
(define elg-dec (elgamal-decrypt elg-ct))
(print "Encrypt m =") (print test-msg)
(print "Ciphertext =") (print elg-ct)
(print "Decrypted =") (print elg-dec)
(ck "elgamal-roundtrip" (= elg-dec test-msg))

(define (test-elgamal msgs ks)
  (cond ((eq msgs nil) t)
        ((= (elgamal-decrypt (elgamal-encrypt (car msgs) (car ks))) (car msgs))
         (test-elgamal (cdr msgs) (cdr ks)))
        (t nil)))

(define elg-test-msgs '(1 2 3 4 5 10 15 20 22))
(define elg-test-ks   '(4 5 1 3 7 2 6 8 9))
(ck "elgamal-multiple" (test-elgamal elg-test-msgs elg-test-ks))

; Verify homomorphic property: E(m1)*E(m2) = E(m1*m2)
(define m1 3)
(define m2 5)
(define ct1 (elgamal-encrypt m1 2))
(define ct2 (elgamal-encrypt m2 3))
(define ct-prod (list (mod (* (car ct1) (car ct2)) elg-p)
                      (mod (* (cadr ct1) (cadr ct2)) elg-p)))
(define dec-prod (elgamal-decrypt ct-prod))
(ck "homomorphic-mult" (= dec-prod (mod (* m1 m2) elg-p)))

(print "=== Diffie-Hellman and ElGamal Done ===")
