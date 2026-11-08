; ============================================================
; sBTC — ECDSA over the sBTC curve
;
; The real ECDSA algorithm; only the field/key sizes are small.
; Keys derive deterministically from a seed string, and the
; per-signature nonce is derived like RFC 6979 (k = H(priv|msg)),
; so no secure RNG is required and a key never reuses a nonce
; across two different messages.
; ============================================================

; fold a digest (list of 32-bit words) into an integer mod m,
; reducing as we go so nothing exceeds 2^63.
(define (words-mod ws m)
  (foldl (lambda (a w) (mod (+ (* a 4294967296) w) m)) 0 ws))

; digest of a byte list / string -> 8 words (SHA-256d)
(define (msg-digest s) (sha256d-words (str->bytes s)))
(define (bytes-digest bl) (sha256d-words bl))

; ---- keys ---------------------------------------------------
; deterministic private key in [1, n-1] from a seed string
(define (priv-from-seed s)
  (+ 1 (words-mod (msg-digest s) (- ec-n 1))))
(define (pub-of priv) (pt-mul priv ec-g))
(define (make-key seed) (list (priv-from-seed seed) (pub-of (priv-from-seed seed))))
(define (key-priv k) (car k))
(define (key-pub k) (cadr k))

; ---- deterministic nonce ------------------------------------
(define (ecdsa-nonce priv digest)
  (+ 1 (words-mod (sha256d-words (append (u64-be priv) (words->bytes digest)))
                  (- ec-n 1))))

; ---- sign ---------------------------------------------------
; digest is a list of 8 words; returns signature (r s)
(define (ecdsa-sign digest priv)
  (sign-loop digest priv (ecdsa-nonce priv digest)))

(define (sign-loop digest priv k)
  (let ((kk (+ 1 (mod (- k 1) (- ec-n 1)))))
    (let ((R (pt-mul kk ec-g)))
      (cond ((eq R nil) (sign-loop digest priv (+ kk 1)))
            (t (let ((r (mod (car R) ec-n)) (e (words-mod digest ec-n)))
                 (cond ((= r 0) (sign-loop digest priv (+ kk 1)))
                       (t (let ((s (mod (* (modinv kk ec-n) (+ e (* r priv))) ec-n)))
                            (cond ((= s 0) (sign-loop digest priv (+ kk 1)))
                                  (t (list r s))))))))))))

; ---- verify -------------------------------------------------
(define (ecdsa-verify digest pub sig)
  (let ((r (car sig)) (s (cadr sig)))
    (cond ((or (< r 1) (> r (- ec-n 1)) (< s 1) (> s (- ec-n 1))) nil)
          ((eq pub nil) nil)
          ((not (on-curve-p pub)) nil)
          (t (let* ((e (words-mod digest ec-n))
                    (w (modinv s ec-n))
                    (u1 (mod (* e w) ec-n))
                    (u2 (mod (* r w) ec-n))
                    (x (pt-add (pt-mul u1 ec-g) (pt-mul u2 pub))))
               (cond ((eq x nil) nil)
                     (t (= (mod (car x) ec-n) r))))))))
