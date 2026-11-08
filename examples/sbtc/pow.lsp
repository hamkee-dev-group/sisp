; ============================================================
; sBTC — proof of work, mining, and block subsidy
;
; Difficulty is expressed as `bits` = the number of leading zero
; bits required in the block hash, i.e. hash < 2^(256-bits).
; Since 256-bit values don't fit in a machine word, the test is
; done word-by-word on the 8-word digest.
; ============================================================

; do the first `k` 32-bit words of the digest equal zero?
(define (all-zero-words words k)
  (cond ((= k 0) t)
        ((= (car words) 0) (all-zero-words (cdr words) (- k 1)))
        (t nil)))

; does this digest have at least `bits` leading zero bits?
(define (hash-meets-p words bits)
  (let ((full (truncate (/ bits 32))) (rem (mod bits 32)))
    (cond ((not (all-zero-words words full)) nil)
          ((= rem 0) t)
          (t (< (nth full words) (pow2 (- 32 rem)))))))

(define (block-pow-ok b) (hash-meets-p (block-hash-words b) (block-bits b)))

; ---- mining -------------------------------------------------
; grind the nonce until the header hash meets its difficulty
(define (mine-from h nonce)
  (let ((hn (hdr-with-nonce h nonce)))
    (cond ((hash-meets-p (sha256d-words (serialize-header hn)) (hdr-bits hn)) hn)
          (t (mine-from h (+ nonce 1))))))

(define (mine-block prev txs ts bits)
  (make-block (mine-from (make-header 1 prev (merkle-root txs) ts bits 0) 0) txs))

; ---- block subsidy (with halving) ---------------------------
(define subsidy-initial 50)
(define halving-interval 20)
(define (block-subsidy height)
  (let ((halvings (truncate (/ height halving-interval))))
    (cond ((>= halvings 6) 0)
          (t (truncate (/ subsidy-initial (pow2 halvings)))))))
