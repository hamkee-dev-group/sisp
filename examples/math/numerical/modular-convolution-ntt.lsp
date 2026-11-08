; ============================================================
; name: ntt
; subject: math
; capability: convolution number-theoretic-transform
; status: healthy
; canonical-home: none
; notes: Number-theoretic transform demo (direct O(n^2) transform, not a fast one).
; ============================================================
; ============================================================
; 92 — Modular Convolution via NTT (TAOCP Vol 2, 4.6.4)
; Number-theoretic transform over modulus p=97, primitive
; root g=5. Forward NTT, pointwise multiply, inverse NTT.
; Compare every result with schoolbook convolution.
; Verify the convolution theorem: NTT(a*b) = NTT(a) * NTT(b).
; ============================================================

(print "=== Modular Convolution (NTT) ===")


; ---------- modular arithmetic ----------
(define p 97)
(define g 5)

(define (modexp base exp mod)
  (labels
    (((walk b e r)
      (cond ((= e 0) r)
            ((= (mod e 2) 1)
             (walk (mod (* b b) mod) (truncate (/ e 2)) (mod (* r b) mod)))
            (t
             (walk (mod (* b b) mod) (truncate (/ e 2)) r)))))
    (walk base exp 1)))

(define (modinv a) (modexp a (- p 2) p))

; ---------- NTT ----------
(define (ntt-root n)
  (modexp g (truncate (/ (- p 1) n)) p))

(define (ntt a n)
  (let ((w (ntt-root n)))
    (labels
      (((outer k)
        (cond ((= k n) nil)
              (t (cons (inner k 0 0 1) (outer (+ k 1))))))
       ((inner k j acc wk)
        (cond ((= j n) (mod acc p))
              (t (inner k (+ j 1)
                        (+ acc (* (nth j a) wk))
                        (mod (* wk (modexp w k p)) p))))))
      (outer 0))))

(define (intt a n)
  (let ((invn (modinv n))
        (w (modinv (ntt-root n))))
    (labels
      (((outer j)
        (cond ((= j n) nil)
              (t (cons (inner j 0 0 1) (outer (+ j 1))))))
       ((inner j k acc wk)
        (cond ((= k n) (mod (* invn acc) p))
              (t (inner j (+ k 1)
                        (+ acc (* (nth k a) wk))
                        (mod (* wk (modexp w j p)) p))))))
      (outer 0))))

(define (point-mul a b)
  (cond ((eq a nil) nil)
        (t (cons (mod (* (car a) (car b)) p)
                 (point-mul (cdr a) (cdr b))))))

; ---------- schoolbook convolution ----------
(define (conv a b)
  (let ((outlen (+ (length a) (length b) -1)))
    (labels
      (((walk i)
        (cond ((= i outlen) nil)
              (t (cons (conv-sum a b i) (walk (+ i 1)))))))
      (walk 0))))

(define (conv-sum a b i)
  (labels
    (((walk k acc)
      (cond ((> k i) (mod acc p))
            ((or (>= k (length a)) (>= (- i k) (length b)))
             (walk (+ k 1) acc))
            (t (walk (+ k 1)
                     (+ acc (* (nth k a) (nth (- i k) b))))))))
    (walk 0 0)))

; ---------- padding ----------
(define (pad xs n)
  (cond ((eq xs nil)
         (cond ((> n 0) (cons 0 (pad nil (- n 1)))) (t nil)))
        ((= n 0) nil)
        (t (cons (car xs) (pad (cdr xs) (- n 1))))))

; ---------- tests ----------
(define a-vec (list 1 2 3))
(define b-vec (list 4 5 6))
(define outlen (+ (length a-vec) (length b-vec) -1))
(define n-len (* 2 (min (length a-vec) (length b-vec))))

(define a-pad (pad a-vec n-len))
(define b-pad (pad b-vec n-len))

(define ntt-A (ntt a-pad n-len))
(define ntt-B (ntt b-pad n-len))
(define ntt-pt  (point-mul ntt-A ntt-B))
(define ntt-c (intt ntt-pt n-len))
(define c-sch (conv a-vec b-vec))

(print (list 'a a-vec 'b b-vec))
(print (list 'N n-len))
(print (list 'conv-schoolbook c-sch))
(print (list 'conv-ntt (take ntt-c outlen)))

(check "ntt-matches-schoolbook"
  (equal (take ntt-c outlen) c-sch))

; Convolution theorem: NTT(a*b) = NTT(a) * NTT(b) pointwise.
(define conv-pad (pad c-sch n-len))
(define ntt-of-conv (ntt conv-pad n-len))
(check "convolution-theorem"
  (equal ntt-pt ntt-of-conv))

; Second test.
(define a2 (list 7 2 1))
(define b2 (list 3 4))
(define outlen2 (+ (length a2) (length b2) -1))
(define n2 4)
(define ntt-a2 (ntt (pad a2 n2) n2))
(define ntt-b2 (ntt (pad b2 n2) n2))
(define ntt-pt2 (point-mul ntt-a2 ntt-b2))
(define ntt-c2 (intt ntt-pt2 n2))
(define c2-sch (conv a2 b2))
(print (list 'c2-sch-is c2-sch 'len (length c2-sch)))
(check "ntt-matches-schoolbook-2"
  (equal (take ntt-c2 outlen2) c2-sch))

; Exhaustive: all pairs of length-2 vectors with entries from 0..4.
(define vals (list 0 1 2 3 4))
(define n-ex 4)

(define (test-pair x0 x1 y0 y1)
  (let ((va (list x0 x1)) (vb (list y0 y1)))
    (let ((c-s (conv va vb))
          (c-n (intt (point-mul (ntt (pad va n-ex) n-ex)
                                (ntt (pad vb n-ex) n-ex))
                      n-ex)))
      (equal (take c-n (+ (length va) (length vb) -1)) c-s))))

(check "exhaustive-len2-convolution"
  (every (lambda (x0)
    (every (lambda (x1)
      (every (lambda (y0)
        (every (lambda (y1)
          (test-pair x0 x1 y0 y1))
          vals))
        vals))
      vals))
    vals))

(print "=== Modular Convolution (NTT) Done ===")
