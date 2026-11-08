; ============================================================
; name: fft
; file: algorithms/math/fft.lsp
; subject: math
; capability: transform spectral
; aliases: "fast fourier transform" "cooley-tukey" "dft"
; signature: (fft x) (ifft x) -> complex list ; (fft-mags sig) -> magnitudes
; complexity: O(n log n) for n a power of two
; see-also: huffman
; derived-from: Cooley & Tukey 1965
; status: example (tested)
; checks: 11 checks incl. textbook vector, roundtrip, linearity;
;         run: src/sisp <this file> from repo root
; ============================================================
; Radix-2 Cooley-Tukey FFT.  A complex number is (re im); input length
; must be a power of two.  ifft(x) = conjugate(fft(conjugate(x))) / n.

(define fft-pi 3.141592653589793)

(define (fft-split x i acc parity)
  (cond ((null x) (reverse acc))
        ((= (mod i 2) parity) (fft-split (cdr x) (+ i 1) (cons (car x) acc) parity))
        (t (fft-split (cdr x) (+ i 1) acc parity))))

(define (fft-evens x) (fft-split x 0 nil 0))
(define (fft-odds x) (fft-split x 0 nil 1))

(define (fft-twiddle n k)
  (let ((theta (* -2.0 fft-pi (/ k n))))
    (list (cos theta) (sin theta))))

(define (fft-butterfly ev od n k half plus)
  (cond ((>= k half) nil)
        (t (let* ((tw (fft-twiddle n k))
                  (evk (nth k ev))
                  (odk (nth k od))
                  (tr (- (* (car tw) (car odk)) (* (cadr tw) (cadr odk))))
                  (ti (+ (* (car tw) (cadr odk)) (* (cadr tw) (car odk))))
                  (re (cond ((= plus 1) (+ (car evk) tr)) (t (- (car evk) tr))))
                  (im (cond ((= plus 1) (+ (cadr evk) ti)) (t (- (cadr evk) ti)))))
             (cons (list re im) (fft-butterfly ev od n (+ k 1) half plus))))))

(define (fft x)
  "discrete Fourier transform of x (length a power of two)"
  (let ((n (length x)))
    (cond ((= n 0) nil)
          ((= n 1) x)
          (t (let* ((ev (fft (fft-evens x)))
                    (od (fft (fft-odds x)))
                    (half (/ n 2)))
               (append (fft-butterfly ev od n 0 half 1)
                       (fft-butterfly ev od n 0 half 0)))))))

(define (fft-conjugate x)
  (map (lambda (z) (list (car z) (- 0 (cadr z)))) x))

(define (fft-scale x s)
  (map (lambda (z) (list (* (car z) s) (* (cadr z) s))) x))

(define (ifft x)
  "inverse DFT of x"
  (fft-scale (fft-conjugate (fft (fft-conjugate x)))
             (/ 1 (length x))))

(define (fft-mag z) (sqrt (+ (* (car z) (car z)) (* (cadr z) (cadr z)))))

(define (fft-real sig)
  (map (lambda (v) (list v 0.0)) sig))

(define (fft-mags sig)
  "magnitude spectrum of a real signal"
  (map fft-mag (fft (fft-real sig))))

; ---- check helpers (defined before use) ----

(define (fft-close-p a b) (< (abs (- a b)) 0.0000001))

(define (fft-zclose-p z w)
  (and (fft-close-p (car z) (car w))
       (fft-close-p (cadr z) (cadr w))))

(define (fft-all-close-p xs ys)
  (cond ((null xs) (null ys))
        ((null ys) nil)
        ((fft-zclose-p (car xs) (car ys))
         (fft-all-close-p (cdr xs) (cdr ys)))
        (t nil)))

; elementwise complex addition of two equal-length lists (map is
; single-list in SISP)
(define (fft-zadd xs ys)
  (cond ((null xs) nil)
        (t (cons (list (+ (caar xs) (caar ys))
                       (+ (cadar xs) (cadar ys)))
                 (fft-zadd (cdr xs) (cdr ys))))))

; ---- checks ----

(check "fft: textbook vector |DFT[1,2,3,4]| = (10, 2*sqrt(2), 2, 2*sqrt(2))"
       (let ((mags (fft-mags (list 1.0 2.0 3.0 4.0))))
         (and (fft-close-p (nth 0 mags) 10.0)
              (fft-close-p (nth 1 mags) 2.8284271247)
              (fft-close-p (nth 2 mags) 2.0)
              (fft-close-p (nth 3 mags) 2.8284271247))))

(check "fft: DFT[1,2,3,4] exact complex values"
       (fft-all-close-p (fft (fft-real (list 1.0 2.0 3.0 4.0)))
                        (list (list 10.0 0.0)
                              (list -2.0 2.0)
                              (list -2.0 0.0)
                              (list -2.0 -2.0))))

(check "fft: delta -> flat spectrum"
       (let ((mags (fft-mags (list 1.0 0.0 0.0 0.0))))
         (and (fft-close-p (nth 0 mags) 1.0)
              (fft-close-p (nth 1 mags) 1.0)
              (fft-close-p (nth 2 mags) 1.0)
              (fft-close-p (nth 3 mags) 1.0))))

(check "fft: constant signal -> energy only in bin 0"
       (let ((mags (fft-mags (list 1.0 1.0 1.0 1.0))))
         (and (fft-close-p (nth 0 mags) 4.0)
              (fft-close-p (nth 1 mags) 0.0)
              (fft-close-p (nth 2 mags) 0.0)
              (fft-close-p (nth 3 mags) 0.0))))

(check "fft: ifft recovers the input (roundtrip)"
       (let ((x (fft-real (list 1.0 2.0 3.0 4.0))))
         (fft-all-close-p (ifft (fft x)) x)))

(check "fft: roundtrip on a longer signal"
       (let ((x (fft-real (list 0.5 -1.5 2.25 3.75 -0.5 1.5 -2.0 0.25))))
         (fft-all-close-p (ifft (fft x)) x)))

(check "fft: linearity -- fft(a+b) = fft(a) + fft(b)"
       (let* ((a (fft-real (list 1.0 2.0 3.0 4.0)))
              (b (fft-real (list 0.5 0.5 0.5 0.5)))
              (ab (fft-zadd a b))
              (fa (fft a))
              (fb (fft b))
              (fab (fft ab)))
         (fft-all-close-p fab (fft-zadd fa fb))))

(check "fft: sine burst at bin 1 peaks at bin 1"
       (let* ((n 8)
              (sig (map (lambda (i) (sin (* 2.0 fft-pi (/ i n))))
                        (seq 0 (- n 1))))
              (mags (fft-mags sig)))
         (and (> (nth 1 mags) 3.9)
              (< (nth 0 mags) 0.000001))))

(check "fft: Parseval -- energy conserved (sum |x|^2 * n = sum |X|^2)"
       (let* ((sig (list 1.0 2.0 3.0 4.0))
              (mags (fft-mags sig)))
         (fft-close-p (* (length sig)
                         (foldl (lambda (acc v) (+ acc (* v v))) 0.0 sig))
                     (foldl (lambda (acc m) (+ acc (* m m))) 0.0 mags))))

(check "fft: n=1 is identity"
       (equal (fft (list (list 5.0 0.0))) (list (list 5.0 0.0))))

(check "fft: ifft of textbook DFT recovers [1,2,3,4]"
       (fft-all-close-p
         (ifft (list (list 10.0 0.0) (list -2.0 2.0)
                     (list -2.0 0.0) (list -2.0 -2.0)))
         (fft-real (list 1.0 2.0 3.0 4.0))))
