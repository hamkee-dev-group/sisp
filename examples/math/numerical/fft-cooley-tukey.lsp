; ============================================================
; FFT (Cooley-Tukey Radix-2 DIT) over rational complex pairs
; Input: (1,2,3,4). Compare with direct DFT. Verify inverse FFT.
; ============================================================

(print "=== FFT (Cooley-Tukey) ===")

(define (ck name ok)
  (cond (ok (print (list 'pass name)))
        (t (print (list 'FAIL name)))))

; Complex arithmetic on (real, imag) pairs
(define (cadd a b)
  (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))

(define (csub a b)
  (list (- (car a) (car b)) (- (cadr a) (cadr b))))

(define (cmul a b)
  (let ((ar (car a)) (ai (cadr a))
        (br (car b)) (bi (cadr b)))
    (list (- (* ar br) (* ai bi)) (+ (* ar bi) (* ai br)))))

(define (cconj z) (list (car z) (- 0 (cadr z))))

(define (cscale z s) (list (* (car z) s) (* (cadr z) s)))

; Exact roots of unity for N=4: w = i
; w^0 = 1, w^1 = i, w^2 = -1, w^3 = -i
(define (w4 k)
  (cond ((= (mod k 4) 0) (list 1 0))
        ((= (mod k 4) 1) (list 0 -1))
        ((= (mod k 4) 2) (list -1 0))
        ((= (mod k 4) 3) (list 0 1))
        (t nil)))

; Twiddle factor: e^(-2*pi*i*k/n) as float pair
(define (twiddle n k)
  (let ((theta (* -6.283185307179586 (/ k n))))
    (list (cos theta) (sin theta))))

; --- Forward FFT (radix-2 DIT, Cooley-Tukey) ---
; Output: X[0..N-1] = FFT(x[0..N-1])
; X[k] = E[k] + W_N^k * O[k]
; X[k+N/2] = E[k] - W_N^k * O[k]

(define (fft x)
  (let ((n (length x)))
    (cond ((= n 1) x)
          (t (let* ((evens (fft-even-split x 0))
                    (odds  (fft-odd-split x 0))
                    (even-fft (fft evens))
                    (odd-fft  (fft odds)))
            (fft-combine even-fft odd-fft n))))))

(define (fft-even-split x i)
  (cond ((eq x nil) nil)
        ((= (mod i 2) 0) (cons (car x) (fft-even-split (cdr x) (+ i 1))))
        (t (fft-even-split (cdr x) (+ i 1)))))

(define (fft-odd-split x i)
  (cond ((eq x nil) nil)
        ((= (mod i 2) 1) (cons (car x) (fft-odd-split (cdr x) (+ i 1))))
        (t (fft-odd-split (cdr x) (+ i 1)))))

(define (fft-combine even-fft odd-fft n)
  (let ((half (length even-fft)))
    (append (fft-upper even-fft odd-fft n 0 half)
            (fft-lower even-fft odd-fft n 0 half))))

(define (fft-upper even-fft odd-fft n k half)
  (cond ((>= k half) nil)
        (t (let* ((e (nth k even-fft))
                  (o (nth k odd-fft))
                  (tw (if (= n 4) (w4 k) (twiddle n k)))
                  (otw (cmul tw o))
                  (val (cadd e otw)))
          (cons val (fft-upper even-fft odd-fft n (+ k 1) half))))))

(define (fft-lower even-fft odd-fft n k half)
  (cond ((>= k half) nil)
        (t (let* ((e (nth k even-fft))
                  (o (nth k odd-fft))
                  (tw (if (= n 4) (w4 k) (twiddle n k)))
                  (otw (cmul tw o))
                  (val (csub e otw)))
          (cons val (fft-lower even-fft odd-fft n (+ k 1) half))))))

; --- Inverse FFT ---
; Uses: ifft(x) = conj(FFT(conj(x))) / N

(define (cmap-conj lst)
  (cond ((eq lst nil) nil)
        (t (cons (cconj (car lst)) (cmap-conj (cdr lst))))))

(define (ifft x)
  (let* ((n (length x))
         (conj-x (cmap-conj x))
         (forward (fft conj-x))
         (conj-result (cmap-conj forward)))
    (cmap-scale conj-result (/ 1.0 n))))

(define (cmap-scale lst s)
  (cond ((eq lst nil) nil)
        (t (cons (cscale (car lst) s) (cmap-scale (cdr lst) s)))))

; --- Direct DFT for comparison ---
(define (dft x)
  (let ((n (length x)))
    (dft-loop x n 0)))

(define (dft-loop x n k)
  (cond ((= k n) nil)
        (t (cons (dft-sum x n k 0 (list 0 0)) (dft-loop x n (+ k 1))))))

(define (dft-sum x n k j acc)
  (cond ((= j n) acc)
        (t (let* ((xj (nth j x))
                  (tw (if (= n 4) (w4 (mod (* k j) 4))
                          (twiddle n (* k j))))
                  (prod (cmul xj tw)))
          (dft-sum x n k (+ j 1) (cadd acc prod))))))

; --- Test ---
(define input (list (list 1 0) (list 2 0) (list 3 0) (list 4 0)))
(print "Input x =") (print input)

(define fft-result (fft input))
(define dft-result (dft input))

(print "FFT(x) =") (print fft-result)
(print "DFT(x) =") (print dft-result)

; Compare FFT and DFT
(define (ccompare a b tol)
  (cond ((eq a nil) t)
        (t (let ((da (csub (car a) (car b))))
          (and (< (abs (car da)) tol)
               (< (abs (cadr da)) tol)
               (ccompare (cdr a) (cdr b) tol))))))

(ck "fft-matches-dft" (ccompare fft-result dft-result 0.0000001))

; Inverse FFT recovers original
(define ifft-result (ifft fft-result))
(print "iFFT(FFT(x)) =") (print ifft-result)
(ck "ifft-recovers-input" (ccompare ifft-result input 0.0000001))

; For a REAL input signal the spectrum is conjugate-symmetric: X[k] = conj(X[N-k]).
; Equivalently the tail X[1..N-1] equals the conjugate of its own reversal.
; (The DC term X[0] is its own mirror, so it must be purely real.)
(define (cconj z) (list (car z) (- (cadr z))))
(define fft-tail (cdr fft-result))
(ck "fft-dc-term-is-real" (< (abs (cadr (car fft-result))) 0.0000001))
(ck "fft-of-real-is-conjugate-symmetric"
    (ccompare fft-tail (map cconj (reverse fft-tail)) 0.0000001))

; Known values for DFT of (1,2,3,4): (10, -2+2i, -2, -2-2i)
(define expected-fft (list (list 10 0) (list -2 2) (list -2 0) (list -2 -2)))
(print "Expected FFT =") (print expected-fft)
(ck "fft-matches-expected" (ccompare fft-result expected-fft 0.0000001))

; Test with 8 elements
(define input8 (list (list 1 0) (list 0 0) (list 2 0) (list 0 0)
                     (list 3 0) (list 0 0) (list 4 0) (list 0 0)))
(define fft8-result (fft input8))
(define dft8-result (dft input8))
(ck "fft8-matches-dft8" (ccompare fft8-result dft8-result 0.0000001))

(define ifft8-result (ifft fft8-result))
(ck "ifft8-recovers-input" (ccompare ifft8-result input8 0.0000001))

(print "=== FFT (Cooley-Tukey) Done ===")
