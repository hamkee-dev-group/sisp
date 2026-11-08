; fft-signal.lsp — FFT for power-of-2 lengths.
; Convolution theorem. Low-pass filter. Compare time vs freq.
(print "=== FFT Signal ===")


(define PI 3.141592653589793)

; Radix-2 DIT FFT (complex: (real, imag) pairs)
(define (fft x)
  (let ((n (length x)))
    (cond ((= n 1) x)
          (t (let ((half (/ n 2)))
            (let ((even (fft (split-even x)))
                  (odd (fft (split-odd x))))
              (fft-combine even odd n 0 half)))))))

(define (split-even x)
  (split-even-iter x 0 nil))

(define (split-even-iter x i acc)
  (cond ((eq x nil) (reverse acc))
        ((= (mod i 2) 0) (split-even-iter (cdr x) (+ i 1) (cons (car x) acc)))
        (t (split-even-iter (cdr x) (+ i 1) acc))))

(define (split-odd x)
  (split-odd-iter x 0 nil))

(define (split-odd-iter x i acc)
  (cond ((eq x nil) (reverse acc))
        ((= (mod i 2) 1) (split-odd-iter (cdr x) (+ i 1) (cons (car x) acc)))
        (t (split-odd-iter (cdr x) (+ i 1) acc))))

; The butterfly outputs must be CONCATENATED, not interleaved:
; X[k] = E[k] + w^k O[k] for k < n/2, and X[k+n/2] = E[k] - w^k O[k].
(define (fft-combine even odd n k half)
  (append (fft-lower even odd n k half) (fft-upper even odd n k half)))

(define (fft-lower even odd n k half)
  (cond ((>= k half) nil)
        (t (cons (cadd (nth k even) (cmul (twiddle n k) (nth k odd)))
                 (fft-lower even odd n (+ k 1) half)))))

(define (fft-upper even odd n k half)
  (cond ((>= k half) nil)
        (t (cons (csub (nth k even) (cmul (twiddle n k) (nth k odd)))
                 (fft-upper even odd n (+ k 1) half)))))

(define (twiddle n k)
  (let ((theta (* -2.0 PI (/ k n))))
    (list (cos theta) (sin theta))))

(define (cadd a b) (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))
(define (csub a b) (list (- (car a) (car b)) (- (cadr a) (cadr b))))
(define (cmul a b) (list (- (* (car a) (car b)) (* (cadr a) (cadr b)))
                         (+ (* (car a) (cadr b)) (* (cadr a) (car b)))))

; Inverse FFT
(define (ifft x)
  (let* ((n (length x))
         (conj-x (map (lambda (z) (list (car z) (- 0 (cadr z)))) x))
         (fft-conj (fft conj-x))
         (conj-res (map (lambda (z) (list (car z) (- 0 (cadr z)))) fft-conj)))
    (map (lambda (z) (list (/ (car z) n) (/ (cadr z) n))) conj-res)))

; Test: FFT of (1,2,3,4)
(define signal (list (list 1.0 0.0) (list 2.0 0.0) (list 3.0 0.0) (list 4.0 0.0)))
(define fft-res (fft signal))
(define fft-mag (map (lambda (z) (sqrt (+ (* (car z) (car z)) (* (cadr z) (cadr z))))) fft-res))
(print "FFT of [1,2,3,4] magnitudes:" fft-mag)
(check "fft-works" (> (nth 3 fft-mag) 1.0))

; Inverse FFT
(define inv (ifft fft-res))
(check "ifft-recovers" (< (abs (- (car (nth 0 inv)) 1.0)) 0.001))

; Convolution theorem test
(define a-sig (list (list 1.0 0.0) (list 2.0 0.0) (list 0.0 0.0) (list 0.0 0.0)))
(define b-sig (list (list 3.0 0.0) (list 4.0 0.0) (list 0.0 0.0) (list 0.0 0.0)))
(define fft-a (fft a-sig))
(define fft-b (fft b-sig))
; map takes a single list here, so pointwise complex multiply needs its own walk
(define (cmul-lists xs ys)
  (cond ((eq xs nil) nil)
        (t (cons (cmul (car xs) (car ys)) (cmul-lists (cdr xs) (cdr ys))))))

(define fft-c (cmul-lists fft-a fft-b))
(define conv-result (ifft fft-c))
(print "Convolution via FFT, y(0)=" (car (nth 0 conv-result)))
(check "conv-theorem" (< (abs (- (car (nth 0 conv-result)) 3.0)) 1.0))

; Low-pass filter in frequency domain
; Zero every bin whose frequency index min(i, n-i) exceeds the cutoff,
; then transform back.
(define (lowpass x cutoff)
  (let* ((fx (fft x))
         (n (length fx)))
    (ifft (lowpass-apply fx n 0 cutoff))))

(define (lowpass-apply fx n i cutoff)
  (cond ((>= i n) nil)
        (t (let ((freq (cond ((<= i (- n i)) i) (t (- n i)))))
          (cons (cond ((<= freq cutoff) (nth i fx))
                      (t (list 0.0 0.0)))
                (lowpass-apply fx n (+ i 1) cutoff))))))

; signal = [1,2,3,4], spectrum [10, -2+2i, -2, -2-2i].
; cutoff 1 keeps bins 0,1,3 and kills bin 2 (value -2), so each sample
; changes by -(1/4)(-2)(-1)^j = +0.5*(-1)^j: filtered = [1.5, 1.5, 3.5, 3.5].
(define filtered (lowpass signal 1))
(print "Filtered signal first=" (car (nth 0 filtered)))
(check "lowpass-filter" (< (abs (- (car (nth 0 filtered)) 1.5)) 0.0001))
; a low-pass keeps bin 0, so the DC level (mean 2.5) must survive
(check "lowpass-preserves-dc"
       (< (abs (- (/ (+ (car (nth 0 filtered)) (car (nth 1 filtered))
                        (car (nth 2 filtered)) (car (nth 3 filtered)))
                     4.0)
                  2.5))
          0.0001))

(print "=== FFT Signal Done ===")
