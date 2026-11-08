; windowing.lsp — Rectangular, Hamming, Hann, Blackman windows.
; Spectral leakage. Apply window before the DFT.
(print "=== Windowing ===")

(define PI 3.141592653589793)
(define EPS 0.000000001)

(define (close-p a b) (< (abs (- a b)) EPS))

(define (list-sum l) (cond ((eq l nil) 0.0) (t (+ (car l) (list-sum (cdr l))))))

; Rectangular window (all ones)
(define (rect-win n)
  (rect-win-iter n 0 nil))

(define (rect-win-iter n i acc)
  (cond ((>= i n) (reverse acc))
        (t (rect-win-iter n (+ i 1) (cons 1.0 acc)))))

; Hamming window: w[k] = 0.54 - 0.46*cos(2*PI*k/(N-1))
(define (hamming-win n)
  (hamming-iter n 0 nil))

(define (hamming-iter n i acc)
  (cond ((>= i n) (reverse acc))
        (t (let ((w (- 0.54 (* 0.46 (cos (/ (* 2.0 PI i) (- n 1)))))))
          (hamming-iter n (+ i 1) (cons w acc))))))

; Hann window: w[k] = 0.5*(1 - cos(2*PI*k/(N-1)))
(define (hann-win n)
  (hann-iter n 0 nil))

(define (hann-iter n i acc)
  (cond ((>= i n) (reverse acc))
        (t (let ((w (* 0.5 (- 1.0 (cos (/ (* 2.0 PI i) (- n 1)))))))
          (hann-iter n (+ i 1) (cons w acc))))))

; Blackman window: w[k] = 0.42 - 0.5*cos(2*PI*k/(N-1)) + 0.08*cos(4*PI*k/(N-1))
(define (blackman-win n)
  (blackman-iter n 0 nil))

(define (blackman-iter n i acc)
  (cond ((>= i n) (reverse acc))
        (t (let* ((ang (* 2.0 PI (/ i (- n 1))))
                  (w (+ 0.42 (* -0.5 (cos ang)) (* 0.08 (cos (* 2.0 ang))))))
          (blackman-iter n (+ i 1) (cons w acc))))))

; --- window values -------------------------------------------------------
(define n-pts 8)
(define rw (rect-win n-pts))
(define hw (hamming-win n-pts))
(define hnw (hann-win n-pts))
(define bw (blackman-win n-pts))

(print "Rect win[0] =" (nth 0 rw) " win[3] =" (nth 3 rw))
(print "Hamming win[0] =" (nth 0 hw) " win[3] =" (nth 3 hw))
(print "Hann win[0] =" (nth 0 hnw) " win[3] =" (nth 3 hnw))
(print "Blackman win[0] =" (nth 0 bw) " win[3] =" (nth 3 bw))

; Rectangular window: every tap is exactly 1, so the sum is exactly N.
(check "rect-uniform" (close-p (list-sum rw) 8.0))

; Hamming edge tap is 0.54 - 0.46 = 0.08 exactly; centre tap (k=3, N=8) is
; 0.54 - 0.46*cos(6*PI/7) = 0.54 + 0.46*0.9009688679 = 0.9544456792.
(check "hamming-edge" (close-p (nth 0 hw) 0.08))
(check "hamming-center" (close-p (nth 3 hw) 0.9544456792351128))

; Hann edge tap is 0.5*(1-cos 0) = 0 exactly; k=3 gives 0.5*(1+0.9009688679).
(check "hann-edge" (close-p (nth 0 hnw) 0.0))
(check "hann-center" (close-p (nth 3 hnw) 0.9504844339512095))

; Blackman edge tap is 0.42 - 0.5 + 0.08 = 0 exactly.
(check "blackman-low-edge" (close-p (nth 0 bw) 0.0))

; Coherent gain.  For the SYMMETRIC (N-1 denominator) definitions the sums are
; exact: cos(2*PI*k/(N-1)) summed over k=0..N-1 is one full period (0) plus the
; wrapped term cos(2*PI) = 1, hence
;   sum(hann)     = 0.5*(N - 1)              = 3.5   for N=8
;   sum(hamming)  = 0.54*N - 0.46            = 3.86
;   sum(blackman) = 0.42*N - 0.5 + 0.08      = 2.94
(check "hann-sum" (close-p (list-sum hnw) 3.5))
(check "hamming-sum" (close-p (list-sum hw) 3.86))
(check "blackman-sum" (close-p (list-sum bw) 2.94))

; Symmetry: w[k] = w[N-1-k] for all three tapers (linear-phase requirement).
(define (symmetric-p w n i)
  (cond ((>= i n) t)
        ((close-p (nth i w) (nth (- (- n 1) i) w)) (symmetric-p w n (+ i 1)))
        (t nil)))

(check "hann-symmetric" (symmetric-p hnw n-pts 0))
(check "hamming-symmetric" (symmetric-p hw n-pts 0))
(check "blackman-symmetric" (symmetric-p bw n-pts 0))

; --- applying a window ---------------------------------------------------
(define (apply-window win sig)
  (apply-win-iter win sig 0 nil))

(define (apply-win-iter win sig i acc)
  (cond ((eq sig nil) (reverse acc))
        (t (apply-win-iter win (cdr sig) (+ i 1)
                           (cons (* (car sig) (nth i win)) acc)))))

(define test-signal (list 1.0 2.0 3.0 4.0 3.0 2.0 1.0 0.0))
(define windowed (apply-window hw test-signal))
(print "Windowed signal[0] =" (nth 0 windowed))
; 1.0 * 0.08 = 0.08 at the edge, 4.0 * 0.9544456792 in the middle.
(check "window-applied-edge" (close-p (nth 0 windowed) 0.08))
(check "window-applied-mid" (close-p (nth 3 windowed) (* 4.0 0.9544456792351128)))

; --- spectral leakage ----------------------------------------------------
; A cosine whose frequency is NOT an integer number of cycles per window
; (1.5 cycles in 8 samples) does not fit the DFT basis, so its energy smears
; across every bin.  Tapering the edges suppresses that smearing.
(define (gen-cosine n freq)
  (gen-cosine-iter n 0 freq nil))

(define (gen-cosine-iter n i freq acc)
  (cond ((>= i n) (reverse acc))
        (t (gen-cosine-iter n (+ i 1) freq
                            (cons (cos (/ (* 2.0 PI freq i) n)) acc)))))

; Direct DFT magnitude spectrum (O(N^2), fine at N=8).
(define (dft-mag sig)
  (let ((n (length sig)))
    (dft-mag-k sig n 0 nil)))

(define (dft-mag-k sig n k acc)
  (cond ((>= k n) (reverse acc))
        (t (dft-mag-k sig n (+ k 1) (cons (dft-bin sig n k 0 0.0 0.0) acc)))))

(define (dft-bin sig n k i re im)
  (cond ((>= i n) (sqrt (+ (* re re) (* im im))))
        (t (let ((ang (/ (* 2.0 PI k i) n)))
          (dft-bin sig n k (+ i 1)
                   (+ re (* (nth i sig) (cos ang)))
                   (- im (* (nth i sig) (sin ang))))))))

(define leaky-sig (gen-cosine n-pts 1.5))
(define leaky-rect (apply-window rw leaky-sig))
(define leaky-hann (apply-window hnw leaky-sig))
(define mag-rect (dft-mag leaky-rect))
(define mag-hann (dft-mag leaky-hann))
(print "Rect spectrum:" mag-rect)
(print "Hann spectrum:" mag-hann)

; Edge tapering: the rectangular window leaves x[0] = cos(0) = 1 untouched,
; the Hann window drives it to exactly 0.
(check "window-reduces-edges" (< (abs (nth 0 leaky-hann)) (abs (nth 0 leaky-rect))))
(check "rect-keeps-edge" (close-p (nth 0 leaky-rect) 1.0))

; Far-sidelobe rejection: at the Nyquist bin (k=4), farthest from the true
; 1.5-cycle tone, the rectangular window still shows magnitude 1.0 while the
; Hann window drops to ~0.058 — better than a factor of ten.
(print "Nyquist bin: rect =" (nth 4 mag-rect) " hann =" (nth 4 mag-hann))
(check "hann-sidelobe-lower" (< (nth 4 mag-hann) (* 0.2 (nth 4 mag-rect))))

; Leakage fraction: energy landing outside the two bins straddling 1.5 cycles
; (k = 1,2 and their conjugates k = 6,7) divided by total energy.
(define (energy-in mag ks)
  (foldl (lambda (a k) (+ a (* (nth k mag) (nth k mag)))) 0.0 ks))

(define main-bins (list 1 2 6 7))
(define all-bins (list 0 1 2 3 4 5 6 7))

(define (leak-frac mag)
  (- 1.0 (/ (energy-in mag main-bins) (energy-in mag all-bins))))

(print "Leakage fraction: rect =" (leak-frac mag-rect) " hann =" (leak-frac mag-hann))
(check "hann-leaks-less" (< (leak-frac mag-hann) (leak-frac mag-rect)))
; Rect leaks ~15.1% of its energy outside the main bins, Hann only ~7.4%.
(check "rect-leak-value" (< (abs (- (leak-frac mag-rect) 0.151313)) 0.0001))
(check "hann-leak-value" (< (abs (- (leak-frac mag-hann) 0.074090)) 0.0001))

(print "=== Windowing Done ===")
