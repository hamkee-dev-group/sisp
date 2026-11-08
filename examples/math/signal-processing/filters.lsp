; filters.lsp — FIR filter design (window method).
; Low-pass, high-pass, band-pass. Convolution, DTFT.
(print "=== Filters ===")

(define PI 3.141592653589793)
(define EPS 0.000000001)
(define TINY 0.000000000001)

(define (close-p a b) (< (abs (- a b)) EPS))
(define (near-p a b tol) (< (abs (- a b)) tol))

(define (list-sum l) (cond ((eq l nil) 0.0) (t (+ (car l) (list-sum (cdr l))))))

; --- ideal (truncated sinc) low-pass -------------------------------------
; h[k] = sin(wc*d) / (PI*d) with d = k - (N-1)/2, and h = wc/PI at d = 0.
(define (sinc-lowpass cutoff numtaps)
  (let ((half (/ (- numtaps 1) 2)))
    (sinc-lp-iter cutoff numtaps half 0 nil)))

(define (sinc-lp-iter cutoff ntaps half k acc)
  (cond ((>= k ntaps) (reverse acc))
        (t (let* ((d (- k half))
                  (val (if (= d 0) (/ cutoff PI)
                          (/ (sin (* cutoff d)) (* PI d)))))
          (sinc-lp-iter cutoff ntaps half (+ k 1) (cons val acc))))))

; --- Hamming taper -------------------------------------------------------
(define (hamming-win n) (hamming-iter n 0 nil))

(define (hamming-iter n i acc)
  (cond ((>= i n) (reverse acc))
        (t (hamming-iter n (+ i 1)
             (cons (- 0.54 (* 0.46 (cos (/ (* 2.0 PI i) (- n 1))))) acc)))))

; --- window method: taper the sinc, then normalise to unit DC gain --------
(define (mul-lists a b)
  (cond ((or (eq a nil) (eq b nil)) nil)
        (t (cons (* (car a) (car b)) (mul-lists (cdr a) (cdr b))))))

(define (scale-list l s)
  (cond ((eq l nil) nil) (t (cons (/ (car l) s) (scale-list (cdr l) s)))))

(define (fir-lowpass cutoff numtaps)
  (let* ((raw (mul-lists (sinc-lowpass cutoff numtaps) (hamming-win numtaps)))
         (dc (list-sum raw)))
    (scale-list raw dc)))

; --- DTFT magnitude ------------------------------------------------------
(define (dtft h omega)
  (dtft-iter h omega (length h) 0 0.0 0.0))

(define (dtft-iter h omega n k re im)
  (cond ((>= k n) (sqrt (+ (* re re) (* im im))))
        (t (let* ((theta (* omega k))
                  (hk (nth k h)))
          (dtft-iter h omega n (+ k 1)
                     (+ re (* hk (cos theta)))
                     (- im (* hk (sin theta))))))))

(define (symmetric-p h n i)
  (cond ((>= i n) t)
        ((close-p (nth i h) (nth (- (- n 1) i) h)) (symmetric-p h n (+ i 1)))
        (t nil)))

; --- low-pass ------------------------------------------------------------
(define ntaps 31)
(define centre 15)

(define raw-lp (sinc-lowpass 1.0 ntaps))
; The centre tap of the ideal low-pass is exactly wc/PI: 1.0/PI = 0.3183098862.
(check "sinc-centre-tap" (close-p (nth centre raw-lp) (/ 1.0 PI)))
; Tap 16 sits at d = 1, so it is sin(1)/PI = 0.8414709848/PI = 0.2678369629.
(check "sinc-tap-16" (close-p (nth 16 raw-lp) (/ (sin 1.0) PI)))
(check "sinc-symmetric" (symmetric-p raw-lp ntaps 0))

(define lp (fir-lowpass 1.0 ntaps))
(print "LP taps 13..17:" (take (drop lp 13) 5))
(print "LP coefficient sum =" (list-sum lp))

; Normalisation makes the taps sum to exactly 1, i.e. |H(0)| = 1.
(check "lp-unit-dc-gain" (near-p (list-sum lp) 1.0 TINY))
(check "lp-symmetric" (symmetric-p lp ntaps 0))

(define lp-h0 (dtft lp 0.0))
(define lp-h-half (dtft lp 0.5))
(define lp-h2 (dtft lp 2.0))
(define lp-hpi (dtft lp PI))
(print "LP |H(0)|=" lp-h0 " |H(0.5)|=" lp-h-half " |H(2)|=" lp-h2 " |H(pi)|=" lp-hpi)

(check "lp-dc-gain" (near-p lp-h0 1.0 TINY))
; Passband (below the 1.0 rad/sample cutoff) is flat to within 1%.
(check "lp-passband-flat" (near-p lp-h-half 1.0 0.01))
; Stopband is more than 60 dB down.
(check "lp-stopband" (< lp-h2 0.001))
(check "lp-nyquist-stopband" (< lp-hpi 0.001))
(check "lp-dtft-low" (> lp-h0 lp-hpi))

; --- high-pass: spectral inversion, delta[k - centre] - lp[k] -------------
(define (fir-highpass cutoff numtaps)
  (let ((low (fir-lowpass cutoff numtaps)))
    (fir-hp-from-lp low numtaps (/ (- numtaps 1) 2) 0 nil)))

(define (fir-hp-from-lp low n mid i acc)
  (cond ((>= i n) (reverse acc))
        (t (let* ((delta (if (= i mid) 1.0 0.0))
                  (val (- delta (nth i low))))
          (fir-hp-from-lp low n mid (+ i 1) (cons val acc))))))

(define hp (fir-highpass 1.0 ntaps))
(print "HP coefficient sum =" (list-sum hp))
; Subtracting a unit-DC-gain low-pass from a unit impulse leaves zero DC gain.
(check "hp-sum-near-zero" (near-p (list-sum hp) 0.0 TINY))
(check "hp-symmetric" (symmetric-p hp ntaps 0))

(define hp-h0 (dtft hp 0.0))
(define hp-hpi (dtft hp PI))
(print "HP |H(0)|=" hp-h0 " |H(pi)|=" hp-hpi)
(check "hp-blocks-dc" (near-p hp-h0 0.0 TINY))
; |H_hp(pi)| = |1 - A_lp(pi)| and A_lp(pi) is tiny, so this sits just above 1.
(check "hp-passes-nyquist" (near-p hp-hpi 1.0 0.001))
(check "hp-complements-lp" (> hp-hpi hp-h0))

; --- band-pass: difference of two low-passes ------------------------------
(define (fir-bandpass flow fhigh numtaps)
  (sub-lists (fir-lowpass fhigh numtaps) (fir-lowpass flow numtaps)))

(define (sub-lists a b)
  (cond ((or (eq a nil) (eq b nil)) nil)
        (t (cons (- (car a) (car b)) (sub-lists (cdr a) (cdr b))))))

(define bp (fir-bandpass 0.8 2.0 ntaps))
(print "BP coefficient sum =" (list-sum bp))
(check "bp-length" (= (length bp) ntaps))
(check "bp-symmetric" (symmetric-p bp ntaps 0))
; Both low-passes have DC gain 1, so their difference has DC gain 0.
(check "bp-blocks-dc" (near-p (list-sum bp) 0.0 TINY))

(define bp-h0 (dtft bp 0.0))
(define bp-lo (dtft bp 0.8))
(define bp-mid (dtft bp 1.4))
(define bp-hi (dtft bp 2.0))
(define bp-stop (dtft bp 2.6))
(print "BP |H| at 0, 0.8, 1.4, 2.0, 2.6 =" (list bp-h0 bp-lo bp-mid bp-hi bp-stop))
(check "bp-dc-null" (near-p bp-h0 0.0 TINY))
(check "bp-passband" (near-p bp-mid 1.0 0.01))
; At each cutoff the two low-pass transitions cross at half amplitude (-6 dB).
(check "bp-lower-cutoff-6db" (near-p bp-lo 0.5 0.01))
(check "bp-upper-cutoff-6db" (near-p bp-hi 0.5 0.01))
(check "bp-stopband" (< bp-stop 0.001))

; --- direct convolution --------------------------------------------------
(define (filter-signal h signal)
  (let* ((hn (length h)) (sn (length signal))
         (on (+ hn sn -1)))
    (conv-direct h signal hn sn on 0 nil)))

(define (conv-direct h signal hn sn on i acc)
  (cond ((>= i on) (reverse acc))
        (t (conv-sum h signal hn sn i 0 0.0 acc on))))

(define (conv-sum h signal hn sn i j sum acc on)
  (cond ((>= j hn) (conv-direct h signal hn sn on (+ i 1) (cons sum acc)))
        (t (let ((sk (- i j)))
          (cond ((and (>= sk 0) (< sk sn))
                 (conv-sum h signal hn sn i (+ j 1)
                          (+ sum (* (nth j h) (nth sk signal))) acc on))
                (t (conv-sum h signal hn sn i (+ j 1) sum acc on)))))))

(define (lists-close l1 l2)
  (cond ((and (eq l1 nil) (eq l2 nil)) t)
        ((or (eq l1 nil) (eq l2 nil)) nil)
        ((close-p (car l1) (car l2)) (lists-close (cdr l1) (cdr l2)))
        (t nil)))

(define test-sig (list 1.0 2.0 3.0 2.0 1.0))
(define filtered (filter-signal lp test-sig))
(print "Filtered signal length =" (length filtered))
(check "conv-length" (= (length filtered) (+ (length lp) (length test-sig) -1)))

; Convolving with a unit impulse must return the filter unchanged, padded to
; the full output length.
(define impulse (list 1.0))
(check "conv-impulse-identity" (lists-close (filter-signal lp impulse) lp))

; Convolution commutes.
(check "conv-commutes" (lists-close filtered (filter-signal test-sig lp)))

; A constant-1 run through a unit-DC-gain filter settles at 1 in the middle of
; the output, where the whole 31-tap kernel overlaps the input.
(define ones (list 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
                   1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
                   1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
                   1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0))
(define step-out (filter-signal lp ones))
(print "Step response, middle sample =" (nth 34 step-out))
(check "conv-dc-gain" (near-p (nth 34 step-out) 1.0 TINY))

; --- cross-correlation ---------------------------------------------------
; Correlation is convolution with the time-reversed kernel; correlating a
; signal with itself peaks at zero lag.
(define (correlate a b)
  (filter-signal (reverse a) b))

(define autoc (correlate test-sig test-sig))
(print "Autocorrelation:" autoc)
; Zero lag sits at index (length a) - 1 = 4 and equals sum of squares:
; 1 + 4 + 9 + 4 + 1 = 19.
(check "autocorr-peak-value" (close-p (nth 4 autoc) 19.0))
(check "autocorr-peak-is-max" (and (> (nth 4 autoc) (nth 3 autoc))
                                   (> (nth 4 autoc) (nth 5 autoc))))
(check "autocorr-symmetric" (symmetric-p autoc (length autoc) 0))

(print "=== Filters Done ===")
