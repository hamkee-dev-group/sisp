; convolution.lsp — Discrete convolution via native matrix multiply.
; 1D and 2D convolution, cross-correlation, edge handling
; (zero-pad, wrap, reflect).
(print "=== Convolution ===")

(define EPS 0.000000001)

(define (close-p a b) (< (abs (- a b)) EPS))

(define (lists-close l1 l2)
  (cond ((and (eq l1 nil) (eq l2 nil)) t)
        ((or (eq l1 nil) (eq l2 nil)) nil)
        ((close-p (car l1) (car l2)) (lists-close (cdr l1) (cdr l2)))
        (t nil)))

(define (rows-close a b)
  (cond ((and (eq a nil) (eq b nil)) t)
        ((or (eq a nil) (eq b nil)) nil)
        ((lists-close (car a) (car b)) (rows-close (cdr a) (cdr b)))
        (t nil)))

(define (list-sum l) (cond ((eq l nil) 0.0) (t (+ (car l) (list-sum (cdr l))))))

; --- 1D convolution as a Toeplitz matrix times a column vector ------------
; y = T x, where row i of T holds the kernel reversed and shifted by i.
; `off` selects which slice of the full output is kept: 0 for 'full,
; (kn-1)/2 truncated for 'same (the convention numpy uses).
(define (make-toeplitz kernel sn kn on off)
  (let ((tm (make-matrix on sn 0)))
    (toeplitz-rows tm kernel sn kn on off 0)))

(define (toeplitz-rows tm kernel sn kn on off i)
  (cond ((>= i on) tm)
        (t (toeplitz-cols tm kernel sn kn off i 0)
           (toeplitz-rows tm kernel sn kn on off (+ i 1)))))

(define (toeplitz-cols tm kernel sn kn off i j)
  (cond ((>= j sn) tm)
        (t (let ((m (+ (- i j) off)))
          (cond ((and (>= m 0) (< m kn)) (mat-set tm i j (nth m kernel)))
                (t nil))
          (toeplitz-cols tm kernel sn kn off i (+ j 1))))))

(define (mat-col-to-list m n) (mat-list-i m n 0 nil))

(define (mat-list-i m n i acc)
  (cond ((>= i n) (reverse acc))
        (t (mat-list-i m n (+ i 1) (cons (mat-ref m i 0) acc)))))

(define (conv1d signal kernel mode)
  (let* ((sn (length signal))
         (kn (length kernel))
         (on (if (eq mode 'same) sn (+ sn kn -1)))
         (off (if (eq mode 'same) (truncate (/ (- kn 1) 2)) 0))
         (tm (make-toeplitz kernel sn kn on off))
         (sv (matrix-from-list sn 1 signal)))
    (mat-col-to-list (mat* tm sv) on)))

; Independent reference: the defining sum y[i] = sum_j x[j] k[i-j].
(define (conv-ref signal kernel)
  (let* ((sn (length signal)) (kn (length kernel)))
    (conv-ref-i signal kernel sn kn (+ sn kn -1) 0 nil)))

(define (conv-ref-i x k sn kn on i acc)
  (cond ((>= i on) (reverse acc))
        (t (conv-ref-i x k sn kn on (+ i 1) (cons (conv-ref-j x k sn kn i 0 0.0) acc)))))

(define (conv-ref-j x k sn kn i j sum)
  (cond ((>= j kn) sum)
        (t (let ((sk (- i j)))
          (cond ((and (>= sk 0) (< sk sn))
                 (conv-ref-j x k sn kn i (+ j 1) (+ sum (* (nth j k) (nth sk x)))))
                (t (conv-ref-j x k sn kn i (+ j 1) sum)))))))

; --- 1D tests ------------------------------------------------------------
(define sig (list 1.0 2.0 3.0 4.0))
(define kern (list 0.5 0.5))

(define cfull (conv1d sig kern 'full))
(print "Conv1d [1,2,3,4] * [0.5,0.5] (full) =" cfull)
; By hand: 0.5*[1] , 0.5*[1+2], 0.5*[2+3], 0.5*[3+4], 0.5*[4]
;        = 0.5, 1.5, 2.5, 3.5, 2.0
(check "conv1d-full-length" (= (length cfull) 5))
(check "conv1d-full-values" (lists-close cfull (list 0.5 1.5 2.5 3.5 2.0)))

(define c1d (conv1d sig kern 'same))
(print "Conv1d (same) =" c1d)
; 'same keeps the first (kn-1)/2 = 0 samples off the front, so the leading
; length-4 slice of the full result.
(check "conv1d-same-length" (= (length c1d) 4))
(check "conv1d-same-values" (lists-close c1d (list 0.5 1.5 2.5 3.5)))

; The matrix formulation must agree with the defining sum.
(check "toeplitz-matches-direct" (lists-close cfull (conv-ref sig kern)))

; Convolving with a unit impulse returns the signal unchanged.
(define impulse (list 1.0))
(check "conv1d-impulse-identity" (lists-close (conv1d sig impulse 'full) sig))
(check "conv1d-impulse-identity-same" (lists-close (conv1d sig impulse 'same) sig))

; A shifted impulse delays the signal: [0,0,1] * x starts with two zeros.
(define delay2 (list 0.0 0.0 1.0))
(print "Delayed by 2 =" (conv1d sig delay2 'full))
(check "conv1d-delay" (lists-close (conv1d sig delay2 'full)
                                   (list 0.0 0.0 1.0 2.0 3.0 4.0)))

; Convolution commutes.
(check "conv1d-commutes" (lists-close cfull (conv1d kern sig 'full)))

; The full output sums to sum(x) * sum(k): 10 * 1 = 10.
(print "Sum of full output =" (list-sum cfull))
(check "conv1d-sum-of-products"
       (close-p (list-sum cfull) (* (list-sum sig) (list-sum kern))))

; Convolution is associative: (x * a) * b = x * (a * b).
(define ka (list 1.0 2.0))
(define kb (list 1.0 -1.0 0.5))
(check "conv1d-associative"
       (lists-close (conv1d (conv1d sig ka 'full) kb 'full)
                    (conv1d sig (conv1d ka kb 'full) 'full)))

; --- cross-correlation ---------------------------------------------------
; Correlation is convolution with the time-reversed kernel.
(define (correlate a b) (conv1d b (reverse a) 'full))

(define autoc (correlate sig sig))
(print "Autocorrelation of [1,2,3,4] =" autoc)
; Zero lag sits at index (length a) - 1 = 3 and equals the energy
; 1 + 4 + 9 + 16 = 30.
(check "autocorr-peak-value" (close-p (nth 3 autoc) 30.0))
(check "autocorr-peak-is-max" (and (> (nth 3 autoc) (nth 2 autoc))
                                   (> (nth 3 autoc) (nth 4 autoc))))
; Autocorrelation is even: r[3-m] = r[3+m].
(check "autocorr-symmetric" (and (close-p (nth 2 autoc) (nth 4 autoc))
                                 (close-p (nth 1 autoc) (nth 5 autoc))
                                 (close-p (nth 0 autoc) (nth 6 autoc))))

; --- edge handling: zero-pad, wrap, reflect -------------------------------
; Odd-length kernel, radius r; y[i] = sum_m k[m] * xpad[i + r - m].
(define (pad-index idx n mode)
  (cond ((and (>= idx 0) (< idx n)) idx)
        ((eq mode 'wrap) (mod (+ (mod idx n) n) n))
        ((eq mode 'reflect) (cond ((< idx 0) (- 0 idx))
                                  (t (- (- (* 2 n) 2) idx))))
        (t -1)))

(define (conv1d-pad signal kernel mode)
  (let* ((sn (length signal))
         (kn (length kernel))
         (r (truncate (/ (- kn 1) 2))))
    (pad-conv-i signal kernel sn kn r mode 0 nil)))

(define (pad-conv-i x k sn kn r mode i acc)
  (cond ((>= i sn) (reverse acc))
        (t (pad-conv-i x k sn kn r mode (+ i 1)
                       (cons (pad-conv-m x k sn kn r mode i 0 0.0) acc)))))

(define (pad-conv-m x k sn kn r mode i m sum)
  (cond ((>= m kn) sum)
        (t (let ((p (pad-index (- (+ i r) m) sn mode)))
          (cond ((< p 0) (pad-conv-m x k sn kn r mode i (+ m 1) sum))
                (t (pad-conv-m x k sn kn r mode i (+ m 1)
                               (+ sum (* (nth m k) (nth p x))))))))))

(define third (/ 1.0 3.0))
(define box3 (list third third third))

(define pad-zero (conv1d-pad sig box3 'zero))
(define pad-wrap (conv1d-pad sig box3 'wrap))
(define pad-refl (conv1d-pad sig box3 'reflect))
(print "3-point average, zero-pad   =" pad-zero)
(print "3-point average, wrap       =" pad-wrap)
(print "3-point average, reflect    =" pad-refl)

; Hand-computed on x = [1,2,3,4]:
;   zero:    (0+1+2)/3, (1+2+3)/3, (2+3+4)/3, (3+4+0)/3 = 1, 2, 3, 7/3
;   wrap:    (4+1+2)/3, 2, 3, (3+4+1)/3                 = 7/3, 2, 3, 8/3
;   reflect: (2+1+2)/3, 2, 3, (3+4+3)/3                 = 5/3, 2, 3, 10/3
(check "pad-zero" (lists-close pad-zero (list 1.0 2.0 3.0 (/ 7.0 3.0))))
(check "pad-wrap" (lists-close pad-wrap (list (/ 7.0 3.0) 2.0 3.0 (/ 8.0 3.0))))
(check "pad-reflect" (lists-close pad-refl (list (/ 5.0 3.0) 2.0 3.0 (/ 10.0 3.0))))

; The interior samples never touch the border, so all three agree there.
(check "pad-interior-agrees" (and (close-p (nth 1 pad-zero) (nth 1 pad-wrap))
                                  (close-p (nth 1 pad-wrap) (nth 1 pad-refl))))

; Wrap-around (circular) convolution preserves the total sum exactly:
; sum(y) = sum(x) * sum(k) = 10 * 1 = 10.
(check "wrap-preserves-sum" (close-p (list-sum pad-wrap) (list-sum sig)))

; --- 2D convolution ------------------------------------------------------
; Flatten the image row-major, build the (h*w) x (h*w) block-Toeplitz matrix
; and let the native matmul do the work.  Zero padding, 'same output size.
(define (flatten-rows rows)
  (foldr append nil rows))

(define (build-2d-matrix kernel h w kh kw)
  (let* ((n (* h w))
         (rh (truncate (/ (- kh 1) 2)))
         (rw (truncate (/ (- kw 1) 2)))
         (tm (make-matrix n n 0)))
    (t2d-p tm kernel h w kh kw rh rw 0)))

(define (t2d-p tm kernel h w kh kw rh rw p)
  (cond ((>= p (* h w)) tm)
        (t (t2d-q tm kernel h w kh kw rh rw p 0)
           (t2d-p tm kernel h w kh kw rh rw (+ p 1)))))

(define (t2d-q tm kernel h w kh kw rh rw p q)
  (cond ((>= q (* h w)) tm)
        (t (let* ((i (truncate (/ p w))) (j (mod p w))
                  (a (truncate (/ q w))) (b (mod q w))
                  (ki (+ (- i a) rh))
                  (kj (+ (- j b) rw)))
          (cond ((and (and (>= ki 0) (< ki kh)) (and (>= kj 0) (< kj kw)))
                 (mat-set tm p q (nth kj (nth ki kernel))))
                (t nil))
          (t2d-q tm kernel h w kh kw rh rw p (+ q 1))))))

(define (chunk l k)
  (cond ((eq l nil) nil)
        (t (cons (take l k) (chunk (drop l k) k)))))

(define (conv2d image kernel)
  (let* ((h (length image)) (w (length (car image)))
         (kh (length kernel)) (kw (length (car kernel)))
         (tm (build-2d-matrix kernel h w kh kw))
         (iv (matrix-from-list (* h w) 1 (flatten-rows image))))
    (chunk (mat-col-to-list (mat* tm iv) (* h w)) w)))

(define image2d (list (list 1.0 2.0 3.0)
                      (list 4.0 5.0 6.0)
                      (list 7.0 8.0 9.0)))

; Identity kernel: a single 1 at the centre reproduces the image.
(define id-kernel (list (list 0.0 0.0 0.0)
                        (list 0.0 1.0 0.0)
                        (list 0.0 0.0 0.0)))
(define id-out (conv2d image2d id-kernel))
(print "2D identity kernel:" id-out)
(check "conv2d-identity" (rows-close id-out image2d))

; Shift kernel: convolution flips the kernel, so a 1 placed one row ABOVE the
; centre yields y[i][j] = x[i+1][j] — the image moves UP one row and the last
; row is zero-padded.
(define shift-kernel (list (list 0.0 1.0 0.0)
                           (list 0.0 0.0 0.0)
                           (list 0.0 0.0 0.0)))
(define shift-out (conv2d image2d shift-kernel))
(print "2D shift kernel:" shift-out)
(check "conv2d-shift" (rows-close shift-out
                                  (list (list 4.0 5.0 6.0)
                                        (list 7.0 8.0 9.0)
                                        (list 0.0 0.0 0.0))))

; 3x3 box blur (each tap 1/9), zero padded.
(define ninth (/ 1.0 9.0))
(define box2d (list (list ninth ninth ninth)
                    (list ninth ninth ninth)
                    (list ninth ninth ninth)))
(define blur (conv2d image2d box2d))
(print "2D box blur:" blur)
; Centre sees all nine pixels: (1+..+9)/9 = 45/9 = 5.
(check "conv2d-blur-centre" (close-p (nth 1 (nth 1 blur)) 5.0))
; Top-left corner sees only the 2x2 block 1,2,4,5: 12/9 = 1.3333333.
(check "conv2d-blur-corner" (close-p (nth 0 (nth 0 blur)) (/ 12.0 9.0)))
; Bottom-right corner sees 5,6,8,9: 28/9 = 3.1111111.
(check "conv2d-blur-corner2" (close-p (nth 2 (nth 2 blur)) (/ 28.0 9.0)))

; Independent reference implementation of the same 2D convolution.
(define (conv2d-ref image kernel)
  (let* ((h (length image)) (w (length (car image)))
         (kh (length kernel)) (kw (length (car kernel)))
         (rh (truncate (/ (- kh 1) 2)))
         (rw (truncate (/ (- kw 1) 2))))
    (ref2d-i image kernel h w kh kw rh rw 0 nil)))

(define (ref2d-i image kernel h w kh kw rh rw i acc)
  (cond ((>= i h) (reverse acc))
        (t (ref2d-i image kernel h w kh kw rh rw (+ i 1)
                    (cons (ref2d-j image kernel h w kh kw rh rw i 0 nil) acc)))))

(define (ref2d-j image kernel h w kh kw rh rw i j acc)
  (cond ((>= j w) (reverse acc))
        (t (ref2d-j image kernel h w kh kw rh rw i (+ j 1)
                    (cons (ref2d-tap image kernel h w kh kw rh rw i j 0 0 0.0) acc)))))

(define (ref2d-tap image kernel h w kh kw rh rw i j ki kj sum)
  (cond ((>= ki kh) sum)
        ((>= kj kw) (ref2d-tap image kernel h w kh kw rh rw i j (+ ki 1) 0 sum))
        (t (let ((a (- (+ i rh) ki)) (b (- (+ j rw) kj)))
          (cond ((and (and (>= a 0) (< a h)) (and (>= b 0) (< b w)))
                 (ref2d-tap image kernel h w kh kw rh rw i j ki (+ kj 1)
                            (+ sum (* (nth kj (nth ki kernel)) (nth b (nth a image))))))
                (t (ref2d-tap image kernel h w kh kw rh rw i j ki (+ kj 1) sum)))))))

(check "conv2d-matches-direct" (rows-close blur (conv2d-ref image2d box2d)))
(check "conv2d-shift-matches-direct"
       (rows-close shift-out (conv2d-ref image2d shift-kernel)))

(print "=== Convolution Done ===")
