; ============================================================
; 102 — Boolean-Function Transforms (TAOCP Vol 4A, 7.1.1, 7.1.2)
; For Boolean functions of 3 variables, compute:
; - Mobius (Zhegalkin) transform: truth-table -> ANF coefficients
; - Algebraic Normal Form: XOR of ANDs, 2^n coefficients
; - Walsh-Hadamard spectrum: 2^n spectral coefficients
; Verify forward then inverse gives identity. Test all 256 functions.
; ============================================================

(print "=== Boolean Function Transforms ===")


(define n-vars 3)
(define n-rows (^ 2 n-vars))

; ---------- helpers ----------
(define (list-set xs i v)
  (cond ((= i 0) (cons v (cdr xs)))
        (t (cons (car xs) (list-set (cdr xs) (- i 1) v)))))

(define (xor-bool a b)
  (cond ((and a b) nil)
        ((and (not a) (not b)) nil)
        (t t)))

(define (int-to-bits val nbits)
  (labels (((go v sh acc)
    (cond ((= sh 0) acc)
          (t (go (truncate (/ v 2))
                 (- sh 1)
                 (cons (not (= (mod v 2) 0)) acc))))))
    (go val nbits nil)))

(define (fun-num-to-tv fnum)
  (int-to-bits fnum n-rows))

; ---------- Möbius (Zhegalkin) transform ----------
; Fast subset-sum transform over GF(2). Involution: applying twice = identity.
(define (mobius-transform fv n)
  (let ((size (^ 2 n)))
    (mobius-outer fv n 0)))

(define (mobius-outer fv n j)
  (cond ((= j n) fv)
        (t (let* ((step (^ 2 j))
                  (size (^ 2 n))
                  (new-fv (mobius-inner fv n j step 0)))
          (mobius-outer new-fv n (+ j 1))))))

(define (mobius-inner fv n j step i)
  (let ((size (^ 2 n)))
    (cond ((>= i size) fv)
          ((= (logand i step) 0) (mobius-inner fv n j step (+ i 1)))
          (t (let* ((a (nth (- i step) fv))
                    (b (nth i fv))
                    (xored (xor-bool a b)))
            (mobius-inner (list-set fv i xored) n j step (+ i 1)))))))

(define (logand a b)
  (labels (((go x y shift acc)
    (cond ((= shift 0) acc)
          (t (go (truncate (/ x 2))
                 (truncate (/ y 2))
                 (- shift 1)
                 (+ acc (* (cond ((and (= (mod x 2) 1) (= (mod y 2) 1)) 1) (t 0))
                           (^ 2 (- shift 1)))))))))
    (go a b 32 0)))

; verify Möbius involution on all 256 functions
(define (verify-mobius n)
  (let ((max-n (+ (^ 2 (^ 2 n)) -1)))
    (labels (((go fnum)
      (cond ((> fnum max-n) t)
            (t (let* ((tv (fun-num-to-tv fnum))
                      (anf (mobius-transform tv n))
                      (back (mobius-transform anf n)))
            (cond ((equal back tv) (go (+ fnum 1)))
                  (t nil)))))))
      (go 0))))

(check "mobius-involution-all-256-3var" (verify-mobius 3))

; ---------- Walsh-Hadamard transform ----------
; WH[f](w) = sum_x (-1)^{f(x) + dot(w,x)}
; Implemented as fast Walsh-Hadamard (FWHT)

(define (wh-outer vec n j)
  (cond ((= j n) vec)
        (t (let* ((step (^ 2 j))
                  (new-vec (wh-inner vec n j step 0)))
          (wh-outer new-vec n (+ j 1))))))

(define (wh-inner vec n j step i)
  (let ((size (^ 2 n)))
    (cond ((>= i size) vec)
          ((= (logand i step) 0)
           (let* ((lo (nth i vec))
                  (hi (nth (+ i step) vec))
                  (sum (+ lo hi))
                  (diff (- lo hi)))
             (wh-inner (list-set (list-set vec i sum) (+ i step) diff)
                       n j step (+ i 1))))
          (t (wh-inner vec n j step (+ i 1))))))

(define (fwht vec n)
  (wh-outer vec n 0))

(define (walsh-hadamard fv n)
  (let ((g (map (lambda (b) (cond (b -1) (t 1))) fv)))
    (fwht g n)))

; Inverse: WH(WH(f))/2^n = f
; Apply FWHT twice (first with +/-1 conversion, second directly on integers)
(define (verify-wh n)
  (let ((max-n (+ (^ 2 (^ 2 n)) -1))
        (size (^ 2 n)))
    (labels (((go fnum)
      (cond ((> fnum max-n) t)
            (t (let* ((tv (fun-num-to-tv fnum))
                      (spec (walsh-hadamard tv n))
                      (wh2 (fwht spec n))
                      (recovered (map (lambda (v) (< v 0)) wh2)))
            (cond ((equal recovered tv) (go (+ fnum 1)))
                  (t nil)))))))
      (go 0))))

(check "walsh-hadamard-inversion-all-256-3var" (verify-wh 3))

; ---------- ANF display ----------
(define (anf-terms anf n)
  (let ((size (^ 2 n)))
    (anf-terms-go anf n 0 size nil)))

(define (anf-terms-go anf n i size acc)
  (cond ((>= i size) (reverse acc))
        ((not (nth i anf)) (anf-terms-go anf n (+ i 1) size acc))
        (t (let* ((bits (int-to-bits i n))
                  (term (build-term bits n 0 nil)))
          (anf-terms-go anf n (+ i 1) size (cons term acc))))))

(define (build-term bits n j acc)
  (cond ((>= j n) acc)
        ((nth j bits) (build-term bits n (+ j 1) (cons (+ j 1) acc)))
        (t (build-term bits n (+ j 1) acc))))

(define (sample-f name bits)
  (let ((tv (fun-num-to-tv bits)))
    (let ((anf (mobius-transform tv 3)))
      (print (list name (quote truth-table) tv))
      (print (list name (quote anf-coeffs) anf))
      (print (list name (quote anf-terms) (anf-terms anf 3)))
      (let ((spec (walsh-hadamard tv 3)))
        (print (list name (quote walsh-spectrum) spec))))))

(sample-f (quote f-x1) 240)
(sample-f (quote f-x3) 170)
(sample-f (quote f-majority) 232)

(print "=== Boolean Transforms Done ===")
