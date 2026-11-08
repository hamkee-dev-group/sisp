; matrix-engine.lsp — parameter, gradient, optimizer, and matrix calculus helpers.
; This path uses native OBJ_MATRIX kernels and explicit reverse-mode derivatives.
; It is independent from the educational scalar Value engine in engine.lsp.

; A parameter is (mparam data gradient first-moment second-moment decay-p).
; Biases and normalization gains opt out of AdamW's decoupled weight decay.
(define (mparam-with-decay data decay-p)
  (list (quote mparam) data
    (make-matrix (mat-rows data) (mat-cols data))
    (make-matrix (mat-rows data) (mat-cols data))
    (make-matrix (mat-rows data) (mat-cols data))
    decay-p))
(define (mparam data) (mparam-with-decay data t))
(define (mparam-no-decay data) (mparam-with-decay data nil))
(define (mp-data p) (cadr p))
(define (mp-grad p) (caddr p))
(define (mp-m p) (cadddr p))
(define (mp-v p) (car (cddddr p)))
(define (mp-decay-p p) (cadr (cddddr p)))

(define (mparam-randn rows cols scale)
  (mparam (matrix-randn rows cols scale)))
(define (mparam-zero rows cols)
  (mparam (make-matrix rows cols)))
(define (mparam-zero-no-decay rows cols)
  (mparam-no-decay (make-matrix rows cols)))

(define (matrix-copy x) (* x 1.0))
(define (matrix-square x) (mat-mul x x))
(define (matrix-ones rows cols) (make-matrix rows cols 1.0))

(define (matrix-add3 a b c) (+ (+ a b) c))

(define (matrix-repeat-call count function)
  (cond ((= count 0) nil)
        (t (cons (function)
                 (matrix-repeat-call (- count 1) function)))))

; Broadcast an rows x 1 column across `cols` columns using native matmul.
(define (matrix-broadcast-column column cols)
  (mat* column (matrix-ones 1 cols)))

(define (matrix-row-to-list-walk matrix row column)
  (cond ((= column (mat-cols matrix)) nil)
        (t (cons (mat-ref matrix row column)
                 (matrix-row-to-list-walk matrix row (+ column 1))))))
(define (matrix-row-to-list matrix row)
  (matrix-row-to-list-walk matrix row 0))

(define (mparams-zero parameters)
  (foreach (lambda (p) (mat-zero (mp-grad p))) parameters))

(define (mparams-scale-gradients parameters scale)
  (foreach (lambda (p) (mat-scale-into (mp-grad p) scale)) parameters))

(define (mparams-count parameters)
  (foldl (lambda (count p)
    (+ count (* (mat-rows (mp-data p)) (mat-cols (mp-data p)))))
    0 parameters))

(define (mparams-gradient-squared parameters)
  (foldl (lambda (total p)
    (let ((n (mat-norm (mp-grad p)))) (+ total (* n n))))
    0.0 parameters))
(define (mparams-gradient-norm parameters)
  (sqrt (mparams-gradient-squared parameters)))

(define (matrix-finite-number value)
  (and (= value value) (< (abs value) 1000000000000000000000000000000.0)))
(define (matrix-finite-matrix matrix)
  (matrix-finite-number (mat-norm matrix)))
(define (mparam-finite p)
  (and (matrix-finite-matrix (mp-data p))
       (matrix-finite-matrix (mp-grad p))
       (matrix-finite-matrix (mp-m p))
       (matrix-finite-matrix (mp-v p))))
(define (mparams-finite parameters)
  (foldl (lambda (ok p) (and ok (mparam-finite p))) t parameters))

(define (mparams-clip parameters maximum)
  (let ((norm (mparams-gradient-norm parameters)))
    (let ((scale (cond ((> norm maximum) (/ maximum (+ norm 0.000000000001)))
                       (t 1.0))))
      (cond ((< scale 1.0)
        (foreach (lambda (p) (mat-scale-into (mp-grad p) scale)) parameters))
        (t nil))
      norm)))

(define (mparam-adamw-step p step learning-rate beta1 beta2 epsilon weight-decay)
  ; m = beta1*m + (1-beta1)*g
  (mat-scale-into (mp-m p) beta1)
  (mat-axpy (mp-m p) (- 1.0 beta1) (mp-grad p))
  ; v = beta2*v + (1-beta2)*g^2
  (mat-scale-into (mp-v p) beta2)
  (mat-axpy (mp-v p) (- 1.0 beta2) (matrix-square (mp-grad p)))
  ; Bias-corrected Adam direction plus decoupled weight decay.
  (let ((mhat (* (mp-m p) (/ 1.0 (- 1.0 (^ beta1 step)))))
        (vhat (* (mp-v p) (/ 1.0 (- 1.0 (^ beta2 step))))))
    (let ((direction
      (mat-div mhat (+ (sqrt vhat) epsilon))))
      (cond ((and (mp-decay-p p) (> weight-decay 0.0))
        (mat-axpy direction weight-decay (mp-data p)))
        (t nil))
      (mat-axpy (mp-data p) (- 0.0 learning-rate) direction))))

(define (mparams-adamw-step parameters step learning-rate beta1 beta2 epsilon weight-decay)
  (foreach (lambda (p)
    (mparam-adamw-step p step learning-rate beta1 beta2 epsilon weight-decay))
    parameters))

; Row-wise softmax backward: dX = Y * (dY - sum(dY*Y)).
(define (matrix-softmax-backward probabilities upstream)
  (let ((row-dot (mat-sum-axis (mat-mul probabilities upstream) 1)))
    (mat-mul probabilities
      (- upstream
        (matrix-broadcast-column row-dot (mat-cols probabilities))))))

; Parameter-free row-wise RMS normalization.
; Forward returns (output cache), cache=(input inverse-rms-column).
(define (matrix-rmsnorm-forward input)
  (let ((dimension (mat-cols input)))
    (let ((mean-square
      (* (mat-sum-axis (matrix-square input) 1) (/ 1.0 dimension))))
      (let ((inverse-rms
        (mat-div (matrix-ones (mat-rows input) 1)
          (sqrt (+ mean-square 0.00001)))))
        (list
          (mat-mul input (matrix-broadcast-column inverse-rms dimension))
          (list input inverse-rms))))))

(define (matrix-rmsnorm-backward upstream cache)
  (let ((input (car cache))
        (inverse-rms (cadr cache)))
    (let ((dimension (mat-cols input)))
      (let ((mean-product
        (* (mat-sum-axis (mat-mul upstream input) 1)
                   (/ 1.0 dimension))))
        (let ((inverse-cubed
          (mat-mul inverse-rms (mat-mul inverse-rms inverse-rms))))
          (-
            (mat-mul upstream
              (matrix-broadcast-column inverse-rms dimension))
            (mat-mul input
              (matrix-broadcast-column
                (mat-mul mean-product inverse-cubed) dimension))))))))

(define (matrix-tanh-backward upstream output)
  (mat-mul upstream
    (+ (* (matrix-square output) -1.0) 1.0)))

; SiLU/Swish: x * sigmoid(x). Forward returns (output cache).
(define (matrix-silu-forward input)
  (let ((sigmoid
    (mat-div (matrix-ones (mat-rows input) (mat-cols input))
      (+ (exp (* input -1.0)) 1.0))))
    (list (mat-mul input sigmoid) (list input sigmoid))))

(define (matrix-silu-backward upstream cache)
  (let ((input (car cache))
        (sigmoid (cadr cache)))
    (mat-mul upstream
      (mat-mul sigmoid
        (+
          (mat-mul input (+ (* sigmoid -1.0) 1.0))
          1.0)))))
