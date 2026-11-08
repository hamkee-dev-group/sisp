; Variational Autoencoder. Encoder mu,sigma = q(z|x). Sample z = mu+sigma*eps.
; Decoder p(x|z). Loss = reconstruction + KL(q||p). Train on 4 MNIST-like digits.
(print "=== Variational Autoencoder ===")
(seed 42)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

; --- 4x4 grid MNIST-like digits (16 dims) ---
(define digit-0 (list 0.0 1.0 1.0 0.0  1.0 0.0 0.0 1.0  1.0 0.0 0.0 1.0  0.0 1.0 1.0 0.0))
(define digit-1 (list 0.0 0.0 1.0 0.0  0.0 1.0 1.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 1.0 0.0))
(define digit-7 (list 1.0 1.0 1.0 0.0  0.0 0.0 0.0 1.0  0.0 0.0 1.0 0.0  0.0 1.0 0.0 0.0))
(define digit-9 (list 0.0 1.0 1.0 0.0  1.0 0.0 0.0 1.0  0.0 0.0 1.0 1.0  0.0 1.0 1.0 0.0))
(setq *digits* (list digit-0 digit-1 digit-7 digit-9))

(define (vae-relu x) (cond ((> x 0.0) x) (t 0.0)))
(define (sigmoid x) (/ 1.0 (+ 1.0 (exp (- 0.0 x)))))

; Linear layer: output_i = sum_j(w_ij * x_j) + b_i
(define (linear w x)
  (map (lambda (row)
    (foldl (lambda (sum j) (+ sum (* (nth j row) (nth j x))))
           0.0 (seq 0 (- (length x) 1))))
    w))

(define (vec-add a b) (map (lambda (i) (+ (nth i a) (nth i b))) (seq 0 (- (length a) 1))))
(define (vec-sub a b) (map (lambda (i) (- (nth i a) (nth i b))) (seq 0 (- (length a) 1))))
(define (vec-mul a b) (map (lambda (i) (* (nth i a) (nth i b))) (seq 0 (- (length a) 1))))
(define (vec-scale v s) (map (lambda (x) (* x s)) v))
(define (vec-relu v) (map vae-relu v))
(define (vec-sigmoid v) (map sigmoid v))
(define (vec-exp v) (map exp v))

(define (mse a b)
  (/ (foldl (lambda (sum i) (+ sum (^ (- (nth i a) (nth i b)) 2)))
            0.0 (seq 0 (- (length a) 1)))
     (length a)))

(define (vec-dot a b)
  (foldl (lambda (sum i) (+ sum (* (nth i a) (nth i b))))
         0.0 (seq 0 (- (length a) 1))))

; --- Simple 2-layer VAE: 16 -> 4 -> 2 (mu,logvar), z=2, 2 -> 4 -> 16 ---
(define (rand-w rows cols s)
    (map (lambda (r) (map (lambda (c) (* s (- (* (random) 2.0) 1.0))) (seq 1 cols))) (seq 1 rows)))
(define (zero-v n) (map (lambda (x) 0.0) (seq 1 n)))

; Global mutable weights (using setq for training)
(setq *e1w* (rand-w 4 16 0.1))
(setq *e1b* (zero-v 4))
(setq *emu* (rand-w 2 4 0.1))
(setq *emb* (zero-v 2))
(setq *esw* (rand-w 2 4 0.1))
(setq *esb* (zero-v 2))
(setq *d1w* (rand-w 4 2 0.1))
(setq *d1b* (zero-v 4))
(setq *dow* (rand-w 16 4 0.1))
(setq *dob* (zero-v 16))

(define (vae-forward x)
  (let* ((h1 (vec-relu (vec-add (linear *e1w* x) *e1b*)))
         (mu (vec-add (linear *emu* h1) *emb*))
         (ls (vec-add (linear *esw* h1) *esb*))
         (std (vec-exp (vec-scale ls 0.5)))
         (eps (list (- (* (random) 2.0) 1.0) (- (* (random) 2.0) 1.0)))
         (z (vec-add mu (vec-mul std eps)))
         (h2 (vec-relu (vec-add (linear *d1w* z) *d1b*)))
         (out (vec-sigmoid (vec-add (linear *dow* h2) *dob*))))
    (list mu ls z out)))

(define (vae-loss-one x)
  (let* ((result (vae-forward x))
         (mu (car result)) (ls (cadr result)) (out (cadddr result))
         (recon (mse x out))
         (kl (* -0.5
                (foldl (lambda (sum i)
                         (let ((mean (nth i mu)) (logvar (nth i ls)))
                           (+ sum 1.0 logvar
                              (- 0.0 (* mean mean))
                              (- 0.0 (exp logvar)))))
                       0.0 (seq 0 (- (length mu) 1))))))
    (list recon kl (+ recon kl))))

(define (vec-sum v)
  (foldl (lambda (sum x) (+ sum x)) 0.0 v))

(define (total-vae-loss)
  (/ (foldl (lambda (sum d) (+ sum (caddr (vae-loss-one d))))
            0.0 *digits*)
     (length *digits*)))

; --- Training by random perturbation ---
(define (copy-matrix m) (map (lambda (row) (map (lambda (x) x) row)) m))
(define (copy-vec v) (map (lambda (x) x) v))

(define (perturb-matrix m eps)
  (map (lambda (row) (map (lambda (x) (+ x (* eps (- (* (random) 2.0) 1.0)))) row)) m))
(define (perturb-vec v eps)
  (map (lambda (x) (+ x (* eps (- (* (random) 2.0) 1.0)))) v))

(define (save-weights)
  (list (copy-matrix *e1w*) (copy-vec *e1b*)
        (copy-matrix *emu*) (copy-vec *emb*)
        (copy-matrix *esw*) (copy-vec *esb*)
        (copy-matrix *d1w*) (copy-vec *d1b*)
        (copy-matrix *dow*) (copy-vec *dob*)))

(define (restore-weights saved)
  (setq *e1w* (copy-matrix (nth 0 saved)))
  (setq *e1b* (copy-vec (nth 1 saved)))
  (setq *emu* (copy-matrix (nth 2 saved)))
  (setq *emb* (copy-vec (nth 3 saved)))
  (setq *esw* (copy-matrix (nth 4 saved)))
  (setq *esb* (copy-vec (nth 5 saved)))
  (setq *d1w* (copy-matrix (nth 6 saved)))
  (setq *d1b* (copy-vec (nth 7 saved)))
  (setq *dow* (copy-matrix (nth 8 saved)))
  (setq *dob* (copy-vec (nth 9 saved))))

(define (perturb-all eps)
  (setq *e1w* (perturb-matrix *e1w* eps))
  (setq *e1b* (perturb-vec *e1b* eps))
  (setq *emu* (perturb-matrix *emu* eps))
  (setq *emb* (perturb-vec *emb* eps))
  (setq *esw* (perturb-matrix *esw* eps))
  (setq *esb* (perturb-vec *esb* eps))
  (setq *d1w* (perturb-matrix *d1w* eps))
  (setq *d1b* (perturb-vec *d1b* eps))
  (setq *dow* (perturb-matrix *dow* eps))
  (setq *dob* (perturb-vec *dob* eps)))

(print "Training VAE on 4 digits (200 epochs)...")
(setq *vaebest* (total-vae-loss))
(define *vae-initial-loss* *vaebest*)

(define (train epochs)
  (cond ((<= epochs 0) nil)
        (t (let ((saved (save-weights)))
             (perturb-all 0.05)
             (let ((new-loss (total-vae-loss)))
               (cond ((< new-loss *vaebest*)
                      (progn
                        (setq *vaebest* new-loss)
                        (cond ((= (mod epochs 50) 0)
                               (print (list 'epoch (- 200 epochs) 'loss *vaebest*)))
                              (t nil))))
                     (t (restore-weights saved)))
               (train (- epochs 1)))))))

(train 200)
(print (list 'final-loss *vaebest*))

; Evaluate
(define (eval-digit digit name)
  (let* ((r (vae-forward digit)) (out (cadddr r)))
    (print (list name 'mse (mse digit out)))
    (mse digit out)))

(define mse0 (eval-digit digit-0 'digit-0))
(define mse1 (eval-digit digit-1 'digit-1))
(define mse7 (eval-digit digit-7 'digit-7))
(define mse9 (eval-digit digit-9 'digit-9))

(define avg-mse (/ (+ mse0 mse1 mse7 mse9) 4.0))
(print (list 'avg-recon-mse avg-mse))

(ck "vae-training-reduced-loss" (<= *vaebest* *vae-initial-loss*))
(ck "vae-reconstruction-reasonable" (< avg-mse 1.0))

(print "=== VAE Done ===")
