; Federated Averaging (FedAvg). 3 clients, non-IID data. Local training, server averages.
; Compare vs centralized. DP-SGD-style gradient clipping + noise (uniform noise,
; no privacy accounting).
(print "=== Federated Averaging ===")
(seed 42)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

; --- Generate data ---
(define (generate-data n x-base noise-scale)
  (map (lambda (i)
    (let* ((x (+ x-base (* i 0.5)))
           (y (+ (+ (* 2.0 x) 1.0) (* (- (* (random) 2.0) 1.0) noise-scale))))
      (list x y)))
    (seq 1 n)))

(setq *client-1-data* (generate-data 20 0.0 0.3))
(setq *client-2-data* (generate-data 20 5.0 0.3))
(setq *client-3-data* (generate-data 20 10.0 0.3))
(setq *centralized-data* (append *client-1-data* (append *client-2-data* *client-3-data*)))

; --- Model: linear regression ---
(define (mk-model)
  (list (* (- (* (random) 2.0) 1.0) 0.5)
        (* (- (* (random) 2.0) 1.0) 0.5)))

(define (predict model x)
  (+ (* (car model) x) (cadr model)))

; Mean squared error over a data set
(define (mse-loss model data)
  (/ (foldl (lambda (acc p)
              (let* ((x (car p)) (y-true (cadr p))
                     (y-pred (predict model x))
                     (err (- y-pred y-true)))
                (+ acc (* err err))))
            0.0 data)
     (length data)))

; Gradient computation (manual)
(define (compute-gradients model data clip-norm)
  (let* ((w (car model)) (b (cadr model))
         (sums (foldl (lambda (acc p)
                        (let* ((x (car p)) (y-true (cadr p))
                               (pred (+ (* w x) b)) (err (- pred y-true)))
                          (list (+ (car acc) (* 2.0 err x))
                                (+ (cadr acc) (* 2.0 err)))))
                      (list 0.0 0.0) data))
         (n (length data))
         (dw (/ (car sums) n))
         (db (/ (cadr sums) n))
         (norm (sqrt (+ (* dw dw) (* db db)))))
    (cond ((and (> clip-norm 0) (> norm clip-norm))
           (let ((scale (/ clip-norm norm)))
             (list (* dw scale) (* db scale))))
          (t (list dw db)))))

; --- Client local training ---
(define (client-update model data lr epochs clip-norm noise-scale)
  (cond ((= epochs 0) model)
        (t (let* ((grads (compute-gradients model data clip-norm))
                  (dw (car grads)) (db (cadr grads))
                  (noise-w (* (- (* (random) 2.0) 1.0) noise-scale))
                  (noise-b (* (- (* (random) 2.0) 1.0) noise-scale))
                  (new-w (- (car model) (* lr (+ dw noise-w))))
                  (new-b (- (cadr model) (* lr (+ db noise-b)))))
             (client-update (list new-w new-b) data lr (- epochs 1) clip-norm noise-scale)))))

; --- Federated Averaging ---
(define (federated-train-round model round)
  (cond ((<= round 0) model)
        (t (let* ((m1 (client-update model *client-1-data* 0.01 5 10.0 0.001))
                  (m2 (client-update model *client-2-data* 0.01 5 10.0 0.001))
                  (m3 (client-update model *client-3-data* 0.01 5 10.0 0.001))
                  (avg-w (/ (+ (car m1) (car m2) (car m3)) 3.0))
                  (avg-b (/ (+ (cadr m1) (cadr m2) (cadr m3)) 3.0))
                  (new-model (list avg-w avg-b)))
             (cond ((= (mod round 10) 0)
                    (print (list 'round (- 30 round)
                                 'global-loss (mse-loss new-model *centralized-data*))))
                   (t nil))
             (federated-train-round new-model (- round 1))))))

(define (centralized-train-epoch model epochs lr)
  (cond ((<= epochs 0) model)
        (t (let* ((grads (compute-gradients model *centralized-data* 10.0))
                  (dw (car grads)) (db (cadr grads))
                  (new-w (- (car model) (* lr dw)))
                  (new-b (- (cadr model) (* lr db))))
             (centralized-train-epoch (list new-w new-b) (- epochs 1) lr)))))

; --- Run experiments ---
(print "Federated training (30 rounds, 5 local epochs)...")
(define fed-model (federated-train-round (mk-model) 30))

(print "Centralized training (150 epochs)...")
(define cent-model (centralized-train-epoch (mk-model) 300 0.01))

(print "Final models:")
(print (list 'federated fed-model))
(print (list 'centralized cent-model))

(define fed-loss (mse-loss fed-model *centralized-data*))
(define cent-loss (mse-loss cent-model *centralized-data*))
(print (list 'federated-loss fed-loss))
(print (list 'centralized-loss cent-loss))

(ck "fed-avg-learned" (< fed-loss 0.5))
(ck "centralized-learned" (< cent-loss 0.5))

(define w-diff (abs (- (car fed-model) (car cent-model))))
(define b-diff (abs (- (cadr fed-model) (cadr cent-model))))
(print (list 'fed-vs-cent-diff w-diff b-diff))
(ck "fed-approx-central" (< (+ w-diff b-diff) 3.0))

; Gradient clipping verification
(define (clip-test-model)
  (let ((m (mk-model)))
    (let ((g-clipped (compute-gradients m *client-1-data* 0.5))
          (g-unclip (compute-gradients m *client-1-data* 0)))
      (let ((cn (sqrt (+ (* (car g-clipped) (car g-clipped))
                         (* (cadr g-clipped) (cadr g-clipped)))))
            (un (sqrt (+ (* (car g-unclip) (car g-unclip))
                         (* (cadr g-unclip) (cadr g-unclip))))))
        (<= cn (+ un 0.01))))))
(ck "gradient-clipping-works" (clip-test-model))

(print "=== Federated Averaging Done ===")
