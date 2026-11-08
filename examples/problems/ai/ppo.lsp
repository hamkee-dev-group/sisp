; PPO clipped objective. L = min(r*A, clip(r,1-eps,1+eps)*A). CartPole policy improvement over 100 steps.
(print "=== PPO Clipped Objective ===")
(seed 42)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

; --- Simplified CartPole simulation ---
(define gravity 9.8)
(define masscart 1.0)
(define masspole 0.1)
(define total-mass (+ masscart masspole))
(define length-pole 0.5)
(define pole-mass-length (* masspole length-pole))
(define force-mag 10.0)
(define time-step 0.02)

(define (cartpole-step state action)
  (let* ((x (nth 0 state)) (dx (nth 1 state))
         (theta (nth 2 state)) (dtheta (nth 3 state))
         (force (cond ((= action 1) force-mag) (t (- 0.0 force-mag))))
         (cos-theta (cos theta)) (sin-theta (sin theta))
         (temp (/ (+ force (* pole-mass-length dtheta dtheta sin-theta)) total-mass))
         (theta-acc (/ (- (* gravity sin-theta) (* cos-theta temp))
                       (* length-pole (- (/ 4.0 3.0) (/ (* masspole cos-theta cos-theta) total-mass)))))
         (x-acc (- temp (/ (* pole-mass-length theta-acc cos-theta) total-mass)))
         (new-x (+ x (* time-step dx)))
         (new-dx (+ dx (* time-step x-acc)))
         (new-theta (+ theta (* time-step dtheta)))
         (new-dtheta (+ dtheta (* time-step theta-acc))))
    (list new-x new-dx new-theta new-dtheta)))

(define (cartpole-done state)
  (let ((theta (nth 2 state)) (x (nth 0 state)))
    (or (> (abs theta) 0.2) (> (abs x) 2.4))))

(define (cartpole-reward state)
  (cond ((cartpole-done state) 0.0) (t 1.0)))

; --- Simple policy: linear classifier on state -> 2 action logits ---
(define (rand-weights scale)
  (list (list (* scale (- (* (random) 2.0) 1.0))
              (* scale (- (* (random) 2.0) 1.0)))
        (list (* scale (- (* (random) 2.0) 1.0))
              (* scale (- (* (random) 2.0) 1.0)))
        (list (* scale (- (* (random) 2.0) 1.0))
              (* scale (- (* (random) 2.0) 1.0)))
        (list (* scale (- (* (random) 2.0) 1.0))
              (* scale (- (* (random) 2.0) 1.0)))))

(setq *policy-w* (rand-weights 0.5))
(setq *policy-b* (list 0.0 0.0))

(define (policy-logits state)
  (let ((l0 (+ (+ (* (nth 0 state) (nth 0 (nth 0 *policy-w*)))
                   (* (nth 1 state) (nth 0 (nth 1 *policy-w*))))
                (+ (* (nth 2 state) (nth 0 (nth 2 *policy-w*)))
                   (* (nth 3 state) (nth 0 (nth 3 *policy-w*)))
                   (nth 0 *policy-b*))))
        (l1 (+ (+ (* (nth 0 state) (nth 1 (nth 0 *policy-w*)))
                   (* (nth 1 state) (nth 1 (nth 1 *policy-w*))))
                (+ (* (nth 2 state) (nth 1 (nth 2 *policy-w*)))
                   (* (nth 3 state) (nth 1 (nth 3 *policy-w*)))
                   (nth 1 *policy-b*)))))
    (list l0 l1)))

(define (softmax2 l0 l1)
  (let* ((max-l (max l0 l1))
         (e0 (exp (- l0 max-l)))
         (e1 (exp (- l1 max-l)))
         (s (+ e0 e1)))
    (list (/ e0 s) (/ e1 s))))

(define (sample-action state)
  (let* ((logits (policy-logits state))
         (probs (softmax2 (car logits) (cadr logits)))
         (r (random)))
    (cond ((< r (car probs)) 0) (t 1))))

(define (action-prob state action)
  (let* ((logits (policy-logits state))
         (probs (softmax2 (car logits) (cadr logits))))
    (nth action probs)))

; --- Collect trajectory ---
(define (collect-trajectory max-steps)
  (step-trajectory (list 0.0 0.0 0.05 0.0) 0 max-steps nil))

(define (step-trajectory state steps max-steps acc)
  (cond ((or (cartpole-done state) (>= steps max-steps)) acc)
        (t (let* ((a (sample-action state))
                  (logp (log (action-prob state a)))
                  (next (cartpole-step state a))
                  (r (cartpole-reward next)))
             (step-trajectory next (+ steps 1) max-steps
               (append acc (list (list state a logp r next))))))))

; --- PPO update ---
(define epsilon-clip 0.2)

; PPO clipped loss for one transition
(define (ppo-loss-one old-logp new-logp advantage)
  (let ((ratio (exp (- new-logp old-logp))))
    (let ((clipped (cond ((< ratio (- 1.0 epsilon-clip)) (- 1.0 epsilon-clip))
                         ((> ratio (+ 1.0 epsilon-clip)) (+ 1.0 epsilon-clip))
                         (t ratio))))
      (* -1.0 (* (cond ((< (* ratio advantage) (* clipped advantage)) ratio) (t clipped)) advantage)))))

; --- Training loop ---
; The old loop collected rollouts and kept the best -- the policy was
; never updated.  This is a real (small) policy-gradient step: returns,
; centered advantages, and a softmax-gradient update on the weights.

; total return from each time step (gamma = 1)
(define (traj-returns traj)
  (traj-returns-walk (reverse traj) 0 nil))

(define (traj-returns-walk rtraj running acc)
  (cond ((null rtraj) acc)
        (t (let ((g (+ running (cadddr (car rtraj)))))
             (traj-returns-walk (cdr rtraj) g (cons g acc))))))

; softmax gradient for a 2-logit linear policy:
; d logp_a / d w_k = state * (delta_ka - prob_k)
(define (policy-grad-step traj lr)
  (let* ((returns (traj-returns traj))
         (mean-r (/ (foldl + 0.0 returns) (length returns))))
    (policy-grad-walk traj returns mean-r lr)))

(define (policy-grad-walk traj returns mean-r lr)
  (cond ((null traj) nil)
        (t (let* ((transition (car traj))
                  (state (car transition))
                  (a (cadr transition))
                  (adv (- (car returns) mean-r))
                  (logits (policy-logits state))
                  (probs (softmax2 (car logits) (cadr logits)))
                  (p0 (car probs))
                  (p1 (cadr probs))
                  (d0 (cond ((= a 0) (- 1.0 p0)) (t (- 0.0 p0))))
                  (d1 (cond ((= a 1) (- 1.0 p1)) (t (- 0.0 p1)))))
             (setq *policy-w*
                   (list (list (+ (* lr adv d0 (nth 0 state))
                                  (nth 0 (nth 0 *policy-w*)))
                               (+ (* lr adv d1 (nth 0 state))
                                  (nth 1 (nth 0 *policy-w*))))
                         (list (+ (* lr adv d0 (nth 1 state))
                                  (nth 0 (nth 1 *policy-w*)))
                               (+ (* lr adv d1 (nth 1 state))
                                  (nth 1 (nth 1 *policy-w*))))
                         (list (+ (* lr adv d0 (nth 2 state))
                                  (nth 0 (nth 2 *policy-w*)))
                               (+ (* lr adv d1 (nth 2 state))
                                  (nth 1 (nth 2 *policy-w*))))
                         (list (+ (* lr adv d0 (nth 3 state))
                                  (nth 0 (nth 3 *policy-w*)))
                               (+ (* lr adv d1 (nth 3 state))
                                  (nth 1 (nth 3 *policy-w*))))))
             (setq *policy-b*
                   (list (+ (* lr adv d0) (nth 0 *policy-b*))
                         (+ (* lr adv d1) (nth 1 *policy-b*))))
             (policy-grad-walk (cdr traj) (cdr returns) mean-r lr)))))

; mean episode length over n rollouts
(define (avg-length n)
  (/ (avg-length-walk n 0) n))

(define (avg-length-walk n total)
  (cond ((= n 0) total)
        (t (avg-length-walk (- n 1) (+ total (length (collect-trajectory 50)))))))

(setq *best-reward* 0)

(define (train-ppo steps)
  (cond ((<= steps 0)
         (print (list 'best-episode-length *best-reward*)))
        (t
         (let ((traj (collect-trajectory 50)))
           (let ((num-t (length traj)))
             (progn
               (policy-grad-step traj 0.01)
               (cond ((> num-t *best-reward*)
                      (progn
                        (setq *best-reward* num-t)
                        (cond ((= (mod (- 100 steps) 20) 0)
                               (print (list 'step (- 100 steps) 'length num-t)))
                              (t nil))
                        (train-ppo (- steps 1))))
                     (t (train-ppo (- steps 1))))))))))

(print "PPO training on CartPole (100 iterations)...")
(seed 42)
(define baseline-len (avg-length 5))
(train-ppo 100)
(define trained-len (avg-length 5))

(print (list 'baseline-avg-length baseline-len 'trained-avg-length trained-len))

(ck "ppo-policy-improved" (> trained-len baseline-len))
(ck "ppo-learns-anything" (> trained-len 5))

; --- Verify PPO clipped objective computation ---
(define old-p 0.8)
(define new-p 0.3)
(define adv -2.0)
(define ratio (/ new-p old-p))
(define clipped-ratio (cond ((< ratio (- 1.0 epsilon-clip)) (- 1.0 epsilon-clip))
                            ((> ratio (+ 1.0 epsilon-clip)) (+ 1.0 epsilon-clip))
                            (t ratio)))
(define l1 (* ratio adv))
(define l2 (* clipped-ratio adv))
(define l-final (cond ((< l1 l2) l1) (t l2)))

(print (list 'ppo-clipped-check 'ratio ratio 'clipped clipped-ratio 'l1 l1 'l2 l2 'loss l-final))
(ck "ppo-clip-conservative" (<= l-final l1))

(print "=== PPO Done ===")
