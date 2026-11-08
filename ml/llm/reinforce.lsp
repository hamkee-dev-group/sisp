; reinforce.lsp — an actual autodifferentiated REINFORCE bandit.
; Use ml/llm/run-reinforce.sh.

(seed 5)
(define *reinforce-arm-probabilities* (list 0.20 0.50 0.90 0.35))
(define *reinforce-preferences*
  (list (val 0.0) (val 0.0) (val 0.0) (val 0.0)))
(define *reinforce-total-reward* 0.0)
(define *reinforce-count* 0)
(define *reinforce-pulls* (list 0 0 0 0))

(define (reinforce-reward action)
  (cond ((< (random) (nth action *reinforce-arm-probabilities*)) 1.0)
        (t 0.0)))

(define (reinforce-increment-index xs target index)
  (cond ((eq xs nil) nil)
        ((= index target) (cons (+ (car xs) 1) (cdr xs)))
        (t (cons (car xs)
                 (reinforce-increment-index (cdr xs) target (+ index 1))))))

(define (reinforce-step)
  (llm-zero-gradients *reinforce-preferences*)
  (let ((probability-values (llm-softmax-values *reinforce-preferences*)))
    (let ((action (llm-sample-index (map vdata probability-values) (random) 0)))
      (let ((reward (reinforce-reward action))
            (baseline (cond ((= *reinforce-count* 0) 0.0)
                            (t (/ *reinforce-total-reward* *reinforce-count*)))))
        (let ((advantage (- reward baseline)))
          ; L = -advantage * log pi(action); autodiff supplies every dL/dH.
          (let ((loss (gscale (glog (nth action probability-values))
                              (- 0.0 advantage))))
            (backward loss)
            (llm-sgd-step *reinforce-preferences* 0.1 5.0)))
        (setq *reinforce-count* (+ *reinforce-count* 1))
        (setq *reinforce-total-reward* (+ *reinforce-total-reward* reward))
        (setq *reinforce-pulls*
          (reinforce-increment-index *reinforce-pulls* action 0))
        (cond ((= (mod *reinforce-count* 200) 0)
          (print (list (quote step) *reinforce-count*
            (quote policy) (map vdata (llm-softmax-values *reinforce-preferences*))
            (quote average-reward)
              (/ *reinforce-total-reward* *reinforce-count*))))
          (t nil))
        reward))))

(print "=== autodifferentiated REINFORCE bandit ===")
(print (list (quote true-win-probabilities) *reinforce-arm-probabilities*))

