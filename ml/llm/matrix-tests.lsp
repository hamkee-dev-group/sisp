; matrix-tests.lsp — calculus and end-to-end gradient checks for manual backprop.

(llm-reset-tests)
(print "=== native matrix LLM checks ===")

; Stable native softmax and its explicit Jacobian-vector product.
(define mt-probabilities
  (mat-softmax (matrix-from-list 1 3 (list 1000.0 1001.0 999.0))))
(llm-check-close "matrix-softmax-sum" (mat-sum mt-probabilities) 1.0
  0.000000000001 0.000000000001)
(define mt-softmax-upstream (matrix-from-list 1 3 (list 0.3 -0.2 0.7)))
(define mt-softmax-gradient
  (matrix-softmax-backward mt-probabilities mt-softmax-upstream))
(llm-check-close "matrix-softmax-gradient-sums-zero"
  (mat-sum mt-softmax-gradient) 0.0 0.000000000001 0.000000000001)
(llm-check "top-k-one-is-argmax"
  (= (llm-sample-top-k-logits (list -2.0 4.0 1.0) 0.8 1) 1))
(llm-check "top-p-retains-required-nucleus"
  (= (llm-sample-top-k-top-p-logits (list 4.0 1.0 -2.0) 1.0 3 0.5) 0))
(llm-check "top-p-keeps-smallest-mass-prefix"
  (equal
    (llm-nucleus-prefix
      (list (list 0 3.0) (list 1 2.0) (list 2 1.0))
      (list 0.6 0.3 0.1) 0.0 0.8)
    (list (list 0 3.0) (list 1 2.0))))
(define mt-logsumexp-loss
  (car (mtransformer-cross-entropy-forward
    (matrix-from-list 1 3 (list 1000.0 1001.0 999.0)) (list 0))))
(llm-check-close "matrix-logsumexp-cross-entropy"
  mt-logsumexp-loss
  (+ 1.0 (log (+ 1.0 (exp -1.0) (exp -2.0))))
  0.000000000001 0.000000000001)

; RMSNorm invariants and directional finite difference.
(define mt-rms-input (matrix-from-list 2 3 (list 1.0 2.0 3.0 -2.0 1.0 0.5)))
(define mt-rms-result (matrix-rmsnorm-forward mt-rms-input))
(define mt-rms-upstream (matrix-from-list 2 3 (list 0.2 -0.1 0.4 0.7 0.3 -0.2)))
(define mt-rms-gradient
  (matrix-rmsnorm-backward mt-rms-upstream (cadr mt-rms-result)))
(define mt-rms-original (mat-ref mt-rms-input 0 1))
(define mt-epsilon 0.000001)
(mat-set mt-rms-input 0 1 (+ mt-rms-original mt-epsilon))
(define mt-rms-plus
  (mat-sum (mat-mul (car (matrix-rmsnorm-forward mt-rms-input)) mt-rms-upstream)))
(mat-set mt-rms-input 0 1 (- mt-rms-original mt-epsilon))
(define mt-rms-minus
  (mat-sum (mat-mul (car (matrix-rmsnorm-forward mt-rms-input)) mt-rms-upstream)))
(mat-set mt-rms-input 0 1 mt-rms-original)
(llm-check-close "matrix-rmsnorm-gradient"
  (mat-ref mt-rms-gradient 0 1)
  (/ (- mt-rms-plus mt-rms-minus) (* 2.0 mt-epsilon))
  0.00001 0.00001)

; SiLU is used by the gated feed-forward path.
(define mt-silu-input (matrix-from-list 1 2 (list -0.7 1.3)))
(define mt-silu-result (matrix-silu-forward mt-silu-input))
(define mt-silu-upstream (matrix-from-list 1 2 (list 0.4 -0.2)))
(define mt-silu-gradient
  (matrix-silu-backward mt-silu-upstream (cadr mt-silu-result)))
(define mt-silu-original (mat-ref mt-silu-input 0 1))
(mat-set mt-silu-input 0 1 (+ mt-silu-original mt-epsilon))
(define mt-silu-plus
  (mat-sum (mat-mul (car (matrix-silu-forward mt-silu-input))
                    mt-silu-upstream)))
(mat-set mt-silu-input 0 1 (- mt-silu-original mt-epsilon))
(define mt-silu-minus
  (mat-sum (mat-mul (car (matrix-silu-forward mt-silu-input))
                    mt-silu-upstream)))
(mat-set mt-silu-input 0 1 mt-silu-original)
(llm-check-close "matrix-silu-gradient" (mat-ref mt-silu-gradient 0 1)
  (/ (- mt-silu-plus mt-silu-minus) (* 2.0 mt-epsilon))
  0.00001 0.00001)

; Complete transformer finite-difference check: embeddings, attention, residuals,
; RMSNorm, feed-forward, output head, and cross-entropy all participate.
(seed 101)
(define mt-model (mtransformer-make 4 3 4 2 1 8 0.15))
(define mt-parameters (mtransformer-parameters mt-model))
(mparams-zero mt-parameters)
(define mt-loss-result (mtransformer-loss-forward mt-model (list 0 1 2) 3))
(mtransformer-backward mt-model (cadr mt-loss-result) (caddr mt-loss-result))
(define mt-probe (mp-data (mhead-wq
  (car (mblock-heads (car (mtransformer-blocks mt-model)))))))
(define mt-probe-grad (mp-grad (mhead-wq
  (car (mblock-heads (car (mtransformer-blocks mt-model)))))))
(define mt-probe-original (mat-ref mt-probe 1 0))
(mat-set mt-probe 1 0 (+ mt-probe-original mt-epsilon))
(define mt-loss-plus (mtransformer-loss-number mt-model (list 0 1 2) 3))
(mat-set mt-probe 1 0 (- mt-probe-original mt-epsilon))
(define mt-loss-minus (mtransformer-loss-number mt-model (list 0 1 2) 3))
(mat-set mt-probe 1 0 mt-probe-original)
(llm-check-close "matrix-transformer-gradient"
  (mat-ref mt-probe-grad 1 0)
  (/ (- mt-loss-plus mt-loss-minus) (* 2.0 mt-epsilon))
  0.00002 0.00002)

(define (mt-gradient-close matrix gradient row column)
  (let ((original (mat-ref matrix row column)))
    (mat-set matrix row column (+ original mt-epsilon))
    (let ((plus (mtransformer-loss-number mt-model (list 0 1 2) 3)))
      (mat-set matrix row column (- original mt-epsilon))
      (let ((minus (mtransformer-loss-number mt-model (list 0 1 2) 3)))
        (mat-set matrix row column original)
        (< (abs (- (mat-ref gradient row column)
                   (/ (- plus minus) (* 2.0 mt-epsilon))))
           0.00002)))))

(define mt-block (car (mtransformer-blocks mt-model)))
(define mt-head (car (mblock-heads mt-block)))
(llm-check "matrix-gradient-token-embedding"
  (mt-gradient-close
    (mp-data (mtransformer-token-embedding mt-model))
    (mp-grad (mtransformer-token-embedding mt-model)) 1 2))
(llm-check "matrix-gradient-position-embedding"
  (mt-gradient-close
    (mp-data (mtransformer-position-embedding mt-model))
    (mp-grad (mtransformer-position-embedding mt-model)) 1 2))
(llm-check "matrix-gradient-attention-k"
  (mt-gradient-close (mp-data (mhead-wk mt-head))
    (mp-grad (mhead-wk mt-head)) 2 1))
(llm-check "matrix-gradient-attention-v"
  (mt-gradient-close (mp-data (mhead-wv mt-head))
    (mp-grad (mhead-wv mt-head)) 0 1))
(llm-check "matrix-gradient-attention-output"
  (mt-gradient-close (mp-data (mhead-wo mt-head))
    (mp-grad (mhead-wo mt-head)) 1 2))
(llm-check "matrix-gradient-ff-in"
  (mt-gradient-close
    (mp-data (mlinear-weight (mblock-ff-in mt-block)))
    (mp-grad (mlinear-weight (mblock-ff-in mt-block))) 2 4))
(llm-check "matrix-gradient-ff-in-bias"
  (mt-gradient-close
    (mp-data (mlinear-bias (mblock-ff-in mt-block)))
    (mp-grad (mlinear-bias (mblock-ff-in mt-block))) 0 4))
(llm-check "matrix-gradient-ff-out"
  (mt-gradient-close
    (mp-data (mlinear-weight (mblock-ff-out mt-block)))
    (mp-grad (mlinear-weight (mblock-ff-out mt-block))) 4 2))
(llm-check "matrix-gradient-swiglu-gate"
  (mt-gradient-close
    (mp-data (mlinear-weight (mblock-ff-gate mt-block)))
    (mp-grad (mlinear-weight (mblock-ff-gate mt-block))) 2 4))
(llm-check "matrix-gradient-learned-rmsnorm"
  (mt-gradient-close
    (mp-data (mrmsnorm-gain (mblock-norm1 mt-block)))
    (mp-grad (mrmsnorm-gain (mblock-norm1 mt-block))) 0 2))
(llm-check "matrix-gradient-final-rmsnorm"
  (mt-gradient-close
    (mp-data (mrmsnorm-gain (mtransformer-final-norm mt-model)))
    (mp-grad (mrmsnorm-gain (mtransformer-final-norm mt-model))) 0 2))
(llm-check "matrix-gradient-tied-output-embedding"
  (mt-gradient-close
    (mp-data (mtransformer-token-embedding mt-model))
    (mp-grad (mtransformer-token-embedding mt-model)) 3 1))
(llm-check "matrix-gradient-language-bias"
  (mt-gradient-close
    (mp-data (mtransformer-output-bias mt-model))
    (mp-grad (mtransformer-output-bias mt-model)) 0 1))

(llm-check "matrix-transformer-parameter-count"
  (= (mparams-count mt-parameters) 224))

; Future tokens cannot change logits at an earlier causal position.
(define mt-causal-a (car (mtransformer-forward-all mt-model (list 0 1 2))))
(define mt-causal-b (car (mtransformer-forward-all mt-model (list 0 3 3))))
(llm-check-close "matrix-no-future-leak"
  (mat-ref mt-causal-a 0 2) (mat-ref mt-causal-b 0 2)
  0.000000000001 0.000000000001)

; Short inference prefixes use their real length instead of artificial padding.
(define mt-prefix-logits
  (car (mtransformer-forward-prefix mt-model (list 0 1))))
(llm-check "matrix-short-prefix-logit-rows" (= (mat-rows mt-prefix-logits) 2))
(llm-check "matrix-short-prefix-logit-columns" (= (mat-cols mt-prefix-logits) 4))
(define mt-prefix-last
  (car (mtransformer-forward-prefix-last mt-model (list 0 1))))
(llm-check "matrix-prefix-last-one-row" (= (mat-rows mt-prefix-last) 1))
(llm-check "matrix-empty-prefix-error-is-catchable"
  (equal
    (catch (mtransformer-forward-prefix mt-model nil)
      (lambda (caught-error) (cadr caught-error)))
    "empty inference history"))
(llm-check "matrix-long-prefix-error-is-catchable"
  (equal
    (catch (mtransformer-forward-prefix mt-model (list 0 1 2 3))
      (lambda (caught-error) (cadr caught-error)))
    "inference history exceeds context"))
(llm-check "matrix-full-prefix-reuses-mask"
  (eq (mtransformer-prefix-mask mt-model 3) (mtransformer-mask mt-model)))
(llm-check-close "matrix-prefix-last-matches-full-prefix"
  (mat-ref mt-prefix-last 0 2) (mat-ref mt-prefix-logits 1 2)
  0.000000000001 0.000000000001)
(llm-check "matrix-short-prefix-sampling"
  (let ((token (mtransformer-next-token-top-k-top-p mt-model
                 (list 0 1) 1.0 1 1.0)))
    (and (>= token 0) (< token 4))))
(llm-check "matrix-one-token-prefix-sampling"
  (let ((token (mtransformer-next-token-top-k-top-p mt-model
                 (list 0) 1.0 1 1.0)))
    (and (>= token 0) (< token 4))))
(llm-check "matrix-full-context-sampling"
  (let ((token (mtransformer-next-token-top-k-top-p mt-model
                 (list 0 1 2) 1.0 1 1.0)))
    (and (>= token 0) (< token 4))))
(llm-check "matrix-over-context-sampling"
  (let ((token (mtransformer-next-token-top-k-top-p mt-model
                 (list 3 0 1 2) 1.0 1 1.0)))
    (and (>= token 0) (< token 4))))

; AdamW decays matrix weights, but not biases or normalization gains.
(define mt-decayed (mparam (make-matrix 1 1 1.0)))
(define mt-no-decay (mparam-no-decay (make-matrix 1 1 1.0)))
(mparam-adamw-step mt-decayed 1 0.1 0.9 0.95 0.00000001 0.5)
(mparam-adamw-step mt-no-decay 1 0.1 0.9 0.95 0.00000001 0.5)
(llm-check-close "matrix-adamw-decays-weight"
  (mat-ref (mp-data mt-decayed) 0 0) 0.95 0.000000000001 0.0)
(llm-check-close "matrix-adamw-preserves-no-decay-parameter"
  (mat-ref (mp-data mt-no-decay) 0 0) 1.0 0.000000000001 0.0)

; Full-sequence loss uses every causal output row.
(mparams-zero mt-parameters)
(define mt-sequence-result
  (mtransformer-sequence-loss-forward mt-model (list 0 1 2) (list 1 2 3)))
(mtransformer-backward-all mt-model (cadr mt-sequence-result)
  (caddr mt-sequence-result))
(llm-check "matrix-sequence-logit-rows"
  (= (mat-rows (car (mtransformer-forward-all mt-model (list 0 1 2)))) 3))
(llm-check "matrix-sequence-loss-finite" (< (car mt-sequence-result) 100.0))

; A target probability that underflows to zero must still produce finite NLL.
(define mt-bias-zero (mat-ref (mp-data (mtransformer-output-bias mt-model)) 0 0))
(define mt-bias-three (mat-ref (mp-data (mtransformer-output-bias mt-model)) 0 3))
(mat-set (mp-data (mtransformer-output-bias mt-model)) 0 0 1000.0)
(mat-set (mp-data (mtransformer-output-bias mt-model)) 0 3 -1000.0)
(define mt-extreme-result
  (mtransformer-loss-forward mt-model (list 0 1 2) 3))
(define mt-extreme-loss (car mt-extreme-result))
(llm-check-close "matrix-extreme-target-gradient"
  (mat-ref (cadr mt-extreme-result) 0 3) -1.0
  0.000000000001 0.000000000001)
(llm-check-close "matrix-extreme-winner-gradient"
  (mat-ref (cadr mt-extreme-result) 0 0) 1.0
  0.000000000001 0.000000000001)
(mat-set (mp-data (mtransformer-output-bias mt-model)) 0 0 mt-bias-zero)
(mat-set (mp-data (mtransformer-output-bias mt-model)) 0 3 mt-bias-three)
(llm-check "matrix-extreme-loss-finite" (matrix-finite-number mt-extreme-loss))
(llm-check "matrix-extreme-loss-not-clamped" (> mt-extreme-loss 1000.0))
(define (mt-sequence-gradient-close matrix gradient row column)
  (let ((original (mat-ref matrix row column)))
    (mat-set matrix row column (+ original mt-epsilon))
    (let ((plus
      (mtransformer-sequence-loss-number mt-model (list 0 1 2) (list 1 2 3))))
      (mat-set matrix row column (- original mt-epsilon))
      (let ((minus
        (mtransformer-sequence-loss-number mt-model (list 0 1 2) (list 1 2 3))))
        (mat-set matrix row column original)
        (< (abs (- (mat-ref gradient row column)
                   (/ (- plus minus) (* 2.0 mt-epsilon))))
           0.00003)))))
(llm-check "matrix-sequence-gradient-all-positions"
  (mt-sequence-gradient-close
    (mp-data (mlinear-weight (mblock-ff-gate mt-block)))
    (mp-grad (mlinear-weight (mblock-ff-gate mt-block))) 2 4))

; One AdamW step should be finite and change the loss/weights.
(define mt-before (mat-ref mt-probe 1 0))
(define mt-step-result
  (mtransformer-train-one mt-model mt-parameters (list 0 1 2) 3
    1 0.005 5.0 0.01))
(llm-check "matrix-adamw-updated" (/= (mat-ref mt-probe 1 0) mt-before))
(llm-check "matrix-loss-finite" (< (car mt-step-result) 100.0))

(llm-test-summary)
(print "=== done ===")
