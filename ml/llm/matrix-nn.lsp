; matrix-nn.lsp — native matrix layers with explicit backward passes.

; Linear layout: (mlinear weight bias), weight is input x output.
(define (mlinear-make input-size output-size scale)
  (list (quote mlinear)
    (mparam-randn input-size output-size scale)
    (mparam-zero-no-decay 1 output-size)))
(define (mlinear-weight layer) (cadr layer))
(define (mlinear-bias layer) (caddr layer))
(define (mlinear-parameters layer)
  (list (mlinear-weight layer) (mlinear-bias layer)))

; Returns (output cache), where the input alone is sufficient for backward.
(define (mlinear-forward layer input)
  (list
    (mat-add-row (mat* input (mp-data (mlinear-weight layer)))
                 (mp-data (mlinear-bias layer)))
    input))

(define (mlinear-backward layer upstream input-cache)
  (mat-axpy (mp-grad (mlinear-weight layer)) 1.0
    (mat* (mat-transpose input-cache) upstream))
  (mat-axpy (mp-grad (mlinear-bias layer)) 1.0
    (mat-sum-axis upstream 0))
  (mat* upstream (mat-transpose (mp-data (mlinear-weight layer)))))

; Learned RMSNorm layout: (mrmsnorm gain).
(define (mrmsnorm-make dimension)
  (list (quote mrmsnorm)
    (mparam-no-decay (make-matrix 1 dimension 1.0))))
(define (mrmsnorm-gain layer) (cadr layer))
(define (mrmsnorm-parameters layer) (list (mrmsnorm-gain layer)))

; cache=(parameter-free-rms-cache normalized-output).
(define (mrmsnorm-forward layer input)
  (let ((base (matrix-rmsnorm-forward input)))
    (let ((normalized (car base)))
      (list
        (mat-mul normalized
          (mat* (matrix-ones (mat-rows input) 1)
                (mp-data (mrmsnorm-gain layer))))
        (list (cadr base) normalized)))))

(define (mrmsnorm-backward layer upstream cache)
  (let ((base-cache (car cache))
        (normalized (cadr cache)))
    (mat-axpy (mp-grad (mrmsnorm-gain layer)) 1.0
      (mat-sum-axis (mat-mul upstream normalized) 0))
    (matrix-rmsnorm-backward
      (mat-mul upstream
        (mat* (matrix-ones (mat-rows upstream) 1)
              (mp-data (mrmsnorm-gain layer))))
      base-cache)))

; Attention head layout: (mhead Wq Wk Wv Wo dimension head-dimension).
(define (mhead-make dimension head-dimension scale residual-scale)
  (list (quote mhead)
    (mparam-randn dimension head-dimension scale)
    (mparam-randn dimension head-dimension scale)
    (mparam-randn dimension head-dimension scale)
    (mparam-randn head-dimension dimension residual-scale)
    dimension head-dimension))
(define (mhead-wq head) (cadr head))
(define (mhead-wk head) (caddr head))
(define (mhead-wv head) (cadddr head))
(define (mhead-wo head) (car (cddddr head)))
(define (mhead-dimension head) (cadr (cddddr head)))
(define (mhead-head-dimension head) (caddr (cddddr head)))
(define (mhead-parameters head)
  (list (mhead-wq head) (mhead-wk head) (mhead-wv head) (mhead-wo head)))

; cache=(input q k v probabilities weighted-values)
(define (mhead-forward head input causal-mask)
  (let ((q (mat* input (mp-data (mhead-wq head))))
        (k (mat* input (mp-data (mhead-wk head))))
        (v (mat* input (mp-data (mhead-wv head)))))
    (let ((scores
      (+ (* (mat* q (mat-transpose k))
                       (/ 1.0 (sqrt (mhead-head-dimension head))))
            causal-mask)))
      (let ((probabilities (mat-softmax scores)))
        (let ((weighted-values (mat* probabilities v)))
          (list
            (mat* weighted-values (mp-data (mhead-wo head)))
            (list input q k v probabilities weighted-values)))))))

(define (mhead-backward head upstream cache)
  (let ((input (nth 0 cache))
        (q (nth 1 cache))
        (k (nth 2 cache))
        (v (nth 3 cache))
        (probabilities (nth 4 cache))
        (weighted-values (nth 5 cache)))
    (mat-axpy (mp-grad (mhead-wo head)) 1.0
      (mat* (mat-transpose weighted-values) upstream))
    (let ((dweighted
      (mat* upstream (mat-transpose (mp-data (mhead-wo head))))))
      (let ((dprobabilities (mat* dweighted (mat-transpose v)))
            (dv (mat* (mat-transpose probabilities) dweighted)))
        (let ((dscores
          (*
            (matrix-softmax-backward probabilities dprobabilities)
            (/ 1.0 (sqrt (mhead-head-dimension head))))))
          (let ((dq (mat* dscores k))
                (dk (mat* (mat-transpose dscores) q)))
            (mat-axpy (mp-grad (mhead-wq head)) 1.0
              (mat* (mat-transpose input) dq))
            (mat-axpy (mp-grad (mhead-wk head)) 1.0
              (mat* (mat-transpose input) dk))
            (mat-axpy (mp-grad (mhead-wv head)) 1.0
              (mat* (mat-transpose input) dv))
            (matrix-add3
              (mat* dq (mat-transpose (mp-data (mhead-wq head))))
              (mat* dk (mat-transpose (mp-data (mhead-wk head))))
              (mat* dv (mat-transpose (mp-data (mhead-wv head)))))))))))

(define (mheads-make count dimension head-dimension scale residual-scale)
  (matrix-repeat-call count
    (lambda ()
      (mhead-make dimension head-dimension scale residual-scale))))
(define (mheads-parameters heads)
  (cond ((eq heads nil) nil)
        (t (append (mhead-parameters (car heads))
                   (mheads-parameters (cdr heads))))))

(define (mheads-forward-walk heads input mask total caches)
  (cond ((eq heads nil) (list total (reverse caches)))
        (t (let ((result (mhead-forward (car heads) input mask)))
          (mheads-forward-walk (cdr heads) input mask
            (cond ((eq total nil) (car result))
                  (t (+ total (car result))))
            (cons (cadr result) caches))))))
(define (mheads-forward heads input mask)
  (mheads-forward-walk heads input mask nil nil))

(define (mheads-backward-walk heads caches upstream total)
  (cond ((eq heads nil) total)
        (t (let ((dx (mhead-backward (car heads) upstream (car caches))))
          (mheads-backward-walk (cdr heads) (cdr caches) upstream
            (cond ((eq total nil) dx) (t (+ total dx))))))))
(define (mheads-backward heads caches upstream)
  (mheads-backward-walk heads caches upstream nil))

; Modern pre-norm block layout:
; (mblock heads norm1 norm2 ff-up ff-gate ff-down).
(define (mblock-make head-count dimension hidden-size scale residual-scale)
  (list (quote mblock)
    (mheads-make head-count dimension (/ dimension head-count)
      scale residual-scale)
    (mrmsnorm-make dimension)
    (mrmsnorm-make dimension)
    (mlinear-make dimension hidden-size scale)
    (mlinear-make dimension hidden-size scale)
    (mlinear-make hidden-size dimension residual-scale)))
(define (mblock-heads block) (cadr block))
(define (mblock-norm1 block) (caddr block))
(define (mblock-norm2 block) (cadddr block))
(define (mblock-ff-in block) (car (cddddr block)))
(define (mblock-ff-gate block) (cadr (cddddr block)))
(define (mblock-ff-out block) (caddr (cddddr block)))
(define (mblock-parameters block)
  (append (mheads-parameters (mblock-heads block))
    (append (mrmsnorm-parameters (mblock-norm1 block))
      (append (mrmsnorm-parameters (mblock-norm2 block))
        (append (mlinear-parameters (mblock-ff-in block))
          (append (mlinear-parameters (mblock-ff-gate block))
                  (mlinear-parameters (mblock-ff-out block))))))))

; cache=(norm1-cache head-caches norm2-cache up gate silu-cache
;        up-linear-cache gate-linear-cache down-linear-cache)
(define (mblock-forward block input causal-mask)
  (let ((norm1 (mrmsnorm-forward (mblock-norm1 block) input)))
    (let ((attention
      (mheads-forward (mblock-heads block) (car norm1) causal-mask)))
      (let ((after-attention (+ input (car attention))))
        (let ((norm2 (mrmsnorm-forward (mblock-norm2 block) after-attention)))
          (let ((up (mlinear-forward (mblock-ff-in block) (car norm2)))
                (gate-linear
                  (mlinear-forward (mblock-ff-gate block) (car norm2))))
            (let ((gate (matrix-silu-forward (car gate-linear))))
              (let ((hidden (mat-mul (car up) (car gate))))
                (let ((down (mlinear-forward (mblock-ff-out block) hidden)))
                  (list (+ after-attention (car down))
                    (list (cadr norm1) (cadr attention) (cadr norm2)
                      (car up) (car gate) (cadr gate)
                      (cadr up) (cadr gate-linear) (cadr down))))))))))))

(define (mblock-backward block upstream cache)
  (let ((norm1-cache (nth 0 cache))
        (head-caches (nth 1 cache))
        (norm2-cache (nth 2 cache))
        (up (nth 3 cache))
        (gate (nth 4 cache))
        (silu-cache (nth 5 cache))
        (up-cache (nth 6 cache))
        (gate-cache (nth 7 cache))
        (down-cache (nth 8 cache)))
    ; output = after-attention + down(up * silu(gate-linear)).
    (let ((dhidden
      (mlinear-backward (mblock-ff-out block) upstream down-cache)))
      (let ((dup (mat-mul dhidden gate))
            (dgate (mat-mul dhidden up)))
        (let ((dnorm-up
          (mlinear-backward (mblock-ff-in block) dup up-cache))
              (dnorm-gate
          (mlinear-backward (mblock-ff-gate block)
            (matrix-silu-backward dgate silu-cache) gate-cache)))
          (let ((dafter
            (+ upstream
              (mrmsnorm-backward (mblock-norm2 block)
                (+ dnorm-up dnorm-gate) norm2-cache))))
            ; after-attention = input + attention(norm1(input)).
            (let ((dnorm1
              (mheads-backward (mblock-heads block) head-caches dafter)))
              (+ dafter
                (mrmsnorm-backward (mblock-norm1 block)
                  dnorm1 norm1-cache)))))))))
