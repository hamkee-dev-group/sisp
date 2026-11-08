; nn.lsp — reusable neural modules made from scalar Values.

; A linear layer is (linear rows bias), where each row produces one output.
(define (llm-make-linear input-size output-size scale)
  (list (quote linear)
        (llm-make-param-matrix output-size input-size scale)
        (llm-make-zero-vector output-size)))
(define (llm-linear-weights layer) (cadr layer))
(define (llm-linear-bias layer) (caddr layer))
(define (llm-linear-parameters layer)
  (append (llm-flatten (llm-linear-weights layer))
          (llm-linear-bias layer)))
(define (llm-linear-forward layer input)
  (llm-map2
    (lambda (row bias) (g+ (dot row input) bias))
    (llm-linear-weights layer)
    (llm-linear-bias layer)))

; An embedding is simply a table of trainable rows.
(define (llm-make-embedding count dimension scale)
  (list (quote embedding) (llm-make-param-matrix count dimension scale)))
(define (llm-embedding-table embedding) (cadr embedding))
(define (llm-embedding-forward embedding index)
  (nth index (llm-embedding-table embedding)))
(define (llm-embedding-parameters embedding)
  (llm-flatten (llm-embedding-table embedding)))

; Parameter-free RMS normalization. Epsilon prevents division by zero.
(define (llm-rmsnorm vector)
  (let ((mean-square
    (gscale (sum-vals (map gsquare vector)) (/ 1.0 (length vector)))))
    (let ((denominator (gsqrt (g+ mean-square (val 0.00001)))))
      (map (lambda (x) (gdiv x denominator)) vector))))

; Attention is (attention q k v output dimension).
(define (llm-make-attention dimension scale)
  (list (quote attention)
        (llm-make-linear dimension dimension scale)
        (llm-make-linear dimension dimension scale)
        (llm-make-linear dimension dimension scale)
        (llm-make-linear dimension dimension scale)
        dimension))
(define (llm-attention-q attention) (cadr attention))
(define (llm-attention-k attention) (caddr attention))
(define (llm-attention-v attention) (cadddr attention))
(define (llm-attention-output attention) (car (cddddr attention)))
(define (llm-attention-dimension attention) (cadr (cddddr attention)))
(define (llm-attention-parameters attention)
  (append (llm-linear-parameters (llm-attention-q attention))
    (append (llm-linear-parameters (llm-attention-k attention))
      (append (llm-linear-parameters (llm-attention-v attention))
              (llm-linear-parameters (llm-attention-output attention))))))

(define (llm-attend-one attention query keys values)
  (let ((scale (/ 1.0 (sqrt (llm-attention-dimension attention)))))
    (let ((scores (map (lambda (key) (gscale (dot query key) scale)) keys)))
      (llm-linear-forward (llm-attention-output attention)
        (llm-weighted-vector-sum
          (llm-softmax-values scores) values)))))

(define (llm-attention-positions attention queries keys values index)
  (cond ((eq queries nil) nil)
        (t (cons
          (llm-attend-one attention (car queries)
            (take keys (+ index 1)) (take values (+ index 1)))
          (llm-attention-positions attention (cdr queries)
            keys values (+ index 1))))))

(define (llm-attention-forward attention inputs)
  (let ((queries (map (lambda (x)
                        (llm-linear-forward (llm-attention-q attention) x)) inputs))
        (keys (map (lambda (x)
                     (llm-linear-forward (llm-attention-k attention) x)) inputs))
        (values (map (lambda (x)
                       (llm-linear-forward (llm-attention-v attention) x)) inputs)))
    (llm-attention-positions attention queries keys values 0)))

; A block is (block attention feed-forward-in feed-forward-out).
(define (llm-make-block dimension hidden-size scale)
  (list (quote block)
        (llm-make-attention dimension scale)
        (llm-make-linear dimension hidden-size scale)
        (llm-make-linear hidden-size dimension scale)))
(define (llm-block-attention block) (cadr block))
(define (llm-block-ff-in block) (caddr block))
(define (llm-block-ff-out block) (cadddr block))
(define (llm-block-parameters block)
  (append (llm-attention-parameters (llm-block-attention block))
    (append (llm-linear-parameters (llm-block-ff-in block))
            (llm-linear-parameters (llm-block-ff-out block)))))

(define (llm-block-feed-forward block vector)
  (llm-linear-forward (llm-block-ff-out block)
    (llm-vector-tanh
      (llm-linear-forward (llm-block-ff-in block) vector))))

(define (llm-block-forward block inputs)
  (let ((normalized (map llm-rmsnorm inputs)))
    (let ((attention-output
      (llm-attention-forward (llm-block-attention block) normalized)))
      (let ((after-attention (llm-map2 llm-vector-add inputs attention-output)))
        (let ((ff-output
          (map (lambda (x)
                 (llm-block-feed-forward block (llm-rmsnorm x)))
               after-attention)))
          (llm-map2 llm-vector-add after-attention ff-output))))))

