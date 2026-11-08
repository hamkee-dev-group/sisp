; model.lsp — a tiny, genuinely trainable causal character transformer.

; Model layout:
; (transformer token-embedding position-embedding block lm-head context vocab-size)
(define (llm-make-transformer vocab-size context-length dimension hidden-size scale)
  (list (quote transformer)
        (llm-make-embedding vocab-size dimension scale)
        (llm-make-embedding context-length dimension scale)
        (llm-make-block dimension hidden-size scale)
        (llm-make-linear dimension vocab-size scale)
        context-length
        vocab-size))

(define (llm-model-token-embedding model) (cadr model))
(define (llm-model-position-embedding model) (caddr model))
(define (llm-model-block model) (cadddr model))
(define (llm-model-head model) (car (cddddr model)))
(define (llm-model-context model) (cadr (cddddr model)))
(define (llm-model-vocab-size model) (caddr (cddddr model)))

(define (llm-model-parameters model)
  (append
    (llm-embedding-parameters (llm-model-token-embedding model))
    (append
      (llm-embedding-parameters (llm-model-position-embedding model))
      (append
        (llm-block-parameters (llm-model-block model))
        (llm-linear-parameters (llm-model-head model))))))

(define (llm-model-inputs model token-ids index)
  (cond ((eq token-ids nil) nil)
        (t (cons
          (llm-vector-add
            (llm-embedding-forward (llm-model-token-embedding model)
                                   (car token-ids))
            (llm-embedding-forward (llm-model-position-embedding model)
                                   index))
          (llm-model-inputs model (cdr token-ids) (+ index 1))))))

(define (llm-model-logits model token-ids)
  (cond ((/= (length token-ids) (llm-model-context model))
          (error "input length does not match context length"))
        (t
          (let ((states
            (llm-block-forward (llm-model-block model)
              (llm-model-inputs model token-ids 0))))
            (llm-linear-forward (llm-model-head model)
              (llm-rmsnorm (llm-last states)))))))

(define (llm-model-loss model window)
  (llm-cross-entropy-logits
    (llm-model-logits model (llm-window-input window))
    (llm-window-target window)))

(define (llm-take-last xs count)
  (drop xs (- (length xs) count)))

(define (llm-model-next-token model history temperature)
  (llm-sample-logits
    (llm-vector-data
      (llm-model-logits model
        (llm-take-last history (llm-model-context model))))
    temperature))

(define (llm-model-generate model history count temperature)
  (cond ((= count 0) history)
        (t (let ((next (llm-model-next-token model history temperature)))
             (llm-model-generate model (append history (list next))
               (- count 1) temperature)))))

; One optimizer step. Keeping each call as its own top-level form allows SISP's
; existing between-form collector to reclaim its temporary computation graph.
(define (llm-train-one model parameters window learning-rate clip-norm)
  (llm-zero-gradients parameters)
  (let ((loss (llm-model-loss model window)))
    (let ((loss-number (vdata loss))
          (nodes (graph-node-count loss)))
      (backward loss)
      (let ((gradient-norm
        (llm-sgd-step parameters learning-rate clip-norm)))
        (list loss-number gradient-norm nodes)))))

(define (llm-mean-evaluation-loss model windows remaining total)
  (cond ((or (= remaining 0) (eq windows nil))
          (/ total (- *llm-evaluation-count* remaining)))
        (t
          (llm-mean-evaluation-loss model (cdr windows) (- remaining 1)
            (+ total (vdata (llm-model-loss model (car windows))))))))

; The demonstration corpus has a six-token period. Its first six windows contain
; every distinct context/target phase exactly once.
(define *llm-evaluation-count* 6)
(define (llm-evaluate model windows)
  (llm-mean-evaluation-loss model windows *llm-evaluation-count* 0.0))
