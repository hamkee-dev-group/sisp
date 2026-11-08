; math.lsp — stable probability operations for plain numbers and Values.

(define (llm-sum-numbers xs)
  (foldl (lambda (acc x) (+ acc x)) 0.0 xs))

(define (llm-max-number xs)
  (cond ((eq (cdr xs) nil) (car xs))
        (t (max (car xs) (llm-max-number (cdr xs))))))

(define (llm-softmax-numbers logits)
  (let ((m (llm-max-number logits)))
    (let ((exps (map (lambda (x) (exp (- x m))) logits)))
      (let ((z (llm-sum-numbers exps)))
        (map (lambda (x) (/ x z)) exps)))))

(define (llm-max-value-data values)
  (cond ((eq (cdr values) nil) (vdata (car values)))
        (t (max (vdata (car values))
                (llm-max-value-data (cdr values))))))

(define (llm-softmax-values logits)
  (let ((m (val (llm-max-value-data logits))))
    (let ((exps (map (lambda (x) (gexp (g- x m))) logits)))
      (let ((z (sum-vals exps)))
        (map (lambda (x) (gdiv x z)) exps)))))

; Stable log(sum(exp(logits))). The maximum is intentionally treated as a
; constant: the derivative of the complete expression is still softmax.
(define (llm-logsumexp-values logits)
  (let ((m-number (llm-max-value-data logits)))
    (let ((m (val m-number)))
      (g+ m (glog (sum-vals
        (map (lambda (x) (gexp (g- x m))) logits)))))))

(define (llm-cross-entropy-logits logits target-index)
  (g- (llm-logsumexp-values logits) (nth target-index logits)))

(define (llm-sample-index probabilities random-number index)
  (cond ((eq (cdr probabilities) nil) index)
        ((< random-number (car probabilities)) index)
        (t (llm-sample-index (cdr probabilities)
             (- random-number (car probabilities)) (+ index 1)))))

(define (llm-sample-logits logits temperature)
  (let ((scaled (map (lambda (x) (/ x temperature)) logits)))
    (llm-sample-index (llm-softmax-numbers scaled) (random) 0)))

; Sample only from the k highest logits, returning the original vocabulary
; index. With k=1 this is greedy decoding; k>=vocabulary is ordinary sampling.
(define (llm-index-logits-walk logits index)
  (cond ((eq logits nil) nil)
        (t (cons (list index (car logits))
                 (llm-index-logits-walk (cdr logits) (+ index 1))))))
(define (llm-sample-top-k-logits logits temperature top-k)
  (llm-sample-top-k-top-p-logits logits temperature top-k 1.0))

; Keep the smallest descending-probability prefix whose mass reaches top-p.
; At least one candidate is always retained. Sampling is then renormalized by
; applying softmax to only that retained prefix.
(define (llm-nucleus-prefix entries probabilities cumulative top-p)
  (cond ((eq (cdr entries) nil) (list (car entries)))
        ((>= (+ cumulative (car probabilities)) top-p)
          (list (car entries)))
        (t (cons (car entries)
          (llm-nucleus-prefix (cdr entries) (cdr probabilities)
            (+ cumulative (car probabilities)) top-p)))))

(define (llm-sample-top-k-top-p-logits logits temperature top-k top-p)
  (let ((ranked
    (sort (lambda (a b) (> (cadr a) (cadr b)))
      (llm-index-logits-walk logits 0))))
    (let ((selected (take ranked (min top-k (length ranked)))))
      (let ((selected-logits (map (lambda (entry) (cadr entry)) selected)))
        (let ((probabilities
          (llm-softmax-numbers
            (map (lambda (value) (/ value temperature)) selected-logits))))
          (let ((nucleus
            (llm-nucleus-prefix selected probabilities 0.0 top-p)))
            (let ((chosen
              (llm-sample-logits
                (map (lambda (entry) (cadr entry)) nucleus) temperature)))
              (car (nth chosen nucleus)))))))))

(define (llm-argmax-walk xs best-value best-index index)
  (cond ((eq xs nil) best-index)
        ((> (car xs) best-value)
          (llm-argmax-walk (cdr xs) (car xs) index (+ index 1)))
        (t (llm-argmax-walk (cdr xs) best-value best-index (+ index 1)))))

(define (llm-argmax xs)
  (llm-argmax-walk (cdr xs) (car xs) 0 1))
