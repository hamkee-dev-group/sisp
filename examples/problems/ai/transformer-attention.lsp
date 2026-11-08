; Self-attention: Q=XWq, K=XWk, V=XWv, softmax(QK^T/sqrt(d))V
; 3-token sentence. Show which tokens attend to which.
(print "=== Transformer Self-Attention ===")
(seed 42)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

; --- Matrix operations on lists-of-lists ---
(define (mx-rows m) (length m))
(define (mx-cols m) (length (car m)))
(define (mx-ref m r c) (nth c (nth r m)))

(define (mx-mul A B)
  (let ((RA (mx-rows A)) (CA (mx-cols A)) (CB (mx-cols B)))
    (map (lambda (r)
      (map (lambda (c)
        (mx-cell A B r c CA 0.0))
        (seq 0 (- CB 1))))
      (seq 0 (- RA 1)))))

(define (mx-cell A B r c k acc)
  (cond ((= k 0) acc)
        (t (mx-cell A B r c (- k 1)
            (+ acc (* (mx-ref A r (- k 1)) (mx-ref B (- k 1) c)))))))

(define (mx-transpose M)
  (let ((R (mx-rows M)) (C (mx-cols M)))
    (map (lambda (c)
      (map (lambda (r) (mx-ref M r c)) (seq 0 (- R 1))))
      (seq 0 (- C 1)))))

(define (mx-scale M s)
  (map (lambda (row) (map (lambda (x) (* x s)) row)) M))

; Softmax over a vector
(define (softmax vec)
  (let ((max-val (vec-max vec (length vec) (car vec))))
    (let ((exps (map (lambda (x) (exp (- x max-val))) vec)))
      (let ((total (vec-sum exps (length exps) 0.0)))
        (map (lambda (x) (/ x total)) exps)))))

(define (vec-max v i best)
  (cond ((= i 0) best)
        ((> (nth (- i 1) v) best) (vec-max v (- i 1) (nth (- i 1) v)))
        (t (vec-max v (- i 1) best))))

(define (vec-sum v i acc)
  (cond ((= i 0) acc) (t (vec-sum v (- i 1) (+ acc (nth (- i 1) v))))))

; Softmax over each row
(define (softmax-rows M) (map softmax M))

; --- Input: 3-token sentence, embedding dim d=4 ---
(define X (list
  (list 0.5 0.2 0.8 -0.3)
  (list -0.4 0.9 0.1 0.6)
  (list 0.7 -0.5 0.3 0.4)))

(define d 4)

(define Wq (list
  (list 0.2 0.1 -0.3 0.5) (list -0.4 0.3 0.2 0.1)
  (list 0.6 -0.2 0.4 -0.3) (list 0.1 0.5 -0.1 0.2)))

(define Wk (list
  (list 0.3 -0.1 0.2 0.4) (list 0.2 0.5 -0.3 0.1)
  (list -0.4 0.2 0.1 0.3) (list 0.1 -0.2 0.5 -0.1)))

(define Wv (list
  (list 0.1 0.3 -0.2 0.5) (list 0.4 -0.1 0.3 0.2)
  (list -0.3 0.5 0.1 -0.4) (list 0.2 0.1 0.4 -0.3)))

; Compute Q, K, V
(define mxQ (mx-mul X Wq))
(define mxK (mx-mul X Wk))
(define mxV (mx-mul X Wv))

(print "X (3 tokens x 4 dims):")
(print X)

; Attention scores: Q * K^T / sqrt(d)
(define mxKt (mx-transpose mxK))
(define mx-scores-raw (mx-mul mxQ mxKt))
(define scale (/ 1.0 (sqrt d)))
(define mx-scores-scaled (mx-scale mx-scores-raw scale))

(print "Attention scores (scaled):")
(print mx-scores-scaled)

(define attention-weights (softmax-rows mx-scores-scaled))
(print "Attention weights (each row sums to ~1):")
(print attention-weights)

; Weighted sum: weights * V
(define output (mx-mul attention-weights mxV))
(print "Output (contextualized token embeddings):")
(print output)

; --- Verification ---
(define (check-row-sums mat)
  (all-rows-ok mat 0 0.01))

(define (all-rows-ok mat i tol)
  (let ((row-len (length (nth 0 mat))))
    (cond ((= i (length mat)) t)
          ((< (abs (- (vec-sum (nth i mat) row-len 0.0) 1.0)) tol)
           (all-rows-ok mat (+ i 1) tol))
          (t nil))))

(ck "attention-weights-sum-to-1" (check-row-sums attention-weights))

; Find which token each position attends to most
(define (argmax row)
  (argmax-aux row 1 (car row) 0))

(define (argmax-aux row i best-val best-i)
  (cond ((= i (length row)) (list best-i best-val))
        ((> (nth i row) best-val) (argmax-aux row (+ i 1) (nth i row) i))
        (t (argmax-aux row (+ i 1) best-val best-i))))

(define attn0 (argmax (nth 0 attention-weights)))
(define attn1 (argmax (nth 1 attention-weights)))
(define attn2 (argmax (nth 2 attention-weights)))

(print (list 'token-0-attends-to (car attn0) 'weight (cadr attn0)))
(print (list 'token-1-attends-to (car attn1) 'weight (cadr attn1)))
(print (list 'token-2-attends-to (car attn2) 'weight (cadr attn2)))

(ck "attention-computed-correctly" (and (> (cadr attn0) 0.0) (> (cadr attn1) 0.0) (> (cadr attn2) 0.0)))
(ck "output-shape-correct" (and (= (length output) 3) (= (length (car output)) 4)))

(print "=== Transformer Attention Done ===")
