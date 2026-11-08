; ============================================================
; engine.lsp — scalar reverse-mode autodiff in SISP.
;
; A Value is a tagged mutable record:
;   (value data grad backward-thunk parents traversal-mark operation)
;
; Never print a raw Value: its backward closure can point back to the Value.
; Use (value-summary v), (vdata v), or (vgrad v).
; ============================================================

; --- Value record ----------------------------------------------------------

(define (mk-op data parents operation)
  (list (quote value) data 0.0 nil parents 0 operation))
(define (mk data parents) (mk-op data parents (quote value)))
(define (val x) (mk-op x nil (quote leaf)))

(define (value-p v)
  (and (consp v) (eq (car v) (quote value))))
(define (vdata v) (cadr v))
(define (vgrad v) (caddr v))
(define (getbw v) (cadddr v))
(define (vpar v) (car (cddddr v)))
(define (vmark v) (cadr (cddddr v)))
(define (vop v) (caddr (cddddr v)))

(define (set-data v x) (set-car (cdr v) x))
(define (set-grad v x) (set-car (cddr v) x))
(define (add-grad v x) (set-grad v (+ (vgrad v) x)))
(define (set-bw v f) (set-car (cdddr v) f))
(define (set-mark v x) (set-car (cdr (cddddr v)) x))

(define (value-summary v)
  (list (quote value) (quote op) (vop v)
        (quote data) (vdata v) (quote grad) (vgrad v)
        (quote parents) (length (vpar v))))

; --- differentiable scalar operations ------------------------------------
; Backward closures capture forward-pass numbers. Mutating a parent after the
; forward pass therefore cannot silently change the derivative of that pass.

(define (g+ a b)
  (let ((o (mk-op (+ (vdata a) (vdata b)) (list a b) (quote add))))
    (set-bw o (lambda ()
      (add-grad a (vgrad o))
      (add-grad b (vgrad o))))
    o))

(define (g* a b)
  (let ((av (vdata a)) (bv (vdata b)))
    (let ((o (mk-op (* av bv) (list a b) (quote mul))))
      (set-bw o (lambda ()
        (add-grad a (* bv (vgrad o)))
        (add-grad b (* av (vgrad o)))))
      o)))

(define (gneg a)
  (let ((o (mk-op (- 0.0 (vdata a)) (list a) (quote neg))))
    (set-bw o (lambda () (add-grad a (- 0.0 (vgrad o)))))
    o))

(define (g- a b) (g+ a (gneg b)))

(define (gtanh a)
  (let ((ta (tanh (vdata a))))
    (let ((o (mk-op ta (list a) (quote tanh))))
      (set-bw o (lambda ()
        (add-grad a (* (- 1.0 (* ta ta)) (vgrad o)))))
      o)))

(define (gexp a)
  (let ((ea (exp (vdata a))))
    (let ((o (mk-op ea (list a) (quote exp))))
      (set-bw o (lambda () (add-grad a (* ea (vgrad o)))))
      o)))

(define (glog a)
  (let ((av (vdata a)))
    (let ((o (mk-op (log av) (list a) (quote log))))
      (set-bw o (lambda () (add-grad a (/ (vgrad o) av))))
      o)))

(define (gdiv a b)
  (let ((av (vdata a)) (bv (vdata b)))
    (let ((o (mk-op (/ av bv) (list a b) (quote div))))
      (set-bw o (lambda ()
        (add-grad a (/ (vgrad o) bv))
        (add-grad b (- 0.0 (/ (* (vgrad o) av) (* bv bv))))))
      o)))

(define (grelu a)
  (let ((av (vdata a)))
    (let ((r (cond ((> av 0.0) av) (t 0.0))))
      (let ((o (mk-op r (list a) (quote relu))))
        (set-bw o (lambda ()
          (add-grad a (cond ((> av 0.0) (vgrad o)) (t 0.0)))))
        o))))

(define (gsqrt a)
  (let ((s (sqrt (vdata a))))
    (let ((o (mk-op s (list a) (quote sqrt))))
      (set-bw o (lambda ()
        (add-grad a (/ (vgrad o) (* 2.0 s)))))
      o)))

(define (gscale a scalar) (g* a (val scalar)))
(define (gsquare a) (g* a a))

; --- reverse traversal -----------------------------------------------------
; A monotonically increasing traversal generation avoids a reset pass and stale
; Boolean marks after an interrupted backward call.

(define *topo* nil)
(define *topo-count* 0)
(define *traversal-generation* 0)

(define (build-topo n)
  (cond ((= (vmark n) *traversal-generation*) nil)
        (t (set-mark n *traversal-generation*)
           (foreach build-topo (vpar n))
           (setq *topo* (cons n *topo*))
           (setq *topo-count* (+ *topo-count* 1)))))

; Contract: backward clears every reachable node gradient before computing one
; derivative. Gradient accumulation across examples must therefore happen in the
; optimizer/model layer, not by calling backward repeatedly on the same graph.
(define (backward root)
  (setq *traversal-generation* (+ *traversal-generation* 1))
  (setq *topo* nil)
  (setq *topo-count* 0)
  (build-topo root)
  (foreach (lambda (n) (set-grad n 0.0)) *topo*)
  (set-grad root 1.0)
  (foreach (lambda (n)
    (cond ((getbw n) ((getbw n))) (t nil))) *topo*)
  ; Do not keep the completed graph globally reachable across a top-level GC.
  (setq *topo* nil)
  nil)

(define (graph-node-count root)
  (setq *traversal-generation* (+ *traversal-generation* 1))
  (setq *topo* nil)
  (setq *topo-count* 0)
  (build-topo root)
  (let ((n *topo-count*)) (setq *topo* nil) n))

; --- list helpers used by the model library -------------------------------

(define (sum-val-pairs vs)
  (cond ((eq vs nil) nil)
        ((eq (cdr vs) nil) vs)
        (t (cons (g+ (car vs) (cadr vs))
                 (sum-val-pairs (cddr vs))))))

; Pairwise reduction keeps the autodiff graph logarithmically deep. A linear
; chain is mathematically valid but exhausts SISP's evaluator stack once dot
; products and attention become more than toy-sized.
(define (sum-vals vs)
  (cond ((eq vs nil) (val 0.0))
        ((eq (cdr vs) nil) (car vs))
        (t (sum-vals (sum-val-pairs vs)))))

(define (dot-products ws xs)
  (cond ((and (eq ws nil) (eq xs nil)) nil)
        ((or (eq ws nil) (eq xs nil)) (error "dot product length mismatch"))
        (t (cons (g* (car ws) (car xs))
                 (dot-products (cdr ws) (cdr xs))))))

(define (dot ws xs) (sum-vals (dot-products ws xs)))

(define (repeat-call n f)
  (cond ((= n 0) nil)
        (t (cons (f) (repeat-call (- n 1) f)))))

(define (randw)
  (val (- (* 2.0 (random)) 1.0)))
(define (randw-scale scale)
  (val (* scale (- (* 2.0 (random)) 1.0))))
