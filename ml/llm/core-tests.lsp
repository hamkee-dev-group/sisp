; core-tests.lsp — invariants for shared math, tensor, data, NN, and optimizer code.
; Load through run-tests.sh so all libraries are already defined.

(llm-reset-tests)
(print "=== shared LLM library checks ===")

(define stable-probs (llm-softmax-numbers (list 1000.0 1001.0 999.0)))
(llm-check-close "stable-softmax-sum" (llm-sum-numbers stable-probs) 1.0
  0.000000000001 0.000000000001)
(llm-check "stable-softmax-order"
  (and (> (cadr stable-probs) (car stable-probs))
       (> (car stable-probs) (caddr stable-probs))))

(define ce-logits (list (val 1.0) (val 2.0) (val -1.0)))
(define ce-loss (llm-cross-entropy-logits ce-logits 1))
(define ce-expected-probs (llm-softmax-numbers (list 1.0 2.0 -1.0)))
(backward ce-loss)
(llm-check-close "cross-entropy-loss" (vdata ce-loss)
  (- (log (+ (exp -1.0) (exp 0.0) (exp -3.0))) 0.0)
  0.000000000001 0.000000000001)
(llm-check-list-close "cross-entropy-gradient"
  (map vgrad ce-logits)
  (list (car ce-expected-probs)
        (- (cadr ce-expected-probs) 1.0)
        (caddr ce-expected-probs))
  0.000000000001 0.000000000001)

; Reductions must remain shallow enough for useful transformer dimensions.
(define balanced-values (repeat-call 32 (lambda () (val 1.0))))
(define balanced-sum (sum-vals balanced-values))
(backward balanced-sum)
(llm-check-close "balanced-sum-value" (vdata balanced-sum) 32.0 0.0 0.0)
(llm-check "balanced-sum-graph-size" (= (graph-node-count balanced-sum) 63))
(llm-check "balanced-sum-gradients"
  (= (foldl (lambda (n value)
       (+ n (cond ((= (vgrad value) 1.0) 1) (t 0))))
     0 balanced-values) 32))

(llm-check-list-close "plain-matmul"
  (flatten (llm-matmul-numbers
    (list (list 1.0 2.0) (list 3.0 4.0))
    (list (list 5.0 6.0) (list 7.0 8.0))))
  (list 19.0 22.0 43.0 50.0) 0.000000000001 0.0)

(llm-check "shape-error-is-catchable"
  (equal
    (catch (llm-map2 g+ (list (val 1.0)) nil)
      (lambda (caught-error) (cadr caught-error)))
    "vector length mismatch"))
(llm-check "dot-length-error-is-catchable"
  (equal
    (catch (dot-products (list (val 1.0)) nil)
      (lambda (caught-error) (cadr caught-error)))
    "dot product length mismatch"))

(define encoded (llm-encode "cab" (list "a" "b" "c" "d")))
(llm-check "character-encode" (equal encoded (list 2 0 1)))
(llm-check "character-roundtrip"
  (equal (llm-decode encoded (list "a" "b" "c" "d")) "cab"))
(define window-check (llm-make-windows (list 0 1 2 3 0) 3))
(llm-check "window-count" (= (length window-check) 2))
(llm-check "window-first" (equal (car window-check) (list (list 0 1 2) 3)))

; One SGD step on (p-3)^2 must move p toward 3.
(define quadratic-p (val 0.0))
(define quadratic-loss (gsquare (g- quadratic-p (val 3.0))))
(llm-zero-gradients (list quadratic-p))
(backward quadratic-loss)
(llm-sgd-step (list quadratic-p) 0.1 100.0)
(llm-check-close "sgd-known-quadratic" (vdata quadratic-p) 0.6
  0.000000000001 0.000000000001)

; Causal invariant: changing future positions cannot affect position zero.
(seed 91)
(define causal-attention (llm-make-attention 2 0.2))
(define causal-input-a
  (list (list (val 1.0) (val 0.0))
        (list (val 0.0) (val 1.0))
        (list (val 1.0) (val 1.0))))
(define causal-input-b
  (list (list (val 1.0) (val 0.0))
        (list (val 9.0) (val -4.0))
        (list (val -3.0) (val 7.0))))
(define causal-output-a (llm-attention-forward causal-attention causal-input-a))
(define causal-output-b (llm-attention-forward causal-attention causal-input-b))
(llm-check-list-close "causal-no-future-leak"
  (llm-vector-data (car causal-output-a))
  (llm-vector-data (car causal-output-b))
  0.000000000001 0.000000000001)

(llm-check "runtime-nonnegative" (>= (runtime) 0.0))

(llm-test-summary)
(print "=== done ===")
