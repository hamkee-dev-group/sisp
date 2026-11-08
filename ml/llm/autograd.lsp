; autograd.lsp — deterministic gradient and graph-invariant tests.
; Run: src/sisp ml/llm/autograd.lsp

(load 'ml/llm/engine)
(load 'ml/llm/metrics)
(llm-reset-tests)

(print "=== scalar reverse-mode autodiff checks ===")

; Composite reference: f(a,b) = a*b + tanh(a)
(define a (val 0.5))
(define b (val -1.2))
(define f (g+ (g* a b) (gtanh a)))
(backward f)
(llm-check-close "composite-value" (vdata f)
  (+ (* 0.5 -1.2) (tanh 0.5)) 0.000000000001 0.000000000001)
(llm-check-close "composite-df-da" (vgrad a)
  (+ -1.2 (- 1.0 (* (tanh 0.5) (tanh 0.5))))
  0.000000000001 0.000000000001)
(llm-check-close "composite-df-db" (vgrad b) 0.5
  0.000000000001 0.000000000001)

; Every differentiable primitive gets an analytic check.
(define x-add (val 2.0))
(define y-add (val -3.0))
(backward (g+ x-add y-add))
(llm-check-close "add-left" (vgrad x-add) 1.0 0.000000000001 0.0)
(llm-check-close "add-right" (vgrad y-add) 1.0 0.000000000001 0.0)

(define x-neg (val 2.0))
(backward (gneg x-neg))
(llm-check-close "neg" (vgrad x-neg) -1.0 0.000000000001 0.0)

(define x-exp (val 0.7))
(backward (gexp x-exp))
(llm-check-close "exp" (vgrad x-exp) (exp 0.7)
  0.000000000001 0.000000000001)

(define x-log (val 1.7))
(backward (glog x-log))
(llm-check-close "log" (vgrad x-log) (/ 1.0 1.7)
  0.000000000001 0.000000000001)

(define x-div (val 2.5))
(define y-div (val 1.25))
(backward (gdiv x-div y-div))
(llm-check-close "divide-numerator" (vgrad x-div) (/ 1.0 1.25)
  0.000000000001 0.000000000001)
(llm-check-close "divide-denominator" (vgrad y-div)
  (- 0.0 (/ 2.5 (* 1.25 1.25))) 0.000000000001 0.000000000001)

(define x-relu-pos (val 2.0))
(backward (grelu x-relu-pos))
(llm-check-close "relu-positive" (vgrad x-relu-pos) 1.0
  0.000000000001 0.0)
(define x-relu-neg (val -2.0))
(backward (grelu x-relu-neg))
(llm-check-close "relu-negative" (vgrad x-relu-neg) 0.0
  0.000000000001 0.0)

(define x-sqrt (val 2.0))
(backward (gsqrt x-sqrt))
(llm-check-close "sqrt" (vgrad x-sqrt) (/ 1.0 (* 2.0 (sqrt 2.0)))
  0.000000000001 0.000000000001)

; Shared subgraphs must accumulate all paths: d(x*x+x)/dx = 2x+1 = 7.
(define x-shared (val 3.0))
(define shared-output (g+ (g* x-shared x-shared) x-shared))
(backward shared-output)
(llm-check-close "shared-subgraph" (vgrad x-shared) 7.0
  0.000000000001 0.0)
; backward's contract clears the reachable graph, so it remains 7, not 14.
(backward shared-output)
(llm-check-close "repeated-backward-clears" (vgrad x-shared) 7.0
  0.000000000001 0.0)

; Backward rules use forward snapshots, even if a leaf is mutated in between.
(define x-snapshot (val 2.0))
(define y-snapshot (val 3.0))
(define snapshot-output (g* x-snapshot y-snapshot))
(set-data x-snapshot 100.0)
(set-data y-snapshot 200.0)
(backward snapshot-output)
(llm-check-close "forward-snapshot-dx" (vgrad x-snapshot) 3.0
  0.000000000001 0.0)
(llm-check-close "forward-snapshot-dy" (vgrad y-snapshot) 2.0
  0.000000000001 0.0)

(llm-check "tagged-value" (value-p x-snapshot))
(llm-check "graph-node-count" (= (graph-node-count shared-output) 3))

(llm-test-summary)
(print "=== done ===")
