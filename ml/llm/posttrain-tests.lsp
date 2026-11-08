; posttrain-tests.lsp — objective and windowing checks for transformer posttraining.

(llm-reset-tests)
(print "=== transformer posttraining checks ===")

(define pt-logits (make-matrix 4 3))
(define pt-masked
  (posttrain-cross-entropy-forward pt-logits (list 0 1 2 0) 1 3))
(llm-check-close "posttrain-masked-loss" (car pt-masked) (log 3.0)
  0.000000000001 0.000000000001)
(llm-check-close "posttrain-prompt-gradient-zero"
  (+ (abs (mat-ref (cadr pt-masked) 0 0))
     (abs (mat-ref (cadr pt-masked) 0 1))
     (abs (mat-ref (cadr pt-masked) 0 2)))
  0.0 0.000000000001 0.000000000001)
(llm-check-close "posttrain-padding-gradient-zero"
  (+ (abs (mat-ref (cadr pt-masked) 3 0))
     (abs (mat-ref (cadr pt-masked) 3 1))
     (abs (mat-ref (cadr pt-masked) 3 2)))
  0.0 0.000000000001 0.000000000001)
(llm-check-close "posttrain-active-gradient-row-sums-zero"
  (+ (mat-ref (cadr pt-masked) 1 0)
     (mat-ref (cadr pt-masked) 1 1)
     (mat-ref (cadr pt-masked) 1 2))
  0.0 0.000000000001 0.000000000001)

; A short example is padded to context while only completion rows are active.
(define pt-window
  (posttrain-completion-window (list 1 2 3) (list 4 5) 0 6 0))
(llm-check "posttrain-window-input-size" (= (length (nth 0 pt-window)) 6))
(llm-check "posttrain-window-target-size" (= (length (nth 1 pt-window)) 6))
(llm-check "posttrain-window-active-rows"
  (and (= (nth 2 pt-window) 2) (= (nth 3 pt-window) 4)))
(llm-check "posttrain-window-covers-short-completion"
  (and (= (nth 4 pt-window) 2) (= (nth 5 pt-window) 2)))

; Long completions advance in non-overlapping windows and retain prompt/history.
(define pt-long (posttrain-completion-window
  (list 9 8 7 6) (list 1 2 3 4 5 6 7 8) 0 5 0))
(llm-check "posttrain-window-first-long-chunk"
  (and (= (nth 4 pt-long) 4) (= (nth 5 pt-long) 4)))
(define pt-long-next (posttrain-completion-window
  (list 9 8 7 6) (list 1 2 3 4 5 6 7 8) (nth 5 pt-long) 5 0))
(llm-check "posttrain-window-long-progress"
  (and (> (nth 4 pt-long-next) 0) (> (nth 5 pt-long-next) (nth 5 pt-long))))

(define pt-dpo (posttrain-dpo-statistics -4.0 -5.0 -4.0 -5.0 0.2 3 4))
(llm-check-close "posttrain-dpo-neutral-loss" (nth 0 pt-dpo) (log 2.0)
  0.000000000001 0.000000000001)
(llm-check-close "posttrain-dpo-chosen-scale" (nth 2 pt-dpo) 0.3
  0.000000000001 0.000000000001)
(llm-check-close "posttrain-dpo-rejected-scale" (nth 3 pt-dpo) -0.4
  0.000000000001 0.000000000001)

(define pt-rl (posttrain-reinforce-statistics 1.0 0.25 0.1 6))
(llm-check-close "posttrain-reinforce-advantage" (nth 0 pt-rl) 0.75
  0.000000000001 0.000000000001)
(llm-check-close "posttrain-reinforce-scale" (nth 1 pt-rl) 4.5
  0.000000000001 0.000000000001)
(llm-check-close "posttrain-reinforce-baseline" (nth 2 pt-rl) 0.325
  0.000000000001 0.000000000001)

(define pt-teacher (matrix-from-list 3 2 (list 0.8 0.2 0.3 0.7 0.6 0.4)))
(define pt-distill (posttrain-distill-forward (make-matrix 3 2) pt-teacher 1 2 2.0))
(llm-check-close "posttrain-distill-prompt-zero"
  (+ (abs (mat-ref (car pt-distill) 0 0))
     (abs (mat-ref (car pt-distill) 0 1)))
  0.0 0.000000000001 0.000000000001)
(llm-check-close "posttrain-distill-padding-zero"
  (+ (abs (mat-ref (car pt-distill) 2 0))
     (abs (mat-ref (car pt-distill) 2 1)))
  0.0 0.000000000001 0.000000000001)
(llm-check "posttrain-distill-active-nonzero"
  (> (abs (mat-ref (car pt-distill) 1 0)) 0.0))

(llm-test-summary)
