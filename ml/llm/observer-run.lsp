; observer-run.lsp — human-readable, fixed pure-SISP microscope report.
; Each expensive probe is a separate top-level form so SISP collects its cache.

(print "=== pure SISP model microscope ===")
(define *observer-starting-weight-norm*
  (observer-component-norm *matrix-parameters* (quote data)))

(observer-model-report)
(observer-training-report)
(observer-contract-report)
(observer-parameter-summary)
(cond (*matrix-observer-full* (observer-parameter-report)) (t nil))

(observer-embedding-neighbors "(" 5)
(observer-embedding-neighbors ")" 5)
(observer-embedding-neighbors ";" 5)

; Fixed teacher-forced probes make reports comparable across checkpoints.
(cond ((= (length *matrix-prompt-path*) 0)
  (observer-probe-report "close-definition"
    "(define (square x) (* x x)" ")" 8)) (t nil))
(cond ((= (length *matrix-prompt-path*) 0)
  (observer-probe-report "list-result"
    "(car '(1 2 3)) ; => " "1" 8)) (t nil))
(cond ((= (length *matrix-prompt-path*) 0)
  (observer-probe-report "comment-to-code"
    (list-to-str (list "; add two numbers" *observer-newline*
      "(define (add a b)" *observer-newline* "  (")) "+" 8)) (t nil))
(cond ((= (length *matrix-prompt-path*) 0)
  (observer-probe-report "matrix-result"
    "(mat-ref [[1 2] [3 4]] 1 0) ; => " "4" 8)) (t nil))
(cond ((= (length *matrix-prompt-path*) 0)
  (observer-probe-report "agent-action"
    (list-to-str (list ";G:Compute 2 plus 3." *observer-newline*
      ";O:start" *observer-newline* ";M:" *observer-newline*
      ";A:(agent-eval 'F)|(agent-answer X)" *observer-newline*)) "(" 8))
  (t nil))

; A custom prompt file replaces the fixed deep prompt and gets no assumed target.
(cond ((> (length *matrix-prompt-path*) 0)
  (observer-custom-report (read-file *matrix-prompt-path*) 12)) (t nil))
(cond ((> (length *matrix-prompt-path*) 0)
  (observer-deep-report (read-file *matrix-prompt-path*)))
  (t (observer-deep-report
    (list-to-str (list "; sum a list" *observer-newline*
      "(define (sum xs)" *observer-newline*
      "  (cond ((eq xs nil) 0)" *observer-newline*
      "        (t (+ (car xs) (sum (cdr xs")))))

(observer-integrity-report *observer-starting-weight-norm*)
(print (list (quote observer-finished) (quote read-only) t
  (quote runtime) (runtime)))
