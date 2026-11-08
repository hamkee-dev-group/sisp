; Report the sample after top-level-scheduled generation.

(define *matrix-generated-text*
  (llm-decode *matrix-generated-ids* *matrix-vocabulary*))
(define *matrix-visible-generated-text*
  (cat *matrix-raw-prompt*
    (substr (length *matrix-generation-prompt*) *matrix-generation-count*
      *matrix-generated-text*)))
(print (list (quote generated) *matrix-visible-generated-text*))

(llm-reset-tests)
(llm-check "matrix-training-ran-or-inference-only"
  (cond ((= *matrix-requested-steps* 0)
    (= *matrix-step* *matrix-run-start-step*))
    (t (> *matrix-step* *matrix-run-start-step*))))
; Validation is a model-selection measurement, not a per-chunk invariant.  A
; healthy long run can plateau or fluctuate upward for one checkpoint; the
; continuous trainer must still publish that finite, resumable state.
(llm-check "matrix-evaluation-finite"
  (and (matrix-finite-number *matrix-final-training-loss*)
       (matrix-finite-number *matrix-final-loss*)))
(llm-check "matrix-generated-length"
  (= (length *matrix-visible-generated-text*)
     (+ (length *matrix-raw-prompt*) *matrix-generation-count*)))
(llm-test-summary)
(cond ((> *llm-tests-failed* 0) (quit 2)) (t nil))
(print "=== done ===")
