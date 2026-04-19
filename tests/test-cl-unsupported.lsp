; ============================================================
; SISP Test Suite — Negative smoke tests for unsupported CL forms
; ============================================================
; Each case documents its current status and uses the error-as-pass
; pattern (flag stays "PASS" unless the form unexpectedly succeeds),
; so the suite stays green until the feature is implemented.

(print "=== UNSUPPORTED CL FORMS ===")

; --- let* (future CL target; already supported) ---
(if (equal (let* ((x 1) (y (+ x 1))) y) 2)
    (print "PASS: let*-supported")
    (print "FAIL: let*-supported"))

; --- funcall (future CL target; already supported) ---
(if (equal (funcall (lambda (x) x) 7) 7)
    (print "PASS: funcall-supported")
    (print "FAIL: funcall-supported"))

; --- flet (future CL target; not implemented) ---
(define flet-flag "PASS")
(progn (flet ((f (x) x)) (f 1)) (define flet-flag "FAIL"))
(print (cat flet-flag ": flet-unsupported"))

; --- function / #'NAME (intentionally unsupported for now) ---
(define function-flag "PASS")
(progn (function car) (define function-flag "FAIL"))
(print (cat function-flag ": function-unsupported"))
