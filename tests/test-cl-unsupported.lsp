; ============================================================
; SISP Test Suite — Negative smoke tests for unsupported CL forms
; ============================================================
; Each case documents its current status and catches the expected error.

(print "=== UNSUPPORTED CL FORMS ===")

; --- let* (future CL target; already supported) ---
(if (equal (let* ((x 1) (y (+ x 1))) y) 2)
    (print "PASS: let*-supported")
    (print "FAIL: let*-supported"))

; --- funcall (future CL target; already supported) ---
(if (equal (funcall (lambda (x) x) 7) 7)
    (print "PASS: funcall-supported")
    (print "FAIL: funcall-supported"))

; --- defun (removed; define is the single definition form) ---
(if (eq (catch (defun f (x) x)
               (lambda (e) 'unsupported))
        'unsupported)
    (print "PASS: defun-unsupported")
    (print "FAIL: defun-unsupported"))

; --- flet (future CL target; not implemented) ---
(if (eq (catch (flet ((f (x) x)) (f 1))
               (lambda (e) 'unsupported))
        'unsupported)
    (print "PASS: flet-unsupported")
    (print "FAIL: flet-unsupported"))

; --- function / #'NAME (intentionally unsupported for now) ---
(if (eq (catch (function car)
               (lambda (e) 'unsupported))
        'unsupported)
    (print "PASS: function-unsupported")
    (print "FAIL: function-unsupported"))

(print "=== UNSUPPORTED CL FORMS DONE ===")
