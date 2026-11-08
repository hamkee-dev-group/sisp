; Tests for the runtime timer.
(print "=== RUNTIME TESTS ===")

(if (numberp (runtime))
    (print "PASS: runtime-numeric")
    (print "FAIL: runtime-numeric"))

(print "=== RUNTIME TESTS DONE ===")
