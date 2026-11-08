; ============================================================
; SISP Test Suite — Error model (error / catch)
; An error is data: (error MESSAGE CULPRIT). catch runs a body; on a
; raise it calls a one-argument handler on that value, else returns the
; body's value.
; ============================================================

(print "=== ERROR MODEL TESTS ===")

; --- catch returns the body value when nothing is raised ---
(if (equal (catch (+ 1 2) (lambda (e) 999)) 3) (print "PASS: catch-no-error") (print "FAIL: catch-no-error"))
(if (equal (catch 42 (lambda (e) 0)) 42) (print "PASS: catch-literal") (print "FAIL: catch-literal"))

; --- the handler runs on a raise and its value is returned ---
(if (equal (catch (error "boom") (lambda (e) 99)) 99) (print "PASS: catch-handler-value") (print "FAIL: catch-handler-value"))
(if (equal (catch (error "boom") (lambda (e) (list 1 2 3))) (list 1 2 3)) (print "PASS: catch-handler-list") (print "FAIL: catch-handler-list"))

; --- an error is a tagged list (error MESSAGE CULPRIT) ---
(if (eq (catch (error "boom") (lambda (e) (car e))) (quote error)) (print "PASS: error-is-tagged") (print "FAIL: error-is-tagged"))
(if (equal (catch (error "boom") (lambda (e) (cadr e))) "boom") (print "PASS: error-message") (print "FAIL: error-message"))
(if (equal (catch (error "boom") (lambda (e) (caddr e))) nil) (print "PASS: error-no-culprit") (print "FAIL: error-no-culprit"))
(if (equal (catch (error "boom" 42) (lambda (e) (caddr e))) 42) (print "PASS: error-culprit") (print "FAIL: error-culprit"))

; --- internal (interpreter) errors are catchable as the same data ---
(if (eq (catch (car 0) (lambda (e) (car e))) (quote error)) (print "PASS: internal-is-error") (print "FAIL: internal-is-error"))
(if (equal (catch (car 0) (lambda (e) (cadr e))) "CAR: UNDEFINED") (print "PASS: internal-message") (print "FAIL: internal-message"))
(if (equal (catch (+ 1 "x") (lambda (e) (cadr e))) "+: NOT NUMERIC") (print "PASS: assert-message") (print "FAIL: assert-message"))
(if (equal (catch (+ 1 "x") (lambda (e) (caddr e))) "x") (print "PASS: assert-culprit") (print "FAIL: assert-culprit"))
(if (equal (catch (length 5) (lambda (e) (cadr e))) "LENGTH: NOT A SEQUENCE") (print "PASS: length-error-message") (print "FAIL: length-error-message"))
(if (equal (catch (length 5) (lambda (e) (caddr e))) 5) (print "PASS: length-error-culprit") (print "FAIL: length-error-culprit"))

; --- errors propagate across function calls ---
(define (raiser) (error "deep"))
(if (equal (catch (raiser) (lambda (e) (cadr e))) "deep") (print "PASS: propagates-through-call") (print "FAIL: propagates-through-call"))

; --- an error mid-expression unwinds the whole expression ---
(if (equal (catch (+ 1 (error "mid")) (lambda (e) (cadr e))) "mid") (print "PASS: mid-expression") (print "FAIL: mid-expression"))

; --- nested catch: the innermost handler wins ---
(if (eq (catch (catch (error "in") (lambda (e) (quote inner))) (lambda (e) (quote outer))) (quote inner))
    (print "PASS: nested-inner-wins") (print "FAIL: nested-inner-wins"))

; --- a handler may re-raise; the outer catch takes it ---
(if (equal (catch (catch (error "a") (lambda (e) (error "b"))) (lambda (e) (cadr e))) "b")
    (print "PASS: rethrow") (print "FAIL: rethrow"))

; --- the environment is restored to the catch point after a raise ---
(define (with-local)
  (let ((x 10))
    (catch (error "x") (lambda (e) x))))
(if (equal (with-local) 10) (print "PASS: env-restored") (print "FAIL: env-restored"))

; --- catch composes with recursion ---
(define (sum-or-fail n)
  (cond ((< n 0) (error "negative"))
        ((= n 0) 0)
        (t (+ n (sum-or-fail (- n 1))))))
(if (equal (sum-or-fail 5) 15) (print "PASS: recursion-normal") (print "FAIL: recursion-normal"))
(if (equal (catch (sum-or-fail -1) (lambda (e) (cadr e))) "negative") (print "PASS: recursion-error") (print "FAIL: recursion-error"))

(print "=== ERROR MODEL TESTS DONE ===")
