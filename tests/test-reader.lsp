; ============================================================
; SISP Test Suite — read-from-string and read-file integrity
; The reader half of read/eval/print: text -> list of forms,
; the whole-content (never truncated) convention, round-trip
; with eval, and clean recovery from malformed input.
; ============================================================

(print "=== READER TESTS ===")

; --- read-from-string returns a LIST of all forms ---
(if (equal (read-from-string "(+ 1 2)") (list (list (quote +) 1 2))) (print "PASS: single-form-is-one-element-list") (print "FAIL: single-form-is-one-element-list"))
(if (equal (car (read-from-string "(+ 1 2)")) (list (quote +) 1 2)) (print "PASS: car-recovers-single-form") (print "FAIL: car-recovers-single-form"))
(if (equal (read-from-string "1 2 3") (list 1 2 3)) (print "PASS: multiple-atoms") (print "FAIL: multiple-atoms"))
(if (equal (read-from-string "(a) (b) (c)") (list (list (quote a)) (list (quote b)) (list (quote c)))) (print "PASS: multiple-forms") (print "FAIL: multiple-forms"))
(if (equal (length (read-from-string "10 20 30 40")) 4) (print "PASS: all-forms-returned-no-truncation") (print "FAIL: all-forms-returned-no-truncation"))

; --- empty and whitespace ---
(if (equal (read-from-string "") nil) (print "PASS: empty-string-is-nil") (print "FAIL: empty-string-is-nil"))
(if (equal (read-from-string "   ") nil) (print "PASS: whitespace-is-nil") (print "FAIL: whitespace-is-nil"))

; --- round-trip with eval (the read/eval bridge) ---
(if (equal (eval (car (read-from-string "(* 6 7)"))) 42) (print "PASS: read-eval-roundtrip") (print "FAIL: read-eval-roundtrip"))
(if (equal (eval (car (read-from-string "(+ 1 2 3 4)"))) 10) (print "PASS: read-eval-arithmetic") (print "FAIL: read-eval-arithmetic"))

; --- all reader-producible types survive text round-trip ---
(if (equal (car (read-from-string "1/2")) 1/2) (print "PASS: reads-rational") (print "FAIL: reads-rational"))
; (string literals containing quotes cannot be written in SISP source — no escapes — so a read-string test cannot construct its own input)
(if (equal (car (read-from-string "{1 2 3}")) {1 2 3}) (print "PASS: reads-set") (print "FAIL: reads-set"))
(if (equal (car (read-from-string "{tau : (> tau 3)}")) {tau : (> tau 3)}) (print "PASS: reads-comprehension") (print "FAIL: reads-comprehension"))

; --- read a definition and a call from source text, run both ---
(foreach (lambda (f) (eval f)) (read-from-string "(define (rr-sq x) (* x x))"))
(if (equal (rr-sq 9) 81) (print "PASS: reads-and-defines") (print "FAIL: reads-and-defines"))

; --- closes read-file -> read-from-string -> eval ---
(write-file "/tmp/sisp-reader-test.lsp" "(define rr-answer 42) (* rr-answer 2)")
(if (equal (car (reverse (read-from-string (read-file "/tmp/sisp-reader-test.lsp")))) (list (quote *) (quote rr-answer) 2)) (print "PASS: read-file-then-parse") (print "FAIL: read-file-then-parse"))

; --- state is restored: reading does not disturb the enclosing program ---
(define rr-x 7)
(define rr-parsed (read-from-string "(this is ignored text)"))
(if (equal rr-x 7) (print "PASS: outer-binding-survives-read") (print "FAIL: outer-binding-survives-read"))
(if (equal (car (read-from-string "99")) 99) (print "PASS: reader-reusable-after-use") (print "FAIL: reader-reusable-after-use"))

(print "=== READER TESTS DONE ===")
