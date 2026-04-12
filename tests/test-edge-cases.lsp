; ============================================================
; SISP Test Suite — Edge Cases and Boundary Conditions
; ============================================================

(print "=== EDGE CASE TESTS ===")

; --- nil behavior ---
(if (equal (car nil) nil) (print "PASS: car-nil-safe") (print "FAIL: car-nil-safe"))
(if (equal (cdr nil) nil) (print "PASS: cdr-nil-safe") (print "FAIL: cdr-nil-safe"))

; --- Zero handling ---
(if (equal (+ 0 0) 0) (print "PASS: zero-add") (print "FAIL: zero-add"))
(if (equal (* 0 0) 0) (print "PASS: zero-mul") (print "FAIL: zero-mul"))
(if (equal (^ 0 0) 1) (print "PASS: zero-pow-zero") (print "FAIL: zero-pow-zero"))
(if (equal (mod 0 1) 0) (print "PASS: zero-mod") (print "FAIL: zero-mod"))

; --- Negative number handling ---
(if (equal (* -1 -1) 1) (print "PASS: neg-mul-neg") (print "FAIL: neg-mul-neg"))
(if (equal (+ -1 1) 0) (print "PASS: neg-add-pos") (print "FAIL: neg-add-pos"))

; --- Rational edge cases ---
(if (equal (/ 2 2) 1) (print "PASS: rat-simplify-1") (print "FAIL: rat-simplify-1"))
(if (equal (/ 6 3) 2) (print "PASS: rat-simplify-int") (print "FAIL: rat-simplify-int"))
(if (equal (/ -1 2) -1/2) (print "PASS: rat-neg-num") (print "FAIL: rat-neg-num"))
(if (equal (/ 1 -2) -1/2) (print "PASS: rat-neg-den") (print "FAIL: rat-neg-den"))

; --- Single element operations ---
(if (equal (ord '(x)) 1) (print "PASS: ord-single") (print "FAIL: ord-single"))
(if (equal (car '(42)) 42) (print "PASS: car-single-elem") (print "FAIL: car-single-elem"))
(if (equal (cdr '(42)) nil) (print "PASS: cdr-single-elem") (print "FAIL: cdr-single-elem"))

; --- Dotted pair edge cases ---
(if (equal (car '(1 . 2)) 1) (print "PASS: dotted-car") (print "FAIL: dotted-car"))
(if (equal (cdr '(1 . 2)) 2) (print "PASS: dotted-cdr") (print "FAIL: dotted-cdr"))

; --- Identity operations ---
(if (equal (+ 0 42) 42) (print "PASS: add-identity") (print "FAIL: add-identity"))
(if (equal (* 1 42) 42) (print "PASS: mul-identity") (print "FAIL: mul-identity"))
(if (equal (^ 42 1) 42) (print "PASS: pow-identity") (print "FAIL: pow-identity"))

; --- Self-equality ---
(define s "test")
(if (equal (equal s s) t) (print "PASS: self-eq-string") (print "FAIL: self-eq-string"))
(define l '(1 2 3))
(if (equal (equal l l) t) (print "PASS: self-eq-list") (print "FAIL: self-eq-list"))
(define st {1 2 3})
(if (equal (equal st st) t) (print "PASS: self-eq-set") (print "FAIL: self-eq-set"))

; --- Typeof ---
(if (equal (typeof '(1 . 2)) 'cons) (print "PASS: typeof-pair") (print "FAIL: typeof-pair"))

; --- Atom edge cases ---
(if (equal (atomp tau) t) (print "PASS: atomp-tau") (print "FAIL: atomp-tau"))
(if (equal (atomp 0) t) (print "PASS: atomp-zero") (print "FAIL: atomp-zero"))

; --- If with computed conditions ---
(if (equal (if (equal (+ 1 1) 2) 'yes 'no) 'yes)
    (print "PASS: if-computed-cond") (print "FAIL: if-computed-cond"))
(if (equal (if (equal (+ 1 1) 3) 'yes 'no) 'no)
    (print "PASS: if-computed-cond-false") (print "FAIL: if-computed-cond-false"))

; --- Cond with all false ---
(if (equal (cond (nil 1) (nil 2) (nil 3)) nil)
    (print "PASS: cond-all-false") (print "FAIL: cond-all-false"))

; --- Let with shadowing ---
(define x 100)
(if (equal (let ((x 1)) (let ((x 2)) (let ((x 3)) x))) 3)
    (print "PASS: let-shadow-deep") (print "FAIL: let-shadow-deep"))
(if (equal x 100) (print "PASS: let-shadow-restore") (print "FAIL: let-shadow-restore"))

; --- Cat with longer strings ---
(define long1 "abcdefghijklmnopqrstuvwxyz")
(define long2 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define longcat (cat long1 long2))
(if (equal (strlen longcat) 52) (print "PASS: cat-long") (print "FAIL: cat-long"))

; --- Seq edge: two elements ---
(if (equal (seq 5 6) '(5 6)) (print "PASS: seq-two") (print "FAIL: seq-two"))

; --- Set with single element ---
(if (equal (ord {42}) 1) (print "PASS: set-single-ord") (print "FAIL: set-single-ord"))
(if (equal (in 42 {42}) t) (print "PASS: set-single-in") (print "FAIL: set-single-in"))

; --- Append single element lists ---
(if (equal (append '(1) '(2)) '(1 2)) (print "PASS: append-singles") (print "FAIL: append-singles"))

; --- Map on single element ---
(define (id x) x)
(if (equal (map id '(42)) '(42)) (print "PASS: map-single") (print "FAIL: map-single"))

; --- Pair with single elements ---
(if (equal (par '(a) '(1)) '((a 1))) (print "PASS: pair-single") (print "FAIL: pair-single"))

; --- Multiple undefs ---
(define a1 1) (define a2 2) (define a3 3)
(undef a1 a2 a3)
(print "PASS: multi-undef")

; cleanup
(undef s l st x long1 long2 longcat id)

(print "=== EDGE CASE TESTS DONE ===")
