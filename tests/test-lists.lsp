; ============================================================
; SISP Test Suite — List Operations
; ============================================================

(print "=== LIST TESTS ===")

; --- car / cdr ---
(if (equal (car '(1 2 3)) 1) (print "PASS: car-basic") (print "FAIL: car-basic"))
(if (equal (cdr '(1 2 3)) '(2 3)) (print "PASS: cdr-basic") (print "FAIL: cdr-basic"))
(if (equal (car '(a)) 'a) (print "PASS: car-single") (print "FAIL: car-single"))
(if (equal (cdr '(a)) nil) (print "PASS: cdr-single") (print "FAIL: cdr-single"))
(if (equal (car nil) nil) (print "PASS: car-nil") (print "FAIL: car-nil"))
(if (equal (cdr nil) nil) (print "PASS: cdr-nil") (print "FAIL: cdr-nil"))

; nested car/cdr
(if (equal (car '((1 2) 3)) '(1 2)) (print "PASS: car-nested") (print "FAIL: car-nested"))
(if (equal (car (cdr '(1 2 3))) 2) (print "PASS: cadr-manual") (print "FAIL: cadr-manual"))

; --- cons ---
(if (equal (cons 1 '(2 3)) '(1 2 3)) (print "PASS: cons-basic") (print "FAIL: cons-basic"))
(if (equal (cons 'a nil) '(a)) (print "PASS: cons-nil") (print "FAIL: cons-nil"))
(if (equal (car (cons 'x 'y)) 'x) (print "PASS: cons-pair-car") (print "FAIL: cons-pair-car"))
(if (equal (cdr (cons 'x 'y)) 'y) (print "PASS: cons-pair-cdr") (print "FAIL: cons-pair-cdr"))

; dotted pair
(if (equal (cons 1 2) '(1 . 2)) (print "PASS: cons-dotted") (print "FAIL: cons-dotted"))

; --- list ---
(if (equal (list 1 2 3) '(1 2 3)) (print "PASS: list-basic") (print "FAIL: list-basic"))
(if (equal (list 'a) '(a)) (print "PASS: list-single") (print "FAIL: list-single"))
(if (equal (list 1 '(2 3) 4) '(1 (2 3) 4)) (print "PASS: list-nested") (print "FAIL: list-nested"))
(if (equal (list) nil) (print "PASS: list-empty") (print "FAIL: list-empty"))

; --- ord (length) ---
(if (equal (ord '(1 2 3)) 3) (print "PASS: ord-basic") (print "FAIL: ord-basic"))
(if (equal (ord '(a)) 1) (print "PASS: ord-single") (print "FAIL: ord-single"))
(if (equal (ord nil) 0) (print "PASS: ord-nil") (print "FAIL: ord-nil"))
(if (equal (ord '(1 2 3 4 5 6 7 8 9 10)) 10) (print "PASS: ord-ten") (print "FAIL: ord-ten"))

; --- nth ---
(if (equal (nth 0 '(a b c)) 'a) (print "PASS: nth-first") (print "FAIL: nth-first"))
(if (equal (nth 1 '(a b c)) 'b) (print "PASS: nth-second") (print "FAIL: nth-second"))
(if (equal (nth 2 '(a b c)) 'c) (print "PASS: nth-third") (print "FAIL: nth-third"))
(if (equal (nth 10 '(a b c)) nil) (print "PASS: nth-out-of-range") (print "FAIL: nth-out-of-range"))

(define nth-neg-flag "PASS")
(progn (+ 0 (nth -1 '(a b c))) (define nth-neg-flag "FAIL"))
(print (cat nth-neg-flag ": nth-negative"))

; --- seq ---
(if (equal (seq 1 5) '(1 2 3 4 5)) (print "PASS: seq-basic") (print "FAIL: seq-basic"))
(if (equal (seq 0 3) '(0 1 2 3)) (print "PASS: seq-from-zero") (print "FAIL: seq-from-zero"))
(if (equal (seq -2 2) '(-2 -1 0 1 2)) (print "PASS: seq-neg-to-pos") (print "FAIL: seq-neg-to-pos"))
(if (equal (ord (seq 1 100)) 100) (print "PASS: seq-100-length") (print "FAIL: seq-100-length"))

; --- evlis (takes unevaluated list, evals each element) ---
(if (equal (evlis (1 2 3)) '(1 2 3)) (print "PASS: evlis-basic") (print "FAIL: evlis-basic"))
(if (equal (evlis ((+ 1 2) (* 3 4))) '(3 12)) (print "PASS: evlis-eval") (print "FAIL: evlis-eval"))

; --- map ---
(define (double x) (* x 2))
(if (equal (map double '(1 2 3)) '(2 4 6)) (print "PASS: map-double") (print "FAIL: map-double"))
(define (square x) (* x x))
(if (equal (map square '(1 2 3 4)) '(1 4 9 16)) (print "PASS: map-square") (print "FAIL: map-square"))
(define (inc x) (+ x 1))
(if (equal (map inc '(0 1 2)) '(1 2 3)) (print "PASS: map-inc") (print "FAIL: map-inc"))
(if (equal (map (lambda (x) x) '(1 2)) '(1 2)) (print "PASS: map-lambda-id") (print "FAIL: map-lambda-id"))
(if (equal (mapcar (lambda (x) x) '(1 2)) '(1 2)) (print "PASS: mapcar-lambda-id") (print "FAIL: mapcar-lambda-id"))
(if (equal (mapcar double '(1 2 3)) '(2 4 6)) (print "PASS: mapcar-identifier") (print "FAIL: mapcar-identifier"))

; --- append / union on lists ---
(if (equal (append '(1 2) '(3 4)) '(1 2 3 4)) (print "PASS: append-basic") (print "FAIL: append-basic"))
(if (equal (append '(a) '(b) '(c)) '(a b c)) (print "PASS: append-triple") (print "FAIL: append-triple"))

; --- pair ---
(if (equal (par '(a b c) '(1 2 3)) '((a 1) (b 2) (c 3)))
    (print "PASS: pair-basic") (print "FAIL: pair-basic"))
(if (equal (par '(x y) '(10 20)) '((x 10) (y 20)))
    (print "PASS: pair-two") (print "FAIL: pair-two"))

; --- push / pop ---
(define mystack '(2 3 4))
(push 1 mystack)
(if (equal mystack '(1 2 3 4)) (print "PASS: push-basic") (print "FAIL: push-basic"))
(define popped (pop mystack))
(if (equal popped 1) (print "PASS: pop-value") (print "FAIL: pop-value"))
(if (equal mystack '(2 3 4)) (print "PASS: pop-stack") (print "FAIL: pop-stack"))

; --- assoc ---
(define alist '((a 1) (b 2) (c 3)))
(if (equal (assoc 'a alist) '(a 1)) (print "PASS: assoc-first") (print "FAIL: assoc-first"))
(if (equal (assoc 'b alist) '(b 2)) (print "PASS: assoc-middle") (print "FAIL: assoc-middle"))
(if (equal (assoc 'c alist) '(c 3)) (print "PASS: assoc-last") (print "FAIL: assoc-last"))
(if (equal (assoc 'd alist) nil) (print "PASS: assoc-missing") (print "FAIL: assoc-missing"))

; assoc with integer keys
(define numalist '((1 a) (2 b) (3 c)))
(if (equal (assoc 2 numalist) '(2 b)) (print "PASS: assoc-int-key") (print "FAIL: assoc-int-key"))

; assoc with structural keys requires explicit equal test
(if (equal (assoc '(1 2) '(((1 2) found) ((3 4) other)) 'equal) '((1 2) found))
    (print "PASS: assoc-list-key") (print "FAIL: assoc-list-key"))
; assoc on structural keys fails with default eql (identity)
(if (equal (assoc '(1 2) '(((1 2) found) ((3 4) other))) nil)
    (print "PASS: assoc-list-key-eql-miss") (print "FAIL: assoc-list-key-eql-miss"))
; assoc with explicit eq test: symbols and numbers work
(if (equal (assoc 'b alist 'eq) '(b 2))
    (print "PASS: assoc-eq-symbol") (print "FAIL: assoc-eq-symbol"))
(if (equal (assoc 2 numalist 'eq) '(2 b))
    (print "PASS: assoc-eq-int") (print "FAIL: assoc-eq-int"))
; assoc with equalp test (case-insensitive strings)
(if (equal (assoc "HI" '(("hi" found)) 'equalp) '("hi" found))
    (print "PASS: assoc-equalp-str") (print "FAIL: assoc-equalp-str"))

; memberp with test argument
(if (equal (memberp '(1 2) '((1 2) (3 4)) 'equal) t)
    (print "PASS: memberp-list-equal") (print "FAIL: memberp-list-equal"))
(if (equal (memberp '(1 2) '((1 2) (3 4))) nil)
    (print "PASS: memberp-list-eql-miss") (print "FAIL: memberp-list-eql-miss"))
(if (equal (memberp 2 '(1 2 3) 'eql) t)
    (print "PASS: memberp-eql-int") (print "FAIL: memberp-eql-int"))

; --- consp ---
(if (equal (consp '(1 . 2)) t) (print "PASS: consp-pair") (print "FAIL: consp-pair"))
(if (equal (consp '(1 2 3)) t) (print "PASS: consp-list") (print "FAIL: consp-list"))
(if (equal (consp 'a) nil) (print "PASS: consp-atom") (print "FAIL: consp-atom"))

; --- Lisp-defined caar/cadr/cdar/cddr ---
(define testl '((1 2) (3 4)))
(if (equal (caar testl) 1) (print "PASS: caar") (print "FAIL: caar"))
(if (equal (cadr testl) '(3 4)) (print "PASS: cadr") (print "FAIL: cadr"))
(if (equal (cdar testl) '(2)) (print "PASS: cdar") (print "FAIL: cdar"))
(if (equal (cddr testl) nil) (print "PASS: cddr") (print "FAIL: cddr"))

; deeper accessors
(define deep '((( 1 2) (3 4)) ((5 6) (7 8))))
(if (equal (caaar deep) 1) (print "PASS: caaar") (print "FAIL: caaar"))
(if (equal (caddr '(1 2 3 4)) 3) (print "PASS: caddr") (print "FAIL: caddr"))
(if (equal (cdddr '(1 2 3 4)) '(4)) (print "PASS: cdddr") (print "FAIL: cdddr"))

; --- butlast ---
(if (equal (butlast '(1 2 3)) '(1 2)) (print "PASS: butlast-basic") (print "FAIL: butlast-basic"))
(if (equal (butlast '(a)) nil) (print "PASS: butlast-single") (print "FAIL: butlast-single"))
(if (equal (butlast '(1 2 3 4 5)) '(1 2 3 4)) (print "PASS: butlast-five") (print "FAIL: butlast-five"))

; --- get-names / get-values / unpair ---
(define pairs '((a 1) (b 2) (c 3)))
(if (equal (get-names pairs) '(a b c)) (print "PASS: get-names") (print "FAIL: get-names"))
(if (equal (get-values pairs) '(1 2 3)) (print "PASS: get-values") (print "FAIL: get-values"))
(if (equal (unpair pairs) '((a b c) (1 2 3))) (print "PASS: unpair") (print "FAIL: unpair"))

; --- succ / pred ---
(define seq1 '(a b c d e))
(if (equal (succ 'a seq1) 'b) (print "PASS: succ-first") (print "FAIL: succ-first"))
(if (equal (succ 'c seq1) 'd) (print "PASS: succ-middle") (print "FAIL: succ-middle"))
(if (equal (succ 'e seq1) nil) (print "PASS: succ-last") (print "FAIL: succ-last"))
(if (equal (pred 'b seq1) 'a) (print "PASS: pred-second") (print "FAIL: pred-second"))
(if (equal (pred 'e seq1) 'd) (print "PASS: pred-last") (print "FAIL: pred-last"))
(if (equal (pred 'a seq1) nil) (print "PASS: pred-first") (print "FAIL: pred-first"))

; cleanup
(undef double square inc mystack popped alist numalist testl deep seq1 pairs)

(print "=== LIST TESTS DONE ===")
