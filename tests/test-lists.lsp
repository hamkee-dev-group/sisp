; ============================================================
; SISP Test Suite — List Operations
; ============================================================

(load "lib/collections/pairs.lsp")
(load "lib/collections/sequence-extra.lsp")

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

; --- set-car / set-cdr (in-place mutation) ---
(define p1 (cons 1 2))
(set-car p1 99)
(if (equal p1 '(99 . 2)) (print "PASS: set-car") (print "FAIL: set-car"))
(set-cdr p1 100)
(if (equal p1 '(99 . 100)) (print "PASS: set-cdr") (print "FAIL: set-cdr"))
; aliasing: two names, same cell, one mutation seen by both
(define p2 (cons 'a 'b))
(define p3 p2)
(set-car p2 'z)
(if (equal p3 '(z . b)) (print "PASS: set-car-alias") (print "FAIL: set-car-alias"))
; mutable-box pattern gives real local state without lexical set!
(define (make-counter) (let ((box (cons 0 nil))) (lambda () (progn (set-car box (+ (car box) 1)) (car box)))))
(define ca (make-counter))
(define cb (make-counter))
(if (and (equal (ca) 1) (equal (ca) 2) (equal (cb) 1) (equal (ca) 3))
    (print "PASS: set-car-local-state") (print "FAIL: set-car-local-state"))

; --- list ---
(if (equal (list 1 2 3) '(1 2 3)) (print "PASS: list-basic") (print "FAIL: list-basic"))
(if (equal (list 'a) '(a)) (print "PASS: list-single") (print "FAIL: list-single"))
(if (equal (list 1 '(2 3) 4) '(1 (2 3) 4)) (print "PASS: list-nested") (print "FAIL: list-nested"))
(if (equal (list) nil) (print "PASS: list-empty") (print "FAIL: list-empty"))

; --- length of a list ---
(if (equal (length '(1 2 3)) 3) (print "PASS: ord-basic") (print "FAIL: ord-basic"))
(if (equal (length '(a)) 1) (print "PASS: ord-single") (print "FAIL: ord-single"))
(if (equal (length nil) 0) (print "PASS: ord-nil") (print "FAIL: ord-nil"))
(if (equal (length '(1 2 3 4 5 6 7 8 9 10)) 10) (print "PASS: ord-ten") (print "FAIL: ord-ten"))

; --- nth ---
(if (equal (nth 0 '(a b c)) 'a) (print "PASS: nth-first") (print "FAIL: nth-first"))
(if (equal (nth 1 '(a b c)) 'b) (print "PASS: nth-second") (print "FAIL: nth-second"))
(if (equal (nth 2 '(a b c)) 'c) (print "PASS: nth-third") (print "FAIL: nth-third"))
(if (equal (nth 10 '(a b c)) nil) (print "PASS: nth-out-of-range") (print "FAIL: nth-out-of-range"))

(if (eq (catch (+ 0 (nth -1 '(a b c))) (lambda (e) 'raised)) 'raised)
    (print "PASS: nth-negative")
    (print "FAIL: nth-negative"))

; --- seq ---
(if (equal (seq 1 5) '(1 2 3 4 5)) (print "PASS: seq-basic") (print "FAIL: seq-basic"))
(if (equal (seq 0 3) '(0 1 2 3)) (print "PASS: seq-from-zero") (print "FAIL: seq-from-zero"))
(if (equal (seq -2 2) '(-2 -1 0 1 2)) (print "PASS: seq-neg-to-pos") (print "FAIL: seq-neg-to-pos"))
(if (equal (length (seq 1 100)) 100) (print "PASS: seq-100-length") (print "FAIL: seq-100-length"))

; --- map ---
(define (double x) (* x 2))
(if (equal (map double '(1 2 3)) '(2 4 6)) (print "PASS: map-double") (print "FAIL: map-double"))
(define (square x) (* x x))
(if (equal (map square '(1 2 3 4)) '(1 4 9 16)) (print "PASS: map-square") (print "FAIL: map-square"))
(define (inc x) (+ x 1))
(if (equal (map inc '(0 1 2)) '(1 2 3)) (print "PASS: map-inc") (print "FAIL: map-inc"))
(if (equal (map (lambda (x) x) '(1 2)) '(1 2)) (print "PASS: map-lambda-id") (print "FAIL: map-lambda-id"))
(if (equal (map (lambda (x) x) '(1 2)) '(1 2)) (print "PASS: mapcar-lambda-id") (print "FAIL: mapcar-lambda-id"))
(if (equal (map double '(1 2 3)) '(2 4 6)) (print "PASS: mapcar-identifier") (print "FAIL: mapcar-identifier"))
(if (equal (map (lambda (x) x) nil) nil) (print "PASS: mapcar-empty") (print "FAIL: mapcar-empty"))

; --- append / union on lists ---
(if (equal (append '(1 2) '(3 4)) '(1 2 3 4)) (print "PASS: append-basic") (print "FAIL: append-basic"))
(if (equal (append '(a) '(b) '(c)) '(a b c)) (print "PASS: append-triple") (print "FAIL: append-triple"))
(if (equal (append) nil) (print "PASS: append-empty") (print "FAIL: append-empty"))
(if (equal (append '(a b) 'tail) '(a b . tail)) (print "PASS: append-dotted-tail") (print "FAIL: append-dotted-tail"))

; --- zip ---
(if (equal (zip '(a b c) '(1 2 3)) '((a 1) (b 2) (c 3)))
    (print "PASS: zip-basic") (print "FAIL: zip-basic"))
(if (equal (zip '(x y) '(10 20)) '((x 10) (y 20)))
    (print "PASS: zip-two") (print "FAIL: zip-two"))

; --- push / pop ---
(define mystack '(2 3 4))
(push 1 mystack)
(if (equal mystack '(1 2 3 4)) (print "PASS: push-basic") (print "FAIL: push-basic"))
(define popped (pop mystack))
(if (equal popped 1) (print "PASS: pop-value") (print "FAIL: pop-value"))
(if (equal mystack '(2 3 4)) (print "PASS: pop-stack") (print "FAIL: pop-stack"))
(define empty-stack nil)
(push 1 empty-stack)
(if (equal empty-stack '(1)) (print "PASS: push-empty") (print "FAIL: push-empty"))
(pop empty-stack)
(if (equal empty-stack nil) (print "PASS: pop-to-empty") (print "FAIL: pop-to-empty"))
(if (equal (pop empty-stack) nil) (print "PASS: pop-empty") (print "FAIL: pop-empty"))

; --- assoc ---
(define alist '((a 1) (b 2) (c 3)))
(if (equal (assoc 'a alist) '(a 1)) (print "PASS: assoc-first") (print "FAIL: assoc-first"))
(if (equal (assoc 'b alist) '(b 2)) (print "PASS: assoc-middle") (print "FAIL: assoc-middle"))
(if (equal (assoc 'c alist) '(c 3)) (print "PASS: assoc-last") (print "FAIL: assoc-last"))
(if (equal (assoc 'd alist) nil) (print "PASS: assoc-missing") (print "FAIL: assoc-missing"))
(if (equal (assoc 'a nil) nil) (print "PASS: assoc-empty") (print "FAIL: assoc-empty"))

; assoc with integer keys
(define numalist '((1 a) (2 b) (3 c)))
(if (equal (assoc 2 numalist) '(2 b)) (print "PASS: assoc-int-key") (print "FAIL: assoc-int-key"))

; assoc compares keys with equal by DEFAULT, so structural and string keys
; work without an explicit test. Under eq, a list or string key is compared
; by identity, so a key equal to the stored one would not be found.
(if (equal (assoc '(1 2) '(((1 2) found) ((3 4) other))) '((1 2) found))
    (print "PASS: assoc-list-key-default") (print "FAIL: assoc-list-key-default"))
(if (equal (assoc "hi" '(("hi" found) ("bye" other))) '("hi" found))
    (print "PASS: assoc-string-key-default") (print "FAIL: assoc-string-key-default"))
(if (equal (assoc "hi" (list (list (cat "h" "i") 'found))) '("hi" found))
    (print "PASS: assoc-computed-string-key") (print "FAIL: assoc-computed-string-key"))
; an explicit equal test is accepted and means the same thing
(if (equal (assoc '(1 2) '(((1 2) found) ((3 4) other)) 'equal) '((1 2) found))
    (print "PASS: assoc-list-key") (print "FAIL: assoc-list-key"))
; explicit eq is available, and misses on structural keys
(if (equal (assoc '(1 2) '(((1 2) found) ((3 4) other)) 'eq) nil)
    (print "PASS: assoc-eq-list-miss") (print "FAIL: assoc-eq-list-miss"))
; assoc with explicit eq test: symbols and numbers work
(if (equal (assoc 'b alist 'eq) '(b 2))
    (print "PASS: assoc-eq-symbol") (print "FAIL: assoc-eq-symbol"))
(if (equal (assoc 2 numalist 'eq) '(2 b))
    (print "PASS: assoc-eq-int") (print "FAIL: assoc-eq-int"))

; in with test argument
(if (equal (in '(1 2) '((1 2) (3 4)) 'equal) t)
    (print "PASS: memberp-list-equal") (print "FAIL: memberp-list-equal"))
(if (equal (in '(1 2) '((1 2) (3 4))) nil)
    (print "PASS: memberp-list-eql-miss") (print "FAIL: memberp-list-eql-miss"))
(if (equal (in 2 '(1 2 3) 'eq) t)
    (print "PASS: memberp-eql-int") (print "FAIL: memberp-eql-int"))
(if (equal (in 2 nil) nil)
    (print "PASS: memberp-empty") (print "FAIL: memberp-empty"))

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

; --- standard list utilities (builtins and prelude) ---
(if (equal (length '(1 2 3 4)) 4) (print "PASS: length") (print "FAIL: length"))
(if (equal (length nil) 0) (print "PASS: length-empty") (print "FAIL: length-empty"))
(if (equal (take '(1 2 3 4 5) 3) '(1 2 3)) (print "PASS: take") (print "FAIL: take"))
(if (equal (take '(1 2) 5) '(1 2)) (print "PASS: take-over") (print "FAIL: take-over"))
(if (equal (drop '(1 2 3 4 5) 2) '(3 4 5)) (print "PASS: drop") (print "FAIL: drop"))
(if (equal (count (lambda (x) (evenp x)) '(1 2 3 4 5 6)) 3) (print "PASS: count") (print "FAIL: count"))
(if (equal (contains 3 '(1 2 3 4)) t) (print "PASS: contains") (print "FAIL: contains"))
(if (equal (contains 9 '(1 2 3 4)) nil) (print "PASS: contains-miss") (print "FAIL: contains-miss"))
(if (equal (index-of 3 '(10 20 3 40)) 2) (print "PASS: index-of") (print "FAIL: index-of"))
(if (equal (index-of 99 '(1 2 3)) nil) (print "PASS: index-of-miss") (print "FAIL: index-of-miss"))
(if (equal (flatten '(1 (2 (3 4)) 5)) '(1 2 3 4 5)) (print "PASS: flatten") (print "FAIL: flatten"))
(if (equal (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c))) (print "PASS: zip") (print "FAIL: zip"))
(if (equal (sort (lambda (a b) (< a b)) '(3 1 4 1 5 9 2 6)) '(1 1 2 3 4 5 6 9)) (print "PASS: sort") (print "FAIL: sort"))
(if (equal (dedup '(1 2 1 3 2 4)) '(1 2 3 4)) (print "PASS: dedup") (print "FAIL: dedup"))

; --- flatmap concatenates ONE level; flatten stays recursive ------
; flatmap removes exactly one level, so a mapper returning lists of lists --
; the permutation and combination idiom -- keeps its inner lists. A fully
; recursive flatten would collapse them; deep-flatmap does that on request.
(if (equal (flatmap (lambda (x) (list (list x))) '(1 2)) '((1) (2)))
    (print "PASS: flatmap-removes-exactly-one-level")
    (print "FAIL: flatmap-removes-exactly-one-level"))
(if (equal (flatmap (lambda (x) (list x x)) '(1 2)) '(1 1 2 2))
    (print "PASS: flatmap-ordinary-duplication")
    (print "FAIL: flatmap-ordinary-duplication"))
(if (eq (flatmap (lambda (x) (list x)) nil) nil)
    (print "PASS: flatmap-empty-input")
    (print "FAIL: flatmap-empty-input"))
(if (eq (catch (flatmap (lambda (x) x) '(1 2)) (lambda (e) 'raised)) 'raised)
    (print "PASS: flatmap-rejects-atom-result")
    (print "FAIL: flatmap-rejects-atom-result"))
(if (equal (deep-flatmap (lambda (x) (list (list x))) '(1 2)) '(1 2))
    (print "PASS: deep-flatmap-keeps-recursive-behaviour")
    (print "FAIL: deep-flatmap-keeps-recursive-behaviour"))
; the permutation idiom, which needs exactly one level removed
(define (tl-perms lst)
  (cond ((eq lst nil) (list nil))
        (t (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                 (tl-perms (remove (lambda (y) (= x y)) lst))))
             lst))))
(if (equal (tl-perms '(1 2 3))
           '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))
    (print "PASS: flatmap-supports-permutations")
    (print "FAIL: flatmap-supports-permutations"))

; --- boundary contracts -------------------------------------------
; take truncates rather than raising when the count exceeds the length,
; and a zero count is empty.
(if (equal (take '(1 2) 9) '(1 2))
    (print "PASS: take-over-length-truncates")
    (print "FAIL: take-over-length-truncates"))
(if (eq (take '(1 2) 0) nil)
    (print "PASS: take-zero-is-empty")
    (print "FAIL: take-zero-is-empty"))
; A negative count is empty, matching list-tail's (<= k 0) convention.
(if (eq (take '(1 2 3) -1) nil)
    (print "PASS: take-negative-is-empty")
    (print "FAIL: take-negative-is-empty"))
; zip stops at the shorter list rather than requiring equal lengths.
(if (equal (zip '(1 2 3) '(9)) '((1 9)))
    (print "PASS: zip-truncates-to-shorter")
    (print "FAIL: zip-truncates-to-shorter"))
(if (eq (butlast nil) nil)
    (print "PASS: butlast-of-nil-is-nil")
    (print "FAIL: butlast-of-nil-is-nil"))
; flatten requires proper lists; a dotted pair is an error, not a silent result.
(if (eq (catch (flatten (cons 1 2)) (lambda (e) 'raised)) 'raised)
    (print "PASS: flatten-rejects-dotted-pair")
    (print "FAIL: flatten-rejects-dotted-pair"))
; Sort is stable: equal keys retain their input order.
(if (equal (sort (lambda (a b) (< (car a) (car b)))
                 (list (list 1 'a) (list 1 'b) (list 0 'c)))
           '((0 c) (1 a) (1 b)))
    (print "PASS: sort-is-stable-on-equal-keys")
    (print "FAIL: sort-is-stable-on-equal-keys"))

(define sort-source '(3 1 2))
(define sort-result (sort < sort-source))
(if (and (equal sort-source '(3 1 2))
         (equal sort-result '(1 2 3)))
    (print "PASS: sort-does-not-modify-input")
    (print "FAIL: sort-does-not-modify-input"))
(set-car sort-result 9)
(if (equal sort-source '(3 1 2))
    (print "PASS: sort-copies-list-cells")
    (print "FAIL: sort-copies-list-cells"))

(define (sort-by-length a b) (< (length a) (length b)))
(if (equal (sort sort-by-length '((1 2 3) (1) (1 2)))
           '((1) (1 2) (1 2 3)))
    (print "PASS: sort-calls-user-comparator")
    (print "FAIL: sort-calls-user-comparator"))

(define sort-function-evaluations 0)
(sort (progn
        (setq sort-function-evaluations
              (+ sort-function-evaluations 1))
        <)
      '(3 2 1))
(if (= sort-function-evaluations 1)
    (print "PASS: sort-evaluates-comparator-once")
    (print "FAIL: sort-evaluates-comparator-once"))
(if (eq (catch (sort < (cons 1 2)) (lambda (e) 'raised)) 'raised)
    (print "PASS: sort-rejects-improper-list")
    (print "FAIL: sort-rejects-improper-list"))
(undef sort-source sort-result sort-by-length sort-function-evaluations)

; --- standard functions depend on no global helper names ------------
; Global names resolve at call time, so a helper with a global name would let
; a user definition silently change a standard function. reverse and sort use
; no global helpers, so there is no name to collide with.
(define reverse-acc 'user-value)
(define acc 'user-value)
(if (equal (reverse '(1 2 3)) '(3 2 1))
    (print "PASS: reverse-helper-cannot-be-clobbered")
    (print "FAIL: reverse-helper-cannot-be-clobbered"))
(undef reverse-acc)
(undef acc)
; a user-defined insert (set-based-sorting.lsp defines one) cannot affect sort
(define (insert x sorted) 'user-value)
(if (equal (sort < '(3 1 2)) '(1 2 3))
    (print "PASS: sort-helper-cannot-be-clobbered")
    (print "FAIL: sort-helper-cannot-be-clobbered"))
(undef insert)
; the helpers have no global name at all, not merely a prefixed one
(if (and (eq (catch prelude-reverse-acc (lambda (e) 'unbound)) 'unbound)
         (eq (catch insert (lambda (e) 'unbound)) 'unbound))
    (print "PASS: prelude-helpers-have-no-global-name")
    (print "FAIL: prelude-helpers-have-no-global-name"))

; --- length has no recursion ceiling -------------------------------
; length counts iteratively in C, so a long list cannot exhaust the stack
; (a per-element recursion fails past roughly 14000 elements).
(define (tl-mk n acc) (cond ((= n 0) acc) (t (tl-mk (- n 1) (cons n acc)))))
(if (= (length (tl-mk 50000 nil)) 50000)
    (print "PASS: length-handles-50000-elements")
    (print "FAIL: length-handles-50000-elements"))
; one length over every sequence: list, string, and set
(if (and (= (length (quote (1 2 3))) 3)
         (and (= (length "abc") 3) (= (length {1 2 3}) 3)))
    (print "PASS: length-spans-list-string-and-set")
    (print "FAIL: length-spans-list-string-and-set"))
(if (and (= (length nil) 0)
         (and (= (length "") 0) (= (length {}) 0)))
    (print "PASS: length-of-every-empty-is-zero")
    (print "FAIL: length-of-every-empty-is-zero"))
(if (equal (catch (length 5) (lambda (e) (cadr e))) "LENGTH: NOT A SEQUENCE")
    (print "PASS: length-rejects-a-non-sequence")
    (print "FAIL: length-rejects-a-non-sequence"))
(if (equal (catch (length (quote (1 . 2))) (lambda (e) (cadr e)))
           "LENGTH: IMPROPER SEQUENCE")
    (print "PASS: length-rejects-an-improper-list")
    (print "FAIL: length-rejects-an-improper-list"))
(if (equal (catch (length (quote {x : (> x 0)})) (lambda (e) (cadr e)))
           "LENGTH: NOT A SEQUENCE")
    (print "PASS: length-rejects-a-comprehension-set")
    (print "FAIL: length-rejects-a-comprehension-set"))
(undef tl-mk)

(print "=== LIST TESTS DONE ===")
