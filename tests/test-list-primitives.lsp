; ============================================================
; SISP Test Suite — List Primitives vs Their Prelude Originals
; ============================================================
;
; reverse, last, butlast, drop, take, contains, index-of, flatten, zip,
; dedup and set-to-list were prelude definitions before they moved to C.
; This file keeps the original definitions as a reference implementation
; and checks the C builtins agree with them across awkward inputs.
;
; This is not redundant with test-lists.lsp: that file checks the cases
; someone thought to write down, this one checks the C against the exact
; code it replaced. It is what caught reverse and flatten mishandling a
; set -- the prelude walked sets with car/cdr, and an early C version
; type-tested for OBJ_CONS and silently returned the wrong thing.

(print "=== LIST PRIMITIVE DIFFERENTIAL TESTS ===")

(define (lp-reverse l) (labels (((acc m a) (cond ((eq m nil) a) (t (acc (cdr m) (cons (car m) a)))))) (acc l nil)))
(define (lp-last l) (cond ((eq (cdr l) nil) (car l)) (t (lp-last (cdr l)))))
(define (lp-butlast l) (cond ((eq (cdr l) nil) nil) (t (cons (car l) (lp-butlast (cdr l))))))
(define (lp-drop l k) (cond ((<= k 0) l) ((eq l nil) nil) (t (lp-drop (cdr l) (- k 1)))))
(define (lp-take l k) (cond ((or (eq l nil) (<= k 0)) nil) (t (cons (car l) (lp-take (cdr l) (- k 1))))))
(define (lp-contains x l) (cond ((eq l nil) nil) ((equal x (car l)) t) (t (lp-contains x (cdr l)))))
(define (lp-index-of x l) (labels (((walk m i) (cond ((eq m nil) nil) ((equal x (car m)) i) (t (walk (cdr m) (+ i 1)))))) (walk l 0)))
(define (lp-flatten l) (cond ((eq l nil) nil) ((consp (car l)) (append (lp-flatten (car l)) (lp-flatten (cdr l)))) (t (cons (car l) (lp-flatten (cdr l))))))
(define (lp-zip a b) (cond ((or (eq a nil) (eq b nil)) nil) (t (cons (list (car a) (car b)) (lp-zip (cdr a) (cdr b))))))
(define (lp-dedup l) (labels (((walk m seen) (cond ((eq m nil) (reverse seen)) ((contains (car m) seen) (walk (cdr m) seen)) (t (walk (cdr m) (cons (car m) seen)))))) (walk l nil)))
(define (lp-set-to-list s) (cond ((eq s nil) nil) ((eq s {}) nil) ((comp-set-p s) (error "COMPREHENSION" s)) (t (cons (car s) (lp-set-to-list (cdr s))))))

; catch needs the expression unevaluated, so every case is passed as a thunk
(define (lp-try th) (catch (th) (lambda (e) (quote raised))))

; the inputs that matter are the ones nobody writes a test for: a set walks
; like a list, an empty set is not nil, an atom and a dotted tail must raise
(define lp-inputs (list '(1 2 3) nil '(1) {1 2 3} {} "abc" '((1 2) (3 4)) '(1 . 2) 5 '(1 nil 2)))

(define lp-fails 0)
(define (lp-check old new)
  (cond ((equal old new) nil) (t (setq lp-fails (+ lp-fails 1)))))

(define (lp-report name)
  (cond ((= lp-fails 0) (print (cat "PASS: " name)))
        (t (print (cat "FAIL: " name))))
  (setq lp-fails 0))

(define (lp-1 name oldf newf)
  (foreach (lambda (v) (lp-check (lp-try (lambda () (oldf v)))
                                 (lp-try (lambda () (newf v))))) lp-inputs)
  (lp-report name))

(define (lp-2 name oldf newf k)
  (foreach (lambda (v) (lp-check (lp-try (lambda () (oldf v k)))
                                 (lp-try (lambda () (newf v k))))) lp-inputs)
  (lp-report name))

(lp-1 "reverse-matches-prelude"     lp-reverse     reverse)
(lp-1 "last-matches-prelude"        lp-last        last)
(lp-1 "butlast-matches-prelude"     lp-butlast     butlast)
(lp-1 "flatten-matches-prelude"     lp-flatten     flatten)
(lp-1 "dedup-matches-prelude"       lp-dedup       dedup)
(lp-1 "set-to-list-matches-prelude" lp-set-to-list set-to-list)

(lp-2 "take-2-matches-prelude"  lp-take take 2)
(lp-2 "take-0-matches-prelude"  lp-take take 0)
(lp-2 "take-neg-matches-prelude" lp-take take -1)
(lp-2 "drop-2-matches-prelude"  lp-drop drop 2)
(lp-2 "drop-0-matches-prelude"  lp-drop drop 0)
(lp-2 "drop-neg-matches-prelude" lp-drop drop -1)

(foreach (lambda (v) (lp-check (lp-try (lambda () (lp-contains 1 v)))
                               (lp-try (lambda () (contains 1 v))))) lp-inputs)
(lp-report "contains-matches-prelude")

(foreach (lambda (v) (lp-check (lp-try (lambda () (lp-index-of 2 v)))
                               (lp-try (lambda () (index-of 2 v))))) lp-inputs)
(lp-report "index-of-matches-prelude")

(foreach (lambda (v) (lp-check (lp-try (lambda () (lp-zip v '(a b c))))
                               (lp-try (lambda () (zip v '(a b c)))))) lp-inputs)
(lp-report "zip-matches-prelude")

; the specific regression: a set is walkable, and reverse/flatten must not
; type-test it away
(if (equal (reverse {1 2 3}) '(3 2 1))
    (print "PASS: reverse-walks-a-set-like-the-prelude")
    (print "FAIL: reverse-walks-a-set-like-the-prelude"))
(if (equal (flatten {1 2 3}) '(1 2 3))
    (print "PASS: flatten-walks-a-set-like-the-prelude")
    (print "FAIL: flatten-walks-a-set-like-the-prelude"))

(undef lp-reverse lp-last lp-butlast lp-drop lp-take lp-contains lp-index-of)
(undef lp-flatten lp-zip lp-dedup lp-set-to-list lp-try lp-inputs lp-fails)
(undef lp-check lp-report lp-1 lp-2)

(print "=== LIST PRIMITIVE DIFFERENTIAL TESTS DONE ===")
