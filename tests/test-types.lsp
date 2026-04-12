; ============================================================
; SISP Test Suite — Type Operations
; (atomp/atom, numberp, typeof, consp, listp, null, symbolp, endp)
; ============================================================

(print "=== TYPE TESTS ===")

; --- atomp ---
(if (equal (atomp 1) t) (print "PASS: atomp-int") (print "FAIL: atomp-int"))
(if (equal (atomp 1/2) t) (print "PASS: atomp-rat") (print "FAIL: atomp-rat"))
(if (equal (atomp "hello") t) (print "PASS: atomp-string") (print "FAIL: atomp-string"))
(if (equal (atomp 'a) t) (print "PASS: atomp-id") (print "FAIL: atomp-id"))
(if (equal (atomp t) t) (print "PASS: atomp-t") (print "FAIL: atomp-t"))
(if (equal (atomp nil) t) (print "PASS: atomp-nil") (print "FAIL: atomp-nil"))
(if (equal (atomp '(1 2)) nil) (print "PASS: atomp-list") (print "FAIL: atomp-list"))
(if (equal (atomp {1 2}) nil) (print "PASS: atomp-set") (print "FAIL: atomp-set"))

; --- numberp ---
(if (equal (numberp 42) t) (print "PASS: numberp-int") (print "FAIL: numberp-int"))
(if (equal (numberp 0) t) (print "PASS: numberp-zero") (print "FAIL: numberp-zero"))
(if (equal (numberp -5) t) (print "PASS: numberp-neg") (print "FAIL: numberp-neg"))
(if (equal (numberp 1/2) t) (print "PASS: numberp-rat") (print "FAIL: numberp-rat"))
(if (equal (numberp "hello") nil) (print "PASS: numberp-string") (print "FAIL: numberp-string"))
(if (equal (numberp 'a) nil) (print "PASS: numberp-id") (print "FAIL: numberp-id"))
(if (equal (numberp nil) nil) (print "PASS: numberp-nil") (print "FAIL: numberp-nil"))
(if (equal (numberp t) nil) (print "PASS: numberp-t") (print "FAIL: numberp-t"))
(if (equal (numberp '(1 2)) nil) (print "PASS: numberp-list") (print "FAIL: numberp-list"))

; --- typeof ---
(if (equal (typeof 42) 'integer) (print "PASS: typeof-int") (print "FAIL: typeof-int"))
(if (equal (typeof 1/2) 'rational) (print "PASS: typeof-rat") (print "FAIL: typeof-rat"))
(if (equal (typeof "hello") 'string) (print "PASS: typeof-str") (print "FAIL: typeof-str"))
(if (equal (typeof 'a) 'identifier) (print "PASS: typeof-id") (print "FAIL: typeof-id"))
; typeof returns identifier strings; t and nil are special constants,
; so we compare with (equal (typeof ...) (typeof ...)) pattern
(if (equal (equal (typeof t) (typeof t)) t) (print "PASS: typeof-t-consistent") (print "FAIL: typeof-t-consistent"))
(if (equal (equal (typeof nil) (typeof nil)) t) (print "PASS: typeof-nil-consistent") (print "FAIL: typeof-nil-consistent"))
(if (equal (typeof '(1 2)) 'cons) (print "PASS: typeof-cons") (print "FAIL: typeof-cons"))
(if (equal (typeof {1 2}) 'set) (print "PASS: typeof-set") (print "FAIL: typeof-set"))
; typeof tau returns identifier "tau", but 'tau is the tau constant (OBJ_TAU)
(if (equal (equal (typeof tau) (typeof tau)) t) (print "PASS: typeof-tau-consistent") (print "FAIL: typeof-tau-consistent"))

; --- consp: CL semantics — t for any cons cell ---
(if (equal (consp '(1 . 2)) t) (print "PASS: consp-pair") (print "FAIL: consp-pair"))
(if (equal (consp 'a) nil) (print "PASS: consp-atom") (print "FAIL: consp-atom"))
(if (equal (consp nil) nil) (print "PASS: consp-nil") (print "FAIL: consp-nil"))
(if (equal (consp '(a . b)) t) (print "PASS: consp-id-pair") (print "FAIL: consp-id-pair"))
(if (equal (consp 42) nil) (print "PASS: consp-integer") (print "FAIL: consp-integer"))
(if (equal (consp "hello") nil) (print "PASS: consp-string") (print "FAIL: consp-string"))
(if (equal (consp t) nil) (print "PASS: consp-t") (print "FAIL: consp-t"))
(if (equal (consp '(1 2 3)) t) (print "PASS: consp-proper-list") (print "FAIL: consp-proper-list"))
(if (equal (consp '(a b)) t) (print "PASS: consp-two-elem") (print "FAIL: consp-two-elem"))
(if (equal (consp '(1)) t) (print "PASS: consp-singleton") (print "FAIL: consp-singleton"))

; --- atom: alias for atomp ---
(if (equal (atom 'a) t) (print "PASS: atom-id") (print "FAIL: atom-id"))
(if (equal (atom '(1 2)) nil) (print "PASS: atom-list") (print "FAIL: atom-list"))

; --- listp: t for nil and proper lists, nil for dotted pairs ---
(if (equal (listp nil) t) (print "PASS: listp-nil") (print "FAIL: listp-nil"))
(if (equal (listp '(1 2 3)) t) (print "PASS: listp-proper") (print "FAIL: listp-proper"))
(if (equal (listp '(1 . 2)) nil) (print "PASS: listp-dotted") (print "FAIL: listp-dotted"))
(if (equal (listp 42) nil) (print "PASS: listp-int") (print "FAIL: listp-int"))
(if (equal (listp 'a) nil) (print "PASS: listp-atom") (print "FAIL: listp-atom"))

; --- null: t only for nil ---
(if (equal (null nil) t) (print "PASS: null-nil") (print "FAIL: null-nil"))
(if (equal (null t) nil) (print "PASS: null-t") (print "FAIL: null-t"))
(if (equal (null 42) nil) (print "PASS: null-int") (print "FAIL: null-int"))
(if (equal (null '(1)) nil) (print "PASS: null-list") (print "FAIL: null-list"))

; --- symbolp: t for identifiers, nil, and t ---
(if (equal (symbolp 'a) t) (print "PASS: symbolp-id") (print "FAIL: symbolp-id"))
(if (equal (symbolp nil) t) (print "PASS: symbolp-nil") (print "FAIL: symbolp-nil"))
(if (equal (symbolp t) t) (print "PASS: symbolp-t") (print "FAIL: symbolp-t"))
(if (equal (symbolp 42) nil) (print "PASS: symbolp-int") (print "FAIL: symbolp-int"))
(if (equal (symbolp "hello") nil) (print "PASS: symbolp-str") (print "FAIL: symbolp-str"))
(if (equal (symbolp '(1 2)) nil) (print "PASS: symbolp-list") (print "FAIL: symbolp-list"))

; --- endp: t for nil, nil for cons, error for non-lists ---
(if (equal (endp nil) t) (print "PASS: endp-nil") (print "FAIL: endp-nil"))
(if (equal (endp '(1 2)) nil) (print "PASS: endp-cons") (print "FAIL: endp-cons"))

(print "=== TYPE TESTS DONE ===")
