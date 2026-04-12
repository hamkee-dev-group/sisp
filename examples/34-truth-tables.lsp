; ============================================================
; 34-truth-tables.lsp
; Build truth tables for logical connectives using SISP's
; built-in and/or/not/xor/=>/<=>.
; Evaluate compound propositions. Verify tautologies.
; ============================================================

(print "=== Truth Tables for Logical Connectives ===")

; --- All truth value pairs ---
(define vals '(t nil))

; --- Print a truth table row ---
(define (show-row p q result)
  (print (list p q result)))

; --- AND truth table ---
(print "--- AND ---")
(print "(p q (and p q))")
(show-row t   t   (and t t))
(show-row t   nil (and t nil))
(show-row nil t   (and nil t))
(show-row nil nil (and nil nil))

; --- OR truth table ---
(print "--- OR ---")
(print "(p q (or p q))")
(show-row t   t   (or t t))
(show-row t   nil (or t nil))
(show-row nil t   (or nil t))
(show-row nil nil (or nil nil))

; --- NOT truth table ---
(print "--- NOT ---")
(print "(p (not p))")
(print (list t (not t)))
(print (list nil (not nil)))

; --- XOR truth table ---
(print "--- XOR ---")
(print "(p q (xor p q))")
(show-row t   t   (xor t t))
(show-row t   nil (xor t nil))
(show-row nil t   (xor nil t))
(show-row nil nil (xor nil nil))

; --- IMPLICATION truth table ---
(print "--- IMPLICATION (=>) ---")
(print "(p q (=> p q))")
(show-row t   t   (=> t t))
(show-row t   nil (=> t nil))
(show-row nil t   (=> nil t))
(show-row nil nil (=> nil nil))

; --- BICONDITIONAL truth table ---
(print "--- BICONDITIONAL (<=>) ---")
(print "(p q (<=> p q))")
(show-row t   t   (<=> t t))
(show-row t   nil (<=> t nil))
(show-row nil t   (<=> nil t))
(show-row nil nil (<=> nil nil))

; ============================================================
; Tautology verification: (p => q) <=> (not p or q)
; Check all 4 rows produce t
; ============================================================
(print "--- Tautology: (p => q) <=> (not p or q) ---")

(define (check-taut p q)
  (let ((lhs (=> p q))
        (rhs (or (not p) q)))
    (let ((equiv (<=> lhs rhs)))
      (progn
        (print (list p q lhs rhs equiv))
        equiv))))

(define t1 (check-taut t t))
(define t2 (check-taut t nil))
(define t3 (check-taut nil t))
(define t4 (check-taut nil nil))

(if (and t1 t2 t3 t4)
    (print "VERIFIED: (p => q) <=> (not p or q) is a TAUTOLOGY")
    (print "FAILED: Not a tautology"))

; ============================================================
; De Morgan's Law: not(p and q) <=> (not p or not q)
; ============================================================
(print "--- De Morgan: not(p and q) <=> (not p or not q) ---")

(define (check-demorgan p q)
  (<=> (not (and p q)) (or (not p) (not q))))

(if (and (check-demorgan t t) (check-demorgan t nil)
         (check-demorgan nil t) (check-demorgan nil nil))
    (print "VERIFIED: De Morgan's Law is a TAUTOLOGY")
    (print "FAILED"))

; ============================================================
; Contrapositive: (p => q) <=> (not q => not p)
; ============================================================
(print "--- Contrapositive: (p => q) <=> (not q => not p) ---")

(define (check-contra p q)
  (<=> (=> p q) (=> (not q) (not p))))

(if (and (check-contra t t) (check-contra t nil)
         (check-contra nil t) (check-contra nil nil))
    (print "VERIFIED: Contrapositive is a TAUTOLOGY")
    (print "FAILED"))

(print "=== Truth Tables Complete ===")
