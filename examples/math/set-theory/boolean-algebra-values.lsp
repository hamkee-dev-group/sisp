; ============================================================
; 56 — Boolean Algebra on {t, nil}
; ({t, nil}, and, or, not, nil, t) forms the two-element Boolean algebra 2
; Every law is verified EXHAUSTIVELY over all truth assignments
; strict and/or/not = a real algebra, proved by finite exhaustion
; ============================================================

(print "=== Boolean Algebra on {t, nil} ===")

; The carrier is finite, so "for all assignments" is a finite conjunction.
; Each helper feeds every combination of truth values to a law and ands
; the results: it returns t iff the law holds for EVERY assignment.
(define (all1 law)
  (and (funcall law t) (funcall law nil)))

(define (all2 law)
  (and (funcall law t   t)   (funcall law t   nil)
       (funcall law nil t)   (funcall law nil nil)))

(define (all3 law)
  (and (funcall law t   t   t)   (funcall law t   t   nil)
       (funcall law t   nil t)   (funcall law t   nil nil)
       (funcall law nil t   t)   (funcall law nil t   nil)
       (funcall law nil nil t)   (funcall law nil nil nil)))

; --- Base operations (the defining truth tables) ---
(print "--- not ---")
(print (not t)) (print (not nil))
(print "--- and : t.t t.nil nil.t nil.nil ---")
(print (and t t)) (print (and t nil)) (print (and nil t)) (print (and nil nil))
(print "--- or  : t.t t.nil nil.t nil.nil ---")
(print (or t t)) (print (or t nil)) (print (or nil t)) (print (or nil nil))

; --- Commutative ---
(print "--- Commutative ---")
(print "a and b = b and a?")
(print (all2 (lambda (a b) (equal (and a b) (and b a)))))
(print "a or b = b or a?")
(print (all2 (lambda (a b) (equal (or a b) (or b a)))))

; --- Associative ---
(print "--- Associative ---")
(print "(a and b) and c = a and (b and c)?")
(print (all3 (lambda (a b c) (equal (and (and a b) c) (and a (and b c))))))
(print "(a or b) or c = a or (b or c)?")
(print (all3 (lambda (a b c) (equal (or (or a b) c) (or a (or b c))))))

; --- Identity ---
(print "--- Identity ---")
(print "a and t = a?")
(print (all1 (lambda (a) (equal (and a t) a))))
(print "a or nil = a?")
(print (all1 (lambda (a) (equal (or a nil) a))))

; --- Domination (annihilator) ---
(print "--- Domination ---")
(print "a and nil = nil?")
(print (all1 (lambda (a) (equal (and a nil) nil))))
(print "a or t = t?")
(print (all1 (lambda (a) (equal (or a t) t))))

; --- Idempotent ---
(print "--- Idempotent ---")
(print "a and a = a?")
(print (all1 (lambda (a) (equal (and a a) a))))
(print "a or a = a?")
(print (all1 (lambda (a) (equal (or a a) a))))

; --- Complement ---
(print "--- Complement ---")
(print "a and (not a) = nil?")
(print (all1 (lambda (a) (equal (and a (not a)) nil))))
(print "a or (not a) = t?")
(print (all1 (lambda (a) (equal (or a (not a)) t))))

; --- Double negation ---
(print "--- Double negation ---")
(print "not (not a) = a?")
(print (all1 (lambda (a) (equal (not (not a)) a))))

; --- De Morgan ---
(print "--- De Morgan ---")
(print "not (a and b) = (not a) or (not b)?")
(print (all2 (lambda (a b) (equal (not (and a b)) (or (not a) (not b))))))
(print "not (a or b) = (not a) and (not b)?")
(print (all2 (lambda (a b) (equal (not (or a b)) (and (not a) (not b))))))

; --- Distributive ---
(print "--- Distributive ---")
(print "a and (b or c) = (a and b) or (a and c)?")
(print (all3 (lambda (a b c) (equal (and a (or b c)) (or (and a b) (and a c))))))
(print "a or (b and c) = (a or b) and (a or c)?")
(print (all3 (lambda (a b c) (equal (or a (and b c)) (and (or a b) (or a c))))))

; --- Absorption ---
(print "--- Absorption ---")
(print "a or (a and b) = a?")
(print (all2 (lambda (a b) (equal (or a (and a b)) a))))
(print "a and (a or b) = a?")
(print (all2 (lambda (a b) (equal (and a (or a b)) a))))

(print "All Boolean algebra laws verified over every assignment!")
(print "=== Boolean Algebra Done ===")
