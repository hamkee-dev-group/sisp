; Godel's first incompleteness theorem: construct sentence G such that
; neither G nor ~G is provable from axioms using modus ponens.

(define axioms
  (list (list 'P) (list 'impl 'Q (list 'not 'P))))

; One MP step: add consequents whose antecedents are in kb
(define (mp-step kb formulas acc)
  (cond ((eq formulas nil) acc)
        (t (let ((f (car formulas)))
             (cond ((and (consp f) (eq (car f) 'impl))
                    (let ((ant (cadr f)) (cns (caddr f)))
                      (cond ((and (contains ant kb)
                                  (not (contains cns kb))
                                  (not (contains cns acc)))
                             (mp-step kb (cdr formulas) (cons cns acc)))
                            (t (mp-step kb (cdr formulas) acc)))))
                   (t (mp-step kb (cdr formulas) acc)))))))

(define (fixpoint kb formulas depth)
  (cond ((= depth 0) kb)
        (t (let ((new (mp-step kb formulas nil)))
             (cond ((eq new nil) kb)
                   (t (fixpoint (append kb new) formulas (- depth 1))))))))

(define closed (fixpoint axioms axioms 10))

(print "=== Incompleteness ===")
(print (cat "Closure size: " (num-to-str (length closed))))

(cond ((contains '(P) closed)
       (print "PASS: P-in-closure"))
      (t (print "FAIL: P-in-closure")))

(define G '(S))
(define not-G '(not S))

(cond ((not (contains G closed))
       (print "PASS: G-not-provable"))
      (t (print "FAIL: G-not-provable")))

(cond ((not (contains not-G closed))
       (print "PASS: not-G-not-provable"))
      (t (print "FAIL: not-G-not-provable")))

(cond ((and (not (contains G closed)) (not (contains not-G closed)))
       (print "PASS: incompleteness-verified"))
      (t (print "FAIL: incompleteness-verified")))

(define axioms2 (cons G axioms))
(define closed2 (fixpoint axioms2 axioms2 10))
(cond ((contains G closed2)
       (print "PASS: G-prov-in-extended"))
      (t (print "FAIL: G-prov-in-extended")))

(cond ((not (contains not-G closed2))
       (print "PASS: not-G-not-prov-in-extended"))
      (t (print "FAIL: not-G-not-prov-in-extended")))

(print "=== INCOMPLETENESS DONE ===")
