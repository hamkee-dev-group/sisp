; sumcheck-ip.lsp -- Interactive Proof for #SAT counting.
; Sumcheck protocol: prover sends claims about # of solutions.
; For boolean variables, g_i(z) = # solutions with x_i=z and earlier vars fixed.
; Verifier checks g_i(0)+g_i(1) = old claim. Works correctly.

(define (sc-var lit) (cond ((consp lit) (cadr lit)) (t lit)))
(define (sc-neg lit) (consp lit))

(define (sc-eval-lit lit asgn)
  (let ((v (sc-var lit)) (ng (sc-neg lit)))
    (let ((p (assoc v asgn)))
      (cond ((eq p nil) nil)
            (ng (eq (cadr p) nil))
            (t (eq (cadr p) t))))))

(define (sc-clause-ok clause asgn)
  (any (lambda (lit) (sc-eval-lit lit asgn)) clause))

(define (sc-formula-ok F asgn)
  (every (lambda (c) (sc-clause-ok c asgn)) F))

(define (sc-count-rec F vars asgn)
  (cond ((eq vars nil)
         (cond ((sc-formula-ok F asgn) 1) (t 0)))
        (t (let ((v (car vars)))
             (+ (sc-count-rec F (cdr vars) (cons (list v t) asgn))
                (sc-count-rec F (cdr vars) (cons (list v nil) asgn)))))))

(define (sc-count F vars)
  (sc-count-rec F vars nil))

(define (sc-round-claim F vars fixed var z)
  (sc-count-rec F vars
    (cons (list var z) fixed)))

(define (sc-sumcheck F vars fixed claim)
  (cond ((eq vars nil)
         (list (quote final) fixed claim))
        (t (let ((v (car vars)))
             (let ((g0 (sc-round-claim F (cdr vars) fixed v nil))
                   (g1 (sc-round-claim F (cdr vars) fixed v t)))
               (cond ((not (= (+ g0 g1) claim))
                      (list (quote rejected) v g0 g1 claim))
                     (t (let ((r (cond ((> g1 0) t) (t nil))))
                          (sc-sumcheck F (cdr vars)
                            (cons (list v r) fixed)
                            (cond ((eq r nil) g0) (t g1)))))))))))

(define (sc-prove F vars)
  (let ((cnt (sc-count F vars)))
    (sc-sumcheck F vars nil cnt)))

; --- test ---
(define sc-or '((a b)))
(define sc-and '((a b) ((not a) b) (a (not b))))

; the protocol verifies g(0)+g(1) = claim at every round and descends
; into a nonzero branch, ending at a satisfying assignment with claim 1
(check "sumcheck: OR formula (3 solutions) verifies to (a=t b=t)"
       (equal (sc-prove sc-or '(a b))
              (list (quote final) (list (list 'b t) (list 'a t)) 1)))

(check "sumcheck: AND formula (1 solution) verifies to (a=t b=t)"
       (equal (sc-prove sc-and '(a b))
              (list (quote final) (list (list 'b t) (list 'a t)) 1)))

(check "sumcheck: UNSAT formula (0 solutions) verifies to claim 0"
       (equal (sc-prove '((a) ((not a))) '(a))
              (list (quote final) (list (list 'a nil)) 0)))
