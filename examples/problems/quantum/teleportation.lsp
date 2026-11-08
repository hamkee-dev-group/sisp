; teleportation.lsp -- Quantum teleportation protocol (algebraic verification).
; Alice's state |psi>=a|0>+b|1>. Shared Bell pair.
; After CNOT+H+measure, Alice gets bits (b1,b2).
; Bob's qubit is in state X^{b1}Z^{b2}|psi>.
; Bob applies Z^{b2}X^{b1} to recover |psi>.
; Verify for all 4 measurement outcomes: Bob recovers original (a,b).

(define (tp-post b1 b2 a b)
  (cond ((= b1 0)
         (cond ((= b2 0) (list a b))
               (t (list a (- 0 b)))))
        (t (cond ((= b2 0) (list b a))
                 (t (list (- 0 b) a))))))

(define (tp-correct b1 b2 a b)
  (cond ((= b1 0)
         (cond ((= b2 0) (list a b))
               (t (list a (- 0 b)))))
        (t (cond ((= b2 0) (list b a))
                 (t (list b (- 0 a)))))))

(define (tp-verify a b b1 b2)
  (let ((post (tp-post b1 b2 a b)))
    (let ((recovered (tp-correct b1 b2 (car post) (cadr post))))
      (and (= (car recovered) a) (= (cadr recovered) b)))))

(list
  (list (quote state) '(0.6 0.8))
  (list (quote bits-00-ok) (tp-verify 0.6 0.8 0 0))
  (list (quote bits-10-ok) (tp-verify 0.6 0.8 1 0))
  (list (quote bits-01-ok) (tp-verify 0.6 0.8 0 1))
  (list (quote bits-11-ok) (tp-verify 0.6 0.8 1 1))
  (list (quote all-ok) (and (tp-verify 0.6 0.8 0 0)
                            (tp-verify 0.6 0.8 1 0)
                            (tp-verify 0.6 0.8 0 1)
                            (tp-verify 0.6 0.8 1 1))))
