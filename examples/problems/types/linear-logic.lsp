; Linear logic: resource-aware propositional logic.
; A -o B = consume A, produce B. No weakening/contraction.
; (derive '(A -o B -o (A * B)) '()) -> proof

; structural equality (the hand-rolled version used and/or on
; non-boolean values and crashed on nested formulas)
(define (ll-eq a b) (equal a b))

(define (ll-rem-one target hyps)
  (ll-rem-one-x target hyps nil))

(define (ll-rem-one-x target hyps before)
  (cond ((eq hyps nil) nil)
        ((ll-eq (car hyps) target) (append (reverse before) (cdr hyps)))
        (t (ll-rem-one-x target (cdr hyps) (cons (car hyps) before)))))

(define (ll-in target hyps)
  (cond ((eq hyps nil) nil) ((ll-eq (car hyps) target) t) (t (ll-in target (cdr hyps)))))

; -o elimination (modus ponens): hyps contain X and (X -> goal);
; derive X from the remaining hyps, everything consumed
; scan walks the hypotheses, but antecedent lookup and removal use the
; FULL hypothesis list (dropping elements while scanning must not lose
; the antecedent)
(define (ll-elim goal hyps depth)
  (ll-elim-scan goal hyps hyps depth))

(define (ll-elim-scan goal hyps scan depth)
  (cond ((null scan) nil)
        ((> depth 0)
         (let ((h (car scan)))
           (cond ((and (listp h) (eq (car h) '->)
                       (ll-eq (caddr h) goal))
                  (let* ((x (cadr h))
                         (rest (ll-rem-one h hyps))
                         (sub (ll-derive x rest (- depth 1))))
                    (cond ((and (not (eq sub nil)) (null (cadr sub)))
                           (list (list 'app h (car sub)) nil))
                          (t (ll-elim-scan goal hyps (cdr scan) depth)))))
                 (t (ll-elim-scan goal hyps (cdr scan) depth)))))
        (t nil)))

(define (ll-derive goal hyps depth)
  (cond ((<= depth 0) nil)
        ((atom goal)
         (cond ((ll-in goal hyps)
                (let ((rem (ll-rem-one goal hyps)))
                  ; the axiom consumes exactly the goal hypothesis
                  (cond ((null rem) (list '(use) rem))
                        (t (ll-elim goal hyps depth)))))
               (t (ll-elim goal hyps depth))))
        ((and (listp goal) (eq (car goal) '->))
         (let ((asm (cadr goal)) (con (caddr goal)))
           (let ((sub (ll-derive con (cons asm hyps) (- depth 1))))
             (cond (sub (list (list 'lam (car sub)) (cadr sub))) (t nil)))))
        ((and (listp goal) (eq (car goal) '*))
         (ll-derive-tensor goal hyps depth))
        (t nil)))

(define (ll-derive-tensor goal hyps depth)
  (let ((a (cadr goal)) (b (caddr goal)))
    (ll-split-search a b hyps depth)))

(define (ll-split-search a b hyps depth)
  (ll-split-iter a b hyps (length hyps) depth))

(define (ll-split-iter a b hyps n depth)
  (cond ((< n 0) nil)
        (t (let ((parts (ll-split hyps n)))
             (let ((pa (ll-derive a (car parts) (- depth 2)))
                   (pb (ll-derive b (cadr parts) (- depth 2))))
               (cond ((and (not (eq pa nil)) (not (eq pb nil)))
                      (list (list 'tensor (car pa) (car pb))
                            (append (cadr pa) (cadr pb))))
                     (t (ll-split-iter a b hyps (- n 1) depth))))))))

(define (ll-split xs n)
  (ll-split-acc xs n nil))

(define (ll-split-acc xs n acc)
  (cond ((= n 0) (list (reverse acc) xs))
        ((eq xs nil) (list (reverse acc) nil))
        (t (ll-split-acc (cdr xs) (- n 1) (cons (car xs) acc)))))

(define (derive goal hyps)
  (ll-derive goal hyps 20))

; A |- A (identity)
(check "axiom: A |- A"
       (not (eq (derive 'A '(A)) nil)))

; Cannot derive A from empty (no magic)
(check "empty context derives nothing"
       (eq (derive 'A '()) nil))

; Linear implication: A -> A from A
(check "identity implication: |- A -> A"
       (not (eq (derive '(-> A A) '()) nil)))

; Using resource exactly once: A, (A -> B) |- B
(check "modus ponens: A, A-oB |- B"
       (not (eq (derive 'B '(A (-> A B))) nil)))

(check "chained modus ponens: A, A-oB, B-oC |- C"
       (not (eq (derive 'C '(A (-> A B) (-> B C))) nil)))

(check "wrong implication rejected: A, C-oB not|- B"
       (eq (derive 'B '(A (-> C B))) nil))

(check "linearity: A, B not|- A (leftover resource)"
       (eq (derive 'A '(A B)) nil))

(check "tensor: A, B |- A * B"
       (not (eq (derive '(* A B) '(A B)) nil)))
