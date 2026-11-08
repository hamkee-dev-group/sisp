; Hoare Logic — Hoare triple calculus for imperative programs.
; Axioms: skip, assignment, sequencing, if-then-else, while.
; {P} C {Q} meaning: if P holds before C, then Q holds after C.

; ---- program representation ----
; (skip), (assign x e), (seq C1 C2), (if B C1 C2), (while B C)

; ---- assertions are functions: State -> bool ----
; State is an alist of (var value) entries.

(define (state-lookup var state)
  (let ((pair (assoc var state)))
    (cond (pair (cadr pair)) (t 0))))

(define (state-set var val state)
  (cons (list var val) (filter (lambda (p) (not (equal (car p) var))) state)))

; ---- expression evaluation ---
(define (eval-expr e state)
  (cond ((numberp e) e)
        ((symbolp e) (state-lookup e state))
        ((equal (car e) '+) (+ (eval-expr (cadr e) state) (eval-expr (caddr e) state)))
        ((equal (car e) '-) (- (eval-expr (cadr e) state) (eval-expr (caddr e) state)))
        ((equal (car e) '*) (* (eval-expr (cadr e) state) (eval-expr (caddr e) state)))
        (t (error "bad expr" e))))

(define (eval-bool B state)
  (cond ((equal (car B) '<) (< (eval-expr (cadr B) state) (eval-expr (caddr B) state)))
        ((equal (car B) '=) (= (eval-expr (cadr B) state) (eval-expr (caddr B) state)))
        ((equal (car B) '<=) (<= (eval-expr (cadr B) state) (eval-expr (caddr B) state)))
        ((equal (car B) '>) (> (eval-expr (cadr B) state) (eval-expr (caddr B) state)))
        ((equal (car B) '>=) (>= (eval-expr (cadr B) state) (eval-expr (caddr B) state)))
        ((equal (car B) 'not) (not (eval-bool (cadr B) state)))
        ((equal (car B) 'and) (and (eval-bool (cadr B) state) (eval-bool (caddr B) state)))
        ((equal (car B) 'or) (or (eval-bool (cadr B) state) (eval-bool (caddr B) state)))
        (t nil)))

; ---- weakest-precondition calculus ----

; wp(skip, Q) = Q
(define (wp-skip Q) Q)

; wp(x:=e, Q) = Q[x\e]  — Q with x replaced by e
(define (wp-assign x e Q)
  (lambda (state) (Q (state-set x (eval-expr e state) state))))

; wp(C1;C2, Q) = wp(C1, wp(C2, Q))
(define (wp-seq C1 C2 Q)
  (wp C1 (wp C2 Q)))

; wp(if B then C1 else C2, Q) = (B => wp(C1,Q)) ∧ (¬B => wp(C2,Q))
(define (wp-if B C1 C2 Q)
  (lambda (state)
    (cond ((eval-bool B state) ((wp C1 Q) state))
          (t ((wp C2 Q) state)))))

; wp(while B do C, Q) is a fixpoint and is not computed here: wp signals an
; error on a while.  Loops are checked by verify-while below, with a given
; invariant I and the rule: from {I∧B} C {I} infer {I} while B do C {I∧¬B}.
(define (wp C Q)
  (cond ((equal C 'skip) (wp-skip Q))
        ((and (consp C) (equal (car C) 'assign))
         (wp-assign (cadr C) (caddr C) Q))
        ((and (consp C) (equal (car C) 'seq))
         (wp-seq (cadr C) (caddr C) Q))
        ((and (consp C) (equal (car C) 'if))
         (wp-if (cadr C) (caddr C) (cadddr C) Q))
        ((and (consp C) (equal (car C) 'while))
         (error "wp of while requires loop invariant" C))
        (t (error "unknown command" C))))

; ---- verify {P} C {Q}: check P => wp(C, Q) on every enumerated state ----

; For verification, we check: ∀state. P(state) ⇒ wp(C,Q)(state)
; We enumerate states over bounded variable domains.

(define (verify-hoare P C Q vars domain-size)
  (let ((states (all-states vars domain-size)))
    (every (lambda (s)
             (cond ((P s) ((wp C Q) s))
                   (t t)))
           states)))

(define (all-states vars n)
  (cond ((eq vars nil) (list nil))
        (t (let ((rest (all-states (cdr vars) n))
                 (vals (seq 0 (- n 1))))
             (foldl (lambda (acc val)
               (append acc (map (lambda (s) (cons (list (car vars) val) s)) rest)))
               nil vals)))))

; ---- standard Hoare logic axioms for while (with invariant) ----

(define (verify-while P I B C Q vars n)
  ; Check: 1) P ⇒ I (precondition implies invariant)
  ;        2) {I ∧ B} C {I} (invariant preserved by loop body)
  ;        3) I ∧ ¬B ⇒ Q (invariant + exit condition ⇒ postcondition)
  ; This is partial correctness: termination is not checked.
  (let ((states (all-states vars n)))
    (and
      ; P => I
      (every (lambda (s) (cond ((P s) (I s)) (t t))) states)
      ; {I ∧ B} C {I}
      (every (lambda (s)
               (cond ((and (I s) (eval-bool B s))
                      ((wp-body C I B) s))
                     (t t)))
             states)
      ; I ∧ ¬B => Q
      (every (lambda (s)
               (cond ((and (I s) (not (eval-bool B s))) (Q s))
                     (t t)))
             states))))

(define (wp-body C I B)
  (let ((w (wp C I)))
    w))

; ---- demonstration: {x=0} while x<5 do x:=x+1 {x=5} ----
(define (pre-x0 state) (= (state-lookup 'x state) 0))
(define (post-x5 state) (= (state-lookup 'x state) 5))
(define (inv-xle5 state) (<= (state-lookup 'x state) 5))

(define while-prog '(while (< x 5) (assign x (+ x 1))))
; For bounded-state verification (x in [0,6]):
(print "=== {x=0} while x<5 do x:=x+1 {x=5} ===")
(print "Loop invariant: x ??? 5")
(define check1 (verify-while pre-x0 inv-xle5 '(< x 5) '(assign x (+ x 1)) post-x5 '(x) 7))
(print "Verified:" check1)

; ---- demonstration: {true} x:=2; y:=x+1 {y=3} ----
(define (pre-true state) t)
(define (post-y3 state) (= (state-lookup 'y state) 3))

(define assign-prog '(seq (assign x 2) (assign y (+ x 1))))
(print "=== {true} x:=2; y:=x+1 {y=3} ===")
(define check2 (verify-hoare pre-true assign-prog post-y3 '(x y) 5))
(print "Verified:" check2)

; ---- manual wp trace ----
(print "=== WP manual trace ===")
(define wp-y3 (wp assign-prog post-y3))
(print "WP(x:=2; y:=x+1, y=3) applied to empty state:")
(print (wp-y3 nil))

; ---- Hoare axioms demonstration ----
(print "=== Hoare axioms ===")
(print "Axiom (skip):   {P} skip {P}")
(print "Axiom (assign): {Q[x\\e]} x:=e {Q}")
(print "Axiom (seq):    {P} C1 {R}, {R} C2 {Q} => {P} C1;C2 {Q}")
(print "Axiom (if):     {P???B} C1 {Q}, {P?????B} C2 {Q} => {P} if B then C1 else C2 {Q}")
(print "Axiom (while):  {I???B} C {I} => {I} while B do C {I?????B}")
(print "Rule (cons):    P'=>P, {P}C{Q}, Q=>Q' => {P'}C{Q'}")

; ---- checks ----

; State handling.
(check "state-lookup defaults to 0" (= (state-lookup 'x nil) 0))
(check "state-set then lookup" (= (state-lookup 'x (state-set 'x 4 nil)) 4))
(check "state-set overwrites" (= (state-lookup 'x (state-set 'x 9 (state-set 'x 4 nil))) 9))

; Expression and boolean evaluation.
(check "eval-expr numeral" (= (eval-expr 7 nil) 7))
(check "eval-expr variable" (= (eval-expr 'x (state-set 'x 3 nil)) 3))
(check "eval-expr nested" (= (eval-expr '(+ x (* x 2)) (state-set 'x 3 nil)) 9))
(check "eval-bool <" (eval-bool '(< x 5) (state-set 'x 3 nil)))
(check "eval-bool not" (not (eval-bool '(not (< x 5)) (state-set 'x 3 nil))))

; The state space really is enumerated (7 values of one variable).
(check "all-states enumerates the domain" (= (length (all-states '(x) 7)) 7))
(check "all-states of two vars is the product" (= (length (all-states '(x y) 5)) 25))

; Axiom (skip): wp(skip, Q) = Q.
(check "wp skip keeps Q (holds)" ((wp 'skip post-x5) (state-set 'x 5 nil)))
(check "wp skip keeps Q (fails)" (not ((wp 'skip post-x5) (state-set 'x 4 nil))))

; Axiom (assign): wp(x:=e, Q) = Q[x\e].
(check "wp assign substitutes" ((wp '(assign x 5) post-x5) nil))
(check "wp assign substitutes (fails)" (not ((wp '(assign x 4) post-x5) nil)))
(check "wp of x:=x+1 for x=5 is x=4"
       (and ((wp '(assign x (+ x 1)) post-x5) (state-set 'x 4 nil))
            (not ((wp '(assign x (+ x 1)) post-x5) (state-set 'x 3 nil)))))

; Axiom (seq): wp(C1;C2, Q) = wp(C1, wp(C2, Q)).
(check "wp of x:=2; y:=x+1 for y=3 is true everywhere" (wp-y3 nil))
(check "wp seq is order sensitive"
       (not ((wp '(seq (assign y (+ x 1)) (assign x 2)) post-y3) (state-set 'x 9 nil))))

; Axiom (if).
(define (post-x10 state) (= (state-lookup 'x state) 10))
(define if-prog '(if (< x 5) (assign x 10) (assign x 0)))
(check "wp if takes the then branch" ((wp if-prog post-x10) (state-set 'x 3 nil)))
(check "wp if takes the else branch" (not ((wp if-prog post-x10) (state-set 'x 7 nil))))

; Full triples: the positive results computed above.
(check "{x=0} while x<5 do x:=x+1 {x=5} verifies" check1)
(check "{true} x:=2; y:=x+1 {y=3} verifies" check2)

; Negative triples: the verifier must reject wrong ones, otherwise the
; positive results above would be meaningless.
(define (post-y4 state) (= (state-lookup 'y state) 4))
(check "{true} x:=2; y:=x+1 {y=4} is rejected"
       (not (verify-hoare pre-true assign-prog post-y4 '(x y) 5)))

; Invariant too strong: x<=4 is not preserved by the body when x=4.
(define (inv-xle4 state) (<= (state-lookup 'x state) 4))
(check "while rule rejects an invariant the body breaks"
       (not (verify-while pre-x0 inv-xle4 '(< x 5) '(assign x (+ x 1)) post-x5 '(x) 7)))

; Invariant too weak: x<=6 with the loop exited does not imply x=5.
(define (inv-xle6 state) (<= (state-lookup 'x state) 6))
(check "while rule rejects an invariant too weak for the postcondition"
       (not (verify-while pre-x0 inv-xle6 '(< x 5) '(assign x (+ x 1)) post-x5 '(x) 7)))

; Precondition that does not imply the invariant.
(define (pre-x9 state) (= (state-lookup 'x state) 9))
(define (inv-xle3 state) (<= (state-lookup 'x state) 3))
(check "while rule rejects a precondition outside the invariant"
       (not (verify-while pre-x9 inv-xle3 '(< x 5) '(assign x (+ x 1)) post-x5 '(x) 12)))

(print "=== Hoare done ===")
t
