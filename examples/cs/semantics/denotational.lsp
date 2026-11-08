; Denotational semantics — compositional meaning via domain theory.
; Domain: State⊥ = {⊥} ∪ State.  [[C]]: State⊥ → State⊥.
; While loops denote a least fixpoint; it is computed below both by direct
; iteration and as the limit of its Kleene chain.

(define bottom 'bottom)

(define (is-bottom s) (equal s bottom))

(define (st-lookup var state)
  (cond ((is-bottom state) bottom)
        (t (let ((pair (assoc var state)))
             (cond (pair (cadr pair)) (t 0))))))

(define (st-update var val state)
  (cond ((is-bottom state) bottom)
        (t (cons (list var val)
                 (filter (lambda (p) (not (equal (car p) var))) state)))))

; ---- arithmetic expressions ----

(define (dn-expr e s)
  (cond ((is-bottom s) bottom)
        ((numberp e) e)
        ((symbolp e) (st-lookup e s))
        ((equal (car e) '+) (+ (dn-expr (cadr e) s) (dn-expr (caddr e) s)))
        ((equal (car e) '-) (- (dn-expr (cadr e) s) (dn-expr (caddr e) s)))
        ((equal (car e) '*) (* (dn-expr (cadr e) s) (dn-expr (caddr e) s)))
        (t bottom)))

(define (dn-bool b s)
  (cond ((is-bottom s) nil)
        ((equal (car b) '<) (< (dn-expr (cadr b) s) (dn-expr (caddr b) s)))
        ((equal (car b) '=) (= (dn-expr (cadr b) s) (dn-expr (caddr b) s)))
        ((equal (car b) '<=) (<= (dn-expr (cadr b) s) (dn-expr (caddr b) s)))
        (t nil)))

; ---- semantic function [[-]] ----

(define (dn-sem C)
  (cond ((eq C 'skip) (lambda (s) s))
        ((and (consp C) (equal (car C) 'assign))
         (let ((x (cadr C)) (e (caddr C)))
           (lambda (s) (st-update x (dn-expr e s) s))))
        ((and (consp C) (equal (car C) 'seq))
         (let ((f1 (dn-sem (cadr C))) (f2 (dn-sem (caddr C))))
           (lambda (s) (f2 (f1 s)))))
        ((and (consp C) (equal (car C) 'if))
         (let ((b (cadr C)) (c1 (caddr C)) (c2 (cadddr C)))
           (let ((f1 (dn-sem c1)) (f2 (dn-sem c2)))
             (lambda (s)
               (cond ((is-bottom s) bottom)
                     ((dn-bool b s) (f1 s))
                     (t (f2 s)))))))
        ((and (consp C) (equal (car C) 'while))
         (let ((b (cadr C)) (body (caddr C)))
           (let ((fn-body (dn-sem body)))
             (dn-while b fn-body))))
        (t (lambda (s) bottom))))

; ---- fixpoint for while ----
; [[while b do C]] = lfp(λF. λs. if B(s) then F([[C]]s) else s)
; dn-while evaluates it on a given state by running the body until B is
; false.  On a divergent loop it does not terminate, where the least
; fixpoint gives bottom.

(define (dn-while b fn-body)
  (lambda (s)
    (labels (((loop s0)
      (cond ((is-bottom s0) bottom)
            ((dn-bool b s0) (loop (fn-body s0)))
            (t s0))))
      (loop s))))

; The same denotation built explicitly as a Kleene chain, which is what
; makes the fixpoint characterisation observable:
;   F_0     = \s. bottom
;   F_(n+1) = \s. if B(s) then F_n([[C]](s)) else s
; F_n is the meaning of loops that run fewer than n iterations, the chain
; is increasing, and its limit is the least fixpoint.  Unlike the loop
; above, F_n always terminates, so a divergent loop shows up as bottom
; instead of hanging.
(define (dn-while-approx b fn-body n)
  (cond ((= n 0) (lambda (s) bottom))
        (t (let ((prev (dn-while-approx b fn-body (- n 1))))
             (lambda (s)
               (cond ((is-bottom s) bottom)
                     ((dn-bool b s) (prev (fn-body s)))
                     (t s)))))))

; ========== demonstrations ==========

; --- x:=2 ---
(print "=== x:=2 ===")
(define sem-assign (dn-sem '(assign x 2)))
(define r-assign (sem-assign nil))
(print "[[x:=2]](nil) =" r-assign)
(print "Expected: ((x 2))" (equal r-assign '((x 2))))

; --- x:=1; y:=x+1 ---
(print "=== x:=1; y:=x+1 ===")
(define sem-seq (dn-sem '(seq (assign x 1) (assign y (+ x 1)))))
(define r-seq (sem-seq nil))
(print "[[x:=1; y:=x+1]](nil) =" r-seq)

; --- while x<3 do x:=x+1 ---
(print "=== while x<3 do x:=x+1 (x=0) ===")
(define sem-while (dn-sem '(while (< x 3) (assign x (+ x 1)))))
(define r-while (sem-while (list (list 'x 0))))
(print "[[while x<3 do x:=x+1]](x=0) =" r-while)
(print "x =" (st-lookup 'x r-while))

; --- while x<5 do x:=x+1 ---
(print "=== while x<5 do x:=x+1 (x=0) ===")
(define sem-while5 (dn-sem '(while (< x 5) (assign x (+ x 1)))))
(define r-while5 (sem-while5 (list (list 'x 0))))
(print "[[while x<5 do x:=x+1]](x=0) =" r-while5)

; --- if-then-else ---
(print "=== if x<5 then x:=10 else x:=0 ===")
(define sem-if (dn-sem '(if (< x 5) (assign x 10) (assign x 0))))
(define r-if-true (sem-if (list (list 'x 3))))
(print "[[if ...]](x=3) =" r-if-true)
(define r-if-false (sem-if (list (list 'x 7))))
(print "[[if ...]](x=7) =" r-if-false)

; --- compositionality proof ---
(print "=== Compositionality ===")
(define fa (dn-sem '(assign x 2)))
(define fb (dn-sem '(assign y (+ x 1))))
(define fseq (dn-sem '(seq (assign x 2) (assign y (+ x 1)))))
(print "[[x:=2; y:=x+1]](nil) =" (fseq nil))
(print "([[y:=x+1]] o [[x:=2]])(nil) =" (fb (fa nil)))
(print "Equal:" (equal (fseq nil) (fb (fa nil))))

; ---- checks ----

; Lifted state operations are strict in bottom.
(check "lookup of bottom is bottom" (is-bottom (st-lookup 'x bottom)))
(check "update of bottom is bottom" (is-bottom (st-update 'x 1 bottom)))
(check "expression on bottom is bottom" (is-bottom (dn-expr '(+ x 1) bottom)))
(check "unbound variables read as 0" (= (st-lookup 'z '((x 1))) 0))

; [[skip]] is the identity.
(check "[[skip]] is the identity" (equal ((dn-sem 'skip) '((x 1))) '((x 1))))

; [[x:=e]].
(check "[[x:=2]](nil) = ((x 2))" (equal r-assign '((x 2))))
(check "[[x:=1; y:=x+1]](nil) has y = 2" (= (st-lookup 'y r-seq) 2))
(check "[[x:=1; y:=x+1]](nil) has x = 1" (= (st-lookup 'x r-seq) 1))

; [[if]].
(check "[[if x<5 ...]](x=3) takes the then branch" (= (st-lookup 'x r-if-true) 10))
(check "[[if x<5 ...]](x=7) takes the else branch" (= (st-lookup 'x r-if-false) 0))

; [[while]].
(check "[[while x<3 do x:=x+1]](x=0) has x = 3" (= (st-lookup 'x r-while) 3))
(check "[[while x<5 do x:=x+1]](x=0) has x = 5" (= (st-lookup 'x r-while5) 5))
(check "[[while x<3 ...]](x=9) leaves the state alone"
       (equal (sem-while '((x 9))) '((x 9))))

; Compositionality: [[C1;C2]] = [[C2]] o [[C1]].
(check "[[C1;C2]] = [[C2]] o [[C1]]" (equal (fseq nil) (fb (fa nil))))
(check "composition is not symmetric here"
       (not (equal (fseq nil) (fa (fb nil)))))

; The Kleene chain for while x<3 do x:=x+1, started at x=0.
(define kleene-body (dn-sem '(assign x (+ x 1))))
(define (kleene n) ((dn-while-approx '(< x 3) kleene-body n) '((x 0))))
(check "F_0 is bottom everywhere" (is-bottom (kleene 0)))
(check "F_3 is still bottom (3 iterations are needed)" (is-bottom (kleene 3)))
(check "F_4 is the first approximant that terminates" (not (is-bottom (kleene 4))))
(check "F_4 agrees with the direct loop" (equal (kleene 4) r-while))
(check "the chain has stabilised by F_4" (equal (kleene 4) (kleene 12)))

; A divergent loop: every approximant is bottom, so the least fixpoint
; sends this state to bottom.  (The direct loop would not terminate.)
(define (kleene-diverge n) ((dn-while-approx '(< x 3) (dn-sem 'skip) n) '((x 0))))
(check "[[while x<3 do skip]](x=0) is bottom at every approximant"
       (and (is-bottom (kleene-diverge 1)) (is-bottom (kleene-diverge 50))))
(check "the divergent loop still terminates on a state failing the guard"
       (equal ((dn-while-approx '(< x 3) (dn-sem 'skip) 50) '((x 5))) '((x 5))))

(print "=== Denotational done ===")
t
