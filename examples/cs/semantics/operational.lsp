; Operational semantics — small-step and big-step for simple WHILE language.
; Small-step: (C, s) -> (C', s')  — one reduction step.
; Big-step:   (C, s) ==> s'       — full evaluation to final state.

(define (os-lookup var state)
  (let ((pair (assoc var state)))
    (cond (pair (cadr pair)) (t 0))))

(define (os-update var val state)
  (cons (list var val) (filter (lambda (p) (not (equal (car p) var))) state)))

(define (os-eval e state)
  (cond ((numberp e) e)
        ((symbolp e) (os-lookup e state))
        ((equal (car e) '+) (+ (os-eval (cadr e) state) (os-eval (caddr e) state)))
        ((equal (car e) '-) (- (os-eval (cadr e) state) (os-eval (caddr e) state)))
        ((equal (car e) '*) (* (os-eval (cadr e) state) (os-eval (caddr e) state)))
        (t 0)))

(define (os-bool b state)
  (cond ((equal (car b) '<) (< (os-eval (cadr b) state) (os-eval (caddr b) state)))
        ((equal (car b) '=) (= (os-eval (cadr b) state) (os-eval (caddr b) state)))
        ((equal (car b) '<=) (<= (os-eval (cadr b) state) (os-eval (caddr b) state)))
        ((equal (car b) '>) (> (os-eval (cadr b) state) (os-eval (caddr b) state)))
        (t nil)))

; Small-step reduction: (C, s) -> (C', s')

(define (small-step C s)
  (cond ((eq C 'skip) nil)
        ((and (consp C) (equal (car C) 'assign))
         (list 'skip (os-update (cadr C) (os-eval (caddr C) s) s)))
        ((and (consp C) (equal (car C) 'seq))
         (let ((c1 (cadr C)) (c2 (caddr C)))
           (cond ((eq c1 'skip) (list c2 s))
                 (t (let ((step1 (small-step c1 s)))
                      (cond (step1 (list (list 'seq (car step1) c2) (cadr step1)))
                            (t nil)))))))
        ((and (consp C) (equal (car C) 'if))
         (let ((b (cadr C)) (c1 (caddr C)) (c2 (cadddr C)))
           (cond ((os-bool b s) (list c1 s))
                 (t (list c2 s)))))
        ((and (consp C) (equal (car C) 'while))
         (let ((b (cadr C)) (body (caddr C)))
           (cond ((os-bool b s) (list (list 'seq body C) s))
                 (t (list 'skip s)))))
        (t nil)))

; Multi-step reduction (bounded)

; Returns the FINAL STATE.  Running out of fuel is an error rather than a
; silent return of the current state: otherwise an under-budgeted run would
; look like a successful one and hide a divergence.
(define (small-steps C s fuel)
  (cond ((= fuel 0) (error "small-steps: step budget exhausted" C))
        ((eq C 'skip) s)
        (t (let ((next (small-step C s)))
             (cond ((eq next nil) s)
                   (t (small-steps (car next) (cadr next) (- fuel 1))))))))

; Big-step evaluation: (C, s) ==> s'

(define (big-step C s)
  (cond ((eq C 'skip) s)
        ((and (consp C) (equal (car C) 'assign))
         (os-update (cadr C) (os-eval (caddr C) s) s))
        ((and (consp C) (equal (car C) 'seq))
         (let ((mid (big-step (cadr C) s)))
           (big-step (caddr C) mid)))
        ((and (consp C) (equal (car C) 'if))
         (cond ((os-bool (cadr C) s) (big-step (caddr C) s))
               (t (big-step (cadddr C) s))))
        ((and (consp C) (equal (car C) 'while))
         (cond ((os-bool (cadr C) s)
                (let ((mid (big-step (caddr C) s)))
                  (big-step C mid)))
               (t s)))
        (t s)))

; Trace collector for small-step

(define (trace-step cmd st max)
  (labels (((go c s n acc)
    (cond ((= n 0) (reverse (cons (list c s) acc)))
          ((eq c 'skip) (reverse (cons (list c s) acc)))
          (t (let ((next (small-step c s)))
               (cond ((eq next nil) (reverse (cons (list c s) acc)))
                     (t (go (car next) (cadr next) (- n 1) (cons (list c s) acc)))))))))
    (go cmd st max nil)))

; Compare two states by the value of every variable of interest, so the
; result does not depend on the incidental order of the alist.
(define (state-equal vars a b)
  (every (lambda (v) (= (os-lookup v a) (os-lookup v b))) vars))

; Equivalence checker: prints the two runs and RETURNS whether they agree.
(define (check-equiv C s fuel vars)
  (let ((ss (small-steps C s fuel))
        (bs (big-step C s)))
    (print "Cmd:" C)
    (print "  small:" ss " big:" bs)
    (print "  equal:" (state-equal vars ss bs))
    (state-equal vars ss bs)))

; ========== Demonstrations ==========

(define prg1 '(seq (assign x 1) (while (< x 3) (assign x (+ x 1)))))
(define init-st nil)

(print "=== Small-step: x:=1; while x<3 do x:=x+1 ===")
(define tr (trace-step prg1 init-st 10))
(print "Trace:")
(foreach (lambda (step) (print "  ->" step)) tr)
(define ss-final (small-steps prg1 init-st 20))
(print "Final (small-step):" ss-final)
(print "x =" (os-lookup 'x ss-final))

(print "=== Big-step: x:=1; while x<3 do x:=x+1 ===")
(define bs-final (big-step prg1 init-st))
(print "Final (big-step):" bs-final)
(print "x =" (os-lookup 'x bs-final))

; Small-step and big-step give the same result on this program (a check on
; one run, not a proof of equivalence)
(print "=== Equivalence proof ===")
(print "Small-step x =" (os-lookup 'x ss-final))
(print "Big-step   x =" (os-lookup 'x bs-final))
(print "Same result:" (equal (os-lookup 'x ss-final) (os-lookup 'x bs-final)))

(print "=== More tests ===")

; ---- checks ----

; Expression and boolean evaluation.
(check "os-lookup defaults to 0" (= (os-lookup 'x nil) 0))
(check "os-update then lookup" (= (os-lookup 'x (os-update 'x 4 nil)) 4))
(check "os-eval nested" (= (os-eval '(+ x (* x 2)) (os-update 'x 3 nil)) 9))
(check "os-bool <" (os-bool '(< x 3) (os-update 'x 1 nil)))
(check "os-bool > is false when equal" (not (os-bool '(> x 3) (os-update 'x 3 nil))))

; One small step at a time.
(check "skip has no successor" (eq (small-step 'skip nil) nil))
(check "assign steps to skip with an updated state"
       (equal (small-step '(assign x 2) nil) '(skip ((x 2)))))
(check "seq with a skip head drops the head"
       (equal (small-step '(seq skip (assign x 1)) nil) '((assign x 1) nil)))
(check "seq steps its head"
       (equal (small-step '(seq (assign x 1) (assign y 2)) nil)
              '((seq skip (assign y 2)) ((x 1)))))
(check "if takes the then branch"
       (equal (small-step '(if (< x 5) (assign x 10) (assign x 0)) '((x 3)))
              '((assign x 10) ((x 3)))))
(check "if takes the else branch"
       (equal (small-step '(if (< x 5) (assign x 10) (assign x 0)) '((x 7)))
              '((assign x 0) ((x 7)))))
(check "while unrolls to seq body while when the guard holds"
       (equal (small-step '(while (< x 3) (assign x (+ x 1))) '((x 0)))
              '((seq (assign x (+ x 1)) (while (< x 3) (assign x (+ x 1)))) ((x 0)))))
(check "while steps to skip when the guard fails"
       (equal (small-step '(while (< x 3) (assign x (+ x 1))) '((x 3)))
              '(skip ((x 3)))))

; The trace of x:=1; while x<3 do x:=x+1 reaches skip in 9 steps.
(check "trace reaches skip" (eq (car (last tr)) 'skip))
(check "trace has 10 configurations (9 steps)" (= (length tr) 10))
(check "trace starts at the original configuration"
       (equal (car tr) (list prg1 init-st)))

; Final values.
(check "small-step leaves x = 3" (= (os-lookup 'x ss-final) 3))
(check "big-step leaves x = 3" (= (os-lookup 'x bs-final) 3))
(check "small-step and big-step agree on the loop"
       (state-equal '(x) ss-final bs-final))

; An exhausted budget is reported, not silently mistaken for termination.
(check "small-steps reports an exhausted budget"
       (eq (catch (small-steps prg1 init-st 2) (lambda (e) 'exhausted)) 'exhausted))

; Equivalence of the two semantics on a range of commands.
(check "equivalence: x:=2"
       (check-equiv '(assign x 2) nil 5 '(x)))
(check "equivalence: x:=3; y:=x+1"
       (check-equiv '(seq (assign x 3) (assign y (+ x 1))) nil 5 '(x y)))
(check "equivalence: if x<5 then x:=10 else x:=0"
       (check-equiv '(if (< x 5) (assign x 10) (assign x 0)) (list (list 'x 3)) 5 '(x)))
(check "equivalence: while x<3 do x:=x+1"
       (check-equiv '(while (< x 3) (assign x (+ x 1))) (list (list 'x 0)) 20 '(x)))
(check "equivalence: nested loop body"
       (check-equiv '(seq (assign y 0) (while (< y 4) (seq (assign y (+ y 1)) (assign x (* y 2)))))
                    nil 60 '(x y)))

; And the sanity of the values themselves, not just their agreement.
(check "x:=3; y:=x+1 gives y = 4"
       (= (os-lookup 'y (big-step '(seq (assign x 3) (assign y (+ x 1))) nil)) 4))
(check "while y<4 doubles the last y into x"
       (= (os-lookup 'x (big-step '(seq (assign y 0)
                                        (while (< y 4)
                                          (seq (assign y (+ y 1)) (assign x (* y 2)))))
                                  nil))
          8))

(print "=== Operational done ===")
t
