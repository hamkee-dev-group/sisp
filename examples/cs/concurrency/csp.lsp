; Communicating Sequential Processes — Hoare's traces model.
; A process is a predicate on traces (event-lists). Prefix-closed.

; ---- process constructors ----

(define (csp-stop tr)
  (eq tr nil))

(define (csp-prefix a P)
  (lambda (tr)
    (cond ((eq tr nil) t)
          ((and (consp tr) (equal a (car tr))) (P (cdr tr)))
          (t nil))))

; Alphabetized parallel: a trace is allowed when its projection onto aP is
; a trace of P and its projection onto aQ is a trace of Q.  Events in both
; alphabets synchronize; the others interleave.  The sync set A is not
; consulted: it is implied by aP ∩ aQ.
(define (csp-parallel P aP Q aQ A)
  (lambda (tr)
    (and (P (filter (lambda (e) (in e aP)) tr))
         (Q (filter (lambda (e) (in e aQ)) tr)))))

; ---- trace enumeration (bounded) ----
; We avoid flatmap because it flattens the trace-lists themselves.
; Instead we use foldl to accumulate trace lists.

(define (csp-traces P alpha max-len)
  (labels (((gen depth prefix)
    (cond ((= depth 0) (list prefix))
          (t (cons prefix
            (foldl (lambda (acc e)
              (let ((new-tr (append prefix (list e))))
                (cond ((P new-tr) (append acc (gen (- depth 1) new-tr)))
                      (t acc))))
              nil alpha))))))
    (dedup (gen max-len nil))))

; ---- deadlock detection ----

(define (csp-deadlock-at P alpha tr)
  (every (lambda (e) (not (P (append tr (list e))))) alpha))

(define (csp-deadlocks P alpha max-len)
  (filter (lambda (tr) (csp-deadlock-at P alpha tr))
          (csp-traces P alpha max-len)))

; ---- refinement: P ⊑ Q  iff traces(Q) ⊆ traces(P) ----

(define (csp-refines P Q alpha max-len)
  (every (lambda (tr) (P tr)) (csp-traces Q alpha max-len)))

; ========== demonstrations ==========

; --- (a→STOP) [|{a}|] (a→STOP) — both offer a on sync set {a}, result a→STOP ---
(define P1 (csp-prefix 'a csp-stop))
(define Q1 (csp-prefix 'a csp-stop))
(define alpha1 (list 'a))
(define A1 (list 'a))
(define R1 (csp-parallel P1 alpha1 Q1 alpha1 A1))

(print "=== (a->STOP) [|{a}|] (a->STOP) ===")
(define trs1 (csp-traces R1 alpha1 3))
(print "traces:" trs1)
(define dl1 (csp-deadlock-at R1 alpha1 (list 'a)))
(print "deadlock after (a):" dl1)
(check "traces = (nil (a)) and deadlock after a"
       (and dl1 (equal trs1 (list nil (list 'a)))))

; --- (a→b→STOP) [|{a}|] (a→c→STOP) — a synchronizes; b,c independent ---
(define P2 (csp-prefix 'a (csp-prefix 'b csp-stop)))
(define Q2 (csp-prefix 'a (csp-prefix 'c csp-stop)))
(define alphaP2 (list 'a 'b))
(define alphaQ2 (list 'a 'c))
(define alphaPQ2 (union alphaP2 alphaQ2))
(define R2 (csp-parallel P2 alphaP2 Q2 alphaQ2 A1))

(print "=== (a->b->STOP) [|{a}|] (a->c->STOP) ===")
(define trs2 (csp-traces R2 alphaPQ2 4))
(print "traces:" trs2)

; Project onto A={a}: only traces restricted to {a} remain
(define proj-tr (dedup (map (lambda (tr) (filter (lambda (e) (in e A1)) tr)) trs2)))
(print "projection onto {a}:" proj-tr)
(check "projected traces = (nil (a)) = a->STOP traces"
       (equal proj-tr (list nil (list 'a))))

; --- refinement tests ---
(print "=== Refinement ===")
; P ⊑ Q (Q refines P) iff traces(Q) ⊆ traces(P): a refinement has fewer
; possible behaviours.  (csp-refines P Q) tests P ⊑ Q.
; P1 = a->STOP, P2 = a->b->STOP.
; traces(P1) = {(), (a)}. traces(P2) = {(), (a), (a,b)}.
; traces(P1) ⊆ traces(P2), so P2 ⊑ P1: a->STOP refines a->b->STOP.
(define ref-2-refines-1 (csp-refines P2 P1 alphaPQ2 3))
(print "a->b->STOP refines a->STOP (more det. refines less det.):" ref-2-refines-1)

; traces(P2) ⊄ traces(P1), since (a,b) is not a trace of P1: P1 ⊑ P2 fails.
(define ref-1-refines-2 (csp-refines P1 P2 alphaPQ2 3))
(print "a->STOP refines a->b->STOP:" ref-1-refines-2)

; Q2 = a->c->STOP. traces(P2) and traces(Q2) are incomparable.
(define ref-2-refines-q2 (csp-refines P2 Q2 alphaPQ2 3))
(print "a->b->STOP refines a->c->STOP (incomparable, both have 3 traces):" ref-2-refines-q2)
(define ref-q2-refines-2 (csp-refines Q2 P2 alphaPQ2 3))
(print "a->c->STOP refines a->b->STOP:" ref-q2-refines-2)

(print "=== CSP done ===")
t
