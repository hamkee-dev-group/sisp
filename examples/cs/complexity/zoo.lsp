; Complexity Zoo — class relationships and polynomial-time SAT verifier.
; Classes: P, NP, PSPACE, EXPTIME.
; Resource notions: TIME(f(n)), NTIME(f(n)), SPACE(f(n)).

; ---- SAT formula representation ----
; A CNF formula is a list of clauses; each clause is a list of literals.
; A literal is (pos var) or (neg var).

(define (sat-var lit) (cadr lit))
(define (sat-pos lit) (equal (car lit) 'pos))

; Given assignment (alist of (var . bool)), evaluate literal
(define (sat-eval-lit lit assign)
  (let ((val (assoc (sat-var lit) assign)))
    (cond ((eq val nil) nil)
          (t (cond ((sat-pos lit) (cadr val))
                   (t (not (cadr val))))))))

; Evaluate a clause (disjunction)
(define (sat-eval-clause clause assign)
  (any (lambda (lit) (sat-eval-lit lit assign)) clause))

; Evaluate a CNF formula
(define (sat-eval cnf assign)
  (every (lambda (clause) (sat-eval-clause clause assign)) cnf))

; SAT verifier: given formula and assignment, verify in poly-time
(define (sat-verify cnf assign)
  (sat-eval cnf assign))

; ---- complexity class definitions ----
; A decision problem is subset of {0,1}*.
; P: problems solvable in polynomial time on deterministic TM.
; NP: problems solvable in polynomial time on non-deterministic TM
;     = polynomial-time verifiable certificate.
; PSPACE: problems solvable in polynomial space.
; EXPTIME: problems solvable in exponential time (2^{n^k}).

; Resource containment: TIME(n^k) ⊆ NTIME(n^k) ⊆ SPACE(n^k) ⊆ TIME(2^{n^k})
; Thus: P ⊆ NP ⊆ PSPACE ⊆ EXPTIME

; ---- demonstrate containments via resource bounds ----

; Simulate poly-time verifier for SAT: verify certificate in O(|cnf|^k) steps.
; n = number of literals, k = constant.
(define (sat-verifier-complexity cnf assign)
  ; each clause is examined exactly once during verification, so the
  ; step count is |cnf| -- linear, hence polynomial
  (let ((result (sat-eval cnf assign)))
    (list 'result result 'steps (length cnf))))

; ---- known separations and open problems ----

; Separations known:
;   EXPTIME ≠ P (time hierarchy)
; Containments known:
;   NP ⊆ EXPTIME (try every certificate)
; Open:
;   P vs NP (the most famous)
;   P vs PSPACE: is PSPACE ≠ P?
;   PSPACE vs EXPTIME

; Known:
;   NL ⊆ P (an NL machine has polynomially many configurations, so
;          reachability in its configuration graph is decided in poly time)
;   NSPACE(f(n)) ⊆ SPACE(f(n)^2) (Savitch), hence NL ⊆ SPACE(log^2 n)
;   PSPACE = NPSPACE (Savitch)
;   P ⊆ NP ⊆ PSPACE ⊆ EXPTIME, with P ⊊ EXPTIME strict

(define *known-separations*
  '(    (EXPTIME neq P "by time-hierarchy theorem")
    (PSPACE = NPSPACE "Savitch's theorem")
    (NL subset P "space-hierarchy")
    (P subsetneq EXPTIME "time-hierarchy")))

(define *open-problems*
  '((P vs NP "Clay prize problem")
    (NP vs PSPACE "Open")
    (PSPACE vs EXPTIME "Open")
    (P vs PSPACE "Open")
    (L vs P "Open")
    (L vs NL "Open")))

; ========== demonstrations ==========

(print "=== SAT example ===")
(define cnf1 '(((pos x1) (pos x2) (neg x3))
               ((neg x1) (neg x2))
               ((pos x1) (pos x3))))
(define assign1 '((x1 t) (x2 nil) (x3 t)))
(print "CNF:" cnf1)
(print "Assignment:" assign1)
(print "SATISFIED:" (sat-verify cnf1 assign1))

; Unsatisfying assignment
(define assign2 '((x1 nil) (x2 nil) (x3 nil)))
(print "Assignment2:" assign2)
(print "SATISFIED:" (sat-verify cnf1 assign2))

(print "=== Complexity class containments ===")
(print "P ??? NP (poly-time deterministic ??? poly-time nondeterministic)")
(print "NP ??? PSPACE (poly-time nondeterministic ??? poly-space)")
(print "PSPACE ??? EXPTIME (poly-space ??? exponential-time)")
(print "Thus: P ??? NP ??? PSPACE ??? EXPTIME")

(print "=== Known separations ===")
(print (length *known-separations*) "known separations:")
(foreach (lambda (x) (print x)) *known-separations*)

(print "=== Open problems ===")
(print (length *open-problems*) "open problems:")
(foreach (lambda (x) (print x)) *open-problems*)

; Demonstrate poly-time verification complexity
(print "=== Verifier complexity demo ===")
(define cnf2 '(((pos a) (pos b)) ((neg a)) ((pos b) (neg c))))
(define assign3 '((a t) (b t) (c nil)))
(print "Small CNF verified:" (sat-verify cnf2 assign3))
(print "Steps ??? |CNF| * max_clause_per_var = O(n^2) = polynomial.")

(print "=== Zoo done ===")
t
