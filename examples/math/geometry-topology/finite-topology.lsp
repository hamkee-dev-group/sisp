; ============================================================
; 60 — Finite Topology = Preorders = Modal Logic S4
; The Alexandrov correspondence: finite topological spaces,
; preorders, and S4 Kripke frames are the same object. All three
; faces are computed from one relation and cross-checked.
; ============================================================

(load "lib/collections/relations.lsp")

(print "=== Finite Topology / Alexandrov / S4 ===")


; ---------- a preorder on four points ----------
; 0 <= 1 <= 3 and 0 <= 2 <= 3 (a diamond), reflexive-transitive.
(define X {0 1 2 3})
(define order-base {(0 1) (0 2) (1 3) (2 3)})
(define (reflexive r carrier)
  (union r (map (lambda (x) (list x x)) carrier)))
(define R (transitive-closure (reflexive order-base X)))

(check "R-is-a-preorder"
  (and (every (lambda (x) (in (list x x) R)) X)
       (equal (transitive-closure R) R)))

; ---------- face 1: the Alexandrov topology ----------
; Opens are the up-closed sets of R: if x is in U and x <= y, y is in U.
(define (up-closed-p u)
  (every (lambda (p)
    (or (notin (car p) u) (in (cadr p) u))) R))
(define opens (filter (lambda (u) (up-closed-p u)) (powerset X)))
(print (list (quote opens) opens))

(check "empty-and-whole-are-open" (and (in {} opens) (in X opens)))
(check "opens-closed-under-union"
  (every (lambda (p)
    (in (union (car p) (cadr p)) opens))
    (pairs-such-that (lambda (u v) t) opens opens)))
(check "opens-closed-under-intersection"
  (every (lambda (p)
    (in (cap (car p) (cadr p)) opens))
    (pairs-such-that (lambda (u v) t) opens opens)))

; ---------- face 2: recover the preorder as specialization ----------
; x <= y  iff  every open containing x contains y.
(define specialization
  (pairs-such-that (lambda (x y)
    (every (lambda (u) (or (notin x u) (in y u))) opens)) X X))
(check "specialization-recovers-preorder" (equal specialization R))

; ---------- interior and closure operators ----------
; interior(A) = union of opens inside A; here computed pointwise:
; x is interior to A iff its whole up-set sits inside A.
; note: {x} would be the literal symbol x — sets are literal, so the
; up-set is computed by filtering the relation instead.
(define (up-set x)
  (rel-range (filter (lambda (p) (equal (car p) x)) R)))
(define (interior a)
  (filter (lambda (x) (subset (up-set x) a)) a))
(define (closure-op a)
  (filter (lambda (x)
    (any (lambda (y) (in y a)) (up-set x))) X))

(check "interior-is-open" (every (lambda (a) (in (interior a) opens)) (powerset X)))
(check "interior-idempotent"
  (every (lambda (a) (equal (interior (interior a)) (interior a))) (powerset X)))
(check "interior-of-intersection"
  (every (lambda (p)
    (equal (interior (cap (car p) (cadr p)))
           (cap (interior (car p)) (interior (cadr p)))))
    (pairs-such-that (lambda (u v) (and (not (equal u {})) (not (equal v {}))))
      (powerset X) (powerset X))))
(check "closure-is-complement-dual"
  (every (lambda (a)
    (equal (closure-op a) (diff X (interior (diff X a))))) (powerset X)))

; ---------- face 3: S4 modal logic ----------
; Propositions are sets of worlds; box(A) = worlds whose successors
; all satisfy A. On a preorder frame, box IS the interior operator.
(define (box a)
  (filter (lambda (w) (subset (up-set w) a)) X))
(define (dia a) (diff X (box (diff X a))))

(check "box-equals-interior"
  (every (lambda (a) (equal (box a) (interior a))) (powerset X)))
; axiom K: box(A -> B) -> (box A -> box B), with -> as material implication
(define (imp a b) (union (diff X a) b))
(check "axiom-K"
  (every (lambda (p)
    (equal (imp (box (imp (car p) (cadr p)))
                (imp (box (car p)) (box (cadr p)))) X)
    ) (pairs-such-that (lambda (a b) t) (powerset X) (powerset X))))
; axiom T (reflexivity): box A -> A
(check "axiom-T"
  (every (lambda (a) (subset (box a) a)) (powerset X)))
; axiom 4 (transitivity): box A -> box box A
(check "axiom-4"
  (every (lambda (a) (subset (box a) (box (box a)))) (powerset X)))
; diamond is the closure operator
(check "diamond-equals-closure"
  (every (lambda (a) (equal (dia a) (closure-op a))) (powerset X)))

; ---------- continuity = monotonicity ----------
; f collapsing the diamond: 1 and 2 both map to 1 in the chain 0<=1<=3.
(define Y {0 1 3})
(define RY (transitive-closure (reflexive {(0 1) (1 3)} Y)))
(define (collapse x) (cond ((= x 2) 1) (t x)))
(define opens-y
  (filter (lambda (u)
    (every (lambda (p) (or (notin (car p) u) (in (cadr p) u))) RY))
    (powerset Y)))
(define (preimage f u) (filter (lambda (x) (in (f x) u)) X))
(check "monotone-map-is-continuous"
  (every (lambda (u) (in (preimage collapse u) opens)) opens-y))

(print "=== Finite Topology / Alexandrov / S4 Done ===")
