; ============================================================
; 61 — Heyting Algebras and Intuitionistic Logic
; The open sets of a finite topological space form a Heyting
; algebra. Its implication is defined by an adjunction, and the
; law of excluded middle FAILS — verified on a concrete space,
; not asserted. Boolean logic is the special (discrete) case.
; ============================================================

(load "lib/collections/relations.lsp")

(print "=== Heyting Algebra / Intuitionistic Logic ===")


; ---------- the space: a three-point chain 0 <= 1 <= 2 ----------
; Alexandrov opens = up-closed sets: {}, {2}, {1 2}, {0 1 2}.
(define X {0 1 2})
(define R (transitive-closure
  (union {(0 1) (1 2)} (map (lambda (x) (list x x)) X))))
(define (up-closed-p u)
  (every (lambda (p) (or (notin (car p) u) (in (cadr p) u))) R))
(define opens (filter (lambda (u) (up-closed-p u)) (powerset X)))
(print (list (quote opens) opens))
(check "four-opens" (= (length opens) 4))

; ---------- the Heyting algebra of opens ----------
; meet = intersection, join = union, top = X, bottom = {}.
; Implication U => V is the LARGEST open W with W /\ U <= V.
(define (heyting-imp u v)
  (foldl (lambda (acc w)
    (cond ((subset (cap w u) v) (union acc w)) (t acc)))
    {} (set-to-list opens)))
(define (heyting-not u) (heyting-imp u {}))

; The defining adjunction (residuation):  W /\ U <= V  <=>  W <= (U => V)
(define (triples-hold-p f)
  (every (lambda (w)
    (every (lambda (u)
      (every (lambda (v) (f w u v)) opens)) opens)) opens))
(check "implication-adjunction"
  (triples-hold-p (lambda (w u v)
    (equal (subset (cap w u) v) (subset w (heyting-imp u v))))))
(check "implication-is-open"
  (every (lambda (u)
    (every (lambda (v) (in (heyting-imp u v) opens)) opens)) opens))

; Intuitionistic theorems hold as identities on opens:
(check "modus-ponens-inequality"
  (every (lambda (u)
    (every (lambda (v)
      (subset (cap u (heyting-imp u v)) v)) opens)) opens))
(check "distributivity"
  (triples-hold-p (lambda (u v w)
    (equal (cap u (union v w)) (union (cap u v) (cap u w))))))
(check "triple-negation-collapses"
  (every (lambda (u)
    (equal (heyting-not (heyting-not (heyting-not u))) (heyting-not u))) opens))
(check "double-negation-is-inflationary"
  (every (lambda (u) (subset u (heyting-not (heyting-not u)))) opens))

; ---------- the failure of classical laws ----------
; U = {2} is open; its Heyting negation is the largest open disjoint
; from it, which is {} here. So U \/ ~U = {2} /= X: no excluded middle.
(define U {2})
(print (list (quote u) U (quote not-u) (heyting-not U)
  (quote u-or-not-u) (union U (heyting-not U))))
(check "excluded-middle-fails"
  (not (equal (union U (heyting-not U)) X)))
(check "double-negation-elimination-fails"
  (not (equal (heyting-not (heyting-not U)) U)))
(check "which-opens-violate-lem"
  (equal (filter (lambda (u) (not (equal (union u (heyting-not u)) X))) opens)
         {{2} {1 2}}))

; ---------- the Boolean special case: a discrete space ----------
; With no order between points every subset is open, negation is
; complement, and classical logic returns.
(define D {0 1})
(define discrete-opens (powerset D))
(define (disc-imp u v)
  (foldl (lambda (acc w)
    (cond ((subset (cap w u) v) (union acc w)) (t acc)))
    {} (set-to-list discrete-opens)))
(define (disc-not u) (disc-imp u {}))
(check "discrete-space-satisfies-lem"
  (every (lambda (u) (equal (union u (disc-not u)) D)) discrete-opens))
(check "discrete-negation-is-complement"
  (every (lambda (u) (equal (disc-not u) (diff D u))) discrete-opens))

(print "=== Heyting Algebra Done ===")
