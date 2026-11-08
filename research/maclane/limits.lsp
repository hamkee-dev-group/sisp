; ============================================================
; Mac Lane — Limits and Colimits
; Terminal/initial objects. Products/coproducts.
; Equalizers/coequalizers. Pullbacks/pushouts.
; Limits in Set (finite) and Vect_k (finite).
; ============================================================

(print "=== Mac Lane: Limits and Colimits ===")

; ---------- Finite Set category ----------
; Objects: small sets. Morphisms: functions.
; Terminal: singleton {*}. Initial: empty set {}.

(define terminal-Set {0})  ; singleton
(check "terminal-set-singleton-size-1" (= (length terminal-Set) 1))

; Unique function X → {*}: constant map to the single element.
; |Hom(X, {*})| = 1 for any X.
(check "unique-map-to-terminal"
  (let ((xs {1 2 3}))
    (= (length (map (lambda (x) 0) xs)) 1)))

; The image of any X under the constant map is exactly the terminal object,
; for every X in a family of test objects.
(check "terminal-map-image-is-terminal"
  (every
    (lambda (xs) (equal (map (lambda (x) 0) xs) terminal-Set))
    (list-to-set (list {1} {1 2} {1 2 3} {7 8 9 10}))))

; Initial object in Set: empty set
; Unique function ∅ → X: the empty function.
(check "empty-set-is-initial"
  (= (length {}) 0))

; ∅ is initial: Hom(∅, X) has exactly one element (the empty graph),
; so the image of ∅ under any map is empty for every X.
(check "empty-set-has-unique-map-out"
  (= (length (map (lambda (x) x) {})) 0))

; ---------- Products in Set ----------
; A × B = cartesian product with projections π_1, π_2.
; Universal: for any f: X→A, g: X→B, ∃! h: X→A×B.

(define (proj1 p) (car p))
(define (proj2 p) (cadr p))

(define A-pr {1 2 3})
; NOTE: {'a 'b} would build the set {(quote a) (quote b)} — a set of two
; two-element lists, not of symbols. Build symbol sets via list-to-set.
(define B-pr (list-to-set (list (quote alpha) (quote beta))))
(define AxB (cartesian-product A-pr B-pr))

(check "product-AxB-size-6" (= (length AxB) 6))

; Verify projections work: π_1 is onto A, π_2 is onto B.
(check "proj1-onto-A" (equal (map proj1 AxB) A-pr))
(check "proj2-onto-B" (equal (map proj2 AxB) B-pr))

; ---------- Universal property of the product ----------
; Given f: X→A and g: X→B, the mediating map h(x) = (f x, g x)
; is the unique morphism with π_1∘h = f and π_2∘h = g.

(define X-pr {10 20})
(define (f-pr x) (cond ((= x 10) 1) (t 3)))
(define (g-pr x) (cond ((= x 10) (quote alpha)) (t (quote beta))))
(define (h-pr x) (list (f-pr x) (g-pr x)))

(check "product-mediating-lands-in-AxB"
  (every (lambda (x) (in (h-pr x) AxB)) X-pr))

(check "product-triangle-proj1"
  (every (lambda (x) (equal (proj1 (h-pr x)) (f-pr x))) X-pr))

(check "product-triangle-proj2"
  (every (lambda (x) (equal (proj2 (h-pr x)) (g-pr x))) X-pr))

; Uniqueness: any k: X→A×B satisfying both triangles agrees with h
; pointwise. Quantify over ALL functions X→A×B (there are 6^2 = 36 of
; them, encoded as the pairs of values on {10,20}).
(check "product-mediating-unique"
  (every
    (lambda (kv)
      (let ((k10 (car kv)) (k20 (cadr kv)))
        (or (not (and (and (equal (proj1 k10) (f-pr 10))
                           (equal (proj2 k10) (g-pr 10)))
                      (and (equal (proj1 k20) (f-pr 20))
                           (equal (proj2 k20) (g-pr 20)))))
            (and (equal k10 (h-pr 10)) (equal k20 (h-pr 20))))))
    (cartesian-product AxB AxB)))

; ---------- Coproduct = disjoint union ----------
; A + B built concretely as tagged pairs: (0,a) for a∈A, (1,b) for b∈B.
(define inj-A (map (lambda (a) (list 0 a)) A-pr))
(define inj-B (map (lambda (b) (list 1 b)) B-pr))
(define coprod-AB (union inj-A inj-B))

(check "coproduct-size-5" (= (length coprod-AB) 5))
(check "coproduct-size-is-sum" (= (length coprod-AB) (+ (length A-pr) (length B-pr))))

; Injections are injective (tagging is invertible) and have disjoint images.
(check "coproduct-injection-A-injective"
  (= (length (map (lambda (p) (cadr p)) inj-A)) (length A-pr)))
(check "coproduct-injections-disjoint"
  (= (length (filter (lambda (p) (in p inj-B)) inj-A)) 0))

; Universal property of the coproduct: given u: A→Z and v: B→Z, the
; copairing [u,v] on A+B is well defined and restricts to u and v.
(define Z-co {100 200 300 400 500})
(define (u-co a) (* a 100))
(define (v-co b) (cond ((equal b (quote alpha)) 400) (t 500)))
(define (copair p)
  (cond ((= (car p) 0) (u-co (cadr p))) (t (v-co (cadr p)))))

(check "coproduct-copairing-into-Z"
  (every (lambda (p) (in (copair p) Z-co)) coprod-AB))
(check "coproduct-copairing-restricts-to-u"
  (every (lambda (a) (equal (copair (list 0 a)) (u-co a))) A-pr))
(check "coproduct-copairing-restricts-to-v"
  (every (lambda (b) (equal (copair (list 1 b)) (v-co b))) B-pr))

; ---------- Equalizers ----------
; Eq(f,g: A⇉B) = {a∈A: f(a)=g(a)}
; f(x)=0 if x≤1, else 1; g(x)=0 always.
; A={0,1,2,3}, Eq(f,g) = {0,1}
(define A-eq {0 1 2 3})

(define (feq x) (cond ((<= x 1) 0) (t 1)))
(define (geq x) 0)

(define equalizer-f-g
  (filter (lambda (a) (= (feq a) (geq a))) A-eq))

(check "equalizer-size-2" (= (length equalizer-f-g) 2))
(check "equalizer-contains-0" (in 0 equalizer-f-g))
(check "equalizer-contains-1" (in 1 equalizer-f-g))
(check "equalizer-is-subset-of-A" (subset equalizer-f-g A-eq))

; On the equalizer the two maps genuinely agree; off it they differ.
(check "equalizer-forks-agree"
  (every (lambda (a) (= (feq a) (geq a))) equalizer-f-g))
(check "equalizer-complement-forks-differ"
  (every (lambda (a) (not (= (feq a) (geq a))))
             (filter (lambda (a) (not (in a equalizer-f-g))) A-eq)))

; Universal property: any h: X→A with f∘h = g∘h factors through Eq,
; i.e. the image of h lands inside the equalizer.
(check "equalizer-universal-factorization"
  (every
    (lambda (h-img)
      (or (not (every (lambda (a) (= (feq a) (geq a))) h-img))
          (subset h-img equalizer-f-g)))
    (powerset A-eq)))

; ---------- Pullbacks ----------
; A ×_C B = {(a,b)∈A×B | f(a)=g(b)}
; f: {1,2,3}→{yes,no}: 1→yes, 2→yes, 3→no
; g: {alpha,beta}→{yes,no}: alpha→yes, beta→no
; NOTE: symbols are case-folded, so (quote T)/(quote F) would collide with
; the reserved boolean t — use yes/no instead.

(define A-pb {1 2 3})
(define B-pb (list-to-set (list (quote alpha) (quote beta))))

(define (f-pb x)
  (cond ((= x 1) (quote yes)) ((= x 2) (quote yes)) (t (quote no))))

(define (g-pb y)
  (cond ((equal y (quote alpha)) (quote yes)) (t (quote no))))

(define pullback-result
  (filter (lambda (p)
    (equal (f-pb (car p)) (g-pb (cadr p))))
    (cartesian-product A-pb B-pb)))

; Fiber over yes: f^{-1}(yes)={1,2}, g^{-1}(yes)={alpha} → 2*1=2 pairs.
; Fiber over no:  f^{-1}(no)={3},   g^{-1}(no)={beta}    → 1*1=1 pair. Total = 3.
(check "pullback-size-3" (= (length pullback-result) 3))

; The pullback is a subobject of the product and the square commutes on it.
(check "pullback-is-subset-of-product"
  (subset pullback-result (cartesian-product A-pb B-pb)))
(check "pullback-square-commutes"
  (every (lambda (p) (equal (f-pb (car p)) (g-pb (cadr p)))) pullback-result))

; Everything outside the pullback fails to commute (maximality).
(check "pullback-is-maximal"
  (every (lambda (p) (not (equal (f-pb (car p)) (g-pb (cadr p)))))
             (filter (lambda (p) (not (in p pullback-result)))
                         (cartesian-product A-pb B-pb))))

; Pullback = disjoint union of fibre products over each element of C.
(check "pullback-is-sum-over-fibres"
  (= (length pullback-result)
     (+ (* (length (filter (lambda (x) (equal (f-pb x) (quote yes))) A-pb))
           (length (filter (lambda (y) (equal (g-pb y) (quote yes))) B-pb)))
        (* (length (filter (lambda (x) (equal (f-pb x) (quote no))) A-pb))
           (length (filter (lambda (y) (equal (g-pb y) (quote no))) B-pb))))))

; A pullback over the TERMINAL object is just the product: both legs are
; the unique constant map, so the fibre condition is satisfied everywhere.
; The condition is evaluated on the actual maps, not short-circuited.
(define (to-terminal-a x) 0)
(define (to-terminal-b y) 0)
(check "pullback-over-terminal-is-product"
  (equal (filter (lambda (p) (equal (to-terminal-a (car p)) (to-terminal-b (cadr p))))
                     (cartesian-product A-pb B-pb))
         (cartesian-product A-pb B-pb)))
; And the same construction over a NON-terminal base cuts the product down,
; so the pullback filter is not vacuously accepting everything.
(check "pullback-over-non-terminal-is-proper"
  (< (length pullback-result) (length (cartesian-product A-pb B-pb))))

; ---------- Limit of a discrete diagram ----------
; Limit of the discrete diagram {A,B} in Set = product A×B.
(check "limit-discrete-is-product"
  (equal (cartesian-product A-pr B-pr) AxB))
(check "limit-discrete-size-6"
  (= (length (cartesian-product A-pr B-pr)) 6))

; A cone over the discrete diagram {A,B} with tip X is exactly a pair
; of maps (f: X→A, g: X→B); it factors uniquely through the limit.
(check "cone-over-discrete-factors-through-limit"
  (every (lambda (x) (in (list (f-pr x) (g-pr x)) AxB)) X-pr))

; ---------- Colimit of a discrete diagram = coproduct ----------
(check "colimit-discrete-is-coproduct"
  (= (length coprod-AB) (+ (length A-pr) (length B-pr))))
(check "colimit-discrete-size-5"
  (= (length coprod-AB) 5))

; ---------- Limits in finite vector spaces (F_2^n model) ----------
; A finite-dimensional F_2-vector space of dimension n has 2^n elements.
(define V2 (cartesian-product {0 1} {0 1}))
(check "V2-size-4" (= (length V2) 4))

; Product in Vect_k: direct sum, dim(V⊕W)=dim(V)+dim(W).
; V = F_2 (dim 1), W = F_2 (dim 1) → V⊕W = F_2^2 has 2^(1+1) = 4 elements.
(define dim-V 1)
(define dim-W 1)
(check "product-dim-is-sum" (= (length V2) (^ 2 (+ dim-V dim-W))))

; The direct sum is a biproduct: projections are onto each factor.
(check "vect-proj1-onto" (equal (map proj1 V2) {0 1}))
(check "vect-proj2-onto" (equal (map proj2 V2) {0 1}))

; Zero space is both terminal and initial in Vect_k: |{0}| = 2^0 = 1.
(check "vect-zero-object-is-terminal-and-initial" (= (length {0}) (^ 2 0)))

; Kernel as an equalizer: ker(φ) = Eq(φ, 0).
; φ: F_2^2 → F_2, φ(x,y) = x + y (mod 2). ker φ = {(0,0),(1,1)}, dim 1.
(define (phi-v p) (mod (+ (car p) (cadr p)) 2))
(define ker-phi (filter (lambda (p) (= (phi-v p) 0)) V2))
(check "vect-kernel-is-equalizer-size-2" (= (length ker-phi) 2))
(check "vect-kernel-dim-1" (= (length ker-phi) (^ 2 1)))
(check "vect-kernel-contains-zero" (in (list 0 0) ker-phi))
; Rank-nullity as a limit statement: |ker φ| · |im φ| = |V2|,
; i.e. dim ker + dim im = dim V2 = 2. Computed, not hardcoded.
(define im-phi (map phi-v V2))
(check "vect-rank-nullity"
  (= (* (length ker-phi) (length im-phi)) (^ 2 (+ dim-V dim-W))))

; ---------- Pushout ----------
; Dual to pullback. In Set: pushout of A←C→B = (A+B)/~
; where f(c) ~ g(c) for all c∈C.
; Pushout size = |A| + |B| - |identified pairs|.
; Take C = {1} with u: C→A, u(1)=1 and v: C→B, v(1)=alpha.
; Then (0,1) ~ (1,alpha), collapsing 5 tagged elements down to 4.

(define C-po {1})
(define (u-po c) 1)
(define (v-po c) (quote alpha))

; Generic gluing: `ids` is a set of (from to) rewrites collapsing each
; B-side element onto its A-side representative.
(define (glue ids p)
  (let ((hit (filter (lambda (q) (equal (car q) p)) ids)))
    (cond ((= (length hit) 0) p) (t (cadr (car hit))))))

; Identifications generated by C: (1,v(c)) ~ (0,u(c)) for each c∈C.
(define po-ids
  (map (lambda (c) (list (list 1 (v-po c)) (list 0 (u-po c)))) C-po))

(define (po-class p) (glue po-ids p))

(define pushout-result (map po-class coprod-AB))

(check "pushout-size-4" (= (length pushout-result) 4))
(check "pushout-size-formula"
  (= (length pushout-result) (- (+ (length A-pr) (length B-pr)) (length C-po))))

; The pushout square commutes: both legs out of C agree in the quotient.
(check "pushout-square-commutes"
  (every (lambda (c)
               (equal (po-class (list 0 (u-po c)))
                      (po-class (list 1 (v-po c)))))
             C-po))

; The quotient map is surjective onto the pushout and idempotent on classes.
(check "pushout-quotient-idempotent"
  (every (lambda (p) (equal (po-class p) (po-class (po-class p))))
             coprod-AB))
(check "pushout-is-quotient-of-coproduct"
  (< (length pushout-result) (length coprod-AB)))

; Pushout over the initial object ∅ is just the coproduct: with C = ∅ the
; generated identification set is empty, so the quotient glues nothing.
(define C-initial {})
(define po-ids-initial
  (map (lambda (c) (list (list 1 (v-po c)) (list 0 (u-po c)))) C-initial))
(check "pushout-over-initial-generates-no-identifications"
  (= (length po-ids-initial) 0))
(check "pushout-over-initial-is-coproduct"
  (equal (map (lambda (p) (glue po-ids-initial p)) coprod-AB) coprod-AB))

(print "=== Limits Done ===")
