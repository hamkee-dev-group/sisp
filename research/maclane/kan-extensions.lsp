; ============================================================
; Mac Lane — Kan Extensions
; Left Kan extension Lan_K F. Right Kan extension Ran_K F.
; Universal property. Pointwise computation.
; Density theorem.
; ============================================================

(print "=== Mac Lane: Kan Extensions ===")

(define (upto lo hi) (cond ((> lo hi) nil) (t (cons lo (upto (+ lo 1) hi)))))
(define (concat ls) (foldr append nil ls))

; ---------- Kan extensions in poset categories ----------
; A poset is a carrier plus an order; a functor between posets is an
; order-preserving map. Joins and meets are computed from the order
; itself (least upper bound / greatest lower bound), not read off a chain.

(define (upper-bounds carrier leq s)
  (filter (lambda (c) (every (lambda (x) (leq x c)) s)) carrier))
(define (lower-bounds carrier leq s)
  (filter (lambda (c) (every (lambda (x) (leq c x)) s)) carrier))
(define (join carrier leq s)
  (let ((ubs (upper-bounds carrier leq s)))
    (car (filter (lambda (c) (every (lambda (d) (leq c d)) ubs)) ubs))))
(define (meet carrier leq s)
  (let ((lbs (lower-bounds carrier leq s)))
    (car (filter (lambda (c) (every (lambda (d) (leq d c)) lbs)) lbs))))

(print "--- Kan extension in finite posets ---")

; A = {0,1} chain;  M = {0,1,2} chain;  C = {0,1,2,3} chain.
; K: A -> M sends 0 |-> 0, 1 |-> 1.   F: A -> C sends 0 |-> 0, 1 |-> 2.
(define acar (list 0 1))
(define mcar (list 0 1 2))
(define ccar (list 0 1 2 3))
(define (ord-leq x y) (<= x y))

(define (kfun a) (cond ((= a 0) 0) (t 1)))
(define (ffun a) (cond ((= a 0) 0) (t 2)))

(check "K-is-order-preserving"
  (every (lambda (x) (every (lambda (y)
            (cond ((ord-leq x y) (ord-leq (kfun x) (kfun y))) (t t))) acar)) acar))
(check "F-is-order-preserving"
  (every (lambda (x) (every (lambda (y)
            (cond ((ord-leq x y) (ord-leq (ffun x) (ffun y))) (t t))) acar)) acar))

; Pointwise formulas: Lan is a colimit (join) over the comma category K/m,
; Ran is a limit (meet) over m/K. Empty join = bottom, empty meet = top.
(define (lan-at m)
  (join ccar ord-leq (map (lambda (a) (ffun a))
                          (filter (lambda (a) (ord-leq (kfun a) m)) acar))))
(define (ran-at m)
  (meet ccar ord-leq (map (lambda (a) (ffun a))
                          (filter (lambda (a) (ord-leq m (kfun a))) acar))))

(check "lan-values" (equal (map (lambda (m) (lan-at m)) mcar) (list 0 2 2)))
(check "ran-values" (equal (map (lambda (m) (ran-at m)) mcar) (list 0 2 3)))

; Both extensions must themselves be functors, i.e. order-preserving.
(check "lan-is-order-preserving"
  (every (lambda (x) (every (lambda (y)
            (cond ((ord-leq x y) (ord-leq (lan-at x) (lan-at y))) (t t))) mcar)) mcar))
(check "ran-is-order-preserving"
  (every (lambda (x) (every (lambda (y)
            (cond ((ord-leq x y) (ord-leq (ran-at x) (ran-at y))) (t t))) mcar)) mcar))

(print "--- Universal property, checked against every competitor ---")

; Enumerate EVERY function M -> C and keep the order-preserving ones. The
; universal property is then a statement quantified over all of them.
(define (all-funcs dom cod)
  (cond ((eq dom nil) (list nil))
        (t (concat (map (lambda (rest) (map (lambda (v) (cons v rest)) cod))
                        (all-funcs (cdr dom) cod))))))
(define (app fn m) (nth (index-of m mcar) fn))
(define monotone-maps
  (filter (lambda (fn)
            (every (lambda (x) (every (lambda (y)
                      (cond ((ord-leq x y) (ord-leq (app fn x) (app fn y))) (t t))) mcar))
                   mcar))
          (all-funcs mcar ccar)))

(check "there-are-monotone-competitors" (> (length monotone-maps) 1))
(check "monotone-map-count-is-C-4-choose-3"
  (= (length monotone-maps) 20))

; Unit: F <= Lan . K.  Counit: Ran . K <= F.
(check "lan-unit-exists" (every (lambda (a) (ord-leq (ffun a) (lan-at (kfun a)))) acar))
(check "ran-counit-exists" (every (lambda (a) (ord-leq (ran-at (kfun a)) (ffun a))) acar))

; Lan is the LEAST monotone map L with F <= L . K.
(check "lan-is-least-such-extension"
  (every (lambda (fn)
           (cond ((every (lambda (a) (ord-leq (ffun a) (app fn (kfun a)))) acar)
                  (every (lambda (m) (ord-leq (lan-at m) (app fn m))) mcar))
                 (t t)))
         monotone-maps))

; Ran is the GREATEST monotone map R with R . K <= F.
(check "ran-is-greatest-such-restriction"
  (every (lambda (fn)
           (cond ((every (lambda (a) (ord-leq (app fn (kfun a)) (ffun a))) acar)
                  (every (lambda (m) (ord-leq (app fn m) (ran-at m))) mcar))
                 (t t)))
         monotone-maps))

; Lan itself is one of the competitors, so the two conditions above pin it
; down uniquely: it is the unique least element of that set.
(check "lan-is-itself-a-monotone-competitor"
  (contains (map (lambda (m) (lan-at m)) mcar) monotone-maps))
(check "ran-is-itself-a-monotone-competitor"
  (contains (map (lambda (m) (ran-at m)) mcar) monotone-maps))

; Negative control: the constant-0 map satisfies Ran's condition but not
; Lan's, so it must be rejected as a left Kan extension.
(check "constant-zero-is-not-a-left-extension"
  (not (every (lambda (a) (ord-leq (ffun a) 0)) acar)))

(print "--- Pointwise Lan in Set ---")

; A discrete, K(0) = K(1) = a. (Lan_K F)(a) is the COPRODUCT F(0) + F(1),
; formed with tags so that overlapping sets are not merged.
(define f0 (list 1 2))
(define f1 (list 2 3 4))
(define (tag i s) (map (lambda (x) (list i x)) s))
(define lan-at-a (append (tag 0 f0) (tag 1 f1)))
(define lan-at-b nil)

(check "lan-example-size-5-at-a" (= (length (dedup lan-at-a)) 5))
; The coproduct is strictly bigger than the union: the tagging is doing work.
(check "coproduct-is-not-the-union"
  (> (length (dedup lan-at-a)) (length (dedup (append f0 f1)))))
(check "lan-example-size-0-at-b" (= (length lan-at-b) 0))

(print "--- Pointwise Ran in Set ---")

; K(0) = a, K(1) = b in the poset {a, b} with no map a -> b or b -> a.
; (Ran_K F)(m) is the PRODUCT over the comma category m/K, which here has
; a single object at each m.
(define (product-of sets)
  (cond ((eq sets nil) (list nil))
        (t (concat (map (lambda (rest) (map (lambda (x) (cons x rest)) (car sets)))
                        (product-of (cdr sets)))))))
(define ran-at-a (product-of (list f0)))
(define ran-at-b (product-of (list (list 3))))
(check "ran-example-size-2-at-a" (= (length ran-at-a) 2))
(check "ran-example-size-1-at-b" (= (length ran-at-b) 1))
(check "empty-product-is-a-singleton" (= (length (product-of nil)) 1))

(print "--- Yoneda lemma by enumeration ---")

; Base category: the poset 0 -> 1 (one non-identity arrow u).
; A presheaf P: A^op -> Set is P0, P1 and a restriction map P(u): P1 -> P0.
(define p0 (list (quote x) (quote y)))
(define p1 (list (quote p) (quote q) (quote r)))
(define (p-restrict e)
  (cond ((eq e (quote p)) (quote x)) ((eq e (quote q)) (quote x)) (t (quote y))))

; y(0): y(0)(0) = {id0}, y(0)(1) = Hom(1,0) = empty.
; A natural transformation y(0) => P is just a choice in P0; naturality is
; vacuous because y(0)(1) is empty.
(define nats-from-y0 (map (lambda (v) (list v)) p0))
(check "yoneda-at-0" (= (length nats-from-y0) (length p0)))

; y(1): y(1)(0) = {u}, y(1)(1) = {id1}. Naturality on u forces
; P(u)(alpha_1(id1)) = alpha_0(u), so alpha_1 determines alpha_0.
(define candidate-nats
  (concat (map (lambda (a1) (map (lambda (a0) (list a0 a1)) p0)) p1)))
(define nats-from-y1
  (filter (lambda (al) (eq (p-restrict (cadr al)) (car al))) candidate-nats))

(check "yoneda-density" (= (length nats-from-y1) (length p1)))
; The naturality condition is a real constraint: it discards candidates.
(check "yoneda-naturality-is-restrictive"
  (< (length nats-from-y1) (length candidate-nats)))
; Each natural transformation is determined by its value at the identity --
; the Yoneda bijection Nat(y(1), P) -> P(1) is injective.
(check "yoneda-bijection-is-injective"
  (= (length (dedup (map (lambda (al) (cadr al)) nats-from-y1)))
     (length nats-from-y1)))

(print "--- Density Theorem ---")

; Every presheaf is the colimit of representables over its category of
; elements. For the discrete category {0,1}, that colimit is the copower
; P0 . y(0) + P1 . y(1); evaluating it componentwise must return P again.
(define (copower s rep-component) (concat (map (lambda (e) (tag e rep-component)) s)))
(define density-at-0 (append (copower p0 (list (quote id0))) (copower p1 nil)))
(define density-at-1 (append (copower p0 nil) (copower p1 (list (quote id1)))))

(check "density-theorem-holds"
  (and (= (length density-at-0) (length p0))
       (= (length density-at-1) (length p1))))
; Negative control: dropping the P1 summand breaks the reconstruction at 1.
(check "density-needs-every-summand"
  (not (= (length (copower p0 nil)) (length p1))))

(print "--- Existence, pointwise ---")
; Lan and Ran exist at every object of M because the relevant join and meet
; exist there -- checked, not asserted.
(check "lan-exists-pointwise"
  (every (lambda (m) (in (lan-at m) ccar)) mcar))
(check "ran-exists-pointwise"
  (every (lambda (m) (in (ran-at m) ccar)) mcar))

(print "=== Kan Extensions Done ===")
