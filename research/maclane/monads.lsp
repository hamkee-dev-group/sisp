; ============================================================
; Mac Lane — Monads and Algebras
; Monad T = (T, η, μ). T-algebra (A, a:TA→A).
; Eilenberg-Moore category. Kleisli category.
; Every adjunction gives a monad.
; ============================================================

(print "=== Mac Lane: Monads ===")

; ---------- Monad on Set ----------
; A monad T: Set → Set consists of:
;   - a functor T: Set → Set
;   - unit η: 1 ⇒ T (natural transformation)
;   - multiplication μ: T² ⇒ T (natural transformation)
; satisfying:
;   μ ∘ Tμ = μ ∘ μT (associativity)
;   μ ∘ ηT = 1 = μ ∘ Tη (unit laws)

; Example: The list monad (free monoid monad)
; T(X) = List(X) = set of finite lists over X.
; η_X(x) = [x] (singleton)
; μ_X([[x1,...,xn], ..., [y1,...,ym]]) = [x1,...,xn,...,y1,...,ym] (flatten)

; For finite sets, we restrict to lists of bounded length.

; ---------- T-algebra ----------
; A T-algebra is a pair (A, a: TA → A) such that:
;   a ∘ η_A = id_A
;   a ∘ μ_A = a ∘ T(a)

; For the list monad: T-algebra = monoid!
; A = underlying set, a: List(A) → A is the monoid multiplication.
; a([]) = e (identity), a([x1,...,xn]) = x1·...·xn.

; ---------- Verify monad axioms for list monad ----------
; Restricted to lists of length ≤ 2 on a finite set.

(define (list-unit x) (list x))

(define (list-flatten xss)
  (cond ((eq xss nil) nil)
        (t (append (car xss) (list-flatten (cdr xss))))))

; The monad laws are quantified over EVERY list in the truncated free
; monoid, not spot-checked on one example.
(define (concat1 ls) (foldr append nil ls))
(define lm-alph (list 1 2))
(define (lm-lists-of-len k)
  (cond ((= k 0) (list nil))
        (t (concat1 (map (lambda (w) (map (lambda (s) (cons s w)) lm-alph))
                         (lm-lists-of-len (- k 1)))))))
(define lm-lists (append (lm-lists-of-len 0)
                         (append (lm-lists-of-len 1) (lm-lists-of-len 2))))
(define (lm-nested-of-len k src)
  (cond ((= k 0) (list nil))
        (t (concat1 (map (lambda (w) (map (lambda (s) (cons s w)) src))
                         (lm-nested-of-len (- k 1) src))))))
(define lm-nested (append (lm-nested-of-len 0 lm-lists)
                          (append (lm-nested-of-len 1 lm-lists)
                                  (lm-nested-of-len 2 lm-lists))))

(check "list-monad-has-nontrivial-test-domain"
  (and (= (length lm-lists) 7) (= (length lm-nested) 57)))

; mu . eta_T = id
(check "monad-unit-law-1"
  (every (lambda (xs) (equal (list-flatten (list xs)) xs)) lm-lists))

; mu . T eta = id
(check "monad-unit-law-2"
  (every (lambda (xs) (equal (list-flatten (map (lambda (x) (list x)) xs)) xs))
         lm-lists))

; Associativity μ ∘ μT = μ ∘ Tμ
; μ_X: TTX → TX flattens one level.  On TTTX there are two ways down to TX:
; μ_TX: TTTX → TTX concatenates the outer list (its elements are lists of
;   lists, treated as atoms);
; Tμ_X: TTTX → TTX applies μ_X inside each element of the outer list.
; For [[[a],[b]], [[c]]]: μ_TX gives [[a],[b],[c]], then μ_X → [a,b,c];
; Tμ_X gives [[a,b],[c]], then μ_X → [a,b,c].  Same result.

; mu . T mu = mu . mu_T, over every triply-nested list in range.
(define lm-nested3 (append (lm-nested-of-len 0 lm-nested)
                           (append (lm-nested-of-len 1 lm-nested)
                                   (lm-nested-of-len 2 lm-nested))))
(check "monad-associativity"
  (every (lambda (xsss)
           (equal (list-flatten (map list-flatten xsss))
                  (list-flatten (list-flatten xsss))))
         lm-nested3))
; Negative control: dropping one level of flattening breaks associativity.
(check "monad-associativity-is-a-real-constraint"
  (not (every (lambda (xsss) (equal (map list-flatten xsss) (list-flatten xsss)))
              lm-nested3)))

; ---------- Eilenberg-Moore category ----------
(print "--- Eilenberg-Moore category ---")

; Objects: T-algebras (A, a: TA→A).
; Morphisms: T-algebra homomorphisms f: (A,a) → (B,b)
;   where f: A→B satisfies f∘a = b∘Tf.

; For list monad: EM(T) ≅ Monoid.
; Objects are monoids, morphisms are monoid homomorphisms.

; Example: (N, +, 0) as a T-algebra.
; A = N, a: List(N) → N by sum of list.
; a([n1,...,nk]) = n1+...+nk, a([]) = 0.

; Verify T-algebra axioms:
(define (list-sum xs)
  (foldl (lambda (acc x) (+ acc x)) 0 xs))

; a . eta = id, for every element.
(check "algebra-unit-law"
  (every (lambda (n) (= (list-sum (list-unit n)) n)) (list 0 1 2 5 17)))

; a . mu = a . T a, over every nested list in range.
(check "algebra-mult-law"
  (every (lambda (xss) (= (list-sum (list-flatten xss)) (list-sum (map list-sum xss))))
         lm-nested))
; Negative control: a non-algebra structure map (list length) fails the
; unit law, so the test above is not satisfied by just anything.
(check "algebra-laws-reject-a-non-algebra"
  (not (every (lambda (n) (= (length (list-unit n)) n)) (list 0 1 2 5 17))))

; ---------- Kleisli category ----------
(print "--- Kleisli category ---")

; Objects: same as base category (Set).
; Kleisli morphisms X →_T Y are maps X → TY in Set.
; Composition: for f: X→TY and g: Y→TZ,
;   g ∘_T f = μ_Z ∘ Tg ∘ f.
; Identity: η_X: X→TX.

; Kleisli(T) ≅ category of free T-algebras.

(define (kleisli-comp kg kf) (lambda (x) (list-flatten (map kg (kf x)))))
(define (kf1 x) (list x x))
(define (kg1 y) (list (+ y 1) y))
(define (kh1 z) (list z (+ z 1)))
(define kdom (list 1 2 3))

; eta is a two-sided identity for Kleisli composition.
(check "kleisli-left-identity"
  (every (lambda (x) (equal ((kleisli-comp (lambda (y) (list-unit y)) kf1) x) (kf1 x))) kdom))
(check "kleisli-right-identity"
  (every (lambda (x) (equal ((kleisli-comp kf1 (lambda (y) (list-unit y))) x) (kf1 x))) kdom))
; Kleisli composition is associative.
(check "kleisli-composition-is-associative"
  (every (lambda (x)
           (equal ((kleisli-comp kh1 (kleisli-comp kg1 kf1)) x)
                  ((kleisli-comp (kleisli-comp kh1 kg1) kf1) x)))
         kdom))
; Negative control: the two composition orders genuinely differ.
(check "kleisli-composition-is-not-commutative"
  (not (every (lambda (x) (equal ((kleisli-comp kg1 kf1) x) ((kleisli-comp kf1 kg1) x)))
              kdom)))

; ---------- Every adjunction gives a monad ----------
(print "--- Adjunction → Monad ---")

; Given F : C ⇄ D : G with F ⊣ G.
; The composite T = GF is a monad on C.
; η = unit of adjunction, μ = GεF where ε is the counit.

; Example: free-forgetful adjunction F ⊣ U between Set and Monoid.
; T = UF: Set → Set is the list monad!
; η_X: X → UF(X), x ↦ [x].
; μ_X: UFUF(X) → UF(X) = flatten.

; Build mu from the adjunction rather than assuming it: the counit
; eps_{F X}: F(U(F X)) -> F X is the PRODUCT in the free monoid, whose
; operation is append and whose unit is the empty word. Then mu = U(eps),
; which must agree with the flatten defined above -- two independent routes.
(define (free-monoid-product wss) (foldr (lambda (u acc) (append u acc)) nil wss))
(check "adjunction-gives-list-monad"
  (every (lambda (wss) (equal (free-monoid-product wss) (list-flatten wss))) lm-nested))

; eta and mu from the adjunction satisfy the monad laws on the nose.
(check "adjunction-monad-satisfies-unit-laws"
  (every (lambda (xs)
           (and (equal (free-monoid-product (list-unit xs)) xs)
                (equal (free-monoid-product (map (lambda (x) (list-unit x)) xs)) xs)))
         lm-lists))
(check "adjunction-monad-satisfies-associativity"
  (every (lambda (xsss)
           (equal (free-monoid-product (map free-monoid-product xsss))
                  (free-monoid-product (free-monoid-product xsss))))
         lm-nested3))

; ---------- Another monad: the powerset monad ----------
(print "--- Powerset monad on Set ---")

; P(X) = set of subsets of X.
; η_X(x) = {x} (singleton)
; μ_X(S) = ∪_{s∈S} s (union of a set of subsets).

(define (powerset-monad-unit x) (list-to-set (list x)))

(define (powerset-monad-mult ss)
  (cond ((eq ss nil) {})
        (t (union (car ss) (powerset-monad-mult (cdr ss))))))

; Verify unit laws (for finite subsets of {1,2,3}):
(define test-set {1 2 3})

(define (pset-mult ss) (foldr (lambda (a b) (union a b)) {} (set-to-list ss)))
(define subsets-123 (set-to-list (powerset test-set)))

; mu . P(eta) = id, on every subset.
(check "powerset-monad-unit-law"
  (every (lambda (s) (equal (pset-mult (map (lambda (x) (powerset-monad-unit x)) s)) s))
         subsets-123))
; mu . eta_P = id, on every subset.
(check "powerset-monad-other-unit-law"
  (every (lambda (s) (equal (pset-mult (powerset-monad-unit s)) s)) subsets-123))
; The two definitions of the multiplication agree.
(check "powerset-mult-agrees-with-recursive-union"
  (every (lambda (s)
           (equal (pset-mult (powerset s)) (powerset-monad-mult (set-to-list (powerset s)))))
         subsets-123))

; Associativity mu . P(mu) = mu . mu_P, quantified over all families of at
; most two elements of P(P({1,2})).
(define x12 {1 2})
(define ppx (powerset (powerset x12)))
(define ppx-list (set-to-list ppx))
(define pppx-small
  (dedup (cons {}
               (append (map (lambda (a) (list-to-set (list a))) ppx-list)
                       (concat1 (map (lambda (a)
                                       (map (lambda (b) (list-to-set (list a b))) ppx-list))
                                     ppx-list))))))
(check "powerset-monad-associativity-domain-is-nontrivial"
  (= (length pppx-small) 137))
(check "powerset-monad-associativity"
  (every (lambda (ss) (equal (pset-mult (map (lambda (s) (pset-mult s)) ss))
                             (pset-mult (pset-mult ss))))
         pppx-small))

; For the "complete lattice" interpretation: P is also the monad
; for the adjunction between Set and the category of complete join-semilattices.

; ---------- Monad morphisms ----------
; A morphism of monads (T,η,μ) → (T',η',μ')
; is a natural transformation φ: T ⇒ T' respecting units and multiplications.

; A morphism of monads phi: List => P sends a list to its set of members.
; It must respect units and multiplications.
(define (list-to-pset xs) (list-to-set xs))
(check "powerset-monad-exists"
  (and (every (lambda (x) (equal (list-to-pset (list-unit x)) (powerset-monad-unit x)))
              lm-alph)
       (every (lambda (wss)
                (equal (list-to-pset (list-flatten wss))
                       (pset-mult (list-to-set (map (lambda (w) (list-to-pset w)) wss)))))
              lm-nested)))
; Negative control: reversing only the inner lists does not commute with μ:
; flatten ∘ T(reverse) ≠ reverse ∘ flatten.  (reverse is nevertheless a monad
; morphism List ⇒ List: the full condition φ∘μ = μ∘φT∘Tφ also reverses the
; outer list, and reverse(concat wss) = concat(reverse(map reverse wss)).)
(check "monad-morphism-test-rejects-reverse"
  (not (every (lambda (wss) (equal (list-flatten (map reverse wss))
                                   (reverse (list-flatten wss))))
              lm-nested)))

(print "=== Monads Done ===")
