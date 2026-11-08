; ============================================================
; Mac Lane — Categories, Functors, Natural Transformations
; Objects, morphisms, composition, identity.
; Examples: Set (finite), Grp (small), Vect_k (finite dim), Ring.
; ============================================================

(print "=== Mac Lane: Categories ===")

; ---------- Category: a finite poset ----------
; P = {a,b,c} with a ≤ b, b ≤ c, a ≤ c (plus identities).
; Morphisms: id_a, id_b, id_c, f:a→b, g:b→c, h:a→c

(define id-a (list (quote id_a) (quote a) (quote a)))
(define id-b (list (quote id_b) (quote b) (quote b)))
(define id-c (list (quote id_c) (quote c) (quote c)))
(define f-ab (list (quote f) (quote a) (quote b)))
(define g-bc (list (quote g) (quote b) (quote c)))
(define h-ac (list (quote h) (quote a) (quote c)))

(define ob-poset (list-to-set (list (quote a) (quote b) (quote c))))
(define mor-poset (list-to-set (list id-a id-b id-c f-ab g-bc h-ac)))

(define (dom-poset m) (nth 1 m))
(define (cod-poset m) (nth 2 m))

(define (comp-poset-full g f)
  (cond ((not (equal (dom-poset g) (cod-poset f))) nil)
        ((and (equal (car g) (quote g)) (equal (car f) (quote f)))
         h-ac)
        ((equal (car f) (quote id_a)) g)
        ((equal (car g) (quote id_b)) f)
        ((equal (car f) (quote id_b)) g)
        ((equal (car g) (quote id_c)) f)
        (t nil)))

; Identity axioms
(check "left-identity-fab"
  (equal (comp-poset-full f-ab id-a) f-ab))

(check "left-identity-gbc"
  (equal (comp-poset-full g-bc id-b) g-bc))

(check "right-identity-fab"
  (equal (comp-poset-full id-b f-ab) f-ab))

(check "right-identity-gbc"
  (equal (comp-poset-full id-c g-bc) g-bc))

; Associativity: g ∘ f = h
(check "poset-associativity"
  (equal (comp-poset-full g-bc f-ab) h-ac))

; ---------- Hom functor Hom(A, -) ----------
(define (hom-A-X A X)
  (filter (lambda (m)
    (and (equal (dom-poset m) A) (equal (cod-poset m) X)))
    mor-poset))

(check "hom-a-a-is-identity"
  (= (length (hom-A-X (quote a) (quote a))) 1))

(check "hom-a-b-has-one-morphism"
  (= (length (hom-A-X (quote a) (quote b))) 1))

; ---------- Composition, checked against the poset structure ----------
; In a poset category there is exactly one morphism x -> y when x <= y, so
; composition is forced. That gives an independent way to compute it, which
; must agree with the case analysis in comp-poset-full.
(define (the-mor x y) (car (set-to-list (hom-A-X x y))))
(define morl (set-to-list mor-poset))
(define (composable n m) (equal (dom-poset n) (cod-poset m)))

(check "composition-agrees-with-poset-structure"
  (every (lambda (m)
           (every (lambda (n)
                    (cond ((composable n m)
                           (equal (comp-poset-full n m)
                                  (the-mor (dom-poset m) (cod-poset n))))
                          (t t)))
                  morl))
         morl))

(check "composition-is-associative-on-all-composable-triples"
  (every (lambda (m)
           (every (lambda (n)
                    (every (lambda (k)
                             (cond ((and (composable n m) (composable k n))
                                    (equal (comp-poset-full k (comp-poset-full n m))
                                           (comp-poset-full (comp-poset-full k n) m)))
                                   (t t)))
                           morl))
                  morl))
         morl))

; Every morphism has the identity of its domain and codomain acting as units.
(check "identities-are-two-sided-units"
  (every (lambda (m)
           (and (equal (comp-poset-full m (the-mor (dom-poset m) (dom-poset m))) m)
                (equal (comp-poset-full (the-mor (cod-poset m) (cod-poset m)) m) m)))
         morl))

; ---------- Functor and natural transformation ----------
(define (const-functor-obj x) 0)
(define (const-functor-mor m) (list (quote const-id) 0 0))

; A functor must send identities to identities and respect composition.
(check "functor-preserves-identity"
  (every (lambda (x)
           (equal (const-functor-mor (the-mor x x))
                  (list (quote const-id) (const-functor-obj x) (const-functor-obj x))))
         (set-to-list ob-poset)))
(check "functor-preserves-composition"
  (every (lambda (m)
           (every (lambda (n)
                    (cond ((composable n m)
                           (equal (const-functor-mor (comp-poset-full n m))
                                  (const-functor-mor n)))
                          (t t)))
                  morl))
         morl))

; A poset has at most one morphism between any two objects, so natural
; transformations there are degenerate. For a real example take BG, the
; one-object category whose morphisms are G = Z/3. A functor BG -> Set is
; a G-set; a natural transformation is an equivariant map.
(define g3 (list 0 1 2))
(define (g-op a b) (mod (+ a b) 3))
(define (act-reg g x) (mod (+ g x) 3))          ; regular action on {0,1,2}

; Functoriality of the action: act(0) = id and act(g+h) = act(g) . act(h).
(check "action-is-a-functor"
  (and (every (lambda (x) (= (act-reg 0 x) x)) g3)
       (every (lambda (a)
                (every (lambda (b)
                         (every (lambda (x) (= (act-reg (g-op a b) x) (act-reg a (act-reg b x))))
                                g3))
                       g3))
              g3)))

; Enumerate every function {0,1,2} -> {0,1,2} and keep the equivariant ones.
(define (all-maps-3)
  (foldr append nil
         (map (lambda (u)
                (foldr append nil
                       (map (lambda (v) (map (lambda (w) (list u v w)) g3)) g3)))
              g3)))
(define (apply-map fn x) (nth x fn))
(define equivariant-maps
  (filter (lambda (fn)
            (every (lambda (a)
                     (every (lambda (x)
                              (= (apply-map fn (act-reg a x)) (act-reg a (apply-map fn x))))
                            g3))
                   g3))
          (all-maps-3)))

; Nat(regular, regular) = G, so there are exactly |G| = 3 of them.
(check "natural-transformation-exists" (= (length equivariant-maps) 3))
; Naturality is a genuine constraint: most maps are not equivariant.
(check "naturality-is-restrictive"
  (and (= (length (all-maps-3)) 27) (< (length equivariant-maps) 27)))
; Each equivariant map is right translation by its value at 0.
(check "equivariant-maps-are-translations"
  (every (lambda (fn)
           (every (lambda (x) (= (apply-map fn x) (mod (+ x (apply-map fn 0)) 3))) g3))
         equivariant-maps))

; ---------- Product category ----------
; Objects of P x D, where D is the discrete category on {0,1}.
(define disc2 (list-to-set (list 0 1)))
(define prod-objects (cartesian-product ob-poset disc2))
(check "product-category-6-objects" (= (length prod-objects) 6))
; Morphisms of the product are pairs (m, id_d).
(define prod-morphisms (cartesian-product mor-poset disc2))
(check "product-category-12-morphisms"
  (= (length prod-morphisms) (* (length mor-poset) (length disc2))))

(print "=== Categories Done ===")
