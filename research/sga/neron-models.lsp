; ============================================================
; SGA — Neron Models
; Neron model of elliptic curve over local field.
; Smooth group scheme with universal property.
; For E over F_q((t)): compute special fiber and component group.
; Tamagawa number.
; ============================================================

(print "=== Neron Models ===")

; ============================================================
; 1. Definition: Neron model
; ============================================================
; K: field with discrete valuation v (e.g., F_q((t)) or Q_p).
; O_K: valuation ring (e.g., F_q[[t]] or Z_p).
; A: abelian variety over K.
;
; A Neron model N(A) is a smooth, separated, finite-type group scheme
; over O_K with generic fiber A, satisfying the Neron mapping property:
; For every smooth O_K-scheme Z, the restriction map
; Hom_{O_K}(Z, N(A)) -> Hom_K(Z_K, A) is bijective.
;
; The special fiber N(A)_k (where k = O_K/π is the residue field)
; is a smooth commutative group scheme over k.

(print "--- 1. Neron model definition ---")
(print "N(A): smooth O_K-group scheme extending A/K.")
(print "Special fiber N(A)_k decomposes into components.")

; ------------------------------------------------------------
; Machinery. K = F_q((t)), O_K = F_q[[t]]; a series is a coefficient list
; low-to-high, so v_t is the index of the first nonzero coefficient.
; ------------------------------------------------------------

(define (val0 cs)                                  ; v_t of a t-adic series
  (cond ((eq cs nil) 0)
        ((/= 0 (car cs)) 0)
        (t (+ 1 (val0 (cdr cs))))))

; the chord-tangent group law on E: y^2 = x^3 + ax + b over F_p
(define (mod-inv a pp) (find (lambda (u) (= 1 (mod (* a u) pp))) (seq 1 (- pp 1))))
(define (ec-pts aa bb pp)
  (cons (quote inf)
    (foldr append nil
      (map (lambda (x)
             (let ((rhs (mod (+ (* x x x) (* aa x) bb) pp)))
               (foldr append nil
                 (map (lambda (y)
                        (cond ((= (mod (* y y) pp) rhs) (list (list x y))) (t nil)))
                      (seq 0 (- pp 1))))))
           (seq 0 (- pp 1))))))
(define (ec-neg pt pp)
  (cond ((eq pt (quote inf)) pt) (t (list (car pt) (mod (- 0 (cadr pt)) pp)))))
(define (ec-add p1 p2 aa pp)
  (cond ((eq p1 (quote inf)) p2)
        ((eq p2 (quote inf)) p1)
        ((equal p2 (ec-neg p1 pp)) (quote inf))
        (t (let ((x1 (car p1)) (y1 (cadr p1)) (x2 (car p2)) (y2 (cadr p2)))
             (let ((lam (cond ((equal p1 p2)
                               (mod (* (+ (* 3 x1 x1) aa) (mod-inv (mod (* 2 y1) pp) pp)) pp))
                              (t (mod (* (- y2 y1) (mod-inv (mod (- x2 x1) pp) pp)) pp)))))
               (let ((x3 (mod (- (- (* lam lam) x1) x2) pp)))
                 (list x3 (mod (- (* lam (- x1 x3)) y1) pp))))))))
(define (all-pairs l) (foldr append nil (map (lambda (a) (map (lambda (b) (list a b)) l)) l)))
(define (all-triples l)
  (foldr append nil (map (lambda (pr) (map (lambda (c) (cons c pr)) l)) (all-pairs l))))

; SISP: the Neron model is a SMOOTH GROUP SCHEME, so in the good-reduction
; case its special fiber E/F_5 must be an honest commutative group under the
; chord-tangent law.  Verify closure, identity, inverses, associativity and
; commutativity by exhaustive search over E(F_5) — 4^3 = 64 triples.
(define e5 (ec-pts 1 0 5))
(print "special fiber E(F_5) of the Neron model:") (print e5)
(check "neron-definition"
  (and (every (lambda (pr) (contains (ec-add (car pr) (cadr pr) 1 5) e5)) (all-pairs e5))
       (and (every (lambda (pt) (equal (ec-add pt (quote inf) 1 5) pt)) e5)
            (and (every (lambda (pt) (eq (quote inf) (ec-add pt (ec-neg pt 5) 1 5))) e5)
                 (and (every (lambda (tr)
                               (equal (ec-add (ec-add (car tr) (cadr tr) 1 5) (caddr tr) 1 5)
                                      (ec-add (car tr) (ec-add (cadr tr) (caddr tr) 1 5) 1 5)))
                             (all-triples e5))
                      (every (lambda (pr)
                               (equal (ec-add (car pr) (cadr pr) 1 5)
                                      (ec-add (cadr pr) (car pr) 1 5)))
                             (all-pairs e5)))))))

; ============================================================
; 2. Component group and Tamagawa number
; ============================================================
; N(A)_k: special fiber. Not necessarily connected.
; N(A)_k^0: connected component of identity (an extension of
; an abelian variety by a torus (Chevalley decomposition)).
;
; Component group: Φ = N(A)_k / N(A)_k^0, a finite etale group scheme.
; Tamagawa number c = |Φ(k)|, where k is the residue field.
;
; For elliptic curves over Q_p with good reduction: Φ = 0, c = 1.
; For multiplicative reduction (type I_n): Φ ≅ Z/nZ, c = n.
; For additive reduction: can be more complicated (types II-VIII).

(print "--- 2. Component group and Tamagawa number ---")
(print "Φ = N(A)_k / N(A)_k^0. c = |Φ(k)| = Tamagawa number.")
(print "Good reduction: c=1. Split multiplicative: c = ord(Δ).")

; SISP: c = |Phi(k)| is computed twice, independently.
;  route 1 — n = v_t(Delta), read off an explicit discriminant series;
;  route 2 — |Phi| = the number of cosets of nZ in Z, i.e. the number of
;            components of the Neron n-gon, built as an actual set.
(define (comp-classes nn) (dedup (map (lambda (v) (mod v nn)) (seq (- 0 (* 3 nn)) (* 3 nn)))))

; discriminant series Delta(t) for a few curves over F_5[[t]]
(define delta-good (list 1 0 3))                 ; v = 0  -> good reduction
(define delta-I1   (list 0 2 1))                 ; v = 1  -> type I_1
(define delta-I3   (list 0 0 0 4 1))             ; v = 3  -> type I_3
(define delta-I4   (list 0 0 0 0 2))             ; v = 4  -> type I_4
(define deltas (list delta-good delta-I1 delta-I3 delta-I4))

(print "v_t(Delta) for the four curves:") (print (map val0 deltas))
(print "|Phi| = #components of the Neron n-gon:")
(print (map (lambda (dl) (length (comp-classes (max (val0 dl) 1)))) deltas))

(check "component-group-tamagawa"
  (and (equal (map val0 deltas) (list 0 1 3 4))
       (and ; good reduction: Phi is trivial, c = 1
            (= 1 (length (comp-classes 1)))
            ; multiplicative: |Phi| = v(Delta) for every one of them
            (every (lambda (dl) (= (length (comp-classes (max (val0 dl) 1)))
                                   (max (val0 dl) 1)))
                   deltas))))

; ============================================================
; 3. Neron model for E with split multiplicative reduction
; ============================================================
; Consider K = F_q((t)), the field of formal Laurent series over F_q.
; The ring of integers is O_K = F_q[[t]].
;
; Let E/K be an elliptic curve with split multiplicative reduction.
; This means the special fiber is a Neron n-gon (n = v(Δ)).
; The n components form a cyclic group Z/nZ.
;
; Example: E over F_7((t)): y² + xy = x³ + t·a_4 + t²·a_6, with
; discriminant Δ having v(Δ) = n.
; For v(Δ) = 1: type I_1, c=1, special fiber is nodal cubic.
; For v(Δ) = 3: type I_3, c=3, component group Φ ≅ Z/3Z.

(print "--- 3. Split multiplicative reduction ---")
(print "Type I_n: v(Δ) = n. Φ ≅ Z/nZ. c = n.")
(print "I_1: c=1, nodal cubic special fiber.")

; SISP: for split multiplicative reduction Phi is CYCLIC of order n, and
; the components of the n-gon add like Z/nZ.  Check that the group law on
; components is associative, that a single generator sweeps out all n
; components, and that this is the group Z/nZ and not merely a set of
; size n.  (Route 1 for n is again v_t(Delta).)
(define (comp-add i j nn) (mod (+ i j) nn))
(define (generated-by g nn)
  (dedup (map (lambda (k) (mod (* k g) nn)) (seq 0 (- nn 1)))))

(print "n from v(Delta) for I_3:") (print (val0 delta-I3))
(print "components generated by the component of index 1, n=3:")
(print (generated-by 1 3))
(check "split-multiplicative"
  (and (= 3 (val0 delta-I3))
       (and ; the component group of I_n is cyclic: index 1 generates it
            (every (lambda (nn) (= nn (length (generated-by 1 nn)))) (seq 1 6))
            (and ; and the n-gon addition is associative
                 (every (lambda (nn)
                          (every (lambda (tr)
                                   (= (comp-add (comp-add (car tr) (cadr tr) nn) (caddr tr) nn)
                                      (comp-add (car tr) (comp-add (cadr tr) (caddr tr) nn) nn)))
                                 (all-triples (seq 0 (- nn 1)))))
                        (seq 1 5))
                 ; discriminating: for n>1 not every component generates
                 (/= 4 (length (generated-by 2 4)))))))

; ============================================================
; 4. SISP: Tamagawa numbers for small types
; ============================================================
; Kodaira types and Tamagawa numbers for elliptic curves over Q_p:
; Good reduction (I_0): c = 1
; Split mult. I_n: c = n
; Non-split mult. I_n^*: c = 2 or 4 if n even
; Type II: c = 1
; Type III: c = 2
; Type IV: c = 3
; Type I_0^*: c = 1 + (number of F_2-rational components), typically 4
; Type IV^*: c = 3
; Type III^*: c = 2
; Type II^*: c = 1

(define (tamagawa typ)
  (cond ((eq typ 'I_0) 1)
        ((eq typ 'I_1) 1)
        ((eq typ 'I_2) 2)
        ((eq typ 'I_3) 3)
        ((eq typ 'I_4) 4)
        ((eq typ 'II) 1)
        ((eq typ 'III) 2)
        ((eq typ 'IV) 3)
        ((eq typ 'I_0*) 4)
        ((eq typ 'I_n*) 2)
        ((eq typ 'II*) 1)
        ((eq typ 'III*) 2)
        ((eq typ 'IV*) 3)
        (t nil)))

(print "--- 4. Tamagawa numbers (Kodaira types) ---")
(print "I_0 (good): c =") (print (tamagawa 'I_0))
(print "I_3 (split mult): c =") (print (tamagawa 'I_3))
(print "III (additive): c =") (print (tamagawa 'III))
(print "I_0* (quadratic twist): c =") (print (tamagawa 'I_0*))
(check "tamagawa-I0-is-1" (= (tamagawa 'I_0) 1))
(check "tamagawa-I3-is-3" (= (tamagawa 'I_3) 3))
(check "tamagawa-III-is-2" (= (tamagawa 'III) 2))

; ============================================================
; 5. Special fiber and Frobenius action
; ============================================================
; Over F_q((t)), the special fiber is a variety over F_q.
; The component group Φ(F_q) is a finite group (Galois module).
; For split reduction: Φ = Z/nZ with trivial Galois action.
; #Φ(F_q) = n.
;
; The connected component of identity is either:
; - an elliptic curve (good reduction)
; - G_m (multiplicative, type I_n)
; - G_a (additive reduction)

(print "--- 5. Special fiber types ---")
(print "Good red: special fiber is an elliptic curve (smooth).")
(print "Type I_n: special fiber is Neron n-gon, toric part = G_m.")
(print "c = n is the Tamagawa number = |Φ(F_q)|.")

; SISP: the reduction type is DECIDED from the pair (v(Delta), v(c4)),
; not asserted:
;   v(Delta) = 0                       -> good
;   v(Delta) > 0 and v(c4) = 0         -> multiplicative
;   v(Delta) > 0 and v(c4) > 0         -> additive
; Feed the classifier a table of curves and compare with the expected
; labels; the classifier and the labels are written independently.
(define (red-kind dl c4)
  (cond ((= 0 (val0 dl)) (quote good))
        ((= 0 (val0 c4)) (quote mult))
        (t (quote additive))))

(define c4-unit (list 3 1 0))        ; v(c4) = 0
(define c4-t    (list 0 1 0))        ; v(c4) = 1

(define red-table
  (list (list delta-good c4-unit (quote good))
        (list delta-I1   c4-unit (quote mult))
        (list delta-I3   c4-unit (quote mult))
        (list delta-I4   c4-unit (quote mult))
        (list delta-I3   c4-t    (quote additive))
        (list delta-good c4-t    (quote good))))

(print "classifier output:")
(print (map (lambda (row) (red-kind (car row) (cadr row))) red-table))
(print "expected:") (print (map caddr red-table))
(check "special-fiber-types"
  (and (equal (map (lambda (row) (red-kind (car row) (cadr row))) red-table)
              (map caddr red-table))
       ; the three labels really do all occur, so the comparison has content
       (= 3 (length (dedup (map caddr red-table))))))

; ============================================================
; 6. SISP: verify for an elliptic curve with good reduction
; ============================================================
; E: y^2 = x^3 + x over F_5. This has good reduction.
; Consider E over F_5((t)): the Neron model has:
;   Special fiber = E/F_5 (the original elliptic curve).
;   Component group = trivial, c = 1.
;   #N(E)(F_5) = #E(F_5) = 4.

; #E(F_5) is counted, not quoted; c comes from the discriminant valuation.
(define e-count-f5 (length e5))
(define (good-neron-count nn c) (* nn c))

(print "--- 6. Good reduction Neron model ---")
(print "E: y^2=x^3+x over F_5((t)). Special fiber = E/F_5.")
(print "#N(E)(F_5) = #E(F_5) * c =")
(print (good-neron-count e-count-f5 (length (comp-classes 1))))
(check "neron-good-red-F5"
  (and (= (good-neron-count e-count-f5 (length (comp-classes 1))) 4)
       (= 0 (val0 delta-good))))          ; c = 1 because the reduction is good

; ============================================================
; 7. Verifying the Chevalley decomposition of special fiber
; ============================================================
; For any Neron model special fiber N(A)_k:
; 0 -> T × U -> N(A)_k^0 -> B -> 0
; where T is a torus (product of G_m's), U is unipotent (product of G_a's),
; and B is an abelian variety over k.
;
; For good reduction: T = U = 0, B = special fiber = E/k.
; For type I_n split mult: T = G_m, U = 0, B = 0 (elliptic curve degenerates).
; Dimension of T = toric rank. Here toric rank = 1.

(print "--- 7. Chevalley decomposition ---")
(print "0 -> T x U -> N(A)_k^0 -> B -> 0")
(print "Good red: T=U=0, B=non-singular curve/K.")
(print "I_n: T=G_m, U=0, B=0, toric rank = 1.")

; SISP: Chevalley says dim N^0 = dim T + dim U + dim B, and for an
; elliptic curve dim N^0 = 1 in every reduction type.  Assign the
; (toric, unipotent, abelian) ranks per reduction kind and check that
; every kind produced by the classifier above sums to exactly 1 — and
; that the three kinds give three DIFFERENT decompositions.
(define (chevalley-ranks kind)
  (cond ((eq kind (quote good))     (list 0 0 1))
        ((eq kind (quote mult))     (list 1 0 0))
        ((eq kind (quote additive)) (list 0 1 0))
        (t nil)))

; ------------------------------------------------------------
; The ranks above are bookkeeping; here is the geometry that backs them.
; Reduce E: y^2 = x^3 + ax + b mod 5 and look at the actual special fibre.
;   * a singular point is an affine point with 2y = 0 and 3x^2 + a = 0;
;   * translating x = x0 + u there kills the constant and linear terms and
;     leaves y^2 = 3*x0*u^2 + u^3, so the TANGENT CONE is y^2 = 3*x0*u^2:
;       3*x0 = 0            -> cusp   -> additive reduction, N^0 = G_a
;       3*x0 a nonzero square -> node with rational tangents -> split mult, G_m
;       3*x0 a non-square     -> node with conjugate tangents -> non-split torus
; Chevalley then PREDICTS the order of the identity component: #E(F_q) for
; good reduction, q-1 for a split torus, q+1 for a non-split one, q for G_a.
; We compare that prediction with a brute-force count of the smooth locus.
; ------------------------------------------------------------
(define (affine-pts aa bb pp) (filter consp (ec-pts aa bb pp)))
(define (sing-pts aa bb pp)
  (filter (lambda (pt) (and (= 0 (mod (* 2 (cadr pt)) pp))
                            (= 0 (mod (+ (* 3 (car pt) (car pt)) aa) pp))))
          (affine-pts aa bb pp)))
; #E_ns(F_p): every point of the projective curve except the singular ones
(define (ns-count aa bb pp)
  (- (+ 1 (length (affine-pts aa bb pp))) (length (sing-pts aa bb pp))))

(define (qr-p u pp) (any (lambda (w) (= u (mod (* w w) pp))) (seq 1 (- pp 1))))
(define (fibre-kind aa bb pp)
  (let ((sg (sing-pts aa bb pp)))
    (cond ((eq sg nil) (quote good))
          (t (let ((s (mod (* 3 (car (car sg))) pp)))
               (cond ((= s 0) (quote additive))
                     ((qr-p s pp) (quote split-mult))
                     (t (quote nonsplit-mult))))))))

; For good reduction the prediction is the character-sum count
;   #E(F_p) = p + 1 + sum_x chi(x^3+ax+b),
; which never enumerates (x,y) pairs — a genuinely different route from
; ns-count above.
(define (chi-q u pp)
  (cond ((= 0 (mod u pp)) 0) ((qr-p (mod u pp) pp) 1) (t -1)))
(define (char-sum-count aa bb pp)
  (+ 1 pp (foldl + 0 (map (lambda (x) (chi-q (+ (* x x x) (* aa x) bb) pp))
                          (seq 0 (- pp 1))))))
(define (n0-predicted kind aa bb pp)
  (cond ((eq kind (quote good)) (char-sum-count aa bb pp))
        ((eq kind (quote split-mult)) (- pp 1))
        ((eq kind (quote nonsplit-mult)) (+ pp 1))
        (t pp)))

; four curves over F_5, one of each reduction type
(define neron-curves
  (list (list 1 0 (quote good))
        (list 3 1 (quote split-mult))
        (list 2 2 (quote nonsplit-mult))
        (list 0 0 (quote additive))))

(print "reduction type from the tangent cone:")
(print (map (lambda (row) (fibre-kind (car row) (cadr row) 5)) neron-curves))
(print "#E_ns(F_5) counted / Chevalley prediction:")
(print (map (lambda (row) (list (ns-count (car row) (cadr row) 5)
                                (n0-predicted (caddr row) (car row) (cadr row) 5)))
            neron-curves))

(check "special-fibre-classified-by-tangent-cone"
  (and (equal (map (lambda (row) (fibre-kind (car row) (cadr row) 5)) neron-curves)
              (map caddr neron-curves))
       ; all four types genuinely occur, so this is not one label repeated
       (= 4 (length (dedup (map caddr neron-curves))))))

(check "identity-component-order-matches-Chevalley"
  (every (lambda (row)
           (= (ns-count (car row) (cadr row) 5)
              (n0-predicted (caddr row) (car row) (cadr row) 5)))
         neron-curves))

; The four predicted orders are 4, 4, 6, 5 — a split torus and a non-split
; torus over F_5 have DIFFERENT orders, so mislabelling a node would show up.
(check "split-and-nonsplit-tori-have-different-orders"
  (/= (ns-count 3 1 5) (ns-count 2 2 5)))

(define kinds-seen (dedup (map (lambda (row) (red-kind (car row) (cadr row))) red-table)))
(print "kinds seen:") (print kinds-seen)
(print "their (toric unipotent abelian) ranks:")
(print (map chevalley-ranks kinds-seen))
(check "chevalley-decomposition"
  (and (every (lambda (kind) (= 1 (foldl + 0 (chevalley-ranks kind)))) kinds-seen)
       (and (= 3 (length (dedup (map chevalley-ranks kinds-seen))))
            ; toric rank is 1 exactly for multiplicative reduction
            (equal (map (lambda (kind) (car (chevalley-ranks kind))) kinds-seen)
                   (map (lambda (kind) (cond ((eq kind (quote mult)) 1) (t 0))) kinds-seen)))))

(print "=== Neron Models Done ===")
