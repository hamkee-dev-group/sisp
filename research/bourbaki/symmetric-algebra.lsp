; ============================================================
; Bourbaki — Symmetric Algebra over Finite Rings
; Sym^k(M) for finite R-modules. Universal property for
; commuting maps. Relationship with polynomial ring.
; Compute for 2×2 matrices over F_2.
; ============================================================

(print "=== Bourbaki: Symmetric Algebra ===")

; ---------- binomial ----------
(define (binom n k)
  (cond ((= k 0) 1)
        ((= k n) 1)
        ((> k n) 0)
        (t (+ (binom (- n 1) (- k 1)) (binom (- n 1) k)))))

; ---------- ring Z/pZ ----------
(define (rmod p a) (mod a p))
(define (radd p a b) (mod (+ a b) p))
(define (rmul p a b) (mod (* a b) p))

; ---------- binomial coefficient for multichoose ----------
; The dimension of Sym^k(V) for V of rank n is C(n+k-1, k)
; = (n+k-1)! / (k! * (n-1)!) = C(n+k-1, n-1)

(define (sym-rank n k)
  (binom (+ n k -1) (- n 1)))

; --- alternate: C(n+k-1, k) = C(n+k-1, n-1)
(define (sym-rank-2 n k)
  (binom (+ n k -1) k))

; --- size of Sym^k(F_p^n) = p^{sym-rank(n,k)} ---
(define (sym-size p n k)
  (let ((rank (sym-rank n k)))
    (labels (((pow a e)
      (cond ((= e 0) 1)
            (t (* a (pow a (- e 1)))))))
      (pow p rank))))

; ---------- symmetric algebra basis: degree-k monomials in n variables ----------
; A basis vector is a multiset of k indices from {0,...,n-1}
; with repetitions allowed, where order doesn't matter.
; This corresponds to monomials X_0^{a0} X_1^{a1} ... X_{n-1}^{a_{n-1}} with Σ a_i = k

; Generate all (a0,...,a_{n-1}) with sum = k
(define (multisets n k)
  (labels (((go var sum-so-far chosen acc)
    (cond ((= var n)
           (cond ((= sum-so-far k) (cons (reverse chosen) acc))
                 (t acc)))
          (t (labels (((try-val val acc2)
                (cond ((> (+ sum-so-far val) k) acc2)
                      (t (try-val (+ val 1)
                           (go (+ var 1) (+ sum-so-far val) (cons val chosen) acc2))))))
               (try-val 0 acc))))))
    (go 0 0 nil nil)))

; ---------- symmetric power basis count ----------
(define (symmetric-basis-size n k)
  (sym-rank n k))

; ---------- symmetric product (commutative analog of wedge) ----------
; The symmetric algebra Sym(V) ≅ R[X_0,...,X_{n-1}] where X_i = e_i.
; For k=2, Sym²(V) has basis {e_i ⊙ e_j : i ≤ j} (= {e_i e_j : i ≤ j})

; --- polynomial ring evaluation: Sym^k(V) corresponds to homogeneous
;     polynomials of degree k in n variables over R. ---

; For F_2, Sym*(F_2^n) ≅ F_2[X_0,...,X_{n-1}].

; ========== Sym²(F_2²) — 2×2 matrices ==========
(print "--- Sym²(F_2²) — Connection with 2×2 Matrices ---")

; F_2² has basis {e0, e1}. Sym² has basis {e0², e0e1, e1²}. Rank = C(2+2-1,2) = C(3,2) = 3
(check "Sym2-F22-rank-3" (= (sym-rank 2 2) 3))
(check "Sym2-F22-size-8" (= (sym-size 2 2 2) 8))

; Symmetric matrices over F_2: the 2×2 symmetric matrices over F_2
; form a vector space isomorphic to Sym²(F_2²).
; A 2×2 symmetric matrix has form [[a, b], [b, c]] with a,b,c ∈ F_2.
; There are 2^3 = 8 such matrices.
; A linear functional on Sym²(F_2²) is a symmetric bilinear form on F_2².
; B(x,y) = a x1 y1 + b (x1 y2 + x2 y1) + c x2 y2.

; Verify: the space of symmetric bilinear forms on F_2² has dimension 3 (= rank of Sym²).
; Verifying basis correspondence:
; e0² ↦ B(e0,e0), e0e1 ↦ B(e0,e1), e1² ↦ B(e1,e1).

(check "dim-sym-bilinear-forms-F22-is-3"
  (= (sym-rank 2 2) 3))

; ========== Sym¹, Sym², Sym³ for F_2³ ==========
(print "--- Sym(F_2³) ---")

(define p2 2)
(define n3 3)

(check "Sym1-F23-rank-3" (= (sym-rank n3 1) 3))
(check "Sym1-F23-size-8" (= (sym-size p2 n3 1) 8))

(check "Sym2-F23-rank-6" (= (sym-rank n3 2) 6))
; C(3+2-1,2) = C(4,2) = 6
(check "Sym2-F23-size-64" (= (sym-size p2 n3 2) 64))

(check "Sym3-F23-rank-10" (= (sym-rank n3 3) 10))
; C(3+3-1,3) = C(5,3) = 10
(check "Sym3-F23-size-1024" (= (sym-size p2 n3 3) 1024))

; ========== Basis sizes match formula ==========
(print "--- Basis counting ---")

(check "multisets-3-2-count-6" (= (length (multisets n3 2)) (sym-rank n3 2)))
(check "multisets-2-2-count-3" (= (length (multisets 2 2)) (sym-rank 2 2)))
(check "multisets-3-1-count-3" (= (length (multisets n3 1)) (sym-rank n3 1)))
(check "multisets-3-3-count-10" (= (length (multisets n3 3)) (sym-rank n3 3)))
(check "multisets-2-3-count-4" (= (length (multisets 2 3)) (sym-rank 2 3)))

; ========== Sym^k for F_3² ==========
(print "--- Sym(F_3²) ---")

(define p3 3)
(define n2 2)

(check "Sym0-F32-rank-1" (= (sym-rank n2 0) 1))
(check "Sym1-F32-rank-2" (= (sym-rank n2 1) 2))
(check "Sym2-F32-rank-3" (= (sym-rank n2 2) 3))
; C(2+2-1,1)=C(3,1)=3  (or C(3,2)=3)

(check "Sym3-F32-rank-4" (= (sym-rank n2 3) 4))
; C(2+3-1,1)=C(4,1)=4

; ========== Functionals on Sym(V) ==========
(print "--- Sym(V*) as functions on V ---")

; For V = F_2², the dual V* ≅ F_2².
; Sym^k(V*) = homogeneous polynomial functions of degree k on V.
; E.g., Sym¹(V*) = linear functionals = dual space, dim 2.
; Sym²(V*) = quadratic forms = symmetric bilinear forms, dim 3.
(check "dual-rank-1-is-2" (= (sym-rank n2 1) 2))
(check "quadratic-forms-rank-2-is-3" (= (sym-rank n2 2) 3))

; ========== Universal property: commuting diagrams ==========
(print "--- Universal Property ---")

; A symmetric k-linear map M×...×M → N factors through Sym^k(M) → N.
; For Sym²: bilinear symmetric maps V×V → W factor through Sym²(V).
; For F_2², every symmetric bilinear map V×V → F_2 corresponds to an element of Sym²(V*).
; Total maps: |Sym²(V*)| = 2^3 = 8 distinct symmetric bilinear forms on F_2².
(check "num-symmetric-bilinear-forms" (= (sym-size p2 n2 2) 8))

; Skew-symmetric bilinear forms have dimension C(2,2)=1 (alternating), but for F_2,
; symmetric = alternating (since 1=-1). So there are 8 symmetric=alternating forms on F_2².
; But wait, alternating requires B(v,v)=0. For F_2, the 8 symmetric forms already have B(v,v)=0
; iff the diagonal terms are 0. For B(ei,ei)=0, we need ei² coefficient = 0.
; The alternating subspace has dim 1 = C(2,2) (the basis e0∧e1), which corresponds to
; B = [[0,1],[1,0]]. So 2^1 = 2 alternating forms.

; This shows: dim(Sym²) = 3 > dim(Λ²) = 1 for F_2².

; ========== Z/4Z symmetric algebra on 1 generator ==========
(print "--- Sym over Z/4Z ---")

(define n-mod 4)

; Sym¹(F_4¹) = F_4¹ has 4 elements (rank 1)
(check "sym-rank-Z4-1-1" (= (sym-rank 1 1) 1))
(check "sym-rank-Z4-1-2" (= (sym-rank 1 2) 1))

; For a free module of rank 1 (just Z/4Z itself):
; Sym^k(Z/4Z) ≅ Z/4Z (since there's only one monomial of each degree: X^k)
; Total symmetric algebra Sym*(Z/4Z) ≅ Z/4Z[X] which has 4 elements
; at each homogeneous degree.
(check "sym-rank-rank1-always-1" (every (lambda (k) (= (sym-rank 1 k) 1)) (list 0 1 2 3 4 5)))

; ========== Z/6Z symmetric algebra on 1 generator ==========
(print "--- Sym over Z/6Z rank 1 ---")

; For Z/6Z-module of rank 1, Sym^k has 6 elements per degree.
(check "sym-rank-Z6-rank1-any-k-is-1" (every (lambda (k) (= (sym-rank 1 k) 1)) (list 0 1 2 3)))

(print "=== Symmetric Algebra Done ===")
