; ============================================================
; Bourbaki — Exterior Algebra Λ^k(M) for Finite R-Modules
; Alternating property: v∧v=0. Dimension C(n,k).
; Compute Λ²(Z/2Z³) and others.
; ============================================================

(print "=== Bourbaki: Exterior Algebra ===")

; ---------- ring operations ----------
(define (rmod n a) (mod a n))
(define (radd n a b) (mod (+ a b) n))
(define (rmul n a b) (mod (* a b) n))

; ---------- binomial coefficient C(n,k) ----------
(define (binomial n k)
  (cond ((> k n) 0)
        ((= k 0) 1)
        ((> k (/ n 2)) (binomial n (- n k)))
        (t (/ (* n (binomial (- n 1) (- k 1))) k))))

; ---------- free module R^n elements as lists of length n ----------
(define (free-module-elements n-mod rank)
  (labels (((prod-lists k)
    (cond ((= k 0) (list nil))
          ; NB: flatmap DEEP-flattens in SISP, which shreds the inner
          ; vectors; concatenate exactly one level with foldr/append.
          (t (let ((sub (prod-lists (- k 1))))
               (foldr append nil
                 (map (lambda (s)
                        (map (lambda (x) (cons x s)) (seq 0 (- n-mod 1))))
                      sub)))))))
    (prod-lists rank)))

; ---------- alternating k-form test: returns nil if any repeated ----------
; v = list of k vectors (each vector is a list of length rank)
; Λ^k(M): an alternating k-form sends (v_1,...,v_k) to 0 if any v_i = v_j.
; We work with the exterior power via antisymmetric tensors.
; For finite modules, we construct Λ^k(R^n) as a free module whose basis
; are k-element subsets of {1,...,n} (wedge of basis vectors).

; ---------- k-subsets of {0,...,n-1} as lists ----------
(define (k-subsets-list n k)
  ; `left` counts down the number of indices still to choose; testing the
  ; outer `k` would test a constant and never terminate the choice.
  (labels (((go start left current)
    (cond ((= left 0) (list (reverse current)))
          ((> start (- n 1)) nil)
          (t (append (go (+ start 1) (- left 1) (cons start current))
                     (go (+ start 1) left current))))))
    (go 0 k nil)))

; ---------- exterior power Λ^k(R^n) as module ----------
; Basis: e_{i1} ∧ ... ∧ e_{ik} for 1 ≤ i1 < ... < ik ≤ n.
; Size = C(n,k) for rank n. Over R=Z/mZ, each basis element generates
; a cyclic submodule Z/mZ, so |Λ^k(R^n)| = m^{C(n,k)}.
(define (exterior-power-size m n k)
  (labels (((power b e)
    (cond ((= e 0) 1)
          (t (* b (power b (- e 1)))))))
    (power m (binomial n k))))

; ---------- alternating property: v∧v = 0 ----------
; For any v in R^n, v∧v = 0 in Λ²(R^n).
; Represent v as a list [a_0,...,a_{n-1}].
; Then v∧v = Σ_{i<j} (a_i*a_j - a_j*a_i) e_i∧e_j = 0.
(define (wedge-same-cancels m n v)
  ; Compute all coefficients of v∧v in terms of (i,j) basis.
  ; coefficient of e_i∧e_j = a_i*a_j - a_j*a_i = 0 mod m (trivially).
  (labels (((coeff i j)
             (rmod m (- (* (nth i v) (nth j v))
                        (* (nth j v) (nth i v)))))
           ((check-pairs i j)
             (cond ((>= i (- n 1)) t)
                   ((>= j n) (check-pairs (+ i 1) (+ i 2)))
                   ((= (coeff i j) 0) (check-pairs i (+ j 1)))
                   (t nil))))
    (check-pairs 0 1)))

; ---------- wedge product of two vectors in Λ¹(R^n) = R^n ----------
; v∧w coefficient of e_i∧e_j = v_i*w_j - v_j*w_i
(define (wedge-2 m n v w)
  (labels (((coeff i j)
    (rmod m (- (* (nth i v) (nth j w))
               (* (nth j v) (nth i w))))))
    (labels (((collect i j acc)
      (cond ((>= i (- n 1)) (reverse acc))
            ((>= j n) (collect (+ i 1) (+ i 2) acc))
            (t (collect i (+ j 1)
                 (cons (list i j (coeff i j)) acc))))))
      (collect 0 1 nil))))

; ---------- verify Λ^k dimension for small cases ----------
; Λ⁰(R^n) = R (size m)
; Λ¹(R^n) = R^n (size m^n)
; Λ^n(R^n) = R (size m)
; Λ^k(R^n) = 0 for k > n.

; ---------- check Λ²(Z/2Z³) ----------
(define (exterior-square-Z2-rank3)
  ; Λ²(Z/2Z³) has basis {e0∧e1, e0∧e2, e1∧e2} = 3 basis elements.
  ; Over Z/2Z, this is a 3-dimensional vector space, size 2³=8.
  (exterior-power-size 2 3 2))

; ---------- compute the full exterior algebra ⊕ Λ^k ----------
(define (exterior-algebra-size m n)
  (labels (((sum-sizes k acc)
    (cond ((> k n) acc)
          (t (sum-sizes (+ k 1) (+ acc (exterior-power-size m n k)))))))
    (sum-sizes 0 0)))

; ========== Z/2Z EXTERIOR ALGEBRA ==========
(print "--- Λ(Z/2Z³) ---")

(define m2 2)
(define n3 3)

; --- Binom checks ---
(check "C3-0-equals-1" (= (binomial 3 0) 1))
(check "C3-1-equals-3" (= (binomial 3 1) 3))
(check "C3-2-equals-3" (= (binomial 3 2) 3))
(check "C3-3-equals-1" (= (binomial 3 3) 1))

; --- Λ⁰(Z/2Z³) size: Λ⁰(R^n) = R, so |Λ⁰| = m = 2 (NOT 1) ---
(define lam0 (exterior-power-size m2 n3 0))
(check "ext-0-size-is-m" (= lam0 2))

; --- Λ¹(Z/2Z³) = Z/2Z³, size 8 ---
(define lam1 (exterior-power-size m2 n3 1))
(check "ext-1-size-is-8" (= lam1 8))

; --- Λ²(Z/2Z³) size: 2^C(3,2) = 2^3 = 8 ---
(define lam2 (exterior-power-size m2 n3 2))
(check "ext-2-size-is-8" (= lam2 8))

; --- Λ³(Z/2Z³) size: 2^C(3,3) = 2^1 = 2 ---
(define lam3 (exterior-power-size m2 n3 3))
(check "ext-3-size-is-2" (= lam3 2))

; --- Λ⁴(Z/2Z³) = 0, the zero module: exactly 1 element ---
(check "ext-4-is-zero-module-size-1" (= (exterior-power-size m2 n3 4) 1))

; --- Alternating property: v∧v=0 ---
; Test all 8 vectors in Z/2Z³
(define r3-free (free-module-elements m2 n3))
(check "r3-has-8-elements" (= (length r3-free) 8))

(check "wedge-2-all-v-v-zero"
  (every (lambda (v) (wedge-same-cancels m2 n3 v)) r3-free))

; --- Wedge product example ---
; v = (1,0,1) = e0+e2, w = (0,1,1) = e1+e2
; v∧w = e0∧e1 + e0∧e2 + e2∧e1 + e2∧e2
;     = e0∧e1 + e0∧e2 - e1∧e2 + 0
;     = e0∧e1 + e0∧e2 - e1∧e2 mod 2
;     = e0∧e1 + e0∧e2 + e1∧e2 (since -1=1 mod 2)
(define v-test (list 1 0 1))
(define w-test (list 0 1 1))
(define w2-result (wedge-2 m2 n3 v-test w-test))
(print (list (quote wedge-v-w) w2-result))

; Verify: coefficient of (0,1) = v0*w1 - v1*w0 = 1*1 - 0*0 = 1. Check!
(check "wedge-v-w-01-is-1"
  (= (caddr (find (lambda (p) (and (= (car p) 0) (= (cadr p) 1))) w2-result)) 1))

; --- Dimension checks for Λ^k(Z/2Z⁴) ---
(print "--- Λ(Z/2Z⁴) ---")
(check "ext-0-size-Z2-4" (= (exterior-power-size 2 4 0) 2))  ; Λ⁰ = R = Z/2Z
(check "ext-1-size-Z2-4" (= (exterior-power-size 2 4 1) 16))
(check "ext-2-size-Z2-4" (= (exterior-power-size 2 4 2) 64))
(check "ext-3-size-Z2-4" (= (exterior-power-size 2 4 3) 16))
(check "ext-4-size-Z2-4" (= (exterior-power-size 2 4 4) 2))

; ========== Z/3Z EXTERIOR ALGEBRA ==========
(print "--- Λ(Z/3Z²) ---")

(define m3 3)
(define n2 2)

(check "ext-0-Z3-rank2" (= (exterior-power-size m3 n2 0) 3))  ; Λ⁰ = R = Z/3Z
(check "ext-1-Z3-rank2" (= (exterior-power-size m3 n2 1) 9))
(check "ext-2-Z3-rank2" (= (exterior-power-size m3 n2 2) 3))

; Alternating property for Z/3Z²
(define r2-free-z3 (free-module-elements m3 n2))
(check "z3-2-all-v-wedge-v-zero"
  (every (lambda (v) (wedge-same-cancels m3 n2 v)) r2-free-z3))

; --- Wedge product of basis vectors ---
; e0 = (1,0), e1 = (0,1)
(define e0 (list 1 0))
(define e1 (list 0 1))
(check "e0-wedge-e0-zero"
  (wedge-same-cancels m3 n2 e0))
(check "e1-wedge-e1-zero"
  (wedge-same-cancels m3 n2 e1))

(define w-e0-e1 (wedge-2 m3 n2 e0 e1))
; (0,1) coefficient = v0*w1 - v1*w0 = 1*1 - 0*0 = 1
(check "e0-wedge-e1-01-coeff-is-1"
  (= (caddr (car w-e0-e1)) 1))

; --- Antisymmetry: e1∧e0 = -e0∧e1 ---
(define w-e1-e0 (wedge-2 m3 n2 e1 e0))
(check "e1-wedge-e0-01-coeff-is-2-mod3"
  (= (rmod 3 (caddr (car w-e1-e0))) 2))  ; -1 ≡ 2 mod 3

; ========== Z/6Z EXTERIOR ALGEBRA ==========
(print "--- Λ(Z/6Z²) ---")

(define m6-val 6)

(check "ext-0-Z6-rank2" (= (exterior-power-size m6-val 2 0) 6))  ; Λ⁰ = R = Z/6Z
(check "ext-1-Z6-rank2" (= (exterior-power-size m6-val 2 1) 36))
(check "ext-2-Z6-rank2" (= (exterior-power-size m6-val 2 2) 6))

; Alternating property for Z/6Z²
(define r2-free-z6 (free-module-elements m6-val 2))
(check "z6-2-all-v-wedge-v-zero"
  (every (lambda (v) (wedge-same-cancels m6-val 2 v)) r2-free-z6))

; ========== RANK OF Λ^k IS C(n,k) ==========
; The basis of Λ^k(R^n) is indexed by the k-element subsets of {0,...,n-1},
; so rank Λ^k(R^n) = C(n,k) exactly.
(check "rank-lambda2-R3-is-C-3-2"
  (= (length (k-subsets-list 3 2)) (binomial 3 2)))
(check "rank-lambda2-R5-is-C-5-2"
  (= (length (k-subsets-list 5 2)) (binomial 5 2)))
(check "rank-lambda3-R5-is-C-5-3"
  (= (length (k-subsets-list 5 3)) (binomial 5 3)))
(check "lambda2-R3-basis-is-e01-e02-e12"
  (equal (k-subsets-list 3 2) (list (list 0 1) (list 0 2) (list 1 2))))
(check "rank-lambda0-is-1" (= (length (k-subsets-list 4 0)) 1))
(check "lambda-k-vanishes-for-k-gt-n" (equal (k-subsets-list 3 4) nil))

; Λ²(Z/2Z³) is 3-dimensional over Z/2Z, hence has 2³ = 8 elements.
(check "exterior-square-Z2-rank3-is-8" (= (exterior-square-Z2-rank3) 8))
; |Λ(Z/2Z³)| summed over k: 2^1 + 2^3 + 2^3 + 2^1 = 20
(check "exterior-algebra-Z2-rank3-total-is-20"
  (= (exterior-algebra-size 2 3) 20))

; --- Binomial checks ---
(check "C5-2-equals-10" (= (binomial 5 2) 10))
(check "C4-2-equals-6" (= (binomial 4 2) 6))
(check "C5-3-equals-10" (= (binomial 5 3) 10))

(print "=== Exterior Algebra Done ===")
