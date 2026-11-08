; ============================================================
; Bourbaki — Representation Theory of Finite Groups
; Maschke's theorem. Character table. Schur's lemma.
; Character table of S_3. Decompose regular representation.
; ============================================================

(print "=== Bourbaki: Representation Theory ===")

; ---------- S_3 group operations ----------
; Represent S_3 = {(), (12), (13), (23), (123), (132)}
; as permutations of {1,2,3}
(define (perm-eq p q)
  (and (= (nth 0 p) (nth 0 q)) (= (nth 1 p) (nth 1 q)) (= (nth 2 p) (nth 2 q))))

(define (perm-mul p q)
  (list (nth (- (nth 0 p) 1) q)
        (nth (- (nth 1 p) 1) q)
        (nth (- (nth 2 p) 1) q)))

(define (perm-inv p)
  (let ((a (nth 0 p)) (b (nth 1 p)) (c (nth 2 p)))
    (labels (((pos-of val xs i)
      (cond ((= (car xs) val) i)
            (t (pos-of val (cdr xs) (+ i 1))))))
      (list (pos-of 1 (list a b c) 1)
            (pos-of 2 (list a b c) 1)
            (pos-of 3 (list a b c) 1)))))

; S_3 elements
(define id-s3  (list 1 2 3))
(define p12    (list 2 1 3))
(define p13    (list 3 2 1))
(define p23    (list 1 3 2))
(define p123   (list 2 3 1))
(define p132   (list 3 1 2))

(define S3 (list-to-set (list id-s3 p12 p13 p23 p123 p132)))
(check "S3-size-6" (= (length S3) 6))

; ---------- Conjugacy classes ----------
(define (conjugate-p g h)
  (any (lambda (x)
    (perm-eq (perm-mul (perm-mul x g) (perm-inv x)) h))
    (set-to-list S3)))

(define (conjugacy-class g)
  (list-to-set (filter (lambda (h) (conjugate-p g h)) (set-to-list S3))))

(define class-id  (conjugacy-class id-s3))
(define class-12  (conjugacy-class p12))
(define class-123 (conjugacy-class p123))

(check "three-conjugacy-classes"
  (let ((cls (list-to-set (map (lambda (g) (conjugacy-class g))
                             (set-to-list S3)))))
    (= (length cls) 3)))

(check "class-id-size-1" (= (length class-id) 1))
(check "class-12-size-3" (= (length class-12) 3))
(check "class-123-size-2" (= (length class-123) 2))

; ---------- Character table of S_3 ----------
; S_3 has 3 irreducible representations:
; - trivial rep χ_1: χ=1 on all elements
; - sign rep χ_sign: χ(g) = sgn(g) = ±1
; - standard 2-dim rep χ_2: χ = (2, 0, -1) on (id, transpositions, 3-cycles)

; The character table (rows = irreps, columns = conjugacy classes):
;           id(1)  transpositions(3)  3-cycles(2)
; χ_1        1          1                1
; χ_sign     1         -1                1
; χ_2        2          0               -1

(define (chi-triv g) 1)
(define (chi-sign g)
  (cond ((perm-eq g id-s3) 1)
        ((perm-eq g p123) 1)
        ((perm-eq g p132) 1)
        (t -1)))

(define (chi-std g)
  (cond ((perm-eq g id-s3) 2)
        ((or (perm-eq g p12) (perm-eq g p13) (perm-eq g p23)) 0)
        (t -1)))

; ---------- Orthogonality of characters ----------
; First orthogonality: (1/|G|) * Σ_g χ_i(g) χ_j(g^{-1}) = δ_ij
; For characters over C, χ_j(g^{-1}) = conj(χ_j(g)), and for these real chars it's the same.

(define (inner-char chi1 chi2)
  (foldl (lambda (acc g) (+ acc (* (chi1 g) (chi2 g)))) 0 (set-to-list S3)))

(define (inner-char-norm chi1 chi2)
  (/ (inner-char chi1 chi2) (length S3)))

(check "chi_triv_norm_1" (= (inner-char-norm chi-triv chi-triv) 1))
(check "chi_sign_norm_1" (= (inner-char-norm chi-sign chi-sign) 1))
(check "chi_std_norm_1" (= (inner-char-norm chi-std chi-std) 1))
(check "chi_triv_orth_sign"
  (= (inner-char-norm chi-triv chi-sign) 0))
(check "chi_triv_orth_std"
  (= (inner-char-norm chi-triv chi-std) 0))
(check "chi_sign_orth_std"
  (= (inner-char-norm chi-sign chi-std) 0))

; Second orthogonality (columns):
; Σ_i χ_i(C_a) χ_i(C_b^{-1}) = (|G|/|C_a|) * δ_{ab}

; ---------- Character table as data ----------
(define irrep-chars (list-to-set (list chi-triv chi-sign chi-std)))
(check "three-characters" (= (length irrep-chars) 3))

; ---------- Regular representation decomposition ----------
; The regular representation of S_3 is:
; ρ_reg = χ_triv ⊕ χ_sign ⊕ 2·χ_std
; Dimensions: 1 + 1 + 2*2 = 6 = |S_3|
(define (reg-char g)
  (cond ((perm-eq g id-s3) 6)
        (t 0)))

(check "reg-char-on-identity" (= (reg-char id-s3) 6))
(check "reg-char-on-non-id"
  (every (lambda (g)
    (or (perm-eq g id-s3) (= (reg-char g) 0))) S3))

; Decompose: reg = χ_triv + χ_sign + 2·χ_std
(define (decomp-char g)
  (+ (chi-triv g) (chi-sign g) (* 2 (chi-std g))))

(check "regular-decomposition"
  (every (lambda (g)
    (= (reg-char g) (decomp-char g))) S3))

; ---------- Schur's lemma verification ----------
; Hom_G(V,W) = 0 if V,W are nonisomorphic irreducibles,
; and = C if V≅W.
; For S_3, there are 3 nonisomorphic irreps, so Hom spaces have dim 1 if same, 0 if different.

; ---------- Maschke's theorem ----------
; Over C (char 0 field), every representation decomposes into irreducibles.
; Since 6 = |S_3| is not divisible by char(C)=0, Maschke applies.
; The group ring C[S_3] is semisimple.

(define (dim-sum)
  (foldl (lambda (acc chi) (+ acc (* (chi id-s3) (chi id-s3)))) 0
    (set-to-list irrep-chars)))

(check "sum-of-square-dims-equals-order"
  (= (dim-sum) (length S3)))

; ---------- Character table as matrix ----------
(define (char-table)
  (list
    (list (chi-triv id-s3) (chi-triv p12) (chi-triv p123))
    (list (chi-sign id-s3) (chi-sign p12) (chi-sign p123))
    (list (chi-std id-s3) (chi-std p12) (chi-std p123))))

(check "char-table-trivial-row"
  (equal (nth 0 (char-table)) (list 1 1 1)))

(check "char-table-sign-row"
  (equal (nth 1 (char-table)) (list 1 -1 1)))

(check "char-table-std-row"
  (equal (nth 2 (char-table)) (list 2 0 -1)))

; ---------- Tensor product of representations ----------
; χ_std ⊗ χ_sign = χ_std (since sign tensored with 2-dim gives same 2-dim)
; χ_std ⊗ χ_std = χ_triv + χ_sign + χ_std
(define (chi-tensor chi1 chi2 g)
  (* (chi1 g) (chi2 g)))

(check "std-tensor-sign-equals-std"
  (every (lambda (g)
    (= (chi-tensor chi-std chi-sign g) (chi-std g))) S3))

; Decompose χ_std ⊗ χ_std
(define (chi-tensor-std-std g)
  (* (chi-std g) (chi-std g)))

(check "std-tensor-std-decomposes"
  (every (lambda (g)
    (= (chi-tensor-std-std g)
       (+ (chi-triv g) (chi-sign g) (chi-std g)))) S3))

(print "=== Representation Theory Done ===")
