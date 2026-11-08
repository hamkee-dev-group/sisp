; ============================================================
; ega/projective-space -- Projective space over finite fields
; P¹(F_5): 6 points (5 finite + ∞). Gluing data.
; P²(F_2): 7 points (Fano plane PG(2,2) connection).
; ============================================================

(load "lib/collections/relations.lsp")

(print "=== EGA: Projective Space ===")

; ---------- P¹(F_5) ----------
; P¹ over field F_5 = {0,1,2,3,4}.
; Points are equivalence classes [x:y] ≠ [0:0] with [x:y] ~ [λx:λy] for λ≠0.
; Standard chart U₀ = {[x:y] : x≠0}: map [x:y] ↦ y/x ∈ F_5.
; Standard chart U₁ = {[x:y] : y≠0}: map [x:y] ↦ x/y ∈ F_5.
; U₀ ≅ A¹(F_5) has 5 points (the "finite" line).
; U₁ ≅ A¹(F_5) has 5 points (the other line).
; Overlap U₀∩U₁ = {[x:y] : x≠0, y≠0} → gluing: z ∈ F_5\{0} maps to 1/z.
; Total points = #U₀ + #U₁ - #(U₀∩U₁) = 5 + 5 - 4 = 6.
; Points: [1:0], [1:1], [1:2], [1:3], [1:4], [0:1] (point at infinity).

(define f5 {0 1 2 3 4})
(define (add5 a b) (mod (+ a b) 5))
(define (mul5 a b) (mod (* a b) 5))

; F_5^* = {1,2,3,4}, order 4.
(define f5star {1 2 3 4})

; P¹(F_5) points as pairs [x:y] with (x,y)≠(0,0) modulo scaling.
(define nonzero-pairs
  (filter (lambda (p)
    (not (and (= (car p) 0) (= (cadr p) 0))))
    (cartesian-product f5 f5)))

; Normalize [x:y] to a canonical representative:
; If x≠0, scale to [1 : y/x]; otherwise [0:1].
(define (normalize-proj-pair p)
  (let ((x (car p)) (y (cadr p)))
    (cond ((not (= x 0))
      (let ((invx (cond ((= x 1) 1) ((= x 2) 3) ((= x 3) 2) ((= x 4) 4))))
        (list 1 (mul5 y invx))))
      (t (list 0 1)))))

(define p1-points
  (map (lambda (p) (normalize-proj-pair p)) nonzero-pairs))

(print (list (quote P1-F5-points) p1-points))
(check "P1-F5-has-6-points" (= (length p1-points) 6))

; Verify the six expected points:
(check "P1-has-point-0-1" (in (list 0 1) p1-points))
(check "P1-has-point-1-0" (in (list 1 0) p1-points))
(check "P1-has-point-1-1" (in (list 1 1) p1-points))
(check "P1-has-point-1-4" (in (list 1 4) p1-points))

; ---------- charts and gluing ----------
; U₀: points with x ≠ 0, chart ψ₀[x:y] = y/x
(define u0 (filter (lambda (p) (not (= (car p) 0))) p1-points))
(check "U0-has-5-points" (= (length u0) 5))

; U₁: points with y ≠ 0, chart ψ₁[x:y] = x/y
(define u1 (filter (lambda (p) (not (= (cadr p) 0))) p1-points))
(check "U1-has-5-points" (= (length u1) 5))

; Overlap U₀∩U₁:
(define u01 (cap u0 u1))
(check "U01-has-4-points" (= (length u01) 4))

; Gluing map: ψ₁∘ψ₀⁻¹(z) = 1/z for z ≠ 0.
; For a point [1:t] in U₀∩U₁ with t≠0:
; ψ₀[1:t] = t, ψ₁[1:t] = 1/t.
; Verify the transition map is t ↦ 1/t.
(define (inv5 x)
  (cond ((= x 1) 1) ((= x 2) 3) ((= x 3) 2) ((= x 4) 4)))

(check "gluing-is-inversion"
  (every (lambda (p)
    (let ((coord-t (cadr p)))
      (let ((psi0 coord-t)
            (psi1 (inv5 coord-t)))
        (= (mul5 coord-t psi1) 1))))
    u01))

; The gluing is consistent: transition function is regular (1/z)
(check "gluing-consistent-6-points"
  (= (length p1-points) (+ (- (length u0) (length u01)) (length u1))))

; ---------- P²(F_2) ----------
; P²(F_2) = PG(2,2) = Fano plane: 7 points, 7 lines, 3 points/line.
; Points are the 7 nonzero vectors in F_2³.
; Build explicitly: all 3-tuples except (0 0 0).

(define p2-points
  (list-to-set
    (list (list 0 0 1) (list 0 1 0) (list 0 1 1)
          (list 1 0 0) (list 1 0 1) (list 1 1 0) (list 1 1 1))))

(define (vec-add2 u v)
  (list (mod (+ (car u) (car v)) 2)
        (mod (+ (cadr u) (cadr v)) 2)
        (mod (+ (caddr u) (caddr v)) 2)))

(print (list (quote P2-F2-points) p2-points))
(check "P2-F2-has-7-points" (= (length p2-points) 7))

; Lines in P²(F_2): for each pair of distinct points, the line
; through them contains exactly 3 points (the two + their sum).
(define p2-pairs
  (filter (lambda (pq)
    (not (equal (car pq) (cadr pq))))
    (cartesian-product p2-points p2-points)))

(define p2-lines
  (map (lambda (pq)
    (let ((p (car pq)) (q (cadr pq)))
      (list-to-set (list p q (vec-add2 p q)))))
    p2-pairs))

(check "P2-F2-has-7-lines" (= (length p2-lines) 7))
(check "each-line-has-3-points"
  (every (lambda (l) (= (length l) 3)) p2-lines))

; Projective plane axioms in P²(F_2):
; Two points determine a unique line.
(check "two-points-one-line"
  (every (lambda (pair2)
    (let ((pl (set-to-list pair2)))
      (= (count (lambda (l) (and (in (car pl) l) (in (cadr pl) l))) p2-lines) 1)))
    (k-subsets p2-points 2)))

; Two lines intersect in exactly one point.
(check "two-lines-one-intersection"
  (every (lambda (l1)
    (every (lambda (l2)
      (or (equal l1 l2) (= (length (cap l1 l2)) 1))) p2-lines)) p2-lines))

; Every point is on exactly 3 lines.
(check "every-point-on-3-lines"
  (every (lambda (p)
    (= (count (lambda (l) (in p l)) p2-lines) 3))
    p2-points))

; This matches the Fano plane (examples/math/63).
(check "fano-connection-7-7"
  (and (= (length p2-points) 7) (= (length p2-lines) 7)))

(print "=== EGA: Projective Space Done ===")
