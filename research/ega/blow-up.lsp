; ============================================================
; ega/blow-up -- Blowup of Spec(R) along ideal I.
; Proj(⊕ I^n). Computation of Bl_{(0,0)} Spec(F_q[x,y]).
; Exceptional divisor.
; ============================================================

(print "=== EGA: Blow-up ===")

; ---------- Blow-up of A² at the origin ----------
; X = Spec(F_2[x,y]), I = (x,y) (the maximal ideal at origin).
; The blow-up Bl_I X = Proj( ⊕_{n≥0} I^n ).
; Rees algebra: R[It] = R ⊕ I t ⊕ I² t² ⊕ ...
; Blow-up: Bl_I Spec(R) = Proj(R[It]).
;
; For R = F_2[x,y], I = (x,y):
; I^n = polynomials all of whose monomials have degree ≥ n in x,y.
; R[It] = F_2[x,y,u,v]/(xv - yu) where u = xt, v = yt.
; Taking Proj, the blow-up is
; {((x,y), [u:v]) ∈ A² × P¹ : xv = yu}.
;
; The exceptional divisor E = π⁻¹(0,0) ≅ P¹.
; E is the preimage of the origin.

; ---------- Working over F_2 concretely ----------
; A²(F_2) = { (0,0), (0,1), (1,0), (1,1) } (4 points).
; P¹(F_2) = { [1:0], [0:1], [1:1] } (3 points).
; Bl_{(0,0)} A²: points of the blow-up over F_2.
;
; The blow-up is the incidence correspondence:
; { ((x,y), [u:v]) ∈ F_2² × F_2-P¹ : xv = yu }.
;
; Write each F_2² point (4 points total):
; (0,0): Condition 0*v = 0*u (always true). So ALL [u:v] ∈ P¹ are in the fiber.
;   Fiber = 3 points.
; (0,1): 0*v = 1*u → u = 0 → [u:v] = [0:1]. 1 point.
; (1,0): 1*v = 0*u → v = 0 → [u:v] = [1:0]. 1 point.
; (1,1): 1*1 = 1*1 → v = u → [u:v] = [1:1]. 1 point.
;
; Total points = 3 + 1 + 1 + 1 = 6.

; ---------- Compute blow-up fiber product ----------
; A²: all pairs (x,y) in F_2 × F_2.
(define f2 {0 1})
(define a2 (cartesian-product f2 f2))
(check "A2-has-4-points" (= (length a2) 4))

; P¹: points [u:v] with [u:v] ~ [λu:λv] for λ ∈ F_2^* = {1}.
; So simply all nonzero vectors in F_2².
(define p1-points
  (filter (lambda (v) (not (and (= (car v) 0) (= (cadr v) 0))))
              (cartesian-product f2 f2)))
(check "P1-F2-has-3-points" (= (length p1-points) 3))

; Blow-up: {((x,y), [u:v]) : xv = yu}
(define blow-up-points
  (filter (lambda (pair)
    (let ((xy (car pair)) (uv (cadr pair)))
      (let ((x (car xy)) (y (cadr xy))
            (u (car uv)) (v (cadr uv)))
        (and (not (and (= u 0) (= v 0)))
             (= (mod (* x v) 2) (mod (* y u) 2))))))
    (cartesian-product a2 p1-points)))

(print (list (quote blowup-F2-points) blow-up-points))
(check "blowup-has-6-points" (= (length blow-up-points) 6))

; ---------- The blow-up map π: Bl → A² ----------
(define (blowup-project point)
  (car point))

(define blowup-image
  (map (lambda (p) (blowup-project p)) blow-up-points))

(check "blowup-surjects-onto-A2" (equal blowup-image a2))

; Fiber over each point:
(define (fiber-over xy)
  (filter (lambda (p) (equal (car p) xy)) blow-up-points))

(define fiber00 (fiber-over (list 0 0)))
(define fiber01 (fiber-over (list 0 1)))
(define fiber10 (fiber-over (list 1 0)))
(define fiber11 (fiber-over (list 1 1)))

(check "fiber-over-origin-size-3" (= (length fiber00) 3))
(check "fiber-over-01-size-1" (= (length fiber01) 1))
(check "fiber-over-10-size-1" (= (length fiber10) 1))
(check "fiber-over-11-size-1" (= (length fiber11) 1))

; The exceptional divisor E = π⁻¹(0,0) ≅ P¹ (3 points).
; Each other point has a unique preimage.
(check "exceptional-divisor-is-P1" (= (length fiber00) (length p1-points)))

; ---------- Charts of the blow-up ----------
; Blow-up is covered by two affine charts:
; Chart U (v ≠ 0): set v = 1, equation x = yu/v = yu (since v=1).
;   Coordinates: (u, y). The blow-up map sends (u,y) ↦ (u·y, y) = (x,y).
;   So x = uy. This is Spec(F_2[x,y,u]/(x - uy)) ≅ Spec(F_2[u,y]).
;   As an affine plane A²=Spec(F_2[u,y]), it has 4 F_2-points.
; Chart V (u ≠ 0): set u = 1, equation y = xv/u = xv.
;   So y = xv. This is Spec(F_2[x,y,v]/(y - xv)) ≅ Spec(F_2[x,v]).
;   Likewise A²=Spec(F_2[x,v]) has 4 F_2-points.
; The two charts cover all 6 points of the blow-up (4 + 4 − 2 = 6).

; Points in chart U (v=1): the 4 F_2-points (u,y) ∈ F_2²,
;   (u,y) ↦ ((uy,y),[u:1]) = (0,0)[0:1], (0,1)[0:1], (0,0)[1:1], (1,1)[1:1].
(define (in-chart-U p)
  (let ((uv (cadr p)))
    (= (cadr uv) 1)))

(define chartU (filter (lambda (p) (in-chart-U p)) blow-up-points))
(check "chart-U-has-4-points" (= (length chartU) 4))

; Points in chart V (u=1): the 4 F_2-points (x,v) ∈ F_2².
(define (in-chart-V p)
  (let ((uv (cadr p)))
    (= (car uv) 1)))

(define chartV (filter (lambda (p) (in-chart-V p)) blow-up-points))
(check "chart-V-has-4-points" (= (length chartV) 4))

; Overlap U ∩ V (u≠0 and v≠0, i.e. [u:v] = [1:1] over F_2): in chart U it is
; Spec(F_2[u,1/u,y]), whose F_2-points have u = 1 and y ∈ F_2, giving 2 points:
; ((0,0),[1:1]) and ((1,1),[1:1]).
(define chartUV (cap chartU chartV))
(check "chartUV-has-2-points" (= (length chartUV) 2))

; Cover property: U ∪ V = Bl
(check "U-union-V-is-blowup"
  (= (length (union chartU chartV)) (length blow-up-points)))

; ---------- Exceptional divisor in charts ----------
; In chart U (v=1): E is given by y = 0 (since π(u,y) = (uy,y) = (0,0) ↔ y=0).
; Then x = u·0 = 0, and u ∈ F_2 is free: u=0 gives [0:1], u=1 gives [1:1].
; So E∩U = {((0,0), [0:1]), ((0,0), [1:1])}.

(define E-cap-U
  (filter (lambda (p)
    (and (equal (car p) (list 0 0))
         (in-chart-U p)))
    blow-up-points))
(check "E-cap-U-size-2" (= (length E-cap-U) 2))

; In chart V (u=1): E is given by x=0.
; Then y = 0·v = 0. v ∈ {0,1} but [1:v] with v=0 is [1:0], v=1 is [1:1].
; E∩V = {((0,0), [1:0]), ((0,0), [1:1])}.

(define E-cap-V
  (filter (lambda (p)
    (and (equal (car p) (list 0 0))
         (in-chart-V p)))
    blow-up-points))
(check "E-cap-V-size-2" (= (length E-cap-V) 2))

; E = E∩U ∪ E∩V. Since E has 3 points, E∩U has 2, E∩V has 2,
; they share [1:1] (which is in both). So total = 2+2-1 = 3 ✓.
(define E-cap-UV (cap E-cap-U E-cap-V))
(check "shared-point-is-11" (= (length E-cap-UV) 1))

; ---------- Proper transform of a curve ----------
; Consider curve C: y = x² in A². Its proper transform C̃ in the blow-up:
; In chart U (v=1): x = uy, so the total transform is y = u²y², i.e.
; y(1 - u²y) = 0.  The factor y = 0 is the exceptional divisor; C̃ is u²y = 1.
; Its only F_2-point in this chart is u = y = 1, i.e. ((1,1), [1:1]).
; In chart V (u=1): y = xv, so xv = x², i.e. x(v - x) = 0, and C̃ is v = x.
; It meets E (x = 0) at v = 0: C̃ ∩ E = {((0,0), [1:0])}, the tangent
; direction of C at the origin (the x-axis).

; The check below only confirms that ((0,0), [1:1]) (a point of E) and
; ((1,1), [1:1]) (a point of C̃) are points of the blow-up.

(check "blowup-of-curve-y-equals-x-squared"
  (and (in (list (list 0 0) (list 1 1)) blow-up-points)
       (in (list (list 1 1) (list 1 1)) blow-up-points)))

(print "=== EGA: Blow-up Done ===")
