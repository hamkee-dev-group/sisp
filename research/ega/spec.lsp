; ============================================================
; ega/spec -- The prime spectrum of a finite ring
; Spec(Z/12Z): two primes (2) and (3). Zariski topology
; verified: V(I+J)=V(I)∩V(J), spectrum is disconnected.
; ============================================================

(print "=== EGA: Spec(Z/12Z) ===")

(define z12 {0 1 2 3 4 5 6 7 8 9 10 11})
(define (add12 a b) (mod (+ a b) 12))
(define (mul12 a b) (mod (* a b) 12))
(define (neg12 a) (mod (- 0 a) 12))

(define (ideal-p12 s)
  (and (in 0 s)
    (every (lambda (a)
      (every (lambda (b) (in (add12 a b) s)) s)) s)
    (every (lambda (a) (in (neg12 a) s)) s)
    (every (lambda (a)
      (every (lambda (r) (in (mul12 r a) s)) z12)) s)))

(define (subgroup-p12 s)
  (and (in 0 s)
    (every (lambda (a) (in (neg12 a) s)) s)
    (every (lambda (a)
      (every (lambda (b) (in (add12 a b) s)) s)) s)))

(define all-subgroups
  (filter (lambda (s) (subgroup-p12 s)) (powerset z12)))

(define ideals12
  (filter (lambda (s) (ideal-p12 s)) all-subgroups))

(print (list (quote ideals) ideals12))

(check "six-ideals" (= (length ideals12) 6))

(define (prime-p p)
  (and (not (equal p z12))
    (every (lambda (a)
      (every (lambda (b)
        (or (notin (mul12 a b) p) (in a p) (in b p))) z12)) z12)))

(define spec12 (filter (lambda (p) (prime-p p)) ideals12))
(print (list (quote spec) spec12))

(check "two-prime-ideals" (= (length spec12) 2))
(check "prime-2-is-0-2-4-6-8-10" (in {0 2 4 6 8 10} spec12))
(check "prime-3-is-0-3-6-9" (in {0 3 6 9} spec12))

(define (V i) (filter (lambda (p) (subset i p)) spec12))

(check "V-zero-is-all" (equal (V {0}) spec12))
(check "V-unit-ideal-empty" (equal (V z12) {}))

(define (ideal-sum i j)
  (filter (lambda (x)
    (any (lambda (a)
      (any (lambda (b) (= (add12 a b) x)) j)) i)) z12))

(define (ideal-intersection i j) (cap i j))

(check "V-sum-is-intersection"
  (every (lambda (i)
    (every (lambda (j)
      (equal (V (ideal-sum i j)) (cap (V i) (V j)))) ideals12)) ideals12))

(check "V-intersection-is-union"
  (every (lambda (i)
    (every (lambda (j)
      (equal (V (ideal-intersection i j)) (union (V i) (V j)))) ideals12)) ideals12))

(define (D f)
  (filter (lambda (p) (notin f p)) spec12))

(check "D-of-3-has-p2-only"
  (equal (D 3) {{0 2 4 6 8 10}}))

(check "D-of-2-has-p3-only"
  (equal (D 2) {{0 3 6 9}}))

(check "D-of-1-is-full-space"
  (equal (D 1) spec12))

(check "D-of-0-is-empty"
  (equal (D 0) {}))

(check "D-of-6-is-empty-nilpotent"
  (equal (D 6) {}))

(check "spec-disconnected"
  (and (in {{0 2 4 6 8 10}} (map (lambda (i) (V i)) ideals12))
       (in {{0 3 6 9}} (map (lambda (i) (V i)) ideals12))
       (equal {{0 2 4 6 8 10} {0 3 6 9}} spec12)))

(define (nilpotent-p a)
  (cond ((= a 0) t)
        ((= (mul12 a a) 0) t)
        ((= (mul12 a (mul12 a a)) 0) t)
        (t nil)))

(define nilradical (filter (lambda (a) (nilpotent-p a)) z12))
(print (list (quote nilradical) nilradical))

(define primes-int
  (foldl (lambda (acc p) (cap acc p)) z12 (set-to-list spec12)))

(check "nilradical-equals-prime-intersection"
  (equal nilradical primes-int))

(print "=== EGA: Spec Done ===")
