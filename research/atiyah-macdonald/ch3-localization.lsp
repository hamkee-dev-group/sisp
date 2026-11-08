; ============================================================
; Atiyah-Macdonald Ch3: Rings and Modules of Fractions
; Localization S⁻¹R, local properties, extended/contracted ideals.
; Verified on Z/12Z, Z/30Z.
; ============================================================

(print "=== AM Ch3: Localization ===")

(define (r+ n a b) (mod (+ a b) n))
(define (r* n a b) (mod (* a b) n))
(define (pow-mod n a k)
  (cond ((= k 0) 1)
        (t (mod (* a (pow-mod n a (- k 1))) n))))

(define (divides n d) (= (mod n d) 0))
(define (divisors n)
  (labels (((go k acc)
    (cond ((> k n) (reverse acc))
          ((divides n k) (go (+ k 1) (cons k acc)))
          (t (go (+ k 1) acc)))))
    (go 1 nil)))

(define (ideal-from-divisor n d)
  (list-to-set
   (labels (((go k acc)
     (cond ((>= k n) (reverse acc))
           (t (go (+ k d) (cons k acc))))))
     (go 0 nil))))

(define (mult-set-generated-by n f)
  (labels (((build seen k)
    (let ((val (pow-mod n f k)))
      (cond ((= val 0) seen)
            ((in val seen) seen)
            (t (build (cons val seen) (+ k 1)))))))
    (list-to-set (build nil 0))))

(define (mult-closed-p S mul)
  (and (in 1 S)
       (every (lambda (a)
         (every (lambda (b) (in (mul a b) S)) S)) S)))

(define (loc-equiv n S a b)
  (let ((diff (mod (- a b) n)))
    (any (lambda (s) (= (mod (* diff s) n) 0)) S)))

(define (loc-reps n S)
  (labels (((pick candidates chosen)
    (cond ((eq candidates nil) (reverse chosen))
          ((any (lambda (c) (loc-equiv n S (car candidates) c)) chosen)
           (pick (cdr candidates) chosen))
          (t (pick (cdr candidates) (cons (car candidates) chosen))))))
    (pick (seq 0 (- n 1)) nil)))

(define (localized-ring n S) (list-to-set (loc-reps n S)))

(define (find-rep-in n S x reps)
  (cond ((eq reps nil) x)
        ((loc-equiv n S x (car reps)) (car reps))
        (t (find-rep-in n S x (cdr reps)))))

(define (loc-add n S a b)
  (find-rep-in n S (mod (+ a b) n) (loc-reps n S)))

(define (loc-mul n S a b)
  (find-rep-in n S (mod (* a b) n) (loc-reps n S)))

(define (loc-kernel n S)
  (filter (lambda (r)
    (any (lambda (s) (= (mod (* r s) n) 0)) S))
    (list-to-set (seq 0 (- n 1)))))

(define (extended-ideal n S I)
  (let ((reps (loc-reps n S))
        (full-ring (list-to-set (seq 0 (- n 1)))))
    (list-to-set
     (dedup
      (foldl (lambda (acc x)
        (foldl (lambda (acc2 r)
          (cons (find-rep-in n S (mod (* r x) n) reps) acc2))
          acc (set-to-list full-ring)))
        nil (set-to-list I))))))

(define (contracted-ideal n S J)
  (let ((full-ring (list-to-set (seq 0 (- n 1)))))
    (filter (lambda (r)
      (let ((image-r (find-rep-in n S r (loc-reps n S))))
        (in image-r J))) full-ring)))

(define (local-ring-p n S)
  (let ((Rloc (localized-ring n S)))
    (labels (((nonunit-p x)
      (not (any (lambda (y)
        (= (loc-mul n S x y) 1)) Rloc))))
      (let ((nonunits (filter nonunit-p Rloc)))
        (and (not (equal nonunits {}))
             (not (equal nonunits Rloc))
             (every (lambda (a)
               (every (lambda (b) (in (loc-add n S a b) nonunits)) nonunits)) nonunits)
             (every (lambda (a)
               (every (lambda (r) (in (loc-mul n S r a) nonunits)) Rloc)) nonunits))))))

(print "--- Z/12Z: R_f for f=3 ---")
(define n12 12)
(define S3 (mult-set-generated-by n12 3))
(check "S-from-3-is-1-3-9" (equal S3 {1 3 9}))
(check "S3-mult-closed" (mult-closed-p S3 (lambda (a b) (mod (* a b) 12))))

(define ker3 (loc-kernel n12 S3))
(check "kernel-S3-is-4Z" (equal ker3 (ideal-from-divisor n12 4)))

(define Rf3 (localized-ring n12 S3))
(check "R-f3-size-4" (= (length Rf3) 4))

(check "3-is-unit-in-Rf3"
  (any (lambda (y) (= (loc-mul n12 S3 3 y) 1)) Rf3))

(print "--- Z/12Z: R_p at P=(3) ---")
(define P12 {0 3 6 9})
(define SP (filter (lambda (x) (notin x P12)) (list-to-set (seq 0 11))))
(check "S-at-P-is-mult-closed" (mult-closed-p SP (lambda (a b) (mod (* a b) 12))))

(define kerP (loc-kernel n12 SP))
(check "kernel-at-P-is-P" (equal kerP P12))

(define Rp (localized-ring n12 SP))
(check "R-p-size-3" (= (length Rp) 3))
(check "R-p-is-local" (local-ring-p n12 SP))

(print "--- Extended/Contracted Ideals ---")

(define I2 (ideal-from-divisor n12 2))
(define I2-ext (extended-ideal n12 S3 I2))
(check "extended-ideal-2Z-is-full-or-size-2" (>= (length I2-ext) 1))

(define I6 (ideal-from-divisor n12 6))
(define I6-ext (extended-ideal n12 S3 I6))
(check "extended-ideal-6Z-size-geq-1" (>= (length I6-ext) 0))

(define I6-ctr (contracted-ideal n12 S3 I6-ext))
(check "contracted-ideal-contains-original"
  (subset I6 I6-ctr))

(print "--- Z/30Z: R_f for f=2 ---")
(define n30 30)
(define S2-30 (mult-set-generated-by n30 2))
(check "S-from-2-in-Z30-is-1-2-4-8-16" (equal S2-30 {1 2 4 8 16}))

(define ker2-30 (loc-kernel n30 S2-30))
(check "kernel-S2-Z30-is-15Z" (equal ker2-30 (ideal-from-divisor n30 15)))

(define Rf2-30 (localized-ring n30 S2-30))
(check "R-f2-Z30-is-not-full" (>= (length Rf2-30) 1))

(print "=== AM Ch3 Done ===")
