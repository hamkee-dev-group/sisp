; ============================================================
; 69 — Universality: Godel Numbers, a Universal Simulator,
;      the Padding Lemma, and the Diagonal Argument
; Every Turing machine is encoded as a single integer; ONE fixed
; function utm runs any machine from its number alone, verified
; equal to direct execution across a battery. The padding lemma
; (many numbers, one behavior) and the constructive diagonal
; function that differs from every machine on its own code are
; then checked instance by instance.
; ============================================================

(print "=== Universality and Godel Numbering ===")


; ---------- the machine model (as in program 67) ----------
(define (tape-read tape pos)
  (let ((entry (assoc pos tape)))
    (cond ((eq entry nil) 0) (t (cdr entry)))))
(define (tape-ones tape seen acc)
  (cond ((eq tape nil) acc)
        ((contains (car (car tape)) seen)
          (tape-ones (cdr tape) seen acc))
        (t (tape-ones (cdr tape) (cons (car (car tape)) seen)
             (+ acc (cdr (car tape)))))))
(define (tm-run trans state pos tape steps cutoff)
  (cond ((= steps cutoff) (list nil steps (tape-ones tape nil 0)))
    (t (let ((tr (nth (+ (* 2 state) (tape-read tape pos)) trans)))
      (let ((tape2 (cons (cons pos (nth 0 tr)) tape)))
        (cond ((= (nth 2 tr) -1)
          (list t (+ steps 1) (tape-ones tape2 nil 0)))
          (t (tm-run trans (nth 2 tr) (+ pos (nth 1 tr))
               tape2 (+ steps 1) cutoff))))))))

; ---------- Godel numbering ----------
; A transition (write move next) becomes the digit
;   write + 2*(move+1)/2 + 4*(next+1)  <  4*(states+2),
; and a machine with s states becomes the single integer
;   s + 8 * (digits in base 4*(s+2), Horner order).
; encode/decode are exact inverses — machines ARE numbers.

(define (tm-digit tr)
  (+ (nth 0 tr)
     (+ (* 2 (truncate (/ (+ (nth 1 tr) 1) 2)))
        (* 4 (+ (nth 2 tr) 1)))))
(define (tm-undigit d)
  (list (mod d 2)
        (- (* 2 (mod (truncate (/ d 2)) 2)) 1)
        (- (truncate (/ d 4)) 1)))

(define (tm-horner trans base)
  (foldr (lambda (tr acc) (+ (tm-digit tr) (* base acc))) 0 trans))
(define (tm-encode trans)
  (let ((states (truncate (/ (length trans) 2))))
    (+ states (* 8 (tm-horner trans (* 4 (+ states 2)))))))

(define (tm-digits value base count)
  (cond ((= count 0) nil)
        (t (cons (tm-undigit (mod value base))
             (tm-digits (truncate (/ value base)) base (- count 1))))))
(define (tm-decode code)
  (let ((states (mod code 8)))
    (tm-digits (truncate (/ code 8)) (* 4 (+ states 2)) (* 2 states))))

; a decoded machine is only meaningful if every transition is sane
(define (tm-valid-p code)
  (let ((states (mod code 8)))
    (and (> states 0)
      (every (lambda (tr)
        (and (>= (nth 2 tr) -1) (< (nth 2 tr) states)))
        (tm-decode code))
      (= (tm-encode (tm-decode code)) code))))

; ---------- the battery: machines from program 67 ----------
(define incr (list (list 1 1 -1) (list 1 1 0)))
(define bb2 (list (list 1 1 1) (list 1 -1 1) (list 1 -1 0) (list 1 1 -1)))
(define bb3 (list (list 1 1 1) (list 1 1 -1)
                  (list 0 1 2) (list 1 1 1)
                  (list 1 -1 2) (list 1 -1 0)))
(define spinner (list (list 0 1 0) (list 1 1 0)))
(define battery (list incr bb2 bb3 spinner))

(check "encode-decode-roundtrip"
  (every (lambda (m) (equal (tm-decode (tm-encode m)) m)) battery))
(check "codes-are-distinct"
  (= (length battery)
     (length (list-to-set (map (lambda (m) (tm-encode m)) battery)))))
(check "battery-codes-are-valid"
  (every (lambda (m) (tm-valid-p (tm-encode m))) battery))
(print (list (quote bb2-code) (tm-encode bb2)
             (quote bb3-code) (tm-encode bb3)))

; ---------- THE universal function ----------
; utm sees only an integer and a tape; the machine exists solely
; as arithmetic on that integer.
(define (utm code tape cutoff)
  (tm-run (tm-decode code) 0 0 tape 0 cutoff))

(check "utm-agrees-with-direct-execution"
  (every (lambda (m)
    (equal (utm (tm-encode m) nil 60) (tm-run m 0 0 nil 0 60)))
    battery))
(check "utm-reproduces-bb2" (equal (utm (tm-encode bb2) nil 60) (list t 6 4)))
(check "utm-reproduces-bb3" (equal (utm (tm-encode bb3) nil 60) (list t 14 6)))
(check "utm-on-input-tape"
  (equal (utm (tm-encode incr) (list (cons 0 1) (cons 1 1)) 60)
         (list t 3 3)))

; ---------- the padding lemma ----------
; Appending an unreachable state yields a DIFFERENT number with
; the SAME behavior: every machine has infinitely many codes.
(define (tm-pad m)
  (append m (list (list 0 1 (truncate (/ (length m) 2)))
                  (list 1 1 (truncate (/ (length m) 2))))))
(check "padding-changes-the-code"
  (every (lambda (m) (not (= (tm-encode m) (tm-encode (tm-pad m)))))
    battery))
(check "padding-preserves-behavior"
  (every (lambda (m)
    (equal (utm (tm-encode m) nil 60) (utm (tm-encode (tm-pad m)) nil 60)))
    battery))

; ---------- the diagonal argument, constructively ----------
; bounded-value(n) = what machine number n outputs on the empty
; tape within k steps (0 if invalid or still running). This IS
; computable. The diagonal function d(n) = bounded-value(n) + 1
; is therefore also computable — and differs from every machine's
; bounded behavior at its own index. Verified over a whole range
; of codes plus the battery: no n satisfies value_n(n) = d(n).
; Lifting k away is exactly the step no computation can make:
; the unbounded version is the halting problem.

(define diag-cutoff 40)
(define (bounded-value n)
  (cond ((tm-valid-p n)
    (let ((r (utm n nil diag-cutoff)))
      (cond ((eq (nth 0 r) t) (nth 2 r)) (t 0))))
    (t 0)))
(define (diag n) (+ (bounded-value n) 1))

(define probe-codes
  (append (map (lambda (m) (tm-encode m)) battery) (seq 1 400)))
(check "diagonal-differs-everywhere"
  (every (lambda (n) (not (= (bounded-value n) (diag n)))) probe-codes))
(check "diagonal-is-itself-computable"
  (numberp (diag (tm-encode bb2))))

(print "=== Universality Done ===")
