; Godel numbers: encode SISP expressions, round-trip verify.
; Each atom gets a small code; conses are encoded by Cantor pairing (below).

(define (gcode-sym s)
  (cond ((eq s '+) 1) ((eq s '-) 2) ((eq s '*) 3)
        ((eq s 'car) 4) ((eq s 'cdr) 5) ((eq s 'cons) 6)
        ((eq s 'eq) 7) ((eq s 'equal) 8) ((eq s 'quote) 9)
        (t 10)))

; t and nil must be matched before symbolp: in SISP, symbolp(nil) is
; true, so nil would otherwise get the generic symbol code
(define (gcode-atom x)
  (cond ((equal x t) 100)
        ((equal x nil) 101)
        ((consp x) 0)
        ((symbolp x) (+ 1 (gcode-sym x)))
        ((numberp x) (+ 12 x))
        (t 102)))

; The first primes, for a prime-power encoding.  Not used by gcode below:
; a fixed table cannot give every code its own prime.
(define (gprime n)
  (cond ((= n 0) 2)  ((= n 1) 3)  ((= n 2) 5)  ((= n 3) 7)  ((= n 4) 11)
        ((= n 5) 13) ((= n 6) 17) ((= n 7) 19) ((= n 8) 23) ((= n 9) 29)
        ((= n 10) 31) ((= n 11) 37) ((= n 12) 41)
        (t 43)))

; Encoding: Cantor pairing tagged by parity.
;
;   atom x       -> 2 * smallcode(x) + 1      (odd)
;   cons (a . b) -> 2 * pair(gcode a, gcode b) (even)
;   pair(a, b) = (a+b)(a+b+1)/2 + b
;
; odd decodes to an atom; even unpairs to a cons.  Cantor pairing is a
; bijection N x N -> N, so two conses collide only if their parts do.
; Each pairing roughly squares the code, so only small terms fit in the
; 64-bit integers.

(define (gpair a b)
  (+ (/ (* (+ a b) (+ a b 1)) 2) b))

; w = a+b from pair(a,b) = w(w+1)/2 + b: the largest w with
; w(w+1)/2 <= z.  Computed from the float sqrt, then adjusted by a few
; steps.  All comparisons use safe-tri to avoid overflowing w(w+1).
(define (safe-tri w)
  (cond ((= (mod w 2) 0) (* (truncate (/ w 2)) (+ w 1)))
        (t (* w (truncate (/ (+ w 1) 2))))))

(define (gunpair-w z)
  (let ((w0 (truncate (* (sqrt z) 1.41421356))))
    (gunpair-adjust z w0)))

(define (gunpair-adjust z w)
  (cond ((> (safe-tri w) z) (gunpair-adjust z (- w 1)))
        ((<= (safe-tri (+ w 1)) z) (gunpair-adjust z (+ w 1)))
        (t w)))

(define (gunpair z)
  (let* ((w (gunpair-w z))
         (t2 (safe-tri w))
         (b (- z t2))
         (a (- w b)))
    (cons a b)))

(define (gcode expr)
  (cond ((consp expr)
         (* 2 (gpair (gcode (car expr)) (gcode (cdr expr)))))
        (t (+ 1 (* 2 (gcode-atom expr))))))

(define (gdecode n)
  (cond ((= (mod n 2) 1) (gdecode-atom (truncate (/ (- n 1) 2))))
        (t (let ((ab (gunpair (truncate (/ n 2)))))
             (cons (gdecode (car ab)) (gdecode (cdr ab)))))))

(define (gdecode-atom n)
  (cond ((= n 100) t)
        ((= n 101) nil)
        ((and (>= n 12) (< n 109)) (- n 12))
        (t (gdecode-sym (- n 1)))))

(define (gdecode-sym n)
  (cond ((= n 1) '+) ((= n 2) '-) ((= n 3) '*)
        ((= n 4) 'car) ((= n 5) 'cdr) ((= n 6) 'cons)
        ((= n 7) 'eq) ((= n 8) 'equal) ((= n 9) 'quote)
        (t 'sym)))

; Test round-trip
(define gtest1 (gcode '(+ 1 2)))
(print "gcode (+ 1 2) = ")
(print gtest1)

(define gtest2 (gcode '(quote a)))
(print "gcode (quote a) = ")
(print gtest2)

(if (= gtest1 (gcode '(+ 1 2)))
    (print "PASS: godel-deterministic")
    (print "FAIL: godel-deterministic"))

(if (> gtest1 0)
    (print "PASS: godel-positive")
    (print "FAIL: godel-positive"))

; Encode a simple modus ponens: P, P->Q |- Q
(define mp-p (gcode 'P))
(define mp-pq (gcode '(=> P Q)))

(if (/= mp-p mp-pq)
    (print "PASS: godel-injective")
    (print "FAIL: godel-injective"))

; NOTE on size: since each pairing roughly squares the code, codes grow
; doubly exponentially with nesting depth, and SISP integers are 64-bit
; (overflow is an error).  '(P (=> P Q) Q) already exceeds 64 bits, so the
; demos keep to terms that fit.

; Verify encoding of nesting (kept shallow: see the depth note above)
(define gnested (gcode '((1) 2)))
(if (> gnested 1)
    (print "PASS: godel-nested")
    (print "FAIL: godel-nested"))

; round-trip verification.
; NOTE: the symbol table has 9 entries; any other symbol shares the
; default code and decodes as 'sym (a Godel numbering of an open symbol
; alphabet would encode the symbol's name).  Integers round-trip for
; 0..87; 88 and 89 share the codes of t and nil.
(check "godel: round-trip (+ 1 2)"
       (equal (gdecode (gcode '(+ 1 2))) '(+ 1 2)))
(check "godel: round-trip (quote cons)"
       (equal (gdecode (gcode '(quote cons))) '(quote cons)))
(check "godel: round-trip a number"
       (equal (gdecode (gcode 42)) 42))
(check "godel: round-trip booleans"
       (and (eq (gdecode (gcode t)) t)
            (eq (gdecode (gcode nil)) nil)))
(check "godel: distinct expressions get distinct numbers"
       (not (equal (gcode '(+ 1 2)) (gcode '(* 1 2)))))

(print "=== GODEL NUMBERS DONE ===")
