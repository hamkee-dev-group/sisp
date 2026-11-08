; ============================================================
; 72 — Combinatorial Generation (TAOCP Vol 4A, 7.2.1)
; Three classic generation orders with their defining invariants
; verified over the complete output: reflected binary Gray code
; (one bit per step, cyclic), plain changes for permutations
; (one adjacent transposition per step), and lexicographic
; combinations — cross-checked against the k-subsets machinery.
; ============================================================

(load "lib/collections/relations.lsp")

(print "=== Combinatorial Generation ===")


; ---------- reflected binary Gray code (7.2.1.1) ----------
; G(n) = 0*G(n-1) followed by 1*reverse(G(n-1)); no bit tricks
; needed, the reflection IS the definition.
(define (gray n)
  (cond ((= n 0) (list nil))
        (t (let ((g (gray (- n 1))))
          (append (map (lambda (v) (cons 0 v)) g)
                  (map (lambda (v) (cons 1 v)) (reverse g)))))))

(define (hamming u v)
  (foldl (lambda (acc p) (cond ((= (car p) (cadr p)) acc) (t (+ acc 1))))
    0 (zip u v)))
(define (adjacent-ok codes)
  (cond ((eq (cdr codes) nil) t)
        ((= (hamming (car codes) (cadr codes)) 1)
          (adjacent-ok (cdr codes)))
        (t nil)))

(define gray4 (gray 4))
(check "gray-4-has-16-codes" (= (length gray4) 16))
(check "gray-codes-all-distinct" (= (length (list-to-set gray4)) 16))
(check "gray-adjacent-differ-one-bit" (adjacent-ok gray4))
(check "gray-is-cyclic"
  (= (hamming (car gray4) (last gray4)) 1))

; rank/unrank: the Gray code of rank k and back (7.2.1.1 formulas,
; done with exact arithmetic instead of bitwise operators).
(define (gray-rank code)
  ; binary value b of the code, then k with b = k XOR (k>>1)
  ; inverted digit by digit from the most significant end.
  (car (foldl (lambda (acc bit)
    (let ((k (car acc)) (prev (cadr acc)))
      (let ((digit (mod (+ bit prev) 2)))
        (list (+ (* 2 k) digit) digit))))
    (list 0 0) code)))
(check "gray-rank-recovers-position"
  (equal (map (lambda (c) (gray-rank c)) gray4) (seq 0 15)))

; ---------- plain changes: permutations by adjacent swaps ----------
; Steinhaus recursion: thread the new largest element through each
; permutation of n-1, alternating direction (7.2.1.2).
(define (insert-at xs i v)
  (cond ((= i 0) (cons v xs))
        (t (cons (car xs) (insert-at (cdr xs) (- i 1) v)))))
(define (thread p v descending n)
  (cond (descending
    (map (lambda (i) (insert-at p (- n 1 i) v)) (seq 0 (- n 1))))
    (t (map (lambda (i) (insert-at p i v)) (seq 0 (- n 1))))))
(define (plain-changes-walk perms v n descending acc)
  (cond ((eq perms nil) acc)
        (t (plain-changes-walk (cdr perms) v n (not descending)
             (append acc (thread (car perms) v descending n))))))
(define (plain-changes n)
  (cond ((= n 1) (list (list 1)))
        (t (plain-changes-walk (plain-changes (- n 1)) n n t nil))))

; one adjacent transposition between consecutive permutations:
; equal prefixes/suffixes around exactly one swapped neighbor pair.
(define (adjacent-transposition-p p q i)
  (cond ((eq (cdr p) nil) nil)
        ((and (= (car p) (cadr q)) (= (cadr p) (car q))
              (equal (cdr (cdr p)) (cdr (cdr q))))
          t)
        ((= (car p) (car q))
          (adjacent-transposition-p (cdr p) (cdr q) (+ i 1)))
        (t nil)))
(define (plain-steps-ok perms)
  (cond ((eq (cdr perms) nil) t)
        ((adjacent-transposition-p (car perms) (cadr perms) 0)
          (plain-steps-ok (cdr perms)))
        (t nil)))

(define pc4 (plain-changes 4))
(print (list (quote first-six) (take pc4 6)))
(check "plain-changes-4-has-24" (= (length pc4) 24))
(check "plain-changes-all-distinct" (= (length (list-to-set pc4)) 24))
(check "plain-changes-starts-at-identity"
  (equal (car pc4) (list 1 2 3 4)))
(check "each-step-is-one-adjacent-swap" (plain-steps-ok pc4))

; ---------- lexicographic t-combinations (7.2.1.3) ----------
; seq requires strictly increasing bounds, so empty/singleton
; ranges are handled explicitly.
(define (range-incl a b)
  (cond ((> a b) nil) ((= a b) (list a)) (t (seq a b))))
(define (combinations-lex n k start)
  (cond ((= k 0) (list nil))
        (t (foldl (lambda (acc i)
             (append acc
               (map (lambda (rest) (cons i rest))
                 (combinations-lex n (- k 1) (+ i 1)))))
             nil (filter (lambda (i) (<= (+ i (- k 1)) n))
                   (range-incl start n))))))
(define combos (combinations-lex 6 3 1))
(define (lex-before p q)
  (cond ((eq p nil) t)
        ((< (car p) (car q)) t)
        ((> (car p) (car q)) nil)
        (t (lex-before (cdr p) (cdr q)))))
(define (lex-ordered xs)
  (cond ((eq (cdr xs) nil) t)
        ((lex-before (car xs) (cadr xs)) (lex-ordered (cdr xs)))
        (t nil)))

(check "c-6-3-has-20" (= (length combos) 20))
(check "combinations-in-lex-order" (lex-ordered combos))
(check "combinations-match-k-subsets"
  (equal (list-to-set (map (lambda (c) (list-to-set c)) combos))
         (k-subsets {1 2 3 4 5 6} 3)))

(print "=== Combinatorial Generation Done ===")
