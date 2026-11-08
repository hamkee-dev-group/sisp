; ============================================================
; 32-derangements.lsp
; Count derangements (permutations with no fixed points)
; using the inclusion-exclusion formula:
;   D(n) = n! * sum_{k=0}^{n} (-1)^k / k!
; Verify D(1)=0, D(2)=1, D(3)=2, D(4)=9, D(5)=44.
; ============================================================

(print "=== Derangements via Inclusion-Exclusion ===")

; --- Factorial ---
(define (fact n) (if (= n 0) 1 (* n (fact (+ n -1)))))

; --- Derangement count using inclusion-exclusion ---
; D(n) = n! * sum_{k=0}^{n} (-1)^k / k!
; We compute with exact integer arithmetic:
; D(n) = sum_{k=0}^{n} (-1)^k * n! / k!
(define (derangement-sum n k)
  (if (> k n) 0
    (+ (* (^ -1 k) (/ (fact n) (fact k)))
       (derangement-sum n (+ k 1)))))

(define (derangements n) (derangement-sum n 0))

; --- Brute force: generate all permutations, count those with no fixed point ---
(define (remove-first x lst)
  (if (equal lst nil) nil
    (if (equal (car lst) x) (cdr lst)
        (cons (car lst) (remove-first x (cdr lst))))))

(define (my-append a b)
  (if (equal a nil) b
    (cons (car a) (my-append (cdr a) b))))

(define (prepend-all x lsts)
  (if (equal lsts nil) nil
    (cons (cons x (car lsts)) (prepend-all x (cdr lsts)))))

(define (perms lst)
  (if (equal lst nil) (list nil)
    (labels (((helper remaining)
       (if (equal remaining nil) nil
           (my-append
             (prepend-all (car remaining)
                          (perms (remove-first (car remaining) lst)))
             (helper (cdr remaining))))))
      (helper lst))))

; Check if permutation has any fixed point: perm[i] = i
(define (has-fixed-point perm pos)
  (if (equal perm nil) nil
    (if (= (car perm) pos) t
        (has-fixed-point (cdr perm) (+ pos 1)))))

; Count derangements by brute force
(define (count-derangements-bf n)
  (labels (((count-them ps)
    (if (equal ps nil) 0
      (+ (if (has-fixed-point (car ps) 1) 0 1)
         (count-them (cdr ps))))))
    (count-them (perms (seq 1 n)))))

; ============================================================
; Verify D(1) through D(5) using the formula
; ============================================================
(print "--- Derangement formula results ---")

(define d1 (derangements 1))
(print "D(1):") (print d1)
(if (= d1 0) (print "VERIFIED: D(1) = 0") (print "FAILED"))

(define d2 (derangements 2))
(print "D(2):") (print d2)
(if (= d2 1) (print "VERIFIED: D(2) = 1") (print "FAILED"))

(define d3 (derangements 3))
(print "D(3):") (print d3)
(if (= d3 2) (print "VERIFIED: D(3) = 2") (print "FAILED"))

(define d4 (derangements 4))
(print "D(4):") (print d4)
(if (= d4 9) (print "VERIFIED: D(4) = 9") (print "FAILED"))

(define d5 (derangements 5))
(print "D(5):") (print d5)
(if (= d5 44) (print "VERIFIED: D(5) = 44") (print "FAILED"))

; ============================================================
; Cross-check D(4) with brute force
; ============================================================
(print "--- Brute-force cross-check for D(4) ---")
(define bf4 (count-derangements-bf 4))
(print "D(4) by brute force:")
(print bf4)
(if (= bf4 d4)
    (print "VERIFIED: Formula matches brute force for n=4")
    (print "FAILED: Mismatch"))

(print "=== Derangements Complete ===")
