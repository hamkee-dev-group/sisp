; ============================================================
; 106 — Necklaces and Lyndon Words (TAOCP Vol 4A, 7.2.1.1)
; Generate all binary necklaces of length n=5 (aperiodic = Lyndon
; words). Verify: count of Lyndon words = (mu(1)*32+mu(5)*2)/5=6.
; List the 6. Generate all necklaces of length 4 over {0,1,2}
; and verify the necklace-count formula.
; ============================================================

(print "=== Necklaces and Lyndon Words ===")


; ---------- Moebius function ----------
(define (moebius n)
  (cond ((= n 1) 1)
        (t (labels (((go p cnt rem)
              (cond ((= (mod rem p) 0)
                     (let ((d (truncate (/ rem p))))
                       (cond ((= (mod d p) 0) 0)
                             (t (go (+ p 1) (+ cnt 1) d)))))
                    ((> (* p p) rem)
                     (cond ((> rem 1)
                            (cond ((= (mod cnt 1) 0) -1) (t 1)))
                           (t (cond ((= (mod cnt 2) 0) 1) (t -1)))))
                    (t (go (+ p 1) cnt rem)))))
             (go 2 0 n)))))

(check "mu-1" (= (moebius 1) 1))
(check "mu-2" (= (moebius 2) -1))
(check "mu-3" (= (moebius 3) -1))
(check "mu-4" (= (moebius 4) 0))
(check "mu-5" (= (moebius 5) -1))

; ---------- totient via gcd ----------
(define (totient n)
  (cond ((= n 1) 1)
        (t (labels (((cnt k acc)
              (cond ((<= k 0) acc)
                    ((= (gcd n k) 1) (cnt (- k 1) (+ acc 1)))
                    (t (cnt (- k 1) acc)))))
             (cnt (- n 1) 0)))))

; ---------- Lyndon count formula ----------
(define (lyndon-count k n)
  (labels (((ds d acc)
    (cond ((> d n) acc)
          ((= (mod n d) 0)
           (ds (+ d 1) (+ acc (* (moebius d) (^ k (truncate (/ n d)))))))
          (t (ds (+ d 1) acc)))))
    (/ (ds 1 0) n)))

(check "lyndon-c-2-5" (= (lyndon-count 2 5) 6))

; ---------- necklace count formula ----------
(define (necklace-count k n)
  (labels (((ds d acc)
    (cond ((> d n) acc)
          ((= (mod n d) 0)
           (let ((p (totient d))
                 (e (truncate (/ n d))))
             (ds (+ d 1) (+ acc (* p (^ k e))))))
          (t (ds (+ d 1) acc)))))
    (/ (ds 1 0) n)))

; ---------- generate all words of length len over alphabet ----------
(define (all-words alpha len)
  (cond ((= len 0) (list nil))
        (t (foldl (lambda (acc ch)
             (append acc (map (lambda (w) (cons ch w))
                              (all-words alpha (- len 1)))))
           nil alpha))))

; ---------- lex comparison for lists ----------
(define (list-lt a b)
  (cond ((eq a nil) (not (eq b nil)))
        ((eq b nil) nil)
        ((< (car a) (car b)) t)
        ((> (car a) (car b)) nil)
        (t (list-lt (cdr a) (cdr b)))))

(define (list-le a b) (or (list-lt a b) (equal a b)))

; ---------- all distinct rotations ----------
(define (all-rots w)
  (let ((n (length w)))
    (labels (((go i acc)
      (cond ((>= i n) acc)
            (t (go (+ i 1)
                 (cons (append (drop w i) (take w i)) acc))))))
      (go 0 nil))))

; ---------- is w a Lyndon word? ----------
(define (is-lyndon w)
  (is-lyndon-chk w (all-rots w) 0))

(define (is-lyndon-chk w rots seen)
  (cond ((eq rots nil) t)
        ((equal (car rots) w)
         (cond ((= seen 0) (is-lyndon-chk w (cdr rots) 1))
               (t nil)))
        ((list-lt w (car rots))
         (is-lyndon-chk w (cdr rots) seen))
        (t nil)))

; ---------- is w a necklace? ----------
; w must be <= all rotations (i.e., w is a canonical rotation).
(define (is-necklace w)
  (let ((n (length w)))
    (cond ((= n 0) nil)
          (t (every (lambda (r) (list-le w r)) (all-rots w))))))

; ---------- filter words by predicate ----------
(define (lyndons alpha n)
  (filter is-lyndon (all-words alpha n)))

(define (necklaces alpha n)
  (filter is-necklace (all-words alpha n)))

; ============================================================
; Binary, length 5
; ============================================================
(print "--- Binary n=5 ---")
(define lynd-2-5 (lyndons (list 0 1) 5))
(define neck-2-5 (necklaces (list 0 1) 5))
(check "bin-lyndon-5-eq-6" (= (length lynd-2-5) 6))
(check "bin-necklace-5-eq-8" (= (length neck-2-5) 8))
(check "neck-minus-lynd-eq-2" (= (- (length neck-2-5) (length lynd-2-5)) 2))
(print (list (quote Lyndon) lynd-2-5))
(print (list (quote Necklace) neck-2-5))

; ============================================================
; Ternary, length 4
; ============================================================
(print "--- Ternary n=4 ---")
(define lynd-3-4 (lyndons (list 0 1 2) 4))
(define neck-3-4 (necklaces (list 0 1 2) 4))
(define lc-3-4 (lyndon-count 3 4))
(define nc-3-4 (necklace-count 3 4))
(check "ter-lyndon-formula" (= (length lynd-3-4) lc-3-4))
(check "ter-necklace-formula" (= (length neck-3-4) nc-3-4))
(print (list (quote Lyndon-count) lc-3-4))
(print (list (quote Necklace-count) nc-3-4))

(print "=== Necklaces Done ===")
