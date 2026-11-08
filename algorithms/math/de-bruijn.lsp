; ============================================================
; name: de-bruijn
; file: algorithms/math/de-bruijn.lsp
; subject: combinatorics
; capability: generate enumerate
; aliases: "de bruijn sequence" "debruijn" "universal cycle"
; signature: (de-bruijn alphabet n) -> sequence
; complexity: O(k^n) output size
; see-also: (none)
; derived-from: TAOCP 4A 7.2.1.1; Fredricksen, Kessler & Maiorana 1978
; status: example (tested)
; checks: 15 - every length-n window appears exactly once
; ============================================================
; De Bruijn Sequences (TAOCP Vol 4A, 7.2.1.1)
; Generate B(2,3) and B(3,2) by the Lyndon-word (FKM) construction.
; A Lyndon word is lexicographically < all its nontrivial rotations.
; Concatenating all Lyndon words whose length divides n, in
; lexicographic order, yields the De Bruijn sequence.
; Verify: |B(k,n)| = k^n, every length-n word appears exactly once.
; ============================================================

(print "=== De Bruijn Sequences ===")


; ---------- comparison of lists lexicographically ----------
(define (list-lt a b)
  (cond ((eq a nil) (not (eq b nil)))
        ((eq b nil) nil)
        ((< (car a) (car b)) t)
        ((> (car a) (car b)) nil)
        (t (list-lt (cdr a) (cdr b)))))

(define (list-le a b)
  (or (list-lt a b) (equal a b)))

; ---------- all rotations of a list ----------
(define (rotations w)
  (labels (((rot w n i acc)
    (cond ((= i n) acc)
          (t (rot (append (cdr w) (list (car w))) n (+ i 1)
                  (cons w acc))))))
    (rot w (length w) 0 nil)))

; ---------- is w a Lyndon word? ----------
(define (lyndon-p w)
  (lyndon-chk w (rotations w) 0))

(define (lyndon-chk w rots seen)
  (cond ((eq rots nil) t)
        ((equal (car rots) w)
         (cond ((= seen 0) (lyndon-chk w (cdr rots) 1))
               (t nil)))
        ((list-lt w (car rots))
         (lyndon-chk w (cdr rots) seen))
        (t nil)))

; ---------- generate all strings of length len over alphabet ----------
(define (all-strings alphabet k)
  (cond ((= k 0) (list nil))
        (t (foldl (lambda (acc ch)
             (append acc (map (lambda (s) (cons ch s))
                              (all-strings alphabet (- k 1)))))
           nil alphabet))))

; ---------- generate all Lyndon words whose length divides n ----------
(define (lyndon-words alphabet n)
  (labels (((collect d acc)
    (cond ((> d n) acc)
          ((= (mod n d) 0)
           (let ((words (all-strings alphabet d)))
             (collect (+ d 1)
               (append acc (filter (lambda (w) (lyndon-p w)) words)))))
          (t (collect (+ d 1) acc)))))
    (collect 1 nil)))

; ---------- sort list lexicographically ----------
(define (insert-sorted x xs)
  (cond ((eq xs nil) (list x))
        ((list-lt x (car xs)) (cons x xs))
        (t (cons (car xs) (insert-sorted x (cdr xs))))))

(define (sort-lex xs)
  (cond ((eq xs nil) nil)
        (t (insert-sorted (car xs) (sort-lex (cdr xs))))))

; ---------- concatenate Lyndon words -> De Bruijn sequence ----------
(define (de-bruijn alphabet n)
  (let ((lyndons (sort-lex (lyndon-words alphabet n))))
    (foldl append nil lyndons)))

; ---------- sliding windows of length k in seq (cyclic) ----------
(define (cyclic-windows seq k)
  (let* ((len (length seq))
         (extended (append seq (take seq (- k 1)))))
    (labels (((collect i acc)
      (cond ((>= i len) (reverse acc))
            (t (collect (+ i 1)
                 (cons (take (drop extended i) k) acc))))))
      (collect 0 nil))))

; ============================================================
; B(2,3) — binary alphabet, window length 3
; ============================================================
(print "--- B(2,3): binary De Bruijn sequence ---")

(define alph2 (list 0 1))
(define db23 (de-bruijn alph2 3))
(print (list (quote sequence) db23))

(check "B(2,3)-length-is-8" (= (length db23) 8))

(define wins23 (cyclic-windows db23 3))
(define num-wins (length wins23))
(check "B(2,3)-has-8-cyclic-windows" (= num-wins 8))

; all 8 three-bit patterns must appear
(define (window-value w)
  (+ (* (nth 0 w) 4) (* (nth 1 w) 2) (nth 2 w)))

(define seen-vals (map window-value wins23))
(define sorted-vals (sort (lambda (x y) (< x y)) seen-vals))
(check "B(2,3)-all-8-windows-distinct" (equal sorted-vals (seq 0 7)))
(check "B(2,3)-window-count-8" (= (length (dedup seen-vals)) 8))

; ============================================================
; B(3,2) — ternary alphabet, window length 2
; ============================================================
(print "--- B(3,2): ternary De Bruijn sequence ---")

(define alph3 (list 0 1 2))
(define db32 (de-bruijn alph3 2))
(print (list (quote sequence) db32))

(check "B(3,2)-length-is-9" (= (length db32) 9))

(define wins32 (cyclic-windows db32 2))
(check "B(3,2)-has-9-cyclic-windows" (= (length wins32) 9))

; all 9 windows must be distinct
(define (pair-value p)
  (+ (* (car p) 3) (cadr p)))
(define seen-pairs (map pair-value wins32))
(define sorted-pairs (sort (lambda (x y) (< x y)) seen-pairs))
(check "B(3,2)-all-9-windows-distinct" (equal sorted-pairs (seq 0 8)))

; ============================================================
; formula verification: number of Lyndon words counted by formula
; For binary length 3: lengths dividing 3 are {1, 3}
; For each d|n, count of Lyndon words length d over k symbols =
; (1/d) * sum_{e|d} mu(e) * k^(d/e); necklace-count evaluates this
; count of aperiodic necklaces for d = n.
; ============================================================

(define (moebius n)
  (cond ((= n 1) 1)
        (t (labels (((go p cnt rem)
              (cond ((= (mod rem p) 0)
                     (let ((d (truncate (/ rem p))))
                       (cond ((= (mod d p) 0) 0)
                             (t (go (+ p 1) (+ cnt 1) d)))))
                    ((> (* p p) rem)
                     (cond ((> rem 1)
                            ; one final prime factor: total count is cnt+1,
                            ; moebius = (-1)^(cnt+1)
                            (cond ((= (mod cnt 2) 1) 1) (t -1)))
                           (t (cond ((= (mod cnt 2) 0) 1) (t -1)))))
                    (t (go (+ p 1) cnt rem)))))
             (go 2 0 n)))))

(define (necklace-count k n)
  (labels (((d-sum d acc)
    (cond ((> d n) acc)
          ((= (mod n d) 0)
           (d-sum (+ d 1) (+ acc (* (moebius d) (^ k (truncate (/ n d)))))))
          (t (d-sum (+ d 1) acc)))))
    (/ (d-sum 1 0) n)))

(check "Lyndon-count-2-3-equals-formula" (= (necklace-count 2 3) 2))
(check "Lyndon-count-2-5-equals-formula" (= (necklace-count 2 5) 6))
(check "Lyndon-count-3-2-equals-formula" (= (necklace-count 3 2) 3))

; moebius values, including a four-prime case that exercises the sign
(check "moebius: mu(210) = 1 (four distinct primes)"
       (= (moebius 210) 1))
(check "moebius: mu(7) = -1 (prime)"
       (= (moebius 7) -1))
(check "moebius: mu(30) = -1 (three distinct primes)"
       (= (moebius 30) -1))
(check "moebius: mu(12) = 0 (square factor)"
       (= (moebius 12) 0))
(check "moebius: mu(1) = 1"
       (= (moebius 1) 1))

(print "=== De Bruijn Sequences Done ===")
