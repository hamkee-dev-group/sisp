; ============================================================
; name: manacher
; file: algorithms/strings/manacher.lsp
; subject: strings
; capability: pattern-matching palindrome
; aliases: "manacher" "longest palindromic substring" "palindrome"
; signature: (manacher s) -> (start length)
; complexity: O(n)
; see-also: suffix-array rabin-karp
; derived-from: Manacher 1975
; status: example (tested)
; checks: 12 - vs naive O(n^2) palindrome check
; ============================================================
; Manacher's Algorithm: Longest Palindromic Substring in O(n)
; Input "ababa" -> "ababa" (5 chars), "babad" -> "bab" (3 chars)
; Compare with naive O(n^2) center expansion for verification.
; ============================================================

(print "=== Manacher's Algorithm ===")


(define (minimum a b) (cond ((< a b) a) (t b)))

; Transform "abc" -> "^#a#b#c#$"
(define (preprocess s)
  (let ((chars (str-to-list s)))
    (let ((n (length chars)))
      (prep-aux chars n (- n 1) (list "$")))))

(define (prep-aux chars n i acc)
  (cond ((< i 0) (cons "^" (cons "#" acc)))
        (t (prep-aux chars n (- i 1)
             (cons (nth i chars) (cons "#" acc))))))

(define (make-zeroes n)
  (map (lambda (ignored) 0) (seq 0 (- n 1))))

(define (list-with xs i v)
  (cond ((= i 0) (cons v (cdr xs)))
        (t (cons (car xs) (list-with (cdr xs) (- i 1) v)))))

; Main Manacher loop
(define (manacher-loop ts p n c r best-c best-r i)
  (cond ((>= i n) (list best-c best-r p))
        (t (let* ((i-mirror (- (* 2 c) i))
                  (init-r (cond ((< i r)
                                 (minimum (- r i) (nth i-mirror p)))
                                (t 0))))
             (let ((radius (expand ts n i init-r)))
               (let ((p-new (list-with p i radius))
                     (new-r (cond ((> (+ i radius) r) (+ i radius)) (t r)))
                     (new-c (cond ((> (+ i radius) r) i) (t c)))
                     (new-best-c (cond ((> radius best-r) i) (t best-c)))
                     (new-best-r (cond ((> radius best-r) radius) (t best-r))))
                 (manacher-loop ts p-new n new-c new-r new-best-c new-best-r (+ i 1))))))))

(define (expand ts n i r)
  (let ((left (- i r 1))
        (right (+ i r 1)))
    (cond ((or (< left 0) (>= right n)) r)
          ((not (equal (nth left ts) (nth right ts))) r)
          (t (expand ts n i (+ r 1))))))

; Convert transformed center/radius to original string (start, length)
(define (original-bounds center radius)
  (list (truncate (/ (- center radius) 2)) radius))

(define (manacher s)
  (let* ((str (preprocess s))
         (n (length str))
         (result (manacher-loop str (make-zeroes n) n 0 0 0 0 1))
         (center (car result))
         (radius (cadr result)))
    (original-bounds center radius)))

; ---------- Naive O(n^2) center expansion ----------
(define (expand-around chars n lo hi)
  (cond ((or (< lo 0) (>= hi n)) (list (+ lo 1) (- hi lo 1)))
        ((equal (nth lo chars) (nth hi chars))
         (expand-around chars n (- lo 1) (+ hi 1)))
        (t (list (+ lo 1) (- hi lo 1)))))

(define (naive-lps s)
  (let ((chars (str-to-list s)))
    (let ((n (length chars)))
      (naive-iter chars n 0 0 0))))

(define (naive-iter chars n i best-start best-len)
  (cond ((>= i n) (list best-start best-len))
        (t (let* ((odd (expand-around chars n i i))
                  (even (expand-around chars n i (+ i 1)))
                  (best (cond ((>= (cadr odd) (cadr even)) odd) (t even)))
                  (bs (car best)) (bl (cadr best))
                  (ns (cond ((> bl best-len) bs) (t best-start)))
                  (nl (cond ((> bl best-len) bl) (t best-len))))
             (naive-iter chars n (+ i 1) ns nl)))))

; ---------- Tests ----------
(define s1 "ababa")
(define r1 (manacher s1))
(print "Input:" s1)
(print "Manacher (start len):" r1)
(check "ababa-len-5" (= (cadr r1) 5))
(check "ababa-matches-naive" (equal r1 (naive-lps s1)))

(define s2 "babad")
(define r2 (manacher s2))
(print "Input:" s2)
(print "Manacher (start len):" r2)
(check "babad-len-3" (= (cadr r2) 3))
(check "babad-len-matches-naive" (= (cadr r2) (cadr (naive-lps s2))))

(define s3 "cbbd")
(define r3 (manacher s3))
(print "Input:" s3)
(print "Manacher (start len):" r3)
(check "cbbd-len-2" (= (cadr r3) 2))
(check "cbbd-matches-naive" (equal r3 (naive-lps s3)))

(define s4 "a")
(check "single-len-1" (= (cadr (manacher s4)) 1))
(check "single-matches-naive" (equal (manacher s4) (naive-lps s4)))

(define s5 "racecar")
(print "Input:" s5)
(print "Manacher:" (manacher s5))
(check "racecar-matches-naive" (equal (manacher s5) (naive-lps s5)))
; the whole 7-character word is itself a palindrome
(check "racecar-len-7" (= (cadr (manacher s5)) 7))

(define s6 "abcdef")
; abcdef has no repeated character, so the longest palindrome is a single letter
(check "no-palin-len1" (= (cadr (manacher s6)) 1))
(check "no-palin-matches-naive" (equal (manacher s6) (naive-lps s6)))

(print "=== Manacher Done ===")
