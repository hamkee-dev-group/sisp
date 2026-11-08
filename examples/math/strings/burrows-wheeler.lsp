; ============================================================
; name: burrows-wheeler
; subject: strings
; capability: text transform
; status: healthy
; canonical-home: none
; notes: Canonical banana vector; hardcoded $a-z alphabet.
; ============================================================
; ============================================================
; Burrows-Wheeler Transform (BWT) and Inverse BWT
; BWT of "banana$" -> "annb$aa". Inverse recovers original.
; Verify round-trip on several strings.
; ============================================================

(print "=== Burrows-Wheeler Transform ===")


; Character ordering via position in alphabet
(define *alphabet* (str-to-list "$abcdefghijklmnopqrstuvwxyz"))

(define (char-index ch alpha i)
  (cond ((eq alpha nil) -1)
        ((equal (car alpha) ch) i)
        (t (char-index ch (cdr alpha) (+ i 1)))))

(define (char-lt a b)
  (let ((ia (char-index a *alphabet* 0))
        (ib (char-index b *alphabet* 0)))
    (< ia ib)))

(define (char-gt a b)
  (let ((ia (char-index a *alphabet* 0))
        (ib (char-index b *alphabet* 0)))
    (> ia ib)))

; Lexicographic list comparison
(define (list-lt a b)
  (cond ((eq a nil) (not (eq b nil)))
        ((eq b nil) nil)
        ((char-lt (car a) (car b)) t)
        ((char-gt (car a) (car b)) nil)
        (t (list-lt (cdr a) (cdr b)))))

; ---------- BWT Forward ----------
(define (rotate1 lst)
  (append (cdr lst) (list (car lst))))

(define (all-rotations lst n i current acc)
  (cond ((= i n) acc)
        (t (all-rotations lst n (+ i 1) (rotate1 current)
             (cons current acc)))))

(define (bwt s)
  (let ((chars (str-to-list s)))
    (let* ((n (length chars))
           (rots (all-rotations chars n 0 chars nil))
           (sorted (sort list-lt rots)))
      (map (lambda (rot) (last rot)) sorted))))

; ---------- BWT Inverse ----------
(define (build-lf-simple last first n)
  (map (lambda (i) (lf-simple last first n i))
       (seq 0 (- n 1))))

(define (lf-simple last first n i)
  (let ((ch (nth i last)))
    (+ (count-before last i ch 0)
       (first-index first n ch 0))))

(define (count-before lst limit ch i)
  (cond ((= i limit) 0)
        ((equal (nth i lst) ch)
         (+ 1 (count-before lst limit ch (+ i 1))))
        (t (count-before lst limit ch (+ i 1)))))

(define (first-index col n ch i)
  (cond ((= i n) 0)
        ((equal (nth i col) ch) i)
        (t (first-index col n ch (+ i 1)))))

(define (inv-bwt last-col)
  (let* ((n (length last-col))
         (first-col (sort (lambda (a b) (< (char-index a *alphabet* 0)
                                           (char-index b *alphabet* 0)))
                          last-col))
         (lf (build-lf-simple last-col first-col n))
         (row (first-index first-col n "$" 0))
         (chars (inv-iter first-col lf n row 0)))
    (reverse chars)))

(define (inv-iter first lf n row count)
  (cond ((= count n) nil)
        (t (cons (nth row first)
                 (inv-iter first lf n (nth row lf) (+ count 1))))))

; ---------- Tests ----------
(define (test-roundtrip s)
  (let ((b (bwt s))
        (r (inv-bwt b)))
    (print (list (quote roundtrip) s (quote =) (equal r (str-to-list s))))
    (equal r (str-to-list s))))

(define s1 "banana$")
(define b1 (bwt s1))
(print "Original:" s1)
(print "BWT (last column):" b1)

(check "bwt-banana-pos0-a" (equal (nth 0 b1) "a"))
(check "bwt-banana-pos3-b" (equal (nth 3 b1) "b"))
(check "bwt-banana-pos4-$" (equal (nth 4 b1) "$"))
(check "bwt-banana-pos6-a" (equal (nth 6 b1) "a"))
; sorted rotations of banana$ are $banana, a$banan, ana$ban, anana$b,
; banana$, na$bana, nana$ba -- their last column is annb$aa
(check "bwt-banana-full-last-column"
  (equal b1 (str-to-list "annb$aa")))

(define r1 (inv-bwt b1))
(print "Recovered from BWT:" r1)
(check "banana-roundtrip" (equal r1 (str-to-list s1)))

(check "roundtrip-abracadabra"
  (equal (inv-bwt (bwt "abracadabra$")) (str-to-list "abracadabra$")))
(check "roundtrip-mississippi"
  (equal (inv-bwt (bwt "mississippi$")) (str-to-list "mississippi$")))
(check "roundtrip-alphabet"
  (equal (inv-bwt (bwt "abcdefgh$")) (str-to-list "abcdefgh$")))

(print "=== BWT Done ===")
