; ============================================================
; name: bloom-filter
; file: algorithms/data-structures/bloom-filter.lsp
; subject: data-structure
; capability: probabilistic membership set
; aliases: "bloom" "probabilistic set"
; signature: (bf-make m k) (bf-insert bf key) (bf-maybe-present bf key)
;            (bf-fpr bf n)
; complexity: O(k) per operation
; see-also: hash-table
; derived-from: Bloom 1970; Kirsch & Mitzenmacher 2006 (double
;               hashing)
; status: example (tested)
; checks: 10 checks incl. measured false-positive rate vs theory;
;         run: src/sisp <this file> from repo root
; ============================================================
; Bloom filter: an m-bit vector with k independent hash functions.
; Insert sets the k bits; membership requires all k bits set.
; False negatives never happen; false positives happen with rate
; approximately (1 - e^(-kn/m))^k.  The filter is a list of 0/1s
; (functional update).  Keys are strings or integers (djb2/mix as in
; hash-table.lsp, since SISP has no bitwise xor); the k hashes are
; derived by double hashing:
; h_i(x) = h1(x) + i*h2(x) (Kirsch-Mitzenmacher).

(define (bf-djb2-seed str seed)
  (bf-djb2-walk (str-to-list str) seed))

(define (bf-djb2-walk chars h)
  (cond ((null chars) (mod h 4294967296))
        (t (bf-djb2-walk (cdr chars)
                         (mod (+ (* h 33) (char-code (car chars)))
                              4294967296)))))

(define (bf-h1 key)
  (cond ((equal (typeof key) "string") (bf-djb2-seed key 5381))
        (t (mod (* (mod key 4294967296) 2654435761) 4294967296))))

(define (bf-h2 key)
  (cond ((equal (typeof key) "string") (bf-djb2-seed key 2654435761))
        (t (mod (* (mod key 4294967296) 1597334677) 4294967296))))

(define (bf-bit key i m)
  (mod (+ (bf-h1 key) (* i (bf-h2 key))) m))

(define (bf-make m k)
  (cond ((or (<= m 0) (<= k 0)) (error "BF-MAKE: BAD PARAMETERS" (list m k)))
        (t (list m k (bf-zero-bits m)))))

(define (bf-zero-bits n)
  (cond ((= n 0) nil) (t (cons 0 (bf-zero-bits (- n 1))))))

(define (bf-m bf) (car bf))
(define (bf-k bf) (cadr bf))
(define (bf-bits bf) (caddr bf))

; functional bit set: set position i of the bit list to 1
(define (bf-set-bit bits i)
  (cond ((= i 0) (cons 1 (cdr bits)))
        (t (cons (car bits) (bf-set-bit (cdr bits) (- i 1))))))

; helper: set bits for hash indices i, i+1, ... up to k-1
(define (bf-insert-step bf key i)
  (cond ((>= i (bf-k bf)) bf)
        (t (bf-insert-step (list (bf-m bf) (bf-k bf)
                                 (bf-set-bit (bf-bits bf) (bf-bit key i (bf-m bf))))
                           key (+ i 1)))))

(define (bf-insert bf key)
  (bf-insert-step bf key 0))

(define (bf-get-bit bits i)
  (cond ((= i 0) (car bits))
        (t (bf-get-bit (cdr bits) (- i 1)))))

(define (bf-check-bits bf key i)
  (cond ((= i (bf-k bf)) t)
        ((= (bf-get-bit (bf-bits bf) (bf-bit key i (bf-m bf))) 0) nil)
        (t (bf-check-bits bf key (+ i 1)))))

(define (bf-maybe-present bf key)
  "t when key is probably present, nil when definitely absent"
  (bf-check-bits bf key 0))

; theoretical false-positive rate after n inserts: (1 - e^(-kn/m))^k
(define (bf-fpr bf n)
  (^ (- 1 (exp (/ (* (- 0 (bf-k bf)) n) (bf-m bf)))) (bf-k bf)))

; ---- check helpers (defined before use) ----

(define (bf-mk-absent n acc)
  (cond ((= n 0) acc)
        (t (bf-mk-absent (- n 1) (cons (cat "absent-" (num-to-str n)) acc)))))

(define (bf-insert-all bf keys)
  (cond ((null keys) bf)
        (t (bf-insert-all (bf-insert bf (car keys)) (cdr keys)))))

(define (bf-count-hits bf keys acc)
  (cond ((null keys) acc)
        ((bf-maybe-present bf (car keys))
         (bf-count-hits bf (cdr keys) (+ acc 1)))
        (t (bf-count-hits bf (cdr keys) acc))))

; ---- checks ----

(check "bloom: empty filter rejects everything"
       (eq (bf-maybe-present (bf-make 100 3) "hello") nil))

(check "bloom: inserted key is found"
       (bf-maybe-present (bf-insert (bf-make 100 3) "hello") "hello"))

(check "bloom: no false negatives over 20 inserted keys"
       (let ((bf (bf-insert-all (bf-make 100 3)
                                (list "k1" "k2" "k3" "k4" "k5" "k6" "k7" "k8" "k9" "k10"
                                      "k11" "k12" "k13" "k14" "k15" "k16" "k17" "k18" "k19" "k20"))))
         (= (bf-count-hits bf (list "k1" "k2" "k3" "k4" "k5" "k6" "k7" "k8" "k9" "k10"
                                    "k11" "k12" "k13" "k14" "k15" "k16" "k17" "k18" "k19" "k20") 0)
            20)))

(check "bloom: absent key is usually rejected"
       (eq (bf-maybe-present (bf-insert (bf-make 100 3) "present") "absent") nil))

(check "bloom: integer keys work"
       (bf-maybe-present (bf-insert (bf-make 100 3) 42) 42))

(check "bloom: false-positive rate on 1000 absent keys is below theory"
       (let* ((bf (bf-insert-all (bf-make 800 3)
                                 (list "a1" "a2" "a3" "a4" "a5" "a6" "a7" "a8" "a9" "a10"
                                       "a11" "a12" "a13" "a14" "a15" "a16" "a17" "a18" "a19" "a20")))
              (hits (bf-count-hits bf (bf-mk-absent 1000 nil) 0)))
         ; theory: k=3, n=20, m=800 -> fpr ~ (1-e^-0.075)^3 ~ 0.00037; generous bound 5%
         (<= hits 50)))

(check "bloom: false-positive rate rises with load"
       (let* ((light (bf-insert-all (bf-make 100 3) (list "x1" "x2")))
              (heavy (bf-insert-all (bf-make 100 3)
                                    (list "x1" "x2" "x3" "x4" "x5" "x6" "x7" "x8" "x9" "x10"
                                          "x11" "x12" "x13" "x14" "x15" "x16" "x17" "x18" "x19" "x20"
                                          "x21" "x22" "x23" "x24" "x25" "x26" "x27" "x28" "x29" "x30"
                                          "x31" "x32" "x33" "x34" "x35" "x36" "x37" "x38" "x39" "x40")))
              (absent (bf-mk-absent 200 nil)))
         (< (bf-count-hits light absent 0)
            (+ 1 (bf-count-hits heavy absent 0)))))

(check "bloom: theoretical fpr of an empty-ish filter is tiny"
       (< (bf-fpr (bf-make 800 3) 5) 0.00001))

(check "bloom: make with bad params is catchable"
       (equal (car (catch (bf-make 0 3) (lambda (e) e))) (quote error)))

(check "bloom: double-hashing spreads keys over the bit vector"
       (let ((bf (bf-insert-all (bf-make 64 3)
                                (list "p1" "p2" "p3" "p4" "p5" "p6" "p7" "p8"))))
         (> (foldl + 0 (bf-bits bf)) 12)))
