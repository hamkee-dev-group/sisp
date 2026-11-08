; ============================================================
; name: exact-cover
; file: algorithms/searching/exact-cover.lsp
; subject: searching
; capability: search backtracking exact-cover
; aliases: "algorithm x" "exact cover" "set cover exact"
; signature: (algx primary rows chosen) -> number of covers; one witness
;            is kept in *witness*
; complexity: exponential worst case (backtracking search)
; see-also: dancing-links
; derived-from: Knuth 2000 ("Dancing Links"); TAOCP 4B 7.2.2.1
; status: example (tested)
; checks: 10 - Langford, dominoes, N-queens; every count verified
; ============================================================
; Exact Cover and Algorithm X (TAOCP Vol 4B, 7.2.2.1)
; Knuth's Algorithm X over native sets: rows are sets, the cover
; condition is disjoint union, and the search is the recursion
; itself. Solves the three problems from the Dancing Links paper
; at small scale — Langford pairs, domino tilings, and N-queens
; with primary/secondary columns — with every count verified.
; ============================================================

(load "lib/collections/relations.lsp")

(print "=== Exact Cover / Algorithm X ===")


; ---------- Algorithm X over sets ----------
; solve: primary = elements that MUST be covered exactly once;
; rows may also contain secondary elements, used at most once
; (enforced by the disjointness filter). Returns the solution
; count and keeps one witness.

(define *witness* nil)
(define (algx primary rows chosen)
  (cond ((equal primary {})
    (setq *witness* chosen)
    1)
    (t (let ((e (find (lambda (x) t) primary)))
      (foldl (lambda (acc r)
        (+ acc
          (algx (diff primary r)
            (filter (lambda (q) (equal (cap q r) {})) rows)
            (cons r chosen))))
        0
        (set-to-list (filter (lambda (r) (in e r)) rows)))))))

; a solved instance really is a partition of the primary elements
(define (covers-exactly rows primary)
  (every (lambda (x)
    (= 1 (foldl (lambda (acc r) (cond ((in x r) (+ acc 1)) (t acc)))
           0 rows)))
    primary))

; ---------- Langford pairs, n = 3 (Knuth's own first example) ----------
; place two copies of 1,2,3 in six slots so that exactly k slots
; lie between the two copies of k. Universe: three symbols and six
; positions; the
; row for "k at i" covers {(s k) (p i) (p i+k+1)}.
(define langford-rows
  (list-to-set (foldl (lambda (acc k)
    (append acc
      (map (lambda (i)
        (list-to-set (list (list (quote s) k)
                         (list (quote p) i)
                         (list (quote p) (+ i (+ k 1))))))
        (filter (lambda (i) (<= (+ i (+ k 1)) 6)) (seq 1 6)))))
    nil (list 1 2 3))))
(define langford-primary
  (union (map (lambda (k) (list (quote s) k)) {1 2 3})
         (map (lambda (i) (list (quote p) i)) {1 2 3 4 5 6})))

(define langford-count (algx langford-primary langford-rows nil))
(print (list (quote langford-solutions) langford-count
             (quote witness) *witness*))
(check "langford-3-has-2-mirror-solutions" (= langford-count 2))
(check "langford-witness-is-exact"
  (covers-exactly *witness* langford-primary))
; the classical statement counts solutions up to reversal: L(3)=1
(check "langford-L3-is-1-up-to-reversal" (= (truncate (/ langford-count 2)) 1))

; ---------- domino tilings of a 2 x 3 board ----------
(define cells23
  (pairs-such-that (lambda (r c) t) {0 1} {0 1 2}))
(define (cell r c) (list r c))
(define domino-rows
  (list-to-set (append
    ; horizontal dominoes
    (foldl (lambda (acc r)
      (append acc (map (lambda (c)
        (list-to-set (list (cell r c) (cell r (+ c 1))))) (list 0 1))))
      nil (list 0 1))
    ; vertical dominoes
    (map (lambda (c) (list-to-set (list (cell 0 c) (cell 1 c))))
      (list 0 1 2)))))
(define domino-count (algx cells23 domino-rows nil))
(print (list (quote domino-2x3-tilings) domino-count))
(check "seven-domino-placements" (= (length domino-rows) 7))
(check "domino-2x3-has-3-tilings" (= domino-count 3))
(check "domino-witness-tiles-the-board"
  (covers-exactly *witness* cells23))

; ---------- N-queens via primary/secondary columns ----------
; files and ranks are primary (exactly one queen each); diagonals
; are secondary (at most one) — exactly Knuth's formulation.
(define (queens-rows n)
  (list-to-set (foldl (lambda (acc i)
    (append acc (map (lambda (j)
      (list-to-set (list (list (quote f) i) (list (quote r) j)
                       (list (quote d) (+ i j))
                       (list (quote a) (- i j)))))
      (seq 0 (- n 1)))))
    nil (seq 0 (- n 1)))))
(define (queens-primary n)
  (union
    (map (lambda (i) (list (quote f) i)) (list-to-set (seq 0 (- n 1))))
    (map (lambda (j) (list (quote r) j)) (list-to-set (seq 0 (- n 1))))))

(define q4 (algx (queens-primary 4) (queens-rows 4) nil))
(define q4-witness *witness*)
(define q5 (algx (queens-primary 5) (queens-rows 5) nil))
(define q6 (algx (queens-primary 6) (queens-rows 6) nil))
(print (list (quote queens) 4 q4 5 q5 6 q6))
(check "four-queens-has-2-solutions" (= q4 2))
(check "five-queens-has-10-solutions" (= q5 10))
(check "six-queens-has-4-solutions" (= q6 4))

; the 4-queens witness places 4 non-attacking queens: all four
; files, ranks, diagonals and antidiagonals distinct.
(define (row-part w tag)
  (map (lambda (r)
    (cadr (find (lambda (x) (eq (car x) tag)) r))) w))
(check "queens-witness-no-two-attack"
  (and (= 4 (length (list-to-set (row-part q4-witness (quote f)))))
       (= 4 (length (list-to-set (row-part q4-witness (quote r)))))
       (= 4 (length (list-to-set (row-part q4-witness (quote d)))))
       (= 4 (length (list-to-set (row-part q4-witness (quote a)))))))

(print "=== Exact Cover Done ===")
