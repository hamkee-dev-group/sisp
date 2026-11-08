; ============================================================
; name: huffman
; file: algorithms/strings/huffman.lsp
; subject: strings
; capability: compression prefix-codes
; aliases: "huffman coding" "optimal prefix code" "entropy coding"
; signature: (huffman-tree pairs) -> tree
;            (huffman-codes tree) -> alist (sym . bits)
;            (huffman-encode codes syms) -> bit list
;            (huffman-decode tree bits) -> symbol list
; complexity: O(n log n) construction, O(L) encode/decode
; see-also: suffix-array rabin-karp
; derived-from: Huffman 1952; CLRS 16.3
; status: example (tested)
; checks: 13 checks incl. known code tables, Kraft equality, prefix
;         property, encode->decode roundtrip; run: src/sisp <file>
; ============================================================
; Huffman coding: repeatedly merge the two lightest trees; a code is
; the path from the root (0 = left, 1 = right).  pairs = ((sym w) ...).
; Tree = (leaf sym w) | (node left right w).

(define (hf-w x)
  (cond ((eq (car x) (quote leaf)) (caddr x))
        (t (cadddr x))))

(define (hf-leaf-p x) (eq (car x) (quote leaf)))

(define (huffman-merge trees)
  (cond ((eq (cdr trees) nil) (car trees))
        (t (let ((s (sort (lambda (u v) (< (hf-w u) (hf-w v))) trees)))
             (huffman-merge
               (cons (list (quote node) (car s) (cadr s)
                           (+ (hf-w (car s)) (hf-w (cadr s))))
                     (cddr s)))))))

(define (huffman-tree pairs)
  "the Huffman tree for ((sym weight) ...)"
  (huffman-merge (map (lambda (p) (list (quote leaf) (car p) (cadr p)))
                      pairs)))

(define (hf-codes-walk x prefix)
  (cond ((hf-leaf-p x) (list (list (cadr x) (reverse prefix))))
        (t (append (hf-codes-walk (cadr x) (cons 0 prefix))
                   (hf-codes-walk (caddr x) (cons 1 prefix))))))

(define (huffman-codes tree)
  "alist of (symbol bit-list) for every leaf"
  (hf-codes-walk tree nil))

(define (hf-code-of codes s)
  (cond ((null codes) (error "HUFFMAN-ENCODE: UNKNOWN SYMBOL" s))
        ((equal (caar codes) s) (cadr (car codes)))
        (t (hf-code-of (cdr codes) s))))

(define (huffman-encode codes syms)
  "bit list for a symbol sequence"
  (cond ((null syms) nil)
        (t (append (hf-code-of codes (car syms))
                   (huffman-encode codes (cdr syms))))))

; walk bits down the tree; on a leaf, emit and restart at the root
(define (huffman-decode-walk tree node bits acc)
  (cond ((hf-leaf-p node)
         (huffman-decode-walk tree tree bits (cons (cadr node) acc)))
        ((null bits)
         (cond ((eq node tree) (reverse acc))
               (t (error "HUFFMAN-DECODE: TRUNCATED BITSTREAM" bits))))
        (t (huffman-decode-walk tree
                                (cond ((= (car bits) 0) (cadr node))
                                      (t (caddr node)))
                                (cdr bits) acc))))

(define (huffman-decode tree bits)
  "symbol sequence for a bit list"
  (huffman-decode-walk tree tree bits nil))

; ---- check helpers (defined before use) ----

; is bit list p a prefix of q
(define (hf-prefix-p p q)
  (cond ((null p) t)
        ((null q) nil)
        ((= (car p) (car q)) (hf-prefix-p (cdr p) (cdr q)))
        (t nil)))

; no code is a proper prefix of another
(define (hf-prefix-free-p codes)
  (cond ((null codes) t)
        (t (and (hf-none-prefix (cadr (car codes)) (cdr codes))
                (hf-prefix-free-p (cdr codes))))))

(define (hf-none-prefix code codes)
  (cond ((null codes) t)
        ((hf-prefix-p code (cadr (car codes))) nil)
        ((hf-prefix-p (cadr (car codes)) code) nil)
        (t (hf-none-prefix code (cdr codes)))))

; Kraft sum in exact rationals: sum 2^(-len)
(define (hf-kraft codes)
  (foldl (lambda (acc c) (+ acc (/ 1 (^ 2 (length (cadr c))))))
         0 codes))

(define (hf-weighted-length codes pairs)
  (foldl (lambda (acc p)
           (+ acc (* (cadr p)
                     (length (hf-code-of codes (car p))))))
         0 pairs))

(define (hf-fill pairs)
  pairs)

; ---- checks ----

; tiny table with a forced structure: a1 b1 c2 d4 -> d=(1) c=(01) a=(000) b=(001)
(define hf-tiny-tree
  (huffman-tree (list (list (quote a) 1) (list (quote b) 1)
                      (list (quote c) 2) (list (quote d) 4))))
(define hf-tiny-codes (huffman-codes hf-tiny-tree))

(check "huffman: tiny table code lengths (d=1, c=2, a=b=3)"
       (and (= (length (hf-code-of hf-tiny-codes (quote d))) 1)
            (= (length (hf-code-of hf-tiny-codes (quote c))) 2)
            (= (length (hf-code-of hf-tiny-codes (quote a))) 3)
            (= (length (hf-code-of hf-tiny-codes (quote b))) 3)))

; exact bit patterns depend on tie order (a and b both weigh 1); the
; deterministic output of this implementation is asserted here.  sort is
; a stable merge sort, so equal-weight trees keep their input order.  Any
; other tie order gives the same Huffman code up to branch labelling --
; same lengths, same optimal weighted length, still prefix-free.
(check "huffman: tiny table exact codes (d=(1), c=(0 1), b=(0 0 1), a=(0 0 0))"
       (and (equal (hf-code-of hf-tiny-codes (quote d)) (list 1))
            (equal (hf-code-of hf-tiny-codes (quote c)) (list 0 1))
            (equal (hf-code-of hf-tiny-codes (quote b)) (list 0 0 1))
            (equal (hf-code-of hf-tiny-codes (quote a)) (list 0 0 0))))

(check "huffman: tiny table weighted length = 14 (optimal)"
       (= (hf-weighted-length hf-tiny-codes
                              (list (list (quote a) 1) (list (quote b) 1)
                                    (list (quote c) 2) (list (quote d) 4)))
          14))

; fixture: a3 b5 c8 d14 e20 -> lengths 4,4,3,2,1
(define hf-fixture-pairs
  (list (list (quote a) 3) (list (quote b) 5) (list (quote c) 8)
        (list (quote d) 14) (list (quote e) 20)))
(define hf-fixture-codes (huffman-codes (huffman-tree hf-fixture-pairs)))

(check "huffman: corpus fixture code lengths (a=b=4, c=3, d=2, e=1)"
       (and (= (length (hf-code-of hf-fixture-codes (quote a))) 4)
            (= (length (hf-code-of hf-fixture-codes (quote b))) 4)
            (= (length (hf-code-of hf-fixture-codes (quote c))) 3)
            (= (length (hf-code-of hf-fixture-codes (quote d))) 2)
            (= (length (hf-code-of hf-fixture-codes (quote e))) 1)))

(check "huffman: corpus fixture weighted length = 104 (optimal)"
       (= (hf-weighted-length hf-fixture-codes hf-fixture-pairs) 104))

(check "huffman: prefix property (tiny)"
       (hf-prefix-free-p hf-tiny-codes))

(check "huffman: prefix property (fixture)"
       (hf-prefix-free-p hf-fixture-codes))

(check "huffman: Kraft equality = 1 (tiny)"
       (= (hf-kraft hf-tiny-codes) 1))

(check "huffman: Kraft equality = 1 (fixture)"
       (= (hf-kraft hf-fixture-codes) 1))

(check "huffman: encode then decode roundtrip"
       (equal (huffman-decode hf-tiny-tree
                              (huffman-encode hf-tiny-codes
                                              (list (quote a) (quote d) (quote b)
                                                    (quote c) (quote a))))
              (list (quote a) (quote d) (quote b) (quote c) (quote a))))

(check "huffman: encode of aba = 000 001 000"
       (equal (huffman-encode hf-tiny-codes (list (quote a) (quote b) (quote a)))
              (list 0 0 0 0 0 1 0 0 0)))

(check "huffman: roundtrip over the whole fixture alphabet"
       (equal (huffman-decode (huffman-tree hf-fixture-pairs)
                              (huffman-encode hf-fixture-codes
                                              (list (quote e) (quote d) (quote c)
                                                    (quote b) (quote a) (quote e))))
              (list (quote e) (quote d) (quote c) (quote b) (quote a) (quote e))))

; (1) is d's complete code, so it decodes; (0) is a proper prefix of
; the codes of a, b and c, so it is a truncated stream.
(check "huffman: truncated bitstream is catchable"
       (equal (car (catch (huffman-decode hf-tiny-tree (list 0))
                          (lambda (e) e)))
              (quote error)))
