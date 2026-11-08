; ============================================================
; name: lz78
; file: algorithms/strings/lz78.lsp
; subject: strings
; capability: compression dictionary-coding
; aliases: "lz78" "dictionary compression" "lempel-ziv 1978"
; signature: (lz78-encode tokens) -> codes  ; codes = ((index token) ...)
;            (lz78-decode codes)  -> tokens
; complexity: O(n^2) encode/decode as written (linear dictionary scan
;             and append), O(n) phrases for an n-token input
; see-also: huffman suffix-array
; derived-from: Ziv & Lempel 1978
; status: example (tested)
; checks: 12 checks incl. known-answer token streams, round-trips on
;         empty/single/runs/alternating/no-repeat/natural inputs,
;         dictionary growth; run: src/sisp <file>
; ============================================================
; LZ78 dictionary compression for arbitrary SISP token sequences.
; The encoder scans the input building a dictionary of phrases.  It
; keeps the longest current phrase already in the dictionary, and when
; the next token would extend it past anything known, it emits a code
; (index token): index is the dictionary position of the current phrase
; (0 = the empty phrase) and token is the extending token.  The new
; phrase (current ++ token) is then added to the dictionary.  A trailing
; phrase that is already fully in the dictionary is flushed as
; (index end), where the sentinel 'end carries no extra token.
;
; Codes are self-describing: the decoder rebuilds the identical
; dictionary in lockstep, so decode(encode(x)) == x for every x.

; index (1-based) of phrase in dictionary, or 0 if absent
(define (lz-index dictionary phrase index)
  (cond ((eq dictionary nil) 0)
        ((equal (car dictionary) phrase) index)
        (t (lz-index (cdr dictionary) phrase (+ index 1)))))

; the phrase stored at a 1-based index; index 0 is the empty phrase
(define (lz-phrase dictionary index)
  (cond ((= index 0) nil)
        (t (nth (- index 1) dictionary))))

(define (lz-encode-loop tokens current dictionary codes)
  (cond ((eq tokens nil)
         ; input exhausted: flush a pending phrase (already in the dict)
         (cond ((eq current nil) (reverse codes))
               (t (reverse (cons (list (lz-index dictionary current 1) (quote end))
                                 codes)))))
        (t (let ((candidate (append current (list (car tokens)))))
             (let ((known (lz-index dictionary candidate 1)))
               (cond ((> known 0)
                      ; candidate already known: keep extending
                      (lz-encode-loop (cdr tokens) candidate dictionary codes))
                     (t
                      ; candidate new: emit code for current, learn candidate
                      (lz-encode-loop (cdr tokens) nil
                        (append dictionary (list candidate))
                        (cons (list (lz-index dictionary current 1) (car tokens))
                              codes)))))))))

(define (lz78-encode tokens)
  "compress a token list into ((index token) ...) codes"
  (lz-encode-loop tokens nil nil nil))

(define (lz-decode-loop codes dictionary output)
  (cond ((eq codes nil) output)
        (t (let ((code (car codes)))
             (let ((phrase (append (lz-phrase dictionary (car code))
                             (cond ((eq (cadr code) (quote end)) nil)
                                   (t (list (cadr code)))))))
               (lz-decode-loop (cdr codes)
                 (cond ((eq (cadr code) (quote end)) dictionary)
                       (t (append dictionary (list phrase))))
                 (append output phrase)))))))

(define (lz78-decode codes)
  "reconstruct the original token list from LZ78 codes"
  (lz-decode-loop codes nil nil))

; ---- check helpers (defined before use) ----

(define (lz-roundtrip x)
  (equal (lz78-decode (lz78-encode x)) x))

; ---- checks ----

; known-answer token stream: the textbook LZ78 example.
; (a b a b a a b): "" a->(0 a); "" b->(0 b); a"ab"->(1 b); a"aa"->(1 a);
; trailing "b" already in dict at index 2 -> (2 end).
(check "lz78: known encode of (a b a b a a b)"
       (equal (lz78-encode (list (quote a) (quote b) (quote a) (quote b)
                                 (quote a) (quote a) (quote b)))
              (list (list 0 (quote a)) (list 0 (quote b)) (list 1 (quote b))
                    (list 1 (quote a)) (list 2 (quote end)))))

; a run of one symbol: "" a->(0 a); a"aa"->(1 a); trailing "a" is index 1.
(check "lz78: known encode of a run (a a a a)"
       (equal (lz78-encode (list (quote a) (quote a) (quote a) (quote a)))
              (list (list 0 (quote a)) (list 1 (quote a)) (list 1 (quote end)))))

; all-distinct tokens: every phrase is new, every code has index 0.
(check "lz78: known encode of distinct (a b c d)"
       (equal (lz78-encode (list (quote a) (quote b) (quote c) (quote d)))
              (list (list 0 (quote a)) (list 0 (quote b))
                    (list 0 (quote c)) (list 0 (quote d)))))

; known-answer decode: decoder rebuilds the textbook example.
(check "lz78: known decode of the textbook codes"
       (equal (lz78-decode (list (list 0 (quote a)) (list 0 (quote b))
                                 (list 1 (quote b)) (list 1 (quote a))
                                 (list 2 (quote end))))
              (list (quote a) (quote b) (quote a) (quote b)
                    (quote a) (quote a) (quote b))))

(check "lz78: round-trip on empty input"
       (lz-roundtrip nil))

(check "lz78: round-trip on a single token"
       (lz-roundtrip (list (quote a))))

(check "lz78: round-trip on a run (a a a a)"
       (lz-roundtrip (list (quote a) (quote a) (quote a) (quote a))))

(check "lz78: round-trip on alternating (a b a b a b a b)"
       (lz-roundtrip (list (quote a) (quote b) (quote a) (quote b)
                           (quote a) (quote b) (quote a) (quote b))))

(check "lz78: round-trip on all-distinct (a b c d)"
       (lz-roundtrip (list (quote a) (quote b) (quote c) (quote d))))

(check "lz78: round-trip on a natural phrase"
       (lz-roundtrip (list (quote the) (quote cat) (quote the)
                           (quote cat) (quote sat))))

(check "lz78: round-trip on a long single-symbol run"
       (lz-roundtrip (list (quote x) (quote x) (quote x) (quote x) (quote x)
                           (quote x) (quote x) (quote x) (quote x) (quote x))))

; the encoder emits one code per learned phrase plus one for a flushed
; tail: (a b a b a a b) learns a, b, ab, aa and flushes b -> 5 codes.
(check "lz78: phrase count of (a b a b a a b) is 5"
       (= (length (lz78-encode (list (quote a) (quote b) (quote a) (quote b)
                                     (quote a) (quote a) (quote b))))
          5))
