; ============================================================
; lib/text/characters.lsp -- character predicates and UTF-8 decoding
;
; A SISP character is a one-byte string.  The core operations char-code and
; code-char cross the byte/integer boundary; everything else remains SISP.
;
; Public:   (characterp value) -> t | nil
;           (ascii-code-p code) -> t | nil
;           (unicode-codepoint-p code) -> t | nil
;           (unicode-scalar-p code) -> t | nil
;           (utf8-to-codepoints text) -> list of integer codepoints
; Private:  utf8-continuation-p, utf8-decode-first, utf8-require-byte,
;           utf8-decode-two, utf8-decode-three, utf8-decode-four
; Depends:  none (uses builtins typeof, strlen, char-code, code-char,
;           str-to-list)
; Derived-from: pre-existing lib/text/characters.lsp (verified unchanged)
; Assumes:  input is a string of raw bytes; malformed input aborts the
;           calling form via error (overlong, surrogate, out-of-range,
;           truncated, invalid leading byte are all rejected per RFC 3629).
;           SISP strings are NUL-free by interpreter restriction
;           (code-char accepts only 1..255; read-file rejects embedded
;           NUL), so U+0000 never appears in practice.
; ============================================================

(define (characterp value)
  (cond ((equal (typeof value) "string") (= (length value) 1))
        (t nil)))

(define (ascii-code-p code) (and (<= 0 code) (< code 128)))
(define (unicode-codepoint-p code) (and (<= 0 code) (<= code 1114111)))
(define (unicode-scalar-p code)
  (and (unicode-codepoint-p code)
       (or (< code 55296) (> code 57343))))
(define (utf8-continuation-p code) (and (<= 128 code) (<= code 191)))

; Return (codepoint remaining-bytes next-position).  Position is only used
; to make malformed-input errors useful to callers.
(define (utf8-decode-first bytes position)
  (cond
    ((eq bytes nil) (error "TRUNCATED UTF-8" position))
    (t
      (let ((b0 (char-code (car bytes))))
        (cond
          ((< b0 128) (list b0 (cdr bytes) (+ position 1)))
          ((and (<= 194 b0) (<= b0 223))
           (utf8-decode-two bytes position b0))
          ((and (<= 224 b0) (<= b0 239))
           (utf8-decode-three bytes position b0))
          ((and (<= 240 b0) (<= b0 244))
           (utf8-decode-four bytes position b0))
          (t (error "INVALID UTF-8 LEADING BYTE" position)))))))

(define (utf8-require-byte bytes position)
  (cond ((eq bytes nil) (error "TRUNCATED UTF-8" position))
        (t (char-code (car bytes)))))

(define (utf8-decode-two bytes position b0)
  (let* ((tail (cdr bytes))
         (b1 (utf8-require-byte tail (+ position 1))))
    (cond
      ((utf8-continuation-p b1)
       (list (+ (* (- b0 192) 64) (- b1 128))
             (cdr tail) (+ position 2)))
      (t (error "INVALID UTF-8 CONTINUATION BYTE" (+ position 1))))))

(define (utf8-decode-three bytes position b0)
  (let* ((tail1 (cdr bytes))
         (b1 (utf8-require-byte tail1 (+ position 1)))
         (tail2 (cond ((eq tail1 nil) nil) (t (cdr tail1))))
         (b2 (utf8-require-byte tail2 (+ position 2))))
    (cond
      ((not (utf8-continuation-p b1))
       (error "INVALID UTF-8 CONTINUATION BYTE" (+ position 1)))
      ((not (utf8-continuation-p b2))
       (error "INVALID UTF-8 CONTINUATION BYTE" (+ position 2)))
      ((and (= b0 224) (< b1 160))
       (error "OVERLONG UTF-8 SEQUENCE" position))
      ((and (= b0 237) (> b1 159))
       (error "UTF-8 ENCODES A SURROGATE" position))
      (t
       (list (+ (* (- b0 224) 4096)
                (* (- b1 128) 64)
                (- b2 128))
             (cdr tail2) (+ position 3))))))

(define (utf8-decode-four bytes position b0)
  (let* ((tail1 (cdr bytes))
         (b1 (utf8-require-byte tail1 (+ position 1)))
         (tail2 (cond ((eq tail1 nil) nil) (t (cdr tail1))))
         (b2 (utf8-require-byte tail2 (+ position 2)))
         (tail3 (cond ((eq tail2 nil) nil) (t (cdr tail2))))
         (b3 (utf8-require-byte tail3 (+ position 3))))
    (cond
      ((not (utf8-continuation-p b1))
       (error "INVALID UTF-8 CONTINUATION BYTE" (+ position 1)))
      ((not (utf8-continuation-p b2))
       (error "INVALID UTF-8 CONTINUATION BYTE" (+ position 2)))
      ((not (utf8-continuation-p b3))
       (error "INVALID UTF-8 CONTINUATION BYTE" (+ position 3)))
      ((and (= b0 240) (< b1 144))
       (error "OVERLONG UTF-8 SEQUENCE" position))
      ((and (= b0 244) (> b1 143))
       (error "UTF-8 CODEPOINT OUT OF RANGE" position))
      (t
       (list (+ (* (- b0 240) 262144)
                (* (- b1 128) 4096)
                (* (- b2 128) 64)
                (- b3 128))
             (cdr tail3) (+ position 4))))))

(define (utf8-to-codepoints text)
  (labels (((walk bytes position result)
            (cond ((eq bytes nil) (reverse result))
                  (t (let ((decoded (utf8-decode-first bytes position)))
                       (walk (cadr decoded) (caddr decoded)
                             (cons (car decoded) result)))))))
    (walk (str-to-list text) 0 nil)))
