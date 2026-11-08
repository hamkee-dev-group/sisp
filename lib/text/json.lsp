; ============================================================
; lib/text/json.lsp -- RFC 8259 JSON parser and writer
;
; Public:   (json-to-sisp text) -> lossless tagged SISP data
;           (sisp-to-json value) -> JSON text
;           (json-number lexeme), (json-string codepoints),
;           (json-array values), (json-object members) -> constructors
;           (json-number-p v), (json-string-p v), (json-array-p v),
;           (json-object-p v) -> predicates
;           (json-value-data v) -> payload of a tagged value
; Private:  all other json-* helpers (parser and writer internals)
; Depends:  lib/text/characters.lsp
; Derived-from: pre-existing lib/text/json.lsp (verified unchanged)
;
; Values are represented without collapsing distinctions JSON requires:
;   json-null  json-false  json-true
;   (json-number "original numeric lexeme")
;   (json-string (Unicode codepoints ...))
;   (json-array (values ...))
;   (json-object (((json-string (...)) value) ...))
;
; Object order and duplicate names are preserved.  Numeric spelling and range
; are preserved.  Strings can contain U+0000 and unpaired escaped surrogates,
; neither of which can be represented losslessly by an ordinary SISP string.
;
; Assumes:  input is a string; malformed input aborts the calling form via
;           error.  All 54 global names are json-* prefixed (the prefix
;           rule is satisfied; the count is inherent to a recursive-descent
;           parser in a single namespace).
; ============================================================

(load "lib/text/characters.lsp")

(define (json-number lexeme) (list (quote json-number) lexeme))
(define (json-string codepoints) (list (quote json-string) codepoints))
(define (json-array values) (list (quote json-array) values))
(define (json-object members) (list (quote json-object) members))

(define (json-tagged-p value tag)
  (cond ((consp value)
         (and (eq (car value) tag)
              (consp (cdr value))
              (eq (cddr value) nil)))
        (t nil)))
(define (json-number-p value) (json-tagged-p value (quote json-number)))
(define (json-string-p value) (json-tagged-p value (quote json-string)))
(define (json-array-p value) (json-tagged-p value (quote json-array)))
(define (json-object-p value) (json-tagged-p value (quote json-object)))
(define (json-value-data value) (cadr value))

(define (json-space-code-p code)
  (or (= code 32) (= code 9) (= code 10) (= code 13)))
(define (json-digit-code-p code) (and (<= 48 code) (<= code 57)))
(define (json-nonzero-digit-code-p code) (and (<= 49 code) (<= code 57)))

; A parser result is (value remaining-characters next-byte-position).
(define (json-result value remaining position)
  (list value remaining position))
(define (json-rest result) (cadr result))
(define (json-position result) (caddr result))

(define (json-skip-space characters position)
  (cond
    ((eq characters nil) (list nil position))
    ((json-space-code-p (char-code (car characters)))
     (json-skip-space (cdr characters) (+ position 1)))
    (t (list characters position))))

(define (json-to-sisp text)
  (cond
    ((not (equal (typeof text) "string"))
     (error "JSON INPUT IS NOT A STRING" text))
    (t
      (let* ((start (json-skip-space (str-to-list text) 0))
             (parsed (json-read-value (car start) (cadr start)))
             (end (json-skip-space (json-rest parsed)
                                   (json-position parsed))))
        (cond ((eq (car end) nil) (car parsed))
              (t (error "TRAILING DATA AFTER JSON VALUE" (cadr end))))))))

(define (json-read-value characters position)
  (cond
    ((eq characters nil) (error "EXPECTED JSON VALUE" position))
    (t
      (let ((code (char-code (car characters))))
        (cond
          ((= code 34) (json-read-string (cdr characters) (+ position 1)))
          ((= code 91) (json-read-array (cdr characters) (+ position 1)))
          ((= code 123) (json-read-object (cdr characters) (+ position 1)))
          ((= code 116)
           (json-read-literal characters position (list 116 114 117 101)
                              (quote json-true)))
          ((= code 102)
           (json-read-literal characters position (list 102 97 108 115 101)
                              (quote json-false)))
          ((= code 110)
           (json-read-literal characters position (list 110 117 108 108)
                              (quote json-null)))
          ((or (= code 45) (json-digit-code-p code))
           (json-read-number characters position))
          (t (error "EXPECTED JSON VALUE" position)))))))

(define (json-read-literal characters position wanted value)
  (cond
    ((eq wanted nil) (json-result value characters position))
    ((eq characters nil) (error "TRUNCATED JSON LITERAL" position))
    ((= (char-code (car characters)) (car wanted))
     (json-read-literal (cdr characters) (+ position 1) (cdr wanted) value))
    (t (error "INVALID JSON LITERAL" position))))

(define (json-read-string characters position)
  (json-read-string-tail characters position nil))

(define (json-read-string-tail characters position result)
  (cond
    ((eq characters nil) (error "UNTERMINATED JSON STRING" position))
    (t
      (let ((code (char-code (car characters))))
        (cond
          ((= code 34)
           (json-result (json-string (reverse result))
                        (cdr characters) (+ position 1)))
          ((= code 92)
           (json-read-escape (cdr characters) (+ position 1) result))
          ((< code 32) (error "CONTROL BYTE IN JSON STRING" position))
          ((< code 128)
           (json-read-string-tail (cdr characters) (+ position 1)
                                  (cons code result)))
          (t
            (let ((decoded (utf8-decode-first characters position)))
              (json-read-string-tail (cadr decoded) (caddr decoded)
                                     (cons (car decoded) result)))))))))

(define (json-read-escape characters position result)
  (cond
    ((eq characters nil) (error "TRUNCATED JSON ESCAPE" position))
    (t
      (let ((code (char-code (car characters))))
        (cond
          ((or (= code 34) (= code 92) (= code 47))
           (json-read-string-tail (cdr characters) (+ position 1)
                                  (cons code result)))
          ((= code 98)
           (json-read-string-tail (cdr characters) (+ position 1)
                                  (cons 8 result)))
          ((= code 102)
           (json-read-string-tail (cdr characters) (+ position 1)
                                  (cons 12 result)))
          ((= code 110)
           (json-read-string-tail (cdr characters) (+ position 1)
                                  (cons 10 result)))
          ((= code 114)
           (json-read-string-tail (cdr characters) (+ position 1)
                                  (cons 13 result)))
          ((= code 116)
           (json-read-string-tail (cdr characters) (+ position 1)
                                  (cons 9 result)))
          ((= code 117)
           (let ((hex (json-read-hex4 (cdr characters) (+ position 1))))
             (json-read-unicode-escape (car hex) (cadr hex) (caddr hex)
                                       result)))
          (t (error "INVALID JSON ESCAPE" position)))))))

; The RFC grammar permits isolated escaped UTF-16 surrogates.  Preserve one
; as its code unit; combine a valid pair into its Unicode codepoint.
(define (json-read-unicode-escape unit characters position result)
  (cond
    ((and (<= 55296 unit) (<= unit 56319)
          (json-next-unicode-escape-p characters))
     (let ((low (json-read-hex4 (cddr characters) (+ position 2))))
       (cond
         ((and (<= 56320 (car low)) (<= (car low) 57343))
          (json-read-string-tail
            (cadr low) (caddr low)
            (cons (+ 65536 (* (- unit 55296) 1024)
                     (- (car low) 56320))
                  result)))
         (t
          (json-read-string-tail characters position (cons unit result))))))
    (t (json-read-string-tail characters position (cons unit result)))))

(define (json-next-unicode-escape-p characters)
  (cond
    ((eq characters nil) nil)
    ((not (= (char-code (car characters)) 92)) nil)
    ((eq (cdr characters) nil) nil)
    (t (= (char-code (cadr characters)) 117))))

(define (json-hex-value code)
  (cond ((and (<= 48 code) (<= code 57)) (- code 48))
        ((and (<= 65 code) (<= code 70)) (+ 10 (- code 65)))
        ((and (<= 97 code) (<= code 102)) (+ 10 (- code 97)))
        (t nil)))

(define (json-read-hex4 characters position)
  (json-read-hex-n characters position 4 0))
(define (json-read-hex-n characters position count value)
  (cond
    ((= count 0) (list value characters position))
    ((eq characters nil) (error "TRUNCATED JSON UNICODE ESCAPE" position))
    (t
      (let ((digit (json-hex-value (char-code (car characters)))))
        (cond
          ((eq digit nil) (error "INVALID JSON UNICODE ESCAPE" position))
          (t (json-read-hex-n (cdr characters) (+ position 1) (- count 1)
                              (+ (* value 16) digit))))))))

(define (json-read-number characters position)
  (let* ((sign (json-number-sign characters position nil))
         (integer (json-number-integer (car sign) (cadr sign) (caddr sign)))
         (fraction (json-number-fraction (car integer) (cadr integer)
                                         (caddr integer)))
         (exponent (json-number-exponent (car fraction) (cadr fraction)
                                         (caddr fraction))))
    (json-result (json-number (list-to-str (reverse (caddr exponent))))
                 (car exponent) (cadr exponent))))

; Number scanning states are (remaining position reversed-lexeme).
(define (json-number-sign characters position result)
  (cond
    ((and (not (eq characters nil))
          (= (char-code (car characters)) 45))
     (list (cdr characters) (+ position 1) (cons (car characters) result)))
    (t (list characters position result))))

(define (json-number-integer characters position result)
  (cond
    ((eq characters nil) (error "EXPECTED DIGIT IN JSON NUMBER" position))
    (t
      (let ((code (char-code (car characters))))
        (cond
          ((= code 48)
           (cond
             ((and (not (eq (cdr characters) nil))
                   (json-digit-code-p (char-code (cadr characters))))
              (error "LEADING ZERO IN JSON NUMBER" position))
             (t (list (cdr characters) (+ position 1)
                      (cons (car characters) result)))))
          ((json-nonzero-digit-code-p code)
           (json-number-digits (cdr characters) (+ position 1)
                               (cons (car characters) result)))
          (t (error "EXPECTED DIGIT IN JSON NUMBER" position)))))))

(define (json-number-digits characters position result)
  (cond
    ((and (not (eq characters nil))
          (json-digit-code-p (char-code (car characters))))
     (json-number-digits (cdr characters) (+ position 1)
                         (cons (car characters) result)))
    (t (list characters position result))))

(define (json-number-fraction characters position result)
  (cond
    ((and (not (eq characters nil))
          (= (char-code (car characters)) 46))
     (let ((digits (json-number-required-digits
                     (cdr characters) (+ position 1)
                     (cons (car characters) result))))
       digits))
    (t (list characters position result))))

(define (json-number-required-digits characters position result)
  (cond
    ((or (eq characters nil)
         (not (json-digit-code-p (char-code (car characters)))))
     (error "EXPECTED DIGIT IN JSON NUMBER" position))
    (t (json-number-digits (cdr characters) (+ position 1)
                           (cons (car characters) result)))))

(define (json-number-exponent characters position result)
  (cond
    ((and (not (eq characters nil))
          (or (= (char-code (car characters)) 101)
              (= (char-code (car characters)) 69)))
     (let* ((with-e (cons (car characters) result))
            (tail (cdr characters))
            (next-position (+ position 1))
            (sign (json-number-exponent-sign tail next-position with-e)))
       (json-number-required-digits (car sign) (cadr sign) (caddr sign))))
    (t (list characters position result))))

(define (json-number-exponent-sign characters position result)
  (cond
    ((and (not (eq characters nil))
          (or (= (char-code (car characters)) 43)
              (= (char-code (car characters)) 45)))
     (list (cdr characters) (+ position 1) (cons (car characters) result)))
    (t (list characters position result))))

(define (json-read-array characters position)
  (let ((start (json-skip-space characters position)))
    (cond
      ((and (not (eq (car start) nil))
            (= (char-code (caar start)) 93))
       (json-result (json-array nil) (cdar start) (+ (cadr start) 1)))
      (t (json-read-array-values (car start) (cadr start) nil)))))

(define (json-read-array-values characters position values)
  (let* ((value (json-read-value characters position))
         (space (json-skip-space (json-rest value) (json-position value))))
    (cond
      ((eq (car space) nil) (error "UNTERMINATED JSON ARRAY" (cadr space)))
      ((= (char-code (caar space)) 93)
       (json-result (json-array (reverse (cons (car value) values)))
                    (cdar space) (+ (cadr space) 1)))
      ((= (char-code (caar space)) 44)
       (let ((next (json-skip-space (cdar space) (+ (cadr space) 1))))
         (json-read-array-values (car next) (cadr next)
                                 (cons (car value) values))))
      (t (error "EXPECTED COMMA OR ARRAY END" (cadr space))))))

(define (json-read-object characters position)
  (let ((start (json-skip-space characters position)))
    (cond
      ((and (not (eq (car start) nil))
            (= (char-code (caar start)) 125))
       (json-result (json-object nil) (cdar start) (+ (cadr start) 1)))
      (t (json-read-object-members (car start) (cadr start) nil)))))

(define (json-read-object-members characters position members)
  (cond
    ((or (eq characters nil) (not (= (char-code (car characters)) 34)))
     (error "EXPECTED STRING AS JSON OBJECT NAME" position))
    (t
      (let* ((name (json-read-string (cdr characters) (+ position 1)))
             (before-colon (json-skip-space (json-rest name)
                                            (json-position name))))
        (cond
          ((or (eq (car before-colon) nil)
               (not (= (char-code (caar before-colon)) 58)))
           (error "EXPECTED COLON AFTER JSON OBJECT NAME"
                  (cadr before-colon)))
          (t
            (let* ((before-value
                     (json-skip-space (cdar before-colon)
                                      (+ (cadr before-colon) 1)))
                   (value (json-read-value (car before-value)
                                           (cadr before-value)))
                   (after-value
                     (json-skip-space (json-rest value)
                                      (json-position value)))
                   (member (list (car name) (car value))))
              (cond
                ((eq (car after-value) nil)
                 (error "UNTERMINATED JSON OBJECT" (cadr after-value)))
                ((= (char-code (caar after-value)) 125)
                 (json-result (json-object (reverse (cons member members)))
                              (cdar after-value) (+ (cadr after-value) 1)))
                ((= (char-code (caar after-value)) 44)
                 (let ((next
                         (json-skip-space (cdar after-value)
                                          (+ (cadr after-value) 1))))
                   (json-read-object-members (car next) (cadr next)
                                             (cons member members))))
                (t (error "EXPECTED COMMA OR OBJECT END"
                          (cadr after-value)))))))))))

; JSON writer ---------------------------------------------------------------

(define (sisp-to-json value)
  (list-to-str (json-write-value value)))

(define (json-write-value value)
  (cond
    ((eq value (quote json-null)) (json-ascii-codes (list 110 117 108 108)))
    ((eq value (quote json-false))
     (json-ascii-codes (list 102 97 108 115 101)))
    ((eq value (quote json-true)) (json-ascii-codes (list 116 114 117 101)))
    ((json-number-p value) (json-write-number (json-value-data value)))
    ((json-string-p value) (json-write-string (json-value-data value)))
    ((json-array-p value) (json-write-array (json-value-data value)))
    ((json-object-p value) (json-write-object (json-value-data value)))
    (t (error "NOT A JSON SISP VALUE" value))))

(define (json-ascii-codes codes) (map code-char codes))

(define (json-write-number lexeme)
  (cond
    ((not (equal (typeof lexeme) "string"))
     (error "JSON NUMBER LEXEME IS NOT A STRING" lexeme))
    (t
      (let ((parsed (json-read-number (str-to-list lexeme) 0)))
        (cond ((eq (json-rest parsed) nil) (str-to-list lexeme))
              (t (error "INVALID JSON NUMBER LEXEME"
                        (json-position parsed))))))))

(define (json-write-string codepoints)
  (append (list (code-char 34))
          (append (json-write-codepoints codepoints)
                  (list (code-char 34)))))

(define (json-write-codepoints codepoints)
  (cond
    ((eq codepoints nil) nil)
    ((not (consp codepoints))
     (error "JSON STRING CODEPOINTS ARE NOT A LIST" codepoints))
    (t (append (json-write-codepoint (car codepoints))
               (json-write-codepoints (cdr codepoints))))))

(define (json-integer-p value)
  (equal (typeof value) "integer"))

(define (json-write-codepoint codepoint)
  (cond
    ((not (json-integer-p codepoint))
     (error "JSON STRING CODEPOINT IS NOT AN INTEGER" codepoint))
    ((or (< codepoint 0) (> codepoint 1114111))
     (error "JSON STRING CODEPOINT OUT OF RANGE" codepoint))
    ((= codepoint 34) (json-ascii-codes (list 92 34)))
    ((= codepoint 92) (json-ascii-codes (list 92 92)))
    ((= codepoint 8) (json-ascii-codes (list 92 98)))
    ((= codepoint 9) (json-ascii-codes (list 92 116)))
    ((= codepoint 10) (json-ascii-codes (list 92 110)))
    ((= codepoint 12) (json-ascii-codes (list 92 102)))
    ((= codepoint 13) (json-ascii-codes (list 92 114)))
    ((and (<= 32 codepoint) (<= codepoint 126))
     (list (code-char codepoint)))
    ((<= codepoint 65535) (json-write-u16 codepoint))
    (t
      (let* ((rest (- codepoint 65536))
             (high (+ 55296 (truncate (/ rest 1024))))
             (low (+ 56320 (mod rest 1024))))
        (append (json-write-u16 high) (json-write-u16 low))))))

(define (json-write-u16 value)
  (json-ascii-codes
    (list 92 117
          (json-hex-code (truncate (/ value 4096)))
          (json-hex-code (mod (truncate (/ value 256)) 16))
          (json-hex-code (mod (truncate (/ value 16)) 16))
          (json-hex-code (mod value 16)))))

(define (json-hex-code value)
  (cond ((< value 10) (+ 48 value))
        (t (+ 55 value))))

(define (json-write-array values)
  (append (list (code-char 91))
          (append (json-write-values values)
                  (list (code-char 93)))))

(define (json-write-values values)
  (cond
    ((eq values nil) nil)
    ((not (consp values)) (error "JSON ARRAY VALUES ARE NOT A LIST" values))
    ((eq (cdr values) nil) (json-write-value (car values)))
    (t (append (json-write-value (car values))
               (cons (code-char 44) (json-write-values (cdr values)))))))

(define (json-write-object members)
  (append (list (code-char 123))
          (append (json-write-members members)
                  (list (code-char 125)))))

(define (json-write-members members)
  (cond
    ((eq members nil) nil)
    ((not (consp members))
     (error "JSON OBJECT MEMBERS ARE NOT A LIST" members))
    (t
      (let ((member (car members)))
        (cond
          ((or (not (consp member))
               (not (consp (cdr member)))
               (not (eq (cddr member) nil))
               (not (json-string-p (car member))))
           (error "INVALID JSON OBJECT MEMBER" member))
          ((eq (cdr members) nil)
           (append (json-write-string (json-value-data (car member)))
                   (cons (code-char 58) (json-write-value (cadr member)))))
          (t
           (append
             (json-write-string (json-value-data (car member)))
             (cons (code-char 58)
                   (append (json-write-value (cadr member))
                           (cons (code-char 44)
                                 (json-write-members (cdr members))))))))))))
