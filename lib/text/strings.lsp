; ============================================================
; lib/text/strings.lsp -- string utilities
;
; Public:   (str-join parts sep) -> string
;           (string-split text delim) -> list of strings
;           (string-trim text) -> string
;           (string-upcase text) -> string
;           (string-downcase text) -> string
; Private:  string-split-acc, string-trim-left, string-trim-right,
;           string-space-code-p, string-case-map
; Depends:  none (uses builtins cat, null, car, cdr, cond, =, <, >, +, -,
;           str-find, substr, strlen, str-to-list, list-to-str, char-code,
;           code-char, list, reverse, cons, eq)
; Assumes:  all string arguments are strings
; ============================================================

(define (str-join parts sep)
  "concatenate the strings in parts, placing sep between adjacent elements"
  (cond ((null parts) "")
        ((null (cdr parts)) (car parts))
        (t (cat (car parts) (cat sep (str-join (cdr parts) sep))))))

(define (string-split text delim)
  "split text at every occurrence of delim, returning list of substrings"
  (cond ((= (length delim) 0) (error "STRING-SPLIT: EMPTY DELIMITER"))
        (t (string-split-acc text delim 0 nil))))

(define (string-split-acc text delim start acc)
  (let* ((text-len (length text))
         (suffix-len (- text-len start)))
    (cond ((<= suffix-len 0)
           (reverse (cons "" acc)))
          (t
            (let ((pos (str-find (substr start suffix-len text) delim)))
              (cond ((eq pos nil)
                     (reverse (cons (substr start suffix-len text) acc)))
                    (t
                      (let* ((piece (substr start pos text))
                             (next-start (+ start pos (length delim))))
                        (string-split-acc text delim next-start
                                          (cons piece acc))))))))))

(define (string-trim text)
  "remove leading and trailing whitespace (space, tab, newline, return)"
  (let ((start (string-trim-left text 0 (length text))))
    (cond ((= start (length text)) "")
          (t (string-trim-right text start (- (length text) 1))))))

(define (string-trim-left text i n)
  (cond ((>= i n) i)
        ((string-space-code-p (char-code (substr i 1 text)))
         (string-trim-left text (+ i 1) n))
        (t i)))

(define (string-trim-right text left right)
  (cond ((< right left)
         (substr left (- (+ right 1) left) text))
        ((string-space-code-p (char-code (substr right 1 text)))
         (string-trim-right text left (- right 1)))
        (t (substr left (- (+ right 1) left) text))))

(define (string-space-code-p code)
  (or (= code 32) (= code 9) (= code 10) (= code 13)))

(define (string-upcase text)
  "return text with lowercase a-z mapped to uppercase"
  (string-case-map text (lambda (c) (cond ((and (<= 97 c) (<= c 122)) (- c 32)) (t c)))))

(define (string-downcase text)
  "return text with uppercase A-Z mapped to lowercase"
  (string-case-map text (lambda (c) (cond ((and (<= 65 c) (<= c 90)) (+ c 32)) (t c)))))

(define (string-case-map text fn)
  (list-to-str (map (lambda (ch) (code-char (fn (char-code ch))))
                    (str-to-list text))))
