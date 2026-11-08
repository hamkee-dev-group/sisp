; Delimited-record codec parses and writes simple CSV-style rows without quoted fields.
(define (dr-split-chars chars delimiter current fields)
  (cond ((eq chars nil) (reverse (cons (list-to-str (reverse current)) fields)))
        ((equal (car chars) delimiter)
         (dr-split-chars (cdr chars) delimiter nil
           (cons (list-to-str (reverse current)) fields)))
        (t (dr-split-chars (cdr chars) delimiter (cons (car chars) current) fields))))
(define (dr-split line delimiter)
  (dr-split-chars (str-to-list line) delimiter nil nil))
(define (dr-join fields delimiter)
  (cond ((eq fields nil) "") ((eq (cdr fields) nil) (car fields))
        (t (cat (car fields) (cat delimiter (dr-join (cdr fields) delimiter))))))
(define (dr-parse-lines lines delimiter)
  (map (lambda (line) (dr-split line delimiter)) lines))
(define (dr-write-lines rows delimiter)
  (map (lambda (row) (dr-join row delimiter)) rows))
; Round-trip: writing a row and splitting it again is the identity, as long
; as no field contains the delimiter (this codec has no quoting).
(define (dr-round-trip row delimiter)
  (dr-split (dr-join row delimiter) delimiter))

(print "=== Delimited records ===")
(print "split:" (dr-split "ada,42,admin" ","))
(print "write:" (dr-write-lines '(("a" "b") ("1" "2")) ","))
(print "parse:" (dr-parse-lines '("x;y" "3;4") ";"))

; ---- splitting ----
(check "split a three-field row"
       (equal (dr-split "ada,42,admin" ",") '("ada" "42" "admin")))
(check "a row with no delimiter is one field"
       (equal (dr-split "ada" ",") '("ada")))
(check "the empty line is one empty field"
       (equal (dr-split "" ",") '("")))
(check "an empty field in the middle is preserved"
       (equal (dr-split "a,,b" ",") '("a" "" "b")))
(check "a leading delimiter yields a leading empty field"
       (equal (dr-split ",a" ",") '("" "a")))
(check "a trailing delimiter yields a trailing empty field"
       (equal (dr-split "a," ",") '("a" "")))
(check "a row of only delimiters is all empty fields"
       (equal (dr-split ",,"  ",") '("" "" "")))
(check "the delimiter is configurable"
       (equal (dr-split "x;y" ";") '("x" "y")))
(check "a non-delimiter separator is just data"
       (equal (dr-split "x;y" ",") '("x;y")))

; ---- joining ----
(check "join a three-field row"
       (equal (dr-join '("ada" "42" "admin") ",") "ada,42,admin"))
(check "join a single field adds no delimiter"
       (equal (dr-join '("ada") ",") "ada"))
(check "join of no fields is the empty string"
       (equal (dr-join nil ",") ""))
(check "join preserves empty fields"
       (equal (dr-join '("a" "" "b") ",") "a,,b"))

; ---- round trips ----
(check "round trip: ordinary row"
       (equal (dr-round-trip '("ada" "42" "admin") ",") '("ada" "42" "admin")))
(check "round trip: empty fields"
       (equal (dr-round-trip '("" "a" "") ",") '("" "a" "")))
(check "round trip: single field"
       (equal (dr-round-trip '("solo") ",") '("solo")))
(check "round trip holds for every sample row"
       (every (lambda (row) (equal (dr-round-trip row ";") row))
              '(("a" "b") ("1" "2" "3") ("" "") ("only"))))

; ---- whole files ----
(check "write-lines renders each row"
       (equal (dr-write-lines '(("a" "b") ("1" "2")) ",") '("a,b" "1,2")))
(check "parse-lines splits each row"
       (equal (dr-parse-lines '("x;y" "3;4") ";") '(("x" "y") ("3" "4"))))
(check "parse-lines inverts write-lines"
       (equal (dr-parse-lines (dr-write-lines '(("a" "b") ("1" "2")) ",") ",")
              '(("a" "b") ("1" "2"))))
(check "every parsed row has the same field count"
       (equal (map length (dr-parse-lines '("a,b,c" "1,2,3" ",,") ",")) '(3 3 3)))

(print "=== Delimited records done ===")

