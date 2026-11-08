; Tests for (read-file), (write-file), (append-file). Uses a temp path.
(print "=== FILE IO TESTS ===")

(write-file "/tmp/sisp-fileio-test.txt" "hello world")
(if (equal (read-file "/tmp/sisp-fileio-test.txt") "hello world")
    (print "PASS: write-read-roundtrip") (print "FAIL: write-read-roundtrip"))
(if (equal (length (read-file "/tmp/sisp-fileio-test.txt")) 11)
    (print "PASS: read-preserves-length") (print "FAIL: read-preserves-length"))

; write-file returns the byte count
(if (equal (write-file "/tmp/sisp-fileio-test.txt" "abc") 3)
    (print "PASS: write-returns-count") (print "FAIL: write-returns-count"))

; append
(append-file "/tmp/sisp-fileio-test.txt" "def")
(if (equal (read-file "/tmp/sisp-fileio-test.txt") "abcdef")
    (print "PASS: append") (print "FAIL: append"))

; overwrite with empty string, then empty read
(write-file "/tmp/sisp-fileio-test.txt" "")
(if (equal (read-file "/tmp/sisp-fileio-test.txt") "")
    (print "PASS: empty-file-roundtrip") (print "FAIL: empty-file-roundtrip"))
(if (equal (length (read-file "/tmp/sisp-fileio-test.txt")) 0)
    (print "PASS: empty-file-length") (print "FAIL: empty-file-length"))

(print "=== FILE IO TESTS DONE ===")
