#!/bin/bash
. "$(dirname "$0")/lib.sh"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SISP="$SCRIPT_DIR/../src/sisp"
TEST_DIR=$(mktemp -d "${TMPDIR:-/tmp}/sisp-reader-files.XXXXXX")
PASS=0
FAIL=0

trap 'rm -rf "$TEST_DIR"' EXIT

run_success() {
	local name="$1"
	local file="$2"
	local expected="$3"
	local output
	local status

	output=$("$SISP" "$file" </dev/null 2>&1)
	status=$?
	if [ "$status" -ne 0 ] || ! echo "$output" | grep -q "$expected"; then
		echo "FAIL: $name"
		echo "$output"
		FAIL=$((FAIL + 1))
		return
	fi
	echo "PASS: $name"
	PASS=$((PASS + 1))
}

run_failure() {
	local name="$1"
	local file="$2"
	local expected="$3"
	local output
	local status

	output=$("$SISP" "$file" </dev/null 2>&1)
	status=$?
	if [ "$status" -eq 0 ] || ! echo "$output" | grep -q "$expected"; then
		echo "FAIL: $name"
		echo "$output"
		FAIL=$((FAIL + 1))
		return
	fi
	echo "PASS: $name"
	PASS=$((PASS + 1))
}

cat > "$TEST_DIR/multiline.lsp" <<'EOF'
(if (= (+
          1
          (* 2
             3))
       7)
    (print "READER-MULTILINE-OK")
    (print "READER-MULTILINE-FAIL"))
EOF
run_success "reader-file-multiline" "$TEST_DIR/multiline.lsp" "READER-MULTILINE-OK"

{
	printf '(if (= '
	for ((index = 0; index < 512; index++)); do
		printf '(+ 1 '
	done
	printf '0'
	for ((index = 0; index < 512; index++)); do
		printf ')'
	done
	printf ' 512) (print "READER-DEEP-OK") (print "READER-DEEP-FAIL"))\n'
} > "$TEST_DIR/deep.lsp"
run_success "reader-file-deep-form" "$TEST_DIR/deep.lsp" "READER-DEEP-OK"

long_identifier=$(printf 'readerlong%.0s' {1..80})
long_string=$(printf 'x%.0s' {1..512})
cat > "$TEST_DIR/long-tokens.lsp" <<EOF
(define $long_identifier 42)
(if (= $long_identifier 42)
    (print "READER-LONG-NAME-OK")
    (print "READER-LONG-NAME-FAIL"))
(if (= (length "$long_string") 512)
    (print "READER-LONG-STRING-OK")
    (print "READER-LONG-STRING-FAIL"))
EOF
run_success "reader-file-growable-tokens" "$TEST_DIR/long-tokens.lsp" "READER-LONG-STRING-OK"

cat > "$TEST_DIR/nested-reader.lsp" <<'EOF'
(if (= (eval (car (read-from-string "(+ 2 3)"))) 5)
    (print "READER-NESTED-OK")
    (print "READER-NESTED-FAIL"))
EOF
run_success "reader-file-nested-reader" "$TEST_DIR/nested-reader.lsp" "READER-NESTED-OK"

printf '(print "READER-EOF-OK")' > "$TEST_DIR/eof.lsp"
run_success "reader-file-token-at-eof" "$TEST_DIR/eof.lsp" "READER-EOF-OK"

cat > "$TEST_DIR/bad-escape.lsp" <<'EOF'
(+
  1
  "bad\q")
EOF
run_failure "reader-file-error-location" "$TEST_DIR/bad-escape.lsp" \
	"$TEST_DIR/bad-escape.lsp:3:.*INVALID STRING ESCAPE"

printf '(list\n  1\n  2' > "$TEST_DIR/unterminated.lsp"
run_failure "reader-file-unterminated-location" "$TEST_DIR/unterminated.lsp" \
	"$TEST_DIR/unterminated.lsp:3:.*UNTERMINATED LIST"

printf '9%.0s' {1..80} > "$TEST_DIR/long-number.lsp"
run_failure "reader-file-long-number-reaches-parser" "$TEST_DIR/long-number.lsp" \
	"INTEGER OVERFLOW"

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"

if [ "$FAIL" -ne 0 ]; then
	exit 1
fi
