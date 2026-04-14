#!/bin/bash
# Regression test: lone '-' and malformed signed number tokenization

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SISP="$SCRIPT_DIR/../src/sisp"
PASS=0
FAIL=0

if [ ! -x "$SISP" ]; then
    echo "ERROR: sisp binary not found at $SISP"
    exit 1
fi

run_test() {
    local name="$1"
    local input="$2"
    local expect_pattern="$3"   # grep pattern that MUST appear (empty to skip)
    local reject_pattern="$4"   # grep pattern that must NOT appear (empty to skip)

    output=$(printf '%s\n(quit 0)\n' "$input" | timeout 3s "$SISP" 2>&1)

    if [ -n "$expect_pattern" ] && ! echo "$output" | grep -q -- "$expect_pattern"; then
        echo "FAIL: $name (expected '$expect_pattern' in output)"
        FAIL=$((FAIL + 1))
        return
    fi

    if [ -n "$reject_pattern" ] && echo "$output" | grep -q -- "$reject_pattern"; then
        echo "FAIL: $name (output contained '$reject_pattern')"
        FAIL=$((FAIL + 1))
        return
    fi

    echo "PASS: $name"
    PASS=$((PASS + 1))
}

echo "=== SIGNED TOKEN TESTS ==="

# Lone '-' is the identifier minus, not integer 0
run_test "quoted-minus-is-symbol" "(quote -)" "^: -" ""

# Functional '-' still works as subtraction
run_test "minus-as-function" "(- 5 3)" "^: 2" ""

# Negative integers parse correctly
run_test "negative-integer" "(quote -1)" "^: -1" ""

# Negative rationals parse correctly
run_test "negative-rational" "(quote -1/2)" "^: -1/2" ""

# '--1' is a single identifier, not split into '-' and '-1'
run_test "double-minus-single-token" "(print (quote --1))" "--1" ""

# '-/2' is a single identifier, not split
run_test "minus-slash-single-token" "(print (quote -/2))" "-/2" ""

# '1-2' is rejected (not partially parsed as integer 1)
run_test "digit-minus-digit-rejected" "1-2" "PARSER ERROR" ""

# '1/-' is rejected (not parsed as rational 1/0)
run_test "rational-bare-minus-denom-rejected" "1/-" "PARSER ERROR" ""

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
