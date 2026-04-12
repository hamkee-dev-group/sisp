#!/bin/bash
# Regression test: REPL must exit cleanly when stdin reaches EOF
# instead of looping with unbounded parser errors.

SISP="../src/sisp"
PASS=0
FAIL=0

if [ ! -x "$SISP" ]; then
    echo "ERROR: sisp binary not found at $SISP"
    exit 1
fi

run_test() {
    local name="$1"
    local input="$2"
    local expect_exit="$3"
    local reject_pattern="$4"

    output=$(printf '%s' "$input" | timeout 3s "$SISP" 2>&1)
    status=$?

    if [ $status -ne "$expect_exit" ]; then
        echo "FAIL: $name (exit $status, expected $expect_exit)"
        FAIL=$((FAIL + 1))
        return
    fi

    if [ -n "$reject_pattern" ] && echo "$output" | grep -q "$reject_pattern"; then
        echo "FAIL: $name (output contained '$reject_pattern')"
        FAIL=$((FAIL + 1))
        return
    fi

    echo "PASS: $name"
    PASS=$((PASS + 1))
}

echo "=== EOF HANDLING TESTS ==="

# Empty stdin should exit immediately, no errors
run_test "empty-stdin-exits" "" 0 "PARSER ERROR"

# Valid expression then EOF should produce result and exit
run_test "valid-then-eof" "(+ 1 2)" 0 "PARSER ERROR"

# Incomplete expression then EOF should not loop
run_test "incomplete-expr-eof" "(+ 1" 0 ""

# Parse error followed by EOF should not loop
run_test "parse-error-eof" ")" 0 ""

# Multiple expressions then EOF
run_test "multi-expr-eof" "(+ 1 2)
(* 3 4)" 0 "PARSER ERROR"

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
