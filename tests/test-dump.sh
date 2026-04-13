#!/bin/bash
# Regression test: dump output contracts for F_dump / dump_object

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

    if [ -n "$expect_pattern" ] && ! echo "$output" | grep -q "$expect_pattern"; then
        echo "FAIL: $name (expected '$expect_pattern' in output)"
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

echo "=== DUMP OUTPUT TESTS ==="

# Default summary path: (dump nil) emits all section headers
run_test "dump-nil-function-cache"    "(dump nil)" "FUNCTION CACHE:" ""
run_test "dump-nil-function-bindings" "(dump nil)" "FUNCTION BINDINGS:" ""
run_test "dump-nil-value-bindings"    "(dump nil)" "VALUE BINDINGS:" ""
run_test "dump-nil-pool-table"        "(dump nil)" "|POOL" ""

# Pool-specific path: (dump 5) shows INT pool stats
run_test "dump-pool-5-header" "(dump 5)" "|INT" ""

# Unsupported pool number: (dump 2) produces no dump output
run_test "dump-unsupported-2-no-cache"    "(dump 2)" "" "FUNCTION CACHE:"
run_test "dump-unsupported-2-no-bindings" "(dump 2)" "" "VALUE BINDINGS:"
run_test "dump-unsupported-2-no-pool"     "(dump 2)" "" "|POOL"

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
