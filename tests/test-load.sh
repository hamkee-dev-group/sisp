#!/bin/bash
# Regression test: load builtin (F_loadfile)

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

echo "=== LOAD TESTS ==="

# Run from tests/ so load-fixture.lsp is found relative to CWD
cd "$SCRIPT_DIR"

# Happy path: (load 'load-fixture) should execute the fixture file
run_test "load-executes-fixture" "(load 'load-fixture)" "LOAD-FIXTURE-OK" ""

# Lowercase normalization: uppercase identifier still finds the file
run_test "load-uppercase-normalized" "(load 'LOAD-FIXTURE)" "LOAD-FIXTURE-OK" ""

# Error path: missing file emits FILE NOT FOUND
run_test "load-missing-file" "(load 'no-such-file)" "FILE NOT FOUND" ""

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
