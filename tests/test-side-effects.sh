#!/bin/bash
# Direct tests for side-effect builtins: gc, print, dump error path
# These builtins produce output or mutate global state, so they need
# shell-level output-capturing tests rather than inline .lsp assertions.

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
    local expect_pattern="$3"
    local reject_pattern="$4"

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

echo "=== SIDE-EFFECT BUILTIN TESTS ==="

# --- gc tests ---
# (gc t) triggers collection and returns t
run_test "gc-returns-t" "(if (equal (gc t) t) (print \"OK\"))" "OK" ""

# (gc) with no real arg still returns t without crashing
run_test "gc-nil-no-crash" "(if (equal (gc nil) t) (print \"OK\"))" "OK" ""

# gc preserves live bindings: define, collect, then verify
run_test "gc-preserves-bindings" \
    "(define x 42) (gc t) (if (equal x 42) (print \"OK\"))" "OK" ""

# --- print tests ---
# print outputs its argument unquoted to stdout
run_test "print-integer" "(print 42)" "42" ""

# print outputs a string without surrounding quotes
run_test "print-string" "(print \"hello\")" "hello" ""

# print outputs multiple arguments separated by spaces
run_test "print-multi" "(print 1 2 3)" "1 2 3" ""

# print returns null (undefined), not a printable value
run_test "print-returns-null" \
    "(typeof (print 1))" "undefined" ""

# --- dump additional coverage ---
# (dump 2) is out of the valid pool range 3-8, produces no dump output
run_test "dump-pool-2-silent" "(dump 2)" "" "|POOL"

# (dump 9) also out of range, no dump output
run_test "dump-pool-9-silent" "(dump 9)" "" "FUNCTION CACHE:"

# (dump 3) shows the ID pool
run_test "dump-pool-3-id" "(dump 3)" "|ID" ""

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
