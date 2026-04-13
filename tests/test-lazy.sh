#!/bin/bash
# Regression test: lazy evaluation toggling (F_lazy / lazy_eval)

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

echo "=== LAZY EVAL TESTS ==="

# Banner output: (lazy t) prints ENABLED
run_test "lazy-enable-banner" "(lazy t)" "LAZY EVAL: ENABLED" ""

# Banner output: (lazy nil) prints DISABLED
run_test "lazy-disable-banner" "(lazy nil)" "LAZY EVAL: DISABLED" ""

# Semantic difference: lazy substitution changes (quote x) inside a function.
# In eager mode, (quote x) returns the symbol x regardless of the argument.
# In lazy mode, sst_lazy_p substitutes the parameter into the body before
# evaluation, so (quote x) becomes (quote <value>) and returns the value.
run_test "lazy-quote-subst" \
    "(define (f x) (quote x)) (lazy t) (f 5)" \
    "^: 5" ""

# Eager mode restores normal behavior: (quote x) returns the symbol x
run_test "eager-quote-symbol" \
    "(define (f x) (quote x)) (lazy nil) (f 5)" \
    "^: x" ""

# Full round-trip: enable, observe lazy result, disable, observe eager result
run_test "lazy-roundtrip-enabled" \
    "(define (f x) (quote x)) (lazy t) (print (f 5)) (lazy nil) (print (f 5))" \
    "LAZY EVAL: ENABLED" ""

run_test "lazy-roundtrip-disabled" \
    "(define (f x) (quote x)) (lazy t) (print (f 5)) (lazy nil) (print (f 5))" \
    "LAZY EVAL: DISABLED" ""

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
