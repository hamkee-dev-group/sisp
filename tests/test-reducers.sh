#!/bin/bash
# Regression test: CL-compatible reducers for + * / - /=

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SISP="$SCRIPT_DIR/../src/sisp"
PASS=0
FAIL=0

if [ ! -x "$SISP" ]; then
    echo "ERROR: sisp binary not found at $SISP"
    exit 1
fi

run_exact() {
    local name="$1"
    local input="$2"
    local expected="$3"

    output=$(printf '%s\n(quit 0)\n' "$input" | timeout 3s "$SISP" 2>&1)
    result=$(echo "$output" | grep -oP '(?<=: ).*' | tail -1)

    if [ "$result" = "$expected" ]; then
        echo "PASS: $name"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $name (expected '$expected', got '$result')"
        FAIL=$((FAIL + 1))
    fi
}

echo "=== REDUCER TESTS ==="

# (+) identity
run_exact "add-zero-args" "(+)" "0"
# (*) identity
run_exact "mul-zero-args" "(*)" "1"

# (- x) unary negation
run_exact "sub-unary-pos" "(- 5)" "-5"
run_exact "sub-unary-neg" "(- -3)" "3"
run_exact "sub-unary-zero" "(- 0)" "0"
run_exact "sub-unary-rational" "(- 3/4)" "-3/4"

# (- a b ...) left-associative subtraction
run_exact "sub-binary" "(- 10 3)" "7"
run_exact "sub-variadic" "(- 10 3 2)" "5"
run_exact "sub-negative-result" "(- 1 5)" "-4"
run_exact "sub-rational" "(- 3/4 1/4)" "1/2"

# (/ x) unary reciprocal
run_exact "div-unary-int" "(/ 4)" "1/4"
run_exact "div-unary-rational" "(/ 2/3)" "3/2"
run_exact "div-unary-one" "(/ 1)" "1"

# (/ a b c) variadic division
run_exact "div-variadic" "(/ 120 2 3 4)" "5"
run_exact "div-binary" "(/ 12 3)" "4"
run_exact "div-to-rational" "(/ 1 3)" "1/3"

# (/= ...) pairwise distinct
run_exact "numneq-two-diff" "(/= 1 2)" "t"
run_exact "numneq-two-same" "(/= 1 1)" "nil"
run_exact "numneq-three-diff" "(/= 1 2 3)" "t"
run_exact "numneq-three-dup" "(/= 1 2 1)" "nil"
run_exact "numneq-rational" "(/= 1/2 1/3)" "t"
run_exact "numneq-cross-type" "(/= 1 1/1)" "nil"
run_exact "numneq-negative" "(/= -1 -2 -3)" "t"
run_exact "numneq-neg-dup" "(/= -1 1 -1)" "nil"

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
