#!/bin/bash
# Regression test: unquote (,) and unquote-splicing (,@) must only be
# legal inside a backquote; at depth 0 they are reader errors. Also
# verifies ,@ rejects non-list values, including improper (dotted) lists,
# with a consistent SPLICE error rather than a downstream failure.

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

echo "=== QUASIQUOTE READER TESTS ==="

# Bare comma and ,@ at top level are reader errors.
run_test "comma-top-level-rejected" ",x" "COMMA OUTSIDE BACKQUOTE" ""
run_test "comma-at-top-level-rejected" ",@x" "COMMA-AT OUTSIDE BACKQUOTE" ""

# Comma inside plain (quote ...) is not inside a backquote — still rejected.
run_test "comma-inside-quote-rejected" "'(,x)" "COMMA OUTSIDE BACKQUOTE" ""
run_test "comma-at-inside-quote-rejected" "'(,@x)" "COMMA-AT OUTSIDE BACKQUOTE" ""

# Depth must fully reset after an error so the next form parses cleanly.
run_test "recovery-after-comma-error" ",x
(+ 1 2)" "^: 3" ""

# Splicing works inside backquote.
run_test "splice-inside-bquote" "(define xs '(2 3))
\`(1 ,@xs 4)" "(1 2 3 4)" "PARSER ERROR"

# ,@ on a non-list (atom) is rejected with the SPLICE error.
run_test "splice-atom-rejected" "\`(,@42)" "SPLICE" "CAR: UNDEFINED"

# ,@ on an improper (dotted) list is rejected with the SPLICE error
# at the dotted tail, not with a downstream CAR/CDR failure.
run_test "splice-improper-list-rejected" "\`(,@(quote (1 . 2)))" "SPLICE: '2' NOT A LIST" "CAR: UNDEFINED"

# Set literals still parse after ,@ is available (no collision with {...}).
run_test "set-literal-still-parses" "{1 2 3}" "^: {1 2 3}" "PARSER ERROR"
run_test "comprehension-still-parses" "{tau : (> tau 0)}" "" "PARSER ERROR"

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
