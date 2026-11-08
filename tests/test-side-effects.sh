#!/bin/bash
. "$(dirname "$0")/lib.sh"
# Direct tests for side-effect builtins.
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

    output=$(printf '%s\n(quit 0)\n' "$input" | sisp_timeout 3s "$SISP" -i 2>&1)

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

# --- print tests ---
# print outputs its argument unquoted to stdout
run_test "print-integer" "(print 42)" "42" ""

# print outputs a string without surrounding quotes
run_test "print-string" "(print \"hello\")" "hello" ""

# print outputs multiple arguments separated by spaces
run_test "print-multi" "(print 1 2 3)" "1 2 3" ""

# float and matrix output use the shared shortest-round-trip double printer
run_test "print-float" "(print 1.5)" "1.5" ""
run_test "print-matrix" "(print [1.5 2.25])" "\\[1\\.5 2\\.25\\]" ""

# print returns the last value it printed
run_test "print-returns-last-value" \
    "(typeof (print 1))" '"integer"' ""

# with no values, print has the ordinary SISP result nil
run_test "print-empty-returns-nil" "(print)" "nil" ""

# a builtin value prints its name, which evaluates back to that builtin
run_test "builtin-value-is-readable" "car" "car" "#<builtin"

# doc can install and then print a manual entry without putting it in the heap
run_test "doc-register-and-read" \
    '(progn (doc sample "sample documentation") (doc sample))' \
    "sample documentation" ""

# Trace exists only in debug builds. Debug builds trace both the tail expression
# reached by eval_cons's loop and identifier resolution in eval.
trace_probe=$(printf '%s\n(quit 0)\n' '(trace missing)' | sisp_timeout 3s "$SISP" -i 2>&1)
if echo "$trace_probe" | grep -q -- "OBJECT 'trace' NOT FOUND"; then
    run_test "trace-debug-only" "(trace missing)" "OBJECT 'trace' NOT FOUND" ""
else
    run_test "trace-tail-expression" \
        '(progn (define (trace-inc x) (+ x 1)) (trace t) (trace-inc 2) (trace nil))' \
        "; eval (+ x 1)" ""
    run_test "trace-identifier-value" \
        '(progn (define (trace-inc x) (+ x 1)) (trace t) (trace-inc 2) (trace nil))' \
        "; x => 2" ""
fi

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
