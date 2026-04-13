#!/bin/bash
# SISP Test Runner — runs all .lsp test files and reports results
# Usage: ./tests/run-all.sh   (from any directory)

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SISP="$SCRIPT_DIR/../src/sisp"
PASS=0
FAIL=0
ERRORS=0

if [ ! -x "$SISP" ]; then
    echo "ERROR: sisp binary not found at $SISP"
    echo "Build first with: make"
    exit 1
fi

for test_file in "$SCRIPT_DIR"/test-*.lsp; do
    echo ""
    echo "========================================"
    echo "Running: $(basename "$test_file")"
    echo "========================================"
    output=$(echo '(quit 0)' | "$SISP" "$test_file" 2>&1)
    status=$?
    echo "$output"

    p=$(echo "$output" | grep -oi "PASS:" | wc -l | tr -d ' ')
    f=$(echo "$output" | grep -oi "FAIL:" | wc -l | tr -d ' ')
    PASS=$((PASS + p))
    FAIL=$((FAIL + f))

    if [ $status -ne 0 ]; then
        echo "*** $(basename "$test_file") exited with status $status ***"
        ERRORS=$((ERRORS + 1))
    fi
done

# Shell-level regression tests
for sh_test in test-dump.sh test-load.sh; do
    echo ""
    echo "========================================"
    echo "Running: $sh_test"
    echo "========================================"
    output=$(cd "$SCRIPT_DIR" && bash "$SCRIPT_DIR/$sh_test" 2>&1)
    status=$?
    echo "$output"

    p=$(echo "$output" | grep -oi "PASS:" | wc -l | tr -d ' ')
    f=$(echo "$output" | grep -oi "FAIL:" | wc -l | tr -d ' ')
    PASS=$((PASS + p))
    FAIL=$((FAIL + f))

    if [ $status -ne 0 ]; then
        echo "*** $sh_test exited with status $status ***"
        ERRORS=$((ERRORS + 1))
    fi
done

echo ""
echo "========================================"
echo "         TEST SUMMARY"
echo "========================================"
echo "PASSED: $PASS"
echo "FAILED: $FAIL"
echo "ERRORS: $ERRORS"
echo "TOTAL:  $((PASS + FAIL))"
echo "========================================"

if [ $FAIL -gt 0 ] || [ $ERRORS -gt 0 ]; then
    exit 1
fi
exit 0
