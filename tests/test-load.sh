#!/bin/bash
. "$(dirname "$0")/lib.sh"
# Regression test: load builtin (builtin_load)

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

    output=$(printf '%s\n(quit 0)\n' "$input" | sisp_timeout 3s "$SISP" -i 2>&1)

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

# Successful LOAD is truthy, so it can participate in ordinary control flow.
run_test "load-returns-t" \
    "(if (load 'load-fixture) (print \"LOAD-RETURN-T\") (print \"LOAD-RETURN-NIL\"))" \
    "LOAD-RETURN-T" "LOAD-RETURN-NIL"

# Error path: missing file emits FILE NOT FOUND
run_test "load-missing-file" "(load 'no-such-file)" "FILE NOT FOUND" ""

# A malformed dependency must make LOAD fail, stop the calling file, and
# produce a non-zero process status.  Earlier definitions in the dependency
# may already have run; this test protects the crucial no-silent-continuation
# contract until parsing itself becomes transactional.
LOAD_TMP=$(mktemp -d "${TMPDIR:-/tmp}/sisp-load-test.XXXXXX")
trap 'rm -rf "$LOAD_TMP"' EXIT
BAD_LOAD="$LOAD_TMP/malformed.lsp"
LOAD_CALLER="$LOAD_TMP/caller.lsp"
ERROR_FILE="$LOAD_TMP/uncaught-error.lsp"

# Ordinary files use the same fail-fast rule as loaded files.
printf '%s\n' \
    '(print "BEFORE-ERROR")' \
    '(car 0)' \
    '(print "AFTER-ERROR")' > "$ERROR_FILE"
output=$(sisp_timeout 3s "$SISP" "$ERROR_FILE" </dev/null 2>&1)
status=$?
if [ "$status" -eq 0 ] || ! echo "$output" | grep -q "BEFORE-ERROR" || \
        echo "$output" | grep -q "AFTER-ERROR"; then
    echo "FAIL: file-errors-fail-fast"
    FAIL=$((FAIL + 1))
else
    echo "PASS: file-errors-fail-fast"
    PASS=$((PASS + 1))
fi

printf '%s\n' \
    '(define loaded-before-error 1)' \
    '(define malformed (list 1 2' > "$BAD_LOAD"
printf '(load "%s")\n(print "CALLER-CONTINUED")\n' "$BAD_LOAD" > "$LOAD_CALLER"

output=$(sisp_timeout 3s "$SISP" "$LOAD_CALLER" </dev/null 2>&1)
status=$?
if [ "$status" -eq 0 ]; then
    echo "FAIL: malformed-load-status (expected non-zero exit)"
    FAIL=$((FAIL + 1))
elif ! echo "$output" | grep -q "LOAD FAILED"; then
    echo "FAIL: malformed-load-error (expected LOAD FAILED)"
    FAIL=$((FAIL + 1))
elif echo "$output" | grep -q "CALLER-CONTINUED"; then
    echo "FAIL: malformed-load-aborts-caller"
    FAIL=$((FAIL + 1))
else
    echo "PASS: malformed-load-aborts-caller"
    PASS=$((PASS + 1))
fi

# LOAD failures remain ordinary catchable SISP errors when recovery is explicit.
printf '(print (catch (load "%s") (lambda (e) (car e))))\n(print "CAUGHT-CONTINUED")\n' \
    "$BAD_LOAD" > "$LOAD_CALLER"
output=$(sisp_timeout 3s "$SISP" "$LOAD_CALLER" </dev/null 2>&1)
status=$?
if [ "$status" -ne 0 ] || ! echo "$output" | grep -q "CAUGHT-CONTINUED"; then
    echo "FAIL: malformed-load-catchable"
    FAIL=$((FAIL + 1))
else
    echo "PASS: malformed-load-catchable"
    PASS=$((PASS + 1))
fi

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
