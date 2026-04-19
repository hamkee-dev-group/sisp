#!/bin/bash
# Regression test: the reader must reject Common Lisp forms that are
# explicitly out of scope (see README.md "Reader contract"). None of
# these inputs may parse cleanly, and none may be misread as a set or
# comprehension literal.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SISP="$SCRIPT_DIR/../src/sisp"
PASS=0
FAIL=0

if [ ! -x "$SISP" ]; then
    echo "ERROR: sisp binary not found at $SISP"
    exit 1
fi

# Run $input through sisp and assert:
#   - output contains at least one error marker (PARSER ERROR or NOT FOUND)
#   - output never contains a '{' character (no set/comprehension value
#     is ever printed, which would mean the form was silently accepted
#     as set-like syntax)
run_reject() {
    local name="$1"
    local input="$2"

    output=$(printf '%s\n(quit 0)\n' "$input" | timeout 3s "$SISP" 2>&1)

    if ! echo "$output" | grep -q -E 'PARSER ERROR|NOT FOUND|NOT A FUNCTION'; then
        echo "FAIL: $name (no stable error marker in output: $output)"
        FAIL=$((FAIL + 1))
        return
    fi

    if echo "$output" | grep -q '{'; then
        echo "FAIL: $name (output contained '{' — misread as set/comprehension)"
        FAIL=$((FAIL + 1))
        return
    fi

    echo "PASS: $name"
    PASS=$((PASS + 1))
}

echo "=== READER SUBSET TESTS ==="

# |escaped symbol| — vertical-bar symbol quoting is unsupported
run_reject "vbar-escaped-symbol"        '|foo bar|'
run_reject "vbar-empty"                 '||'

# Package qualifiers (single and double colon) are unsupported
run_reject "package-single-colon"       'foo:bar'
run_reject "package-double-colon"       'foo::bar'
run_reject "package-cl-user"            'cl-user::baz'
run_reject "package-uninterned"         '#:sym'

# # dispatch reader macros are unsupported
run_reject "sharp-vector"               '#(1 2 3)'
run_reject "sharp-function-quote"       "#'foo"
run_reject "sharp-block-comment"        '#|block|#'
run_reject "sharp-block-comment-empty"  '#||#'
run_reject "sharp-char-literal"         '#\c'
run_reject "sharp-binary"               '#b101'
run_reject "sharp-hex"                  '#x1f'
run_reject "sharp-octal"                '#o17'
run_reject "sharp-struct"               '#S(point 1 2)'

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
