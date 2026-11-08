#!/bin/bash
# P4: checked integer arithmetic — overflow raises a consistent error, and
# ordinary in-range arithmetic is unaffected (eval and VM alike).
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SISP="$SCRIPT_DIR/../src/sisp"

check() {  # name  input  expected-substring
    out=$(printf '%s\n(quit 0)\n' "$2" | "$SISP" -i 2>&1)
    if echo "$out" | grep -q "$3"; then
        echo "PASS: $1"
    else
        echo "FAIL: $1 (expected '$3', got: $out)"
    fi
}

echo "=== OVERFLOW TESTS ==="
check "add-overflow"     "(+ 9223372036854775807 1)"                       "INTEGER OVERFLOW"
check "mul-overflow"     "(* 9223372036854775807 2)"                       "INTEGER OVERFLOW"
check "sub-overflow"     "(- -9223372036854775808 1)"                      "INTEGER OVERFLOW"
check "prod-overflow"    "(* 3037000500 3037000500)"                       "INTEGER OVERFLOW"
# in-range arithmetic must NOT falsely trip
check "add-ok"           "(+ 1000000 2000000)"                             ": 3000000"
check "mul-ok"           "(* 1000000000 3)"                                ": 3000000000"
echo "=== OVERFLOW TESTS DONE ==="
exit 0
