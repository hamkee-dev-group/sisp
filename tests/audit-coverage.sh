#!/bin/bash
# Builtin coverage audit: verifies that every registered builtin in
# src/funcs.c functions[] has an entry in builtin-coverage.manifest,
# and that no manifest entries refer to stale builtins.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FUNCS="$SCRIPT_DIR/../src/funcs.c"
MANIFEST="$SCRIPT_DIR/builtin-coverage.manifest"
PASS=0
FAIL=0

if [ ! -f "$FUNCS" ]; then
    echo "ERROR: $FUNCS not found"
    exit 1
fi
if [ ! -f "$MANIFEST" ]; then
    echo "ERROR: $MANIFEST not found"
    exit 1
fi

# Extract registered builtin names from functions[] in src/funcs.c.
# Each entry looks like:  \t{"name", F_func},
# We capture the quoted name, then unescape C string backslashes.
registered=$(sed -n '/^const funcs functions\[\]/,/^};/{
  /^[[:space:]]*{"/{ s/.*{"\([^"]*\)".*/\1/p }
}' "$FUNCS" | sed 's/\\\\/\\/g')

# Extract manifest entries (first field of non-comment, non-blank lines).
manifest_names=$(grep -v '^\s*#' "$MANIFEST" | grep -v '^\s*$' | awk '{print $1}')

echo "=== BUILTIN COVERAGE AUDIT ==="

# 1. Every registered builtin must appear in the manifest.
while IFS= read -r builtin; do
    if echo "$manifest_names" | grep -qxF "$builtin"; then
        echo "PASS: '$builtin' in manifest"
        PASS=$((PASS + 1))
    else
        echo "FAIL: '$builtin' registered in functions[] but not in manifest"
        FAIL=$((FAIL + 1))
    fi
done <<< "$registered"

# 2. Every manifest entry must correspond to a registered builtin (no stale entries).
while IFS= read -r entry; do
    if echo "$registered" | grep -qxF "$entry"; then
        echo "PASS: '$entry' not stale"
        PASS=$((PASS + 1))
    else
        echo "FAIL: '$entry' in manifest but not in functions[] (stale)"
        FAIL=$((FAIL + 1))
    fi
done <<< "$manifest_names"

# 3. Non-exception entries must reference existing test files.
while IFS= read -r line; do
    name=$(echo "$line" | awk '{print $1}')
    second=$(echo "$line" | awk '{print $2}')
    [ "$second" = "EXCEPTION" ] && continue
    files=$(echo "$line" | awk '{print $2}')
    IFS=',' read -ra file_list <<< "$files"
    for f in "${file_list[@]}"; do
        if [ -f "$SCRIPT_DIR/$f" ]; then
            echo "PASS: '$name' -> '$f' exists"
            PASS=$((PASS + 1))
        else
            echo "FAIL: '$name' references '$f' which does not exist"
            FAIL=$((FAIL + 1))
        fi
    done
done < <(grep -v '^\s*#' "$MANIFEST" | grep -v '^\s*$')

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
